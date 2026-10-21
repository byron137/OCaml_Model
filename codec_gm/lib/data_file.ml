open Types

let b2i b = if b then 1 else 0
let i2b s = match s with "1" | "on" | "true" -> true | "0" | "off" | "false" -> false
  | _ -> failwith ("expected 0/1, got " ^ s)

let params_to_kv (p : Params.t) =
  Printf.sprintf "res_h=%d slice_num=%d target=%d table_num=%d rlc=%d palette=%d palette_tf=%d palette_dead=%d,%d rgbg=%d deblock=%d deblock_th=%d"
    p.res_h (Params.slice_count p) p.target_data_num p.table_num (b2i p.rlc_mode)
    (b2i p.palette_mode) p.palette_tf p.palette_dead_lo p.palette_dead_hi (b2i p.rgbg_mode)
    (b2i p.deblock_mode) p.deblock_edge_th

let params_of_kv tokens (base : Params.t) =
  List.fold_left (fun (p : Params.t) tok ->
      match String.index_opt tok '=' with
      | None -> failwith ("bad token " ^ tok)
      | Some i ->
        let k = String.sub tok 0 i and v = String.sub tok (i + 1) (String.length tok - i - 1) in
        let int () = int_of_string v in
        (match k with
         | "res_h" -> { p with res_h = int () }
         | "slice_num" -> { p with slice_num = (match int () with 1 -> Params.S1 | 2 -> S2 | 4 -> S4 | _ -> failwith "slice_num") }
         | "target" -> { p with target_data_num = int () }
         | "table_num" -> { p with table_num = int () }
         | "rlc" -> { p with rlc_mode = i2b v }
         | "palette" -> { p with palette_mode = i2b v }
         | "palette_tf" -> { p with palette_tf = int () }
         | "palette_dead" ->
           (match String.split_on_char ',' v with
            | [ lo; hi ] -> { p with palette_dead_lo = int_of_string lo; palette_dead_hi = int_of_string hi }
            | _ -> failwith "palette_dead")
         | "rgbg" -> { p with rgbg_mode = i2b v }
         | "deblock" -> { p with deblock_mode = i2b v }
         | "deblock_th" -> { p with deblock_edge_th = int () }
         | "w" | "h" | "v1" | "codec_gm" -> p
         | _ -> failwith ("unknown key " ^ k)))
    base tokens

let words s = List.filter (fun x -> x <> "") (String.split_on_char ' ' (String.trim s))

let write path (p : Params.t) ~w ~h (records : Frame.slice_record array) =
  let oc = open_out path in
  Printf.fprintf oc "# codec_gm v1 w=%d h=%d %s\n" w h (params_to_kv p);
  Array.iter (fun (r : Frame.slice_record) ->
      let d = r.decision in
      Printf.fprintf oc "S y=%d s=%d idx=%d cb=%d,%d,%d n=%d\n" r.y r.slice_idx d.Slice.q_order_idx
        d.Slice.codebook_idx.(0) d.Slice.codebook_idx.(1) d.Slice.codebook_idx.(2) (Array.length r.blocks);
      Array.iter (fun blk -> output_string oc (Bitvec.block_to_hex blk); output_char oc '\n') r.blocks)
    records;
  close_out oc

let read path =
  let ic = open_in path in
  let lines = ref [] in
  (try while true do lines := input_line ic :: !lines done with End_of_file -> close_in ic);
  let lines = List.rev !lines in
  let header, rest = match lines with
    | h :: rest when String.length h > 0 && h.[0] = '#' -> (h, rest)
    | _ -> failwith (path ^ ": missing '# codec_gm' header") in
  let kv = List.filter (fun t -> String.contains t '=') (words (String.sub header 1 (String.length header - 1))) in
  let get k = match List.find_opt (fun t -> String.length t > String.length k && String.sub t 0 (String.length k + 1) = k ^ "=") kv with
    | Some t -> int_of_string (String.sub t (String.length k + 1) (String.length t - String.length k - 1))
    | None -> failwith (path ^ ": header lacks " ^ k) in
  let w = get "w" and h = get "h" in
  let p = params_of_kv kv Params.default in
  let slices = ref [] in
  let cur = ref None in
  let flush () = match !cur with
    | None -> ()
    | Some (y, s, d, n, blocks) ->
      if List.length blocks <> n then
        failwith (Printf.sprintf "%s: slice y=%d s=%d declares %d blocks, has %d" path y s n (List.length blocks));
      slices := (y, s, d, Array.of_list (List.rev blocks)) :: !slices; cur := None in
  List.iteri (fun ln line ->
      let line = String.trim line in
      if line = "" || line.[0] = '#' then ()
      else if line.[0] = 'S' then begin
        flush ();
        let f = ref (-1, -1, -1, [| 0; 0; 0 |], -1) in
        List.iter (fun tok ->
            let (y, s, idx, cb, n) = !f in
            match String.index_opt tok '=' with
            | None -> ()
            | Some i ->
              let k = String.sub tok 0 i and v = String.sub tok (i + 1) (String.length tok - i - 1) in
              f := (match k with
                  | "y" -> (int_of_string v, s, idx, cb, n)
                  | "s" -> (y, int_of_string v, idx, cb, n)
                  | "idx" -> (y, s, int_of_string v, cb, n)
                  | "cb" -> (y, s, idx, Array.of_list (List.map int_of_string (String.split_on_char ',' v)), n)
                  | "n" -> (y, s, idx, cb, int_of_string v)
                  | _ -> failwith (Printf.sprintf "%s:%d: unknown field %s" path (ln + 2) k)))
          (words (String.sub line 1 (String.length line - 1)));
        let (y, s, idx, cb, n) = !f in
        if y < 0 || s < 0 || idx < 0 || n < 0 then failwith (Printf.sprintf "%s:%d: incomplete S line" path (ln + 2));
        cur := Some (y, s, { Slice.q_order_idx = idx; codebook_idx = cb }, n, [])
      end
      else begin
        match !cur with
        | None -> failwith (Printf.sprintf "%s:%d: block before any S line" path (ln + 2))
        | Some (y, s, d, n, blocks) ->
          let blk = try Bitvec.block_of_hex ~words:Bitstream.block_words line
            with _ -> failwith (Printf.sprintf "%s:%d: bad block '%s'" path (ln + 2) line) in
          cur := Some (y, s, d, n, blk :: blocks)
      end) rest;
  flush ();
  (p, w, h, Array.of_list (List.rev !slices))

let entropy_bits hist =
  let total = Array.fold_left (+) 0 hist in
  if total = 0 then 0.0
  else Array.fold_left (fun acc c ->
      if c = 0 then acc else
        let p = float c /. float total in acc -. p *. log p /. log 2.0) 0.0 hist

let ints a = "[" ^ String.concat "," (Array.to_list (Array.map string_of_int a)) ^ "]"
let bools a = "[" ^ String.concat "," (Array.to_list (Array.map (fun b -> if b then "true" else "false") a)) ^ "]"
let floats a = "[" ^ String.concat "," (Array.to_list (Array.map (Printf.sprintf "%.3f") a)) ^ "]"

let write_stats path (p : Params.t) ~w ~h (records : Frame.slice_record array) =
  let oc = open_out path in
  let total_blocks = Array.fold_left (fun a (r : Frame.slice_record) -> a + Array.length r.blocks) 0 records in
  let pixels = w * h in
  let idx_hist = Array.make 13 0 in
  Array.iter (fun (r : Frame.slice_record) ->
      let i = r.decision.Slice.q_order_idx in idx_hist.(i) <- idx_hist.(i) + 1) records;
  let bpp = float (96 * total_blocks) /. float pixels in
  Printf.fprintf oc "{\n  \"params\": \"%s\",\n  \"width\": %d, \"height\": %d, \"slices\": %d,\n" (params_to_kv p) w h (Array.length records);
  Printf.fprintf oc "  \"total_blocks\": %d, \"bits_per_pixel\": %.4f, \"compression_ratio\": %.4f,\n"
    total_blocks bpp (24.0 /. bpp);
  Printf.fprintf oc "  \"q_order_idx_hist\": %s,\n  \"slice\": [\n" (ints idx_hist);
  Array.iteri (fun i (r : Frame.slice_record) ->
      let d = r.decision and s = r.stats in
      Printf.fprintf oc "    {\"y\": %d, \"s\": %d, \"idx\": %d, \"cb\": %s, \"data_num\": %d, \"payload_bits\": %d,\n"
        r.y r.slice_idx d.Slice.q_order_idx (ints d.Slice.codebook_idx) s.Slice.data_num s.Slice.payload_bits;
      Printf.fprintf oc "     \"mode_hist\": %s, \"bits_per_ch\": %s,\n" (ints s.Slice.mode_hist) (ints s.Slice.bits_per_ch);
      Printf.fprintf oc "     \"entropy_per_ch\": %s,\n" (floats (Array.map entropy_bits s.Slice.symbol_hist));
      Printf.fprintf oc "     \"symbol_hist\": [%s]" (String.concat "," (Array.to_list (Array.map ints s.Slice.symbol_hist)));
      (match r.trace with
       | None -> ()
       | Some tr ->
         Printf.fprintf oc ",\n     \"rdo\": {\"cost_vec\": %s, \"data_num_vec\": %s, \"flags\": %s, \"rlc_num\": %s, \"rlc_header\": %s, \"best_book\": [%s], \"best_cost\": [%s], \"cost_overflow\": %b}"
           (ints tr.Rdo.cost_vec) (ints tr.Rdo.data_num_vec) (bools tr.Rdo.flags) (ints tr.Rdo.rlc_num)
           (ints tr.Rdo.rlc_header)
           (String.concat "," (Array.to_list (Array.map ints tr.Rdo.best_book)))
           (String.concat "," (Array.to_list (Array.map ints tr.Rdo.best_cost)))
           tr.Rdo.cost_overflow);
      Printf.fprintf oc "}%s\n" (if i + 1 < Array.length records then "," else ""))
    records;
  Printf.fprintf oc "  ]\n}\n";
  close_out oc
