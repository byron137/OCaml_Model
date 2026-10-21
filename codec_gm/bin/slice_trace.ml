(* Stand-alone slice tracer: raw RGB in, decompressed RGB out, with every
   intermediate stage printed so the data flow can be followed by hand.

   Usage:
     slice_trace <inc_dir> [--in rgb.txt] [--out dec.txt] [--idx N] [--cb N]
                 [--quiet] [Params options: --slice 1|2|4 --target N --table-num N
                 --rlc on|off --palette on|off --rgbg on|off --deblock on|off ...]

   rgb.txt: whitespace-separated integers, R G B per pixel, exactly
   12 * slice_en_num values (= 4 * slice_en_num pixels, 180 pixels for the
   default 720 / 4-slice layout). Without --in a synthetic slice is used.
   dec.txt receives the decoded pixels, one "R G B" line per pixel. *)
open Codec_gm

let read_rgb path =
  let ic = open_in path in
  let acc = ref [] in
  (try
     while true do
       String.split_on_char ' ' (input_line ic)
       |> List.concat_map (String.split_on_char '\t')
       |> List.filter (fun s -> s <> "")
       |> List.iter (fun s -> acc := int_of_string s :: !acc)
     done
   with End_of_file -> close_in ic);
  Array.of_list (List.rev !acc)

(* gradient, a flat stretch, a repeated non-flat pattern, then noise-like texture *)
let synthetic n =
  let px = Array.make (12 * n) 0 in
  for k = 0 to n - 1 do
    for lane = 0 to 3 do
      let i = 4 * k + lane in
      let (r, g, b) =
        if k < n / 4 then (i * 3 land 255, 128 - i land 255, 40 + i / 2)
        else if k < n / 2 then (200, 100, 50)
        else if k < 3 * n / 4 then
          (match lane with 0 -> (10, 200, 30) | 1 -> (240, 20, 60) | 2 -> (90, 90, 220) | _ -> (0, 0, 0))
        else ((i * 97 + 13) land 255, (i * 57 + 101) land 255, (i * 31 + 7) land 255) in
      px.(3 * i) <- r; px.(3 * i + 1) <- g; px.(3 * i + 2) <- b
    done
  done;
  px

let mode_str = function
  | Types.Normal -> "normal" | Types.Bypass -> "bypass"
  | Types.Escape n -> Printf.sprintf "escape(%d)" n | Types.Palette -> "palette"

let ints a = String.concat " " (Array.to_list (Array.map (Printf.sprintf "%3d") a))

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let inc_dir = match args with d :: _ -> d | [] -> failwith "usage: slice_trace <inc_dir> [options]" in
  let args = List.tl args in
  (* local options first, then Params options *)
  let input = ref None and output = ref None and idx = ref 1 and cb = ref 0 and quiet = ref false in
  let rec strip acc = function
    | "--in" :: v :: tl -> input := Some v; strip acc tl
    | "--out" :: v :: tl -> output := Some v; strip acc tl
    | "--idx" :: v :: tl -> idx := int_of_string v; strip acc tl
    | "--cb" :: v :: tl -> cb := int_of_string v; strip acc tl
    | "--quiet" :: tl -> quiet := true; strip acc tl
    | x :: tl -> strip (x :: acc) tl
    | [] -> List.rev acc in
  let rest = strip [] args in
  let (p, leftover) = Params.of_args Params.default rest in
  if leftover <> [] then failwith ("unexpected arguments: " ^ String.concat " " leftover);
  let t = Tables.load ~inc_dir in
  (match Tables.check t with
   | [] -> ()
   | l -> prerr_endline "Tables.check:"; List.iter (fun s -> prerr_endline ("  " ^ s)) l);
  let n = Params.slice_en_num p in
  let rgb = match !input with Some f -> read_rgb f | None -> synthetic n in
  if Array.length rgb <> 12 * n then
    failwith (Printf.sprintf "need %d values (%d pixels), got %d" (12 * n) (4 * n) (Array.length rgb));
  let q = t.Tables.qorder.(!idx).Tables.q_order in
  let d = { Slice.q_order_idx = !idx;
            codebook_idx = Array.map (fun o -> if o = 6 then 0 else !cb) q } in
  let r = Slice.resolve t p d in
  let say fmt = Printf.ksprintf (fun s -> if not !quiet then print_endline s) fmt in

  say "== decision ==";
  say "q_order_idx=%d  q_order(cg,co,y)=%s  codebook_idx=%s  macro=%b  rlc=%b  escape_sym=%d"
    !idx (ints q) (ints d.Slice.codebook_idx) r.Slice.macro r.Slice.rlc r.Slice.escape_sym;
  say "slice_en_num=%d  palette_mode=%b  rgbg=%b  deblock=%b" n p.Params.palette_mode
    p.Params.rgbg_mode p.Params.deblock_mode;

  (* --- stage 1: color transform, shown per group --- *)
  let groups = Array.init n (fun k -> Array.sub rgb (12 * k) 12) in
  let ycocg = Array.map (fun g -> Color.group_to_ycocg ~rgb:g) groups in
  let column col = Array.init (4 * n) (fun i -> ycocg.(i / 4).(i mod 4 * 3 + col)) in

  (* --- stage 2: palette flags, DPCM + quant + mapper per column --- *)
  let palette = Rlc.palette_flags (Params.palette_cfg p) ~slice_en_num:n ~rgb_groups:groups in
  let sym = Array.make 3 [||] and rec_ = Array.make 3 [||] in
  for col = 0 to 2 do
    let (s, rc) = Slice.column_encode t ~col ~q_order:q.(col) ~macro:r.Slice.macro ~zero:palette
        ~pix:(column col) in
    sym.(col) <- s; rec_.(col) <- rc
  done;
  let all_zero = Array.init n (fun k ->
      List.for_all (fun col -> List.for_all (fun lane -> sym.(col).(4 * k + lane) = 0) [0;1;2;3]) [0;1;2]) in
  let modes = Rlc.classify ~rlc_mode:r.Slice.rlc ~slice_en_num:n ~all_zero ~palette in

  say "\n== per group: rgb | ycocg (y co cg per lane) | symbols (ch0..ch11 = lane*3+col, col 0=Cg 1=Co 2=Y) | mode ==";
  for k = 0 to n - 1 do
    let yc = Array.init 12 (fun i -> let lane = i / 3 and c = i mod 3 in
                             ycocg.(k).(lane * 3 + (match c with 0 -> 2 | 1 -> 1 | _ -> 0))) in
    let s = Array.init 12 (fun ch -> sym.(ch mod 3).(4 * k + ch / 3)) in
    say "g%3d  rgb[%s]\n      ycocg[%s]\n      sym[%s]  all_zero=%b palette=%b -> %s"
      k (ints groups.(k)) (ints yc) (ints s) all_zero.(k) palette.(k) (mode_str modes.(k))
  done;

  (* --- stage 3: the real encoder (same computation, plus entropy coding) --- *)
  let (blocks, st, recon) = Slice.encode_slice t p d ~rgb in
  say "\n== bitstream ==";
  say "payload_bits=%d  header_bits=%d  data_num=%d blocks  (target %d)"
    st.Slice.payload_bits (Bitstream.header_length ~q_order_idx:!idx) st.Slice.data_num p.Params.target_data_num;
  say "mode_hist normal/bypass/escape/palette = %s" (ints st.Slice.mode_hist);
  say "bits_per_ch = %s" (ints st.Slice.bits_per_ch);
  Array.iteri (fun j blk -> say "block %2d: %s" j (Bitvec.block_to_hex blk)) blocks;
  let rd = Bitstream.Reader.of_blocks blocks in
  let h = Bitstream.Reader.pop_header rd in
  say "header: idx=%d cb=%s last_pixel(cg,co,y)=%s states=%s" h.Bitstream.q_order_idx
    (ints h.Bitstream.codebook_idx) (ints h.Bitstream.last_pixel) (ints h.Bitstream.states);

  (* --- stage 4: independent decoder --- *)
  let (out, d', st') = Slice.decode_slice t p blocks in
  say "\n== decoded (deblock=%b) vs encoder reconstruction vs original ==" p.Params.deblock_mode;
  let max_err = ref 0 and mismatch = ref 0 in
  for i = 0 to 4 * n - 1 do
    let o = Array.sub out (3 * i) 3 and e = Array.sub recon (3 * i) 3 and s = Array.sub rgb (3 * i) 3 in
    let err = Array.fold_left max 0 (Array.mapi (fun c v -> abs (v - s.(c))) o) in
    max_err := max !max_err err;
    let same = (o = e) || p.Params.deblock_mode in
    if not same then incr mismatch;
    say "px%4d  orig[%s]  recon[%s]  dec[%s]  err=%3d%s" i (ints s) (ints e) (ints o) err
      (if o <> e then "  <-- differs from encoder recon" else "")
  done;
  Printf.printf "\nsummary: decision_roundtrip=%b modes_roundtrip=%b decoder_vs_encoder_mismatch=%d max_err=%d data_num=%d\n"
    (d' = d) (st'.Slice.modes = st.Slice.modes) !mismatch !max_err st.Slice.data_num;
  (match !output with
   | None -> ()
   | Some f ->
     let oc = open_out f in
     for i = 0 to 4 * n - 1 do
       Printf.fprintf oc "%d %d %d\n" out.(3 * i) out.(3 * i + 1) out.(3 * i + 2)
     done;
     close_out oc;
     Printf.printf "decoded rgb written to %s\n" f)
