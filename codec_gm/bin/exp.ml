(* Experiment runner for the interpolation-prediction spec.

   exp <inc_dir> <image_dir> <out_dir> [options]
     --name E1.0            experiment label (output subdirectory)
     --structure serial|trunk4|a|b
     --m1 m1a|m1b|m1c       --seg 12|20|36
     --force-macro          Params.rgbg_mode = true (every slice macro)
     --books shared|lane3|oddeven   codebook override (needs --books-file)
     --books-file f         file written by --dump-hist + build_books
     --dump-hist f          write per (lane, col, q_order) symbol histograms
     --images               write decoded / heatmap BMPs
     --slice 1|2|4 --target N --table-num N --rlc on|off --palette on|off
   <image_dir> is walked recursively; the category is the parent directory
   name of each .bmp.

   Outputs <out_dir>/<name>/per_slice.csv, per_image.csv, summary.txt, and
   with --images <name>/<category>_<file>.bmp (decoded) and _heat.bmp. *)
open Codec_gm
open Types

let rec walk dir acc =
  Array.fold_left (fun acc f ->
      let p = Filename.concat dir f in
      if Sys.is_directory p then walk p acc
      else if Filename.check_suffix f ".bmp" then p :: acc else acc)
    acc (Sys.readdir dir)

let category path = Filename.basename (Filename.dirname path)

(* --- codebook override file: lines "lane col q_order km ks0 ks1 ..." --- *)
let load_books file =
  let tbl = Hashtbl.create 64 in
  let ic = open_in file in
  (try while true do
       let l = input_line ic in
       match List.map int_of_string (String.split_on_char ' ' (String.trim l)) with
       | lane :: col :: q :: km :: ks ->
         let ks = Array.of_list ks in
         let cdf = Array.make (Array.length ks) 0 in
         let acc = ref 0 in
         Array.iteri (fun s k -> cdf.(s) <- !acc; acc := !acc + (1 lsl k)) ks;
         Hashtbl.replace tbl (lane, col, q) { Tables.km; ks; cdf }
       | _ -> ()
     done with End_of_file -> close_in ic);
  tbl

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let name = ref "run" and structure = ref Xcodec.Serial and m1 = ref Xcodec.M1a and seg = ref 20 in
  let force_macro = ref false and books = ref "shared" and books_file = ref None in
  let dump_hist = ref None and images = ref false in
  let rec strip acc = function
    | "--name" :: v :: tl -> name := v; strip acc tl
    | "--structure" :: v :: tl -> structure := Xcodec.structure_of_string v; strip acc tl
    | "--m1" :: v :: tl -> m1 := Xcodec.m1_of_string v; strip acc tl
    | "--seg" :: v :: tl -> seg := int_of_string v; strip acc tl
    | "--force-macro" :: tl -> force_macro := true; strip acc tl
    | "--books" :: v :: tl -> books := v; strip acc tl
    | "--books-file" :: v :: tl -> books_file := Some v; strip acc tl
    | "--dump-hist" :: v :: tl -> dump_hist := Some v; strip acc tl
    | "--images" :: tl -> images := true; strip acc tl
    | x :: tl -> strip (x :: acc) tl
    | [] -> List.rev acc in
  let rest = strip [] args in
  let (p, pos) = Params.of_args { Params.default with Params.slice_num = Params.S2; target_data_num = 24;
                                                      table_num = 4; palette_mode = false } rest in
  let inc_dir, image_dir, out_dir = match pos with
    | [ a; b; c ] -> (a, b, c)
    | _ -> prerr_endline "usage: exp <inc_dir> <image_dir> <out_dir> [options]"; exit 2 in
  let p = { p with Params.rgbg_mode = !force_macro || p.Params.rgbg_mode } in
  let t = Tables.load ~inc_dir in
  let override = match !books_file with Some f -> Some (load_books f) | None -> None in
  let book_for =
    match !books, override with
    | "shared", _ | _, None -> None
    | mode, Some tbl ->
      Some (fun ~lane ~col ~q_order ~codebook_idx ->
          let default () = t.Tables.codebooks.(Tables.codebook_index ~q_order ~codebook_idx) in
          let custom cls = match Hashtbl.find_opt tbl (cls, col, q_order) with
            | Some b -> b | None -> default () in
          match mode with
          | "lane3" -> if lane = 3 then custom 3 else default ()
          | "oddeven" -> custom (lane land 1)   (* class 0 = even lanes, 1 = odd lanes *)
          | _ -> failwith ("unknown --books " ^ mode)) in
  let cfg = { Xcodec.structure = !structure; m1 = !m1; seg_len = !seg; book_for } in
  let out = Filename.concat out_dir !name in
  (try Sys.mkdir out_dir 0o755 with Sys_error _ -> ());
  (try Sys.mkdir out 0o755 with Sys_error _ -> ());
  let files = List.sort compare (walk image_dir []) in
  let ps = open_out (Filename.concat out "per_slice.csv") in
  output_string ps "image,category,y,slice_idx,idx,macro,fallback,q_cg,q_co,q_y,bits,side_bits,data_num,arb_num,over_target,psnr,max_err\n";
  let pi = open_out (Filename.concat out "per_image.csv") in
  output_string pi "image,category,pixels,slices,blocks,bpp,bits_raw,side_bits,psnr,psnr_r,psnr_g,psnr_b,max_err,de_mean,de_p99,fallback_rate,macro_rate,over_target_rate,beta_hist\n";
  (* histograms per (lane, col, q_order) and per-(lane,col) symbol streams for entropy *)
  let hist = Hashtbl.create 64 in
  let streams = Array.init 12 (fun _ -> ref []) in
  let count = Params.slice_count p in
  List.iter (fun file ->
      let img = Bmp.read file in
      let p = { p with Params.res_h = img.w } in
      Frame.check_geometry p img;
      let recon = { w = img.w; h = img.h; data = Array.make (Array.length img.data) 0 } in
      let blocks = ref 0 and bits_raw = ref 0 and side_bits = ref 0 in
      let fallbacks = ref 0 and macros = ref 0 and overs = ref 0 and nslices = ref 0 in
      let beta_hist = Array.make 8 0 in
      for y = 0 to img.h - 1 do
        for s = 0 to count - 1 do
          let rgb = Frame.slice_rgb p img ~y ~slice_idx:s in
          let (d, arb_num, fallback) = Xrdo.decide cfg t p ~rgb in
          let e = Xcodec.encode_slice cfg t p d ~rgb in
          let dec = Xcodec.decode_slice cfg t p d e.Xcodec.side e.Xcodec.tokens in
          if dec <> e.Xcodec.recon_rgb then
            spec_violation "%s y=%d s=%d: decoder differs from encoder reconstruction" file y s;
          Frame.blit_slice p recon ~y ~slice_idx:s e.Xcodec.recon_rgb;
          let r = Slice.resolve t p d in
          let q = r.Slice.q_order in
          incr nslices;
          blocks := !blocks + e.Xcodec.data_num;
          bits_raw := !bits_raw + e.Xcodec.bits;
          side_bits := !side_bits + e.Xcodec.side_bits;
          if fallback then incr fallbacks;
          if r.Slice.macro then incr macros;
          let over = e.Xcodec.data_num > p.Params.target_data_num in
          if over then incr overs;
          Array.iter (Array.iter (fun b -> beta_hist.(b) <- beta_hist.(b) + 1)) e.Xcodec.side.Xcodec.betas;
          (* per-slice psnr / max err *)
          let se = ref 0 and me = ref 0 in
          Array.iteri (fun i v -> let d = v - rgb.(i) in se := !se + d * d; me := max !me (abs d)) e.Xcodec.recon_rgb;
          let mse = float !se /. float (Array.length rgb) in
          let psnr = if mse = 0.0 then 99.0 else 10.0 *. log10 (65025.0 /. mse) in
          Printf.fprintf ps "%s,%s,%d,%d,%d,%b,%b,%d,%d,%d,%d,%d,%d,%d,%b,%.3f,%d\n"
            (Filename.basename file) (category file) y s d.Slice.q_order_idx r.Slice.macro fallback
            q.(0) q.(1) q.(2) e.Xcodec.bits e.Xcodec.side_bits e.Xcodec.data_num arb_num over psnr !me;
          (* symbol statistics of coded (unmasked) channels *)
          for ch = 0 to 11 do
            if not r.Slice.masked.(ch) then begin
              let lane = ch / 3 and col = ch mod 3 in
              let key = (lane, col, q.(col)) in
              let h = match Hashtbl.find_opt hist key with
                | Some h -> h
                | None -> let h = Array.make (Tables.level_max ~q_order:q.(col) + 1) 0 in Hashtbl.replace hist key h; h in
              Array.iter (fun sym -> h.(sym) <- h.(sym) + 1; streams.(ch) := sym :: !(streams.(ch)))
                e.Xcodec.symbols.(ch)
            end
          done
        done
      done;
      let m = Metrics.compare ~orig:img ~recon in
      let (de_mean, de_p99) = Metrics.ciede2000 ~orig:img ~recon in
      let pixels = img.w * img.h in
      Printf.fprintf pi "%s,%s,%d,%d,%d,%.4f,%d,%d,%.3f,%.3f,%.3f,%.3f,%d,%.4f,%.4f,%.4f,%.4f,%.4f,%s\n"
        (Filename.basename file) (category file) pixels !nslices !blocks
        (float (96 * !blocks) /. float pixels) !bits_raw !side_bits
        m.Metrics.psnr m.Metrics.psnr_ch.(0) m.Metrics.psnr_ch.(1) m.Metrics.psnr_ch.(2) m.Metrics.max_err
        de_mean de_p99 (float !fallbacks /. float !nslices) (float !macros /. float !nslices)
        (float !overs /. float !nslices)
        (String.concat ";" (Array.to_list (Array.map string_of_int beta_hist)));
      if !images then begin
        let base = Filename.concat out (category file ^ "_" ^ Filename.chop_suffix (Filename.basename file) ".bmp") in
        Bmp.write (base ^ ".bmp") recon;
        Bmp.write (base ^ "_heat.bmp") (Metrics.heatmap32 ~orig:img ~recon)
      end;
      Printf.printf "%-40s %s bpp=%.3f psnr=%.2f maxerr=%d dE=%.3f/%.3f fb=%.2f\n%!" (Filename.basename file) (category file)
        (float (96 * !blocks) /. float pixels) m.Metrics.psnr m.Metrics.max_err de_mean de_p99
        (float !fallbacks /. float !nslices))
    files;
  close_out ps; close_out pi;
  (* entropy and zero / +-1 share per (lane, col) *)
  let sm = open_out (Filename.concat out "symbols.csv") in
  output_string sm "lane,col,count,entropy,zero_share,pm1_share\n";
  for ch = 0 to 11 do
    let a = Array.of_list !(streams.(ch)) in
    let n = Array.length a in
    let zero = Array.fold_left (fun c s -> if s = 0 then c + 1 else c) 0 a in
    let pm1 = Array.fold_left (fun c s -> if s = 1 || s = 2 then c + 1 else c) 0 a in
    Printf.fprintf sm "%d,%d,%d,%.4f,%.4f,%.4f\n" (ch / 3) (ch mod 3) n (Metrics.residual_entropy a)
      (if n = 0 then 0.0 else float zero /. float n) (if n = 0 then 0.0 else float pm1 /. float n)
  done;
  close_out sm;
  (match !dump_hist with
   | None -> ()
   | Some f ->
     let oc = open_out f in
     Hashtbl.iter (fun (lane, col, q) h ->
         Printf.fprintf oc "%d %d %d %s\n" lane col q
           (String.concat " " (Array.to_list (Array.map string_of_int h)))) hist;
     close_out oc);
  let sm = open_out (Filename.concat out "config.txt") in
  Printf.fprintf sm "name=%s structure=%s m1=%s seg=%d force_macro=%b books=%s slice=%d target=%d table_num=%d rlc=%b palette=%b\n"
    !name (Xcodec.structure_to_string !structure) (Xcodec.m1_to_string !m1) !seg !force_macro !books
    (Params.slice_count p) p.Params.target_data_num p.Params.table_num p.Params.rlc_mode p.Params.palette_mode;
  close_out sm
