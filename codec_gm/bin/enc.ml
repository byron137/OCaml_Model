(* Encoder CLI.
   enc <inc_dir> in.bmp out.data [--stats out.json] [--recon recon.bmp]
                                 [--heatmap err.bmp] [Params options]
   Params options: --slice 1|2|4 --target N --table-num N --rlc on|off
   --palette on|off --palette-tf N --rgbg on|off (deblock is a decoder setting).
   res_h is taken from the image width. *)
open Codec_gm

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let stats = ref None and recon_out = ref None and heat = ref None in
  let rec strip acc = function
    | "--stats" :: v :: tl -> stats := Some v; strip acc tl
    | "--recon" :: v :: tl -> recon_out := Some v; strip acc tl
    | "--heatmap" :: v :: tl -> heat := Some v; strip acc tl
    | x :: tl -> strip (x :: acc) tl
    | [] -> List.rev acc in
  let rest = strip [] args in
  let (p, pos) = Params.of_args Params.default rest in
  let inc_dir, input, output = match pos with
    | [ a; b; c ] -> (a, b, c)
    | _ -> prerr_endline "usage: enc <inc_dir> in.bmp out.data [--stats f] [--recon f] [--heatmap f] [params]"; exit 2 in
  let t = Tables.load ~inc_dir in
  List.iter (fun s -> prerr_endline ("Tables.check: " ^ s)) (Tables.check t);
  let img = Bmp.read input in
  let p = { p with Params.res_h = img.Types.w } in
  let t0 = Sys.time () in
  let (records, recon) = Frame.encode t p img in
  let dt = Sys.time () -. t0 in
  Data_file.write output p ~w:img.Types.w ~h:img.Types.h records;
  (match !stats with Some f -> Data_file.write_stats f p ~w:img.Types.w ~h:img.Types.h records | None -> ());
  (match !recon_out with Some f -> Bmp.write f recon | None -> ());
  (match !heat with Some f -> Bmp.write f (Metrics.error_heatmap ~orig:img ~recon ()) | None -> ());
  let blocks = Array.fold_left (fun a (r : Frame.slice_record) -> a + Array.length r.blocks) 0 records in
  let hist = Array.make 13 0 in
  Array.iter (fun (r : Frame.slice_record) ->
      let i = r.decision.Slice.q_order_idx in hist.(i) <- hist.(i) + 1) records;
  let overflow = Array.exists (fun (r : Frame.slice_record) ->
      match r.trace with Some tr -> tr.Rdo.cost_overflow | None -> false) records in
  let m = Metrics.compare ~orig:img ~recon in
  let bpp = float (96 * blocks) /. float (img.Types.w * img.Types.h) in
  Printf.printf "%s: %dx%d, %d slices, %d blocks, %.3f bpp, CR %.2fx, %.1fs\n" input img.Types.w img.Types.h
    (Array.length records) blocks bpp (24.0 /. bpp) dt;
  Printf.printf "q_order_idx histogram: %s\n"
    (String.concat " " (Array.to_list (Array.mapi (fun i c -> Printf.sprintf "%d:%d" i c) hist)));
  let (ex, ey, ec) = m.Metrics.max_err_pos in
  Printf.printf "encoder reconstruction: PSNR %.2f dB (R %.2f G %.2f B %.2f), max err %d at (%d,%d,ch%d), VIF %.4f\n"
    m.Metrics.psnr m.Metrics.psnr_ch.(0) m.Metrics.psnr_ch.(1) m.Metrics.psnr_ch.(2) m.Metrics.max_err
    ex ey ec m.Metrics.vif;
  if overflow then print_endline "WARNING: at least one slice exceeded the RTL arbiter's 13/14-bit cost width"
