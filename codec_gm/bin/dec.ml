(* Decoder CLI.
   dec <inc_dir> in.data out.bmp [--ref orig.bmp] [--heatmap err.bmp]
                                 [--metrics m.json] [--stats s.json] [--deblock on|off] [--deblock-th N]
   All coding parameters come from the .data header; only the decoder-side
   deblock settings may be overridden. *)
open Codec_gm

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let reference = ref None and heat = ref None and metrics = ref None and stats = ref None in
  let overrides = ref [] in
  let rec strip acc = function
    | "--ref" :: v :: tl -> reference := Some v; strip acc tl
    | "--heatmap" :: v :: tl -> heat := Some v; strip acc tl
    | "--metrics" :: v :: tl -> metrics := Some v; strip acc tl
    | "--stats" :: v :: tl -> stats := Some v; strip acc tl
    | ("--deblock" | "--deblock-th") as k :: v :: tl -> overrides := !overrides @ [ k; v ]; strip acc tl
    | x :: tl -> strip (x :: acc) tl
    | [] -> List.rev acc in
  let inc_dir, input, output = match strip [] args with
    | [ a; b; c ] -> (a, b, c)
    | _ -> prerr_endline "usage: dec <inc_dir> in.data out.bmp [--ref f] [--heatmap f] [--metrics f] [--stats f] [--deblock on|off] [--deblock-th N]"; exit 2 in
  let t = Tables.load ~inc_dir in
  List.iter (fun s -> prerr_endline ("Tables.check: " ^ s)) (Tables.check t);
  let (p, w, h, slices) = Data_file.read input in
  let (p, _) = Params.of_args p !overrides in
  let t0 = Sys.time () in
  let (img, records) = Frame.decode t p ~w ~h (Array.map (fun (y, s, _, blocks) -> (y, s, blocks)) slices) in
  let dt = Sys.time () -. t0 in
  (* header decisions in the file must agree with what the decoder parsed *)
  Array.iter (fun (y, s, d, _) ->
      let r = records.(y * Params.slice_count p + s) in
      if r.Frame.decision <> d then
        Printf.printf "WARNING: slice y=%d s=%d: .data says idx %d, bitstream header says idx %d\n" y s
          d.Slice.q_order_idx r.Frame.decision.Slice.q_order_idx) slices;
  Bmp.write output img;
  (match !stats with Some f -> Data_file.write_stats f p ~w ~h records | None -> ());
  Printf.printf "%s: %dx%d, %d slices decoded in %.1fs, deblock=%b -> %s\n" input w h (Array.length records) dt
    p.Params.deblock_mode output;
  match !reference with
  | None -> ()
  | Some f ->
    let orig = Bmp.read f in
    let m = Metrics.compare ~orig ~recon:img in
    let (x, y, c) = m.Metrics.max_err_pos in
    Printf.printf "vs %s: PSNR %.2f dB (R %.2f G %.2f B %.2f), max err %d at (%d,%d,ch%d), VIF %.4f\n" f
      m.Metrics.psnr m.Metrics.psnr_ch.(0) m.Metrics.psnr_ch.(1) m.Metrics.psnr_ch.(2) m.Metrics.max_err x y c
      m.Metrics.vif;
    (match !metrics with
     | Some mf -> let oc = open_out mf in output_string oc (Metrics.to_json m); output_char oc '\n'; close_out oc
     | None -> ());
    (match !heat with Some hf -> Bmp.write hf (Metrics.error_heatmap ~orig ~recon:img ()) | None -> ())
