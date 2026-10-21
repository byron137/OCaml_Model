(* Stage 8 tests: Bmp, Data_file and Frame round trips, Metrics sanity. *)
open Codec_gm

let inc_dir =
  match Sys.getenv_opt "CODEC_INC_DIR" with
  | Some d -> d
  | None -> failwith "set CODEC_INC_DIR to the directory containing the .inc files"

let tables =
  let t = Tables.load ~inc_dir in
  Array.iteri (fun b (cb : Tables.codebook) ->
      let n = Tables.alphabet_size ~book_idx:b in
      for s = 0 to n - 1 do
        let next = if s + 1 < n then cb.cdf.(s + 1) else 1 lsl cb.km in
        let gap = next - cb.cdf.(s) in
        if gap > 0 && gap land (gap - 1) = 0 then begin
          let k = ref 0 in while 1 lsl !k < gap do incr k done; cb.ks.(s) <- !k
        end
      done) t.Tables.codebooks;
  t

let tmp name = Filename.concat (Filename.get_temp_dir_name ()) ("codec_gm_" ^ name)

let gen_image = QCheck.Gen.(
    oneofl [ (Params.S4, 15, 96); (Params.S4, 4, 96); (Params.S2, 12, 192); (Params.S1, 60, 192); (Params.S1, 20, 192) ] >>= fun (slice_num, target, w) ->
    int_range 1 6 >>= fun h ->
    bool >>= fun palette_mode -> bool >>= fun rlc_mode -> bool >>= fun rgbg_mode ->
    let p = { Params.default with res_h = w; slice_num; target_data_num = target; table_num = 4;
                                  palette_mode; rlc_mode; rgbg_mode } in
    (* smooth-ish content with repeats so that RLC and palette trigger *)
    array_size (return (3 * w * h)) (int_bound 255) >>= fun noise ->
    int_bound 3 >>= fun kind ->
    let data = Array.mapi (fun i v ->
        match kind with
        | 0 -> v
        | 1 -> (i / 3 mod w) * 255 / w
        | 2 -> if (i / 3 / w) mod 2 = 0 then 100 + (i mod 3) * 40 else v
        | _ -> ((i / 3) * 7 + (i mod 3) * 50) land 255) noise in
    return (p, { Types.w; h; data }))

let print_image ((p : Params.t), (img : Types.image)) =
  Printf.sprintf "%dx%d slices=%d target=%d palette=%b rlc=%b rgbg=%b" img.Types.w img.Types.h
    (Params.slice_count p) p.target_data_num p.palette_mode p.rlc_mode p.rgbg_mode

let test_bmp =
  QCheck.Test.make ~count:50 ~name:"bmp: write then read is the identity"
    (QCheck.make ~print:print_image gen_image)
    (fun (_, img) ->
       let f = tmp "rt.bmp" in
       Bmp.write f img;
       let back = Bmp.read f in
       back = img)

let test_frame_roundtrip =
  QCheck.Test.make ~count:40 ~name:"frame: encode -> .data -> decode reproduces the encoder reconstruction"
    (QCheck.make ~print:print_image gen_image)
    (fun (p, img) ->
       let (records, recon) = Frame.encode tables p img in
       let f = tmp "rt.data" in
       Data_file.write f p ~w:img.Types.w ~h:img.Types.h records;
       let (p', w, h, slices) = Data_file.read f in
       let (dec, records') = Frame.decode tables p' ~w ~h
           (Array.map (fun (y, s, _, b) -> (y, s, b)) slices) in
       let st = tmp "rt.json" in
       Data_file.write_stats st p ~w ~h records;
       p' = p && w = img.Types.w && h = img.Types.h
       && dec = recon
       && Array.for_all2 (fun (r : Frame.slice_record) (r' : Frame.slice_record) ->
           r.decision = r'.decision && r.blocks = r'.blocks && r.stats.Slice.modes = r'.stats.Slice.modes)
         records records'
       && Array.for_all2 (fun (y, s, d, _) (r : Frame.slice_record) ->
           y = r.Frame.y && s = r.Frame.slice_idx && d = r.Frame.decision) slices records
       && Array.for_all (fun (r : Frame.slice_record) ->
           Array.length r.blocks <= p.Params.target_data_num) records)

let test_metrics =
  QCheck.Test.make ~count:30 ~name:"metrics: identical images give inf PSNR, VIF 1, zero heatmap"
    (QCheck.make ~print:print_image gen_image)
    (fun (_, img) ->
       let m = Metrics.compare ~orig:img ~recon:img in
       let heat = Metrics.error_heatmap ~orig:img ~recon:img () in
       m.Metrics.psnr = infinity && m.Metrics.max_err = 0 && abs_float (m.Metrics.vif -. 1.0) < 1e-9
       && Array.for_all (fun v -> v = 0) heat.Types.data)

let test_metrics_monotone =
  QCheck.Test.make ~count:30 ~name:"metrics: adding noise lowers PSNR and VIF, max_err exact"
    (QCheck.make ~print:print_image gen_image)
    (fun (_, img) ->
       let noisy = { img with Types.data = Array.mapi (fun i v -> if i mod 7 = 0 then min 255 (v + 9) else v) img.Types.data } in
       let m = Metrics.compare ~orig:img ~recon:noisy in
       m.Metrics.max_err <= 9 && m.Metrics.psnr < 60.0 && m.Metrics.vif <= 1.0 +. 1e-9)

let () =
  QCheck_base_runner.run_tests_main [ test_bmp; test_frame_roundtrip; test_metrics; test_metrics_monotone ]
