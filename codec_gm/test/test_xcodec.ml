(* Experimental codec (spec "水平降取樣與插值預測改進", improvements 1-2):
   - with the default config the symbol-level codec reproduces the golden
     model's reconstruction and exact payload bits, and Xrdo reproduces Rdo;
   - every structure x M1 variant round-trips through the forward decoder;
   - improvement 1 never changes the chain (even samples) nor the bits of
     M1a/M1b; M1c adds exactly 6 bits per segment;
   - Interp passes the spec's numeric self-check; Xbook builds valid books. *)
open Codec_gm

let inc_dir =
  match Sys.getenv_opt "CODEC_INC_DIR" with
  | Some d -> d
  | None -> failwith "set CODEC_INC_DIR to the directory containing the .inc files"

let tables = Tables.load ~inc_dir

let decision_for cb q_order_idx =
  let q = tables.Tables.qorder.(q_order_idx).Tables.q_order in
  { Slice.q_order_idx; codebook_idx = Array.map (fun o -> if o = 6 then 0 else cb) q }

let gen_rgb n = QCheck.Gen.(
    let noise = array_size (return 12) (int_bound 255) in
    let smooth = map (fun (a, s) -> Array.init 12 (fun i -> (a.(i mod 3) + (i / 3) * s) land 255))
        (pair (array_size (return 3) (int_bound 255)) (int_range (-6) 6)) in
    list_size (return n) (int_bound 9) >>= fun kinds ->
    let rec build prev acc = function
      | [] -> return (List.rev acc)
      | kind :: tl ->
        let g =
          if kind < 2 then noise
          else if kind < 5 then return (match prev with Some g -> Array.init 12 (fun i -> g.(9 + i mod 3)) | None -> Array.make 12 77)
          else if kind < 7 then (match prev with Some g -> return (Array.map (fun v -> min 255 (v + 3)) g) | None -> smooth)
          else smooth in
        g >>= fun g -> build (Some g) (g :: acc) tl in
    build None [] kinds >>= fun groups -> return (Array.concat groups))

let gen_case = QCheck.Gen.(
    oneofl [ (Params.S4, 720); (Params.S2, 720); (Params.S1, 720); (Params.S4, 128) ] >>= fun (slice_num, res_h) ->
    let p = { Params.default with slice_num; res_h; table_num = 4; palette_mode = false } in
    let n = Params.slice_en_num p in
    int_bound 12 >>= fun idx ->
    bool >>= fun rgbg_mode ->
    bool >>= fun rlc_mode ->
    oneofl [ Xcodec.Serial; Trunk4; VariantA; VariantB ] >>= fun structure ->
    oneofl [ Xcodec.M1a; M1b; M1c ] >>= fun m1 ->
    oneofl [ 12; 20; 36 ] >>= fun seg_len ->
    map (fun rgb -> ({ p with rgbg_mode; rlc_mode }, idx, { Xcodec.default_config with structure; m1; seg_len }, rgb))
      (gen_rgb n))

let print_case (p, idx, cfg, rgb) =
  Printf.sprintf "n=%d idx=%d rgbg=%b rlc=%b structure=%s m1=%s L=%d pixels=%d"
    (Params.slice_en_num p) idx p.Params.rgbg_mode p.Params.rlc_mode
    (Xcodec.structure_to_string cfg.Xcodec.structure) (Xcodec.m1_to_string cfg.Xcodec.m1)
    cfg.Xcodec.seg_len (Array.length rgb / 3)

let arb = QCheck.make ~print:print_case gen_case

(* 1. default config == golden model *)
let test_vs_golden =
  QCheck.Test.make ~count:300 ~name:"xcodec: Serial/M1a = Slice.encode_slice (recon, modes, payload bits, data_num)"
    arb (fun (p, idx, _, rgb) ->
        let d = decision_for 3 idx in
        let (blocks, st, recon) = Slice.encode_slice tables p d ~rgb in
        let e = Xcodec.encode_slice Xcodec.default_config tables p d ~rgb in
        e.Xcodec.recon_rgb = recon && e.Xcodec.modes = st.Slice.modes
        && e.Xcodec.bits = st.Slice.payload_bits && e.Xcodec.data_num = Array.length blocks)

let test_xrdo_vs_rdo =
  QCheck.Test.make ~count:200 ~name:"xrdo: default config = Rdo.decide (decision and data_num)"
    arb (fun (p, _, _, rgb) ->
        let (d, tr) = Rdo.decide tables p ~rgb in
        let (d', num, _) = Xrdo.decide Xcodec.default_config tables p ~rgb in
        d = d' && num = tr.Rdo.chosen_num)

(* 2. forward decoder round trip for every structure and M1 variant *)
let test_roundtrip =
  QCheck.Test.make ~count:600 ~name:"xcodec: decode (encode rgb) = encoder reconstruction for every structure x M1"
    arb (fun (p, idx, cfg, rgb) ->
        let d = decision_for 3 idx in
        let e = Xcodec.encode_slice cfg tables p d ~rgb in
        let out = Xcodec.decode_slice cfg tables p d e.Xcodec.side e.Xcodec.tokens in
        out = e.Xcodec.recon_rgb)

(* 3. improvement 1 invariants *)
let test_m1_invariants =
  QCheck.Test.make ~count:200 ~name:"xcodec: M1 never changes the chain or even samples; M1a/M1b same bits; M1c +6/segment"
    arb (fun (p, idx, cfg, rgb) ->
        let d = decision_for 3 idx in
        let run m1 = Xcodec.encode_slice { cfg with Xcodec.m1 } tables p d ~rgb in
        let a = run Xcodec.M1a and b = run Xcodec.M1b and c = run Xcodec.M1c in
        let n_total = Array.length rgb / 3 in
        let macro = (Slice.resolve tables p d).Slice.macro in
        let even_same x y = (* even pixel RGB equal *)
          let ok = ref true in
          for i = 0 to n_total - 1 do
            if i land 1 = 0 then for c = 0 to 2 do
                if x.Xcodec.recon_rgb.(3 * i + c) <> y.Xcodec.recon_rgb.(3 * i + c) then ok := false done
          done; !ok in
        a.Xcodec.chain = b.Xcodec.chain && b.Xcodec.chain = c.Xcodec.chain
        && a.Xcodec.tokens = b.Xcodec.tokens && b.Xcodec.tokens = c.Xcodec.tokens
        && a.Xcodec.bits = b.Xcodec.bits
        && c.Xcodec.bits = a.Xcodec.bits + (if macro then 6 * Xcodec.segments cfg ~n_total else 0)
        && even_same a b && even_same b c
        && (macro || (a.Xcodec.recon_rgb = b.Xcodec.recon_rgb && b.Xcodec.recon_rgb = c.Xcodec.recon_rgb)))

(* 4. structure invariants: trunk and even chain shared between Trunk4 / A / B *)
let test_trunk_shared =
  QCheck.Test.make ~count:200 ~name:"xcodec: lane-0 chain identical for Trunk4, A and B (Y column)"
    arb (fun (p, idx, _, rgb) ->
        let d = decision_for 3 idx in
        let run structure = Xcodec.encode_slice { Xcodec.default_config with structure } tables p d ~rgb in
        let t4 = run Xcodec.Trunk4 and a = run Xcodec.VariantA and b = run Xcodec.VariantB in
        let n_total = Array.length rgb / 3 in
        let lane0 e = Array.init (n_total / 4) (fun k -> e.Xcodec.chain.(2).(4 * k)) in
        lane0 t4 = lane0 a && lane0 a = lane0 b
        (* lanes 0..2 of A equal Trunk4 (only lane 3 changed) *)
        && Array.init n_total (fun i -> if i land 3 = 3 then -1 else a.Xcodec.chain.(2).(i))
           = Array.init n_total (fun i -> if i land 3 = 3 then -1 else t4.Xcodec.chain.(2).(i)))

(* 5. Interp: spec 1.1 numeric self-check and kernel sanity *)
let test_interp_selfcheck =
  QCheck.Test.make ~count:1 ~name:"interp: spec 1.1 self-check (M1a 70,70,70 / M1b 145,45,45 / M1c 220,20,20)" QCheck.unit
    (fun () ->
       let r = [| 20; 20; 20; 220; 220; 220 |] in
       let yco = Array.map (fun r -> Color.rgb_to_ycocg_u ~r ~g:20 ~b:20) r in
       let y = Array.map (fun (y, _, _) -> y) yco and co = Array.map (fun (_, c, _) -> c) yco
       and cg = Array.map (fun (_, _, c) -> c) yco in
       let n = 3 in
       let rgb co_u cg_u = Color.ycocg_u_to_rgb ~y:y.(n) ~co_u ~cg_u in
       rgb co.(n - 1) cg.(n - 1) = (70, 70, 70)
       && rgb (Interp.lin co.(n - 1) co.(n + 1)) (Interp.lin cg.(n - 1) cg.(n + 1)) = (145, 45, 45)
       && rgb (Interp.predict_chroma_beta_u ~beta_idx:4 ~c_rec:co ~y_rec:y n)
            (Interp.predict_chroma_beta_u ~beta_idx:6 ~c_rec:cg ~y_rec:y n) = (220, 20, 20)
       && Interp.choose_beta ~c_orig:co ~c_rec:co ~y_rec:y ~seg_start:0 ~seg_len:6 = 4
       && Interp.choose_beta ~c_orig:cg ~c_rec:cg ~y_rec:y ~seg_start:0 ~seg_len:6 = 6
       && Interp.predict_chroma_beta ~beta_idx:4 ~c_rec:[| 0; 0; 0; 200; 200; 200 |]
            ~y_rec:[| 20; 20; 20; 70; 70; 70 |] 3 = 200)

let test_interp_kernels =
  QCheck.Test.make ~count:300 ~name:"interp: every kernel is exact on a line, within [e-1,e+1] when clamped, and only reads even samples"
    QCheck.(make Gen.(pair (int_range (-20) 20) (array_size (int_range 4 40) (int_bound 255))))
    (fun (slope, rnd) ->
       let n = Array.length rnd land (lnot 1) in
       QCheck.assume (n >= 4);
       let line = Array.init n (fun i -> 100 + slope * i) in
       let kernels = [ Interp.Linear; QuadL; QuadR; DD4; Wiener (-4); Wiener 3; Auto ] in
       let exact = List.for_all (fun k ->
           let ok = ref true in
           let i = ref 3 in
           while !i < n - 3 do   (* odd positions whose e(n-3)..e(n+3) all exist *)
             if Interp.predict_odd k line !i <> line.(!i) then ok := false; i := !i + 2 done;
           !ok) [ Interp.Linear; QuadL; QuadR; DD4; Wiener 0; Auto ] in
       let rnd = Array.sub rnd 0 n in
       let clamped = List.for_all (fun k ->
           let ok = ref true in
           let i = ref 1 in
           while !i < n do
             let v = Interp.predict_odd k rnd !i in
             let a = Interp.ge rnd (!i - 1) and b = Interp.ge rnd (!i + 1) in
             (match k with
              | Interp.DD4 | Wiener _ | Auto | Linear -> if v < min a b || v > max a b then ok := false
              | _ -> ());
             i := !i + 2
           done; !ok) kernels in
       let odd_independent =
         let r2 = Array.mapi (fun i v -> if i land 1 = 1 then 255 - v else v) rnd in
         List.for_all (fun k ->
             let ok = ref true in
             let i = ref 1 in
             while !i < n do
               if Interp.predict_odd k rnd !i <> Interp.predict_odd k r2 !i then ok := false; i := !i + 2 done;
             !ok) kernels in
       exact && clamped && odd_independent)

(* 6. codebook builder *)
let test_xbook =
  QCheck.Test.make ~count:300 ~name:"xbook: built books are consistent (ranges abut, sum 2^km, escape encodable)"
    QCheck.(make Gen.(oneofl [ 33; 17; 9 ] >>= fun a -> array_size (return a) (int_bound 1000)))
    (fun hist ->
       let cb = Xbook.build ~hist in
       let a = Array.length hist in
       let sum = Array.fold_left (fun acc k -> acc + (1 lsl k)) 0 cb.Tables.ks in
       sum = 1 lsl cb.Tables.km
       && Array.for_all (fun k -> k >= 0 && k <= 7) cb.Tables.ks
       && (let ok = ref true in
           for s = 1 to a - 1 do
             if cb.Tables.cdf.(s) <> cb.Tables.cdf.(s - 1) + (1 lsl cb.Tables.ks.(s - 1)) then ok := false done;
           !ok)
       && cb.Tables.cdf.(a - 1) + (1 lsl cb.Tables.ks.(a - 1)) = 1 lsl cb.Tables.km)

let () =
  QCheck_base_runner.run_tests_main
    [ test_vs_golden; test_xrdo_vs_rdo; test_roundtrip; test_m1_invariants; test_trunk_shared;
      test_interp_selfcheck; test_interp_kernels; test_xbook ]
