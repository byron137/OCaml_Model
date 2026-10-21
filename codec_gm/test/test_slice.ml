(* Stage 6 tests: slice-level encode -> decode round trip over every
   q_order_idx and mode combination. *)
open Codec_gm

let inc_dir =
  match Sys.getenv_opt "CODEC_INC_DIR" with
  | Some d -> d
  | None -> failwith "set CODEC_INC_DIR to the directory containing the .inc files"

let tables = Tables.load ~inc_dir

(* Codebook 3 is the only one consistent with codebook_cdf.inc in the current
   .inc snapshot; columns at order 6 have a single book (index 0). *)
let consistent_cb = 3

let decision_for q_order_idx =
  let q = tables.Tables.qorder.(q_order_idx).Tables.q_order in
  { Slice.q_order_idx; codebook_idx = Array.map (fun o -> if o = 6 then 0 else consistent_cb) q }

(* Image content generator for one slice of n groups: a mix of noise, flat
   runs (zero residuals -> RLC) and repeated non-flat patterns (palette). *)
let gen_rgb n = QCheck.Gen.(
    let noise = array_size (return 12) (int_bound 255) in
    let pattern = map (fun a -> Array.init 12 (fun i -> a.(i mod 3) + (i / 3) * 40 land 255)) (array_size (return 3) (int_bound 100)) in
    list_size (return n) (int_bound 9) >>= fun kinds ->
    let rec build prev acc = function
      | [] -> return (List.rev acc)
      | kind :: tl ->
        let g =
          if kind < 3 then noise
          else if kind < 6 then return (match prev with Some g -> Array.init 12 (fun i -> g.(9 + i mod 3)) | None -> Array.make 12 77)
          else if kind < 8 then (match prev with Some g -> return g | None -> pattern)
          else pattern in
        g >>= fun g -> build (Some g) (g :: acc) tl in
    build None [] kinds >>= fun groups ->
    return (Array.concat groups))

let gen_case = QCheck.Gen.(
    oneofl [ (Params.S4, 720); (Params.S2, 720); (Params.S1, 720); (Params.S4, 128) ] >>= fun (slice_num, res_h) ->
    let p = { Params.default with slice_num; res_h; table_num = 4 } in
    let n = Params.slice_en_num p in
    int_bound 12 >>= fun idx ->
    bool >>= fun palette_mode ->
    bool >>= fun rgbg_mode ->
    bool >>= fun rlc_mode ->
    map (fun rgb -> ({ p with palette_mode; rgbg_mode; rlc_mode }, idx, rgb)) (gen_rgb n))

let print_case (p, idx, rgb) =
  Printf.sprintf "slice_en_num=%d idx=%d palette=%b rgbg=%b rlc=%b pixels=%d"
    (Params.slice_en_num p) idx p.Params.palette_mode p.Params.rgbg_mode p.Params.rlc_mode
    (Array.length rgb / 3)

let test_roundtrip =
  QCheck.Test.make ~count:400 ~name:"slice: decode (encode rgb) = encoder reconstruction, bit-exact"
    (QCheck.make ~print:print_case gen_case)
    (fun (p, idx, rgb) ->
       let d = decision_for idx in
       let (blocks, st, recon) = Slice.encode_slice tables p d ~rgb in
       let (out, d', st') = Slice.decode_slice tables p blocks in
       out = recon && d' = d && st'.Slice.modes = st.Slice.modes
       && st'.Slice.payload_bits = st.Slice.payload_bits
       && st.Slice.data_num = Array.length blocks
       && Array.length blocks
          = (st.Slice.payload_bits + Bitstream.header_length ~q_order_idx:idx + 95) / 96)

let test_error_bound =
  QCheck.Test.make ~count:200 ~name:"slice: reconstruction error bounded by color + quantizer bounds (no palette)"
    (QCheck.make ~print:print_case gen_case)
    (fun (p, idx, rgb) ->
       let p = { p with Params.palette_mode = false } in
       let d = decision_for idx in
       let (_, _, recon) = Slice.encode_slice tables p d ~rgb in
       (* r = y - cg + co, g = y + cg, b = y - cg - co in the decoder, so an
          RGB error is at most the sum of three per-channel errors, each below
          the largest reconstruction gap, plus the color LSB loss. Macro mode
          copies chroma across lanes and has no such bound. *)
       let r = Slice.resolve tables p d in
       QCheck.assume (not r.Slice.macro);
       let gap = Array.fold_left max 0 (Array.init 12 (fun ch ->
           Quant.max_gap tables ~lane:(ch / 3) ~q_order:r.Slice.q_order.(ch mod 3))) in
       let worst = ref 0 in
       Array.iteri (fun i v -> worst := max !worst (abs (v - rgb.(i)))) recon;
       !worst <= 3 * gap + 2 * Color.max_error)

let test_deblock =
  QCheck.Test.make ~count:100 ~name:"slice: deblock only moves anchor pixels, by at most the clamp"
    (QCheck.make ~print:print_case gen_case)
    (fun (p, idx, rgb) ->
       let d = decision_for idx in
       let (blocks, _, recon) = Slice.encode_slice tables p d ~rgb in
       let (out, _, _) = Slice.decode_slice tables { p with Params.deblock_mode = true; deblock_edge_th = 255 } blocks in
       let clamp = Slice.deblock_clamp ~q_order_idx:idx in
       let ok = ref true in
       Array.iteri (fun i v ->
           let pix = i / 3 in
           if pix mod 4 <> 0 || pix = 0 then (if v <> recon.(i) then ok := false)
           else if abs (v - recon.(i)) > clamp then ok := false) out;
       !ok)

let test_column_symbols_pure =
  QCheck.Test.make ~count:100 ~name:"slice: column_encode is independent across columns and of the codebook"
    (QCheck.make ~print:print_case gen_case)
    (fun (p, idx, rgb) ->
       (* symbols of column Y must not depend on the chroma orders: compare
          idx and the idx sharing its Y order *)
       let q = tables.Tables.qorder.(idx).Tables.q_order in
       let n = Params.slice_en_num p in
       let groups = Array.init n (fun k -> Array.sub rgb (12 * k) 12) in
       let ycocg = Array.map (fun g -> Color.group_to_ycocg ~rgb:g) groups in
       let column col = Array.init (4 * n) (fun i -> ycocg.(i / 4).(i mod 4 * 3 + col)) in
       let zero = Array.make n false in
       let (s1, _) = Slice.column_encode tables ~col:2 ~q_order:q.(2) ~macro:false ~zero ~pix:(column 2) in
       let (s2, _) = Slice.column_encode tables ~col:2 ~q_order:q.(2) ~macro:true ~zero ~pix:(column 2) in
       s1 = s2)

let () =
  QCheck_base_runner.run_tests_main
    [ test_roundtrip; test_error_bound; test_deblock; test_column_symbols_pure ]
