(* Arbiter replica for the experimental codec: Rdo.decide with the scanner
   datapath replaced by Xcodec.column_encode (structure-aware), the RLC
   zero-group test taken over emission groups, codebook costs taken from the
   config's book override, and the macro cost extended by the M1c side
   information (3 + 6/L bits per chroma pixel, spec 1.1). With the default
   config and palette off it must reproduce Rdo.decide exactly (tested). *)
open Types

let scan_orders = Rdo.scan_orders
let candidates = Rdo.candidates

let macro_side_bits cfg (p : Params.t) =
  if cfg.Xcodec.m1 = Xcodec.M1c then 6 * ((4 * Params.slice_en_num p + cfg.Xcodec.seg_len - 1) / cfg.Xcodec.seg_len)
  else 0

let decide ?(consts = Rdo.rtl_constants) cfg (t : Tables.t) (p : Params.t) ~rgb =
  let n = Params.slice_en_num p in
  let n_total = 4 * n in
  if Array.length rgb <> 3 * n_total then
    spec_violation "xrdo: expected %d rgb values, got %d" (3 * n_total) (Array.length rgb);
  let ycocg = Array.make_matrix 3 n_total 0 in
  for i = 0 to n_total - 1 do
    let (y, co, cg) = Color.rgb_to_ycocg_u ~r:rgb.(3 * i) ~g:rgb.(3 * i + 1) ~b:rgb.(3 * i + 2) in
    ycocg.(col_y).(i) <- y; ycocg.(col_co).(i) <- co; ycocg.(col_cg).(i) <- cg
  done;
  let macro = p.Params.rgbg_mode in
  let groups = Xcodec.emission_groups cfg.Xcodec.structure ~n_groups:n in
  let ng = Array.length groups in
  let dead k = Rlc.dead ~rlc_mode:p.Params.rlc_mode ~slice_en_num:ng k in
  let sym = Array.init scan_orders (fun o ->
      Array.init 3 (fun col ->
          fst (Xcodec.column_encode cfg t ~col ~q_order:o ~macro ~pix:ycocg.(col)))) in
  let book ~lane ~col ~q_order ~b =
    match cfg.Xcodec.book_for with
    | Some f -> f ~lane ~col ~q_order ~codebook_idx:b
    | None -> t.Tables.codebooks.(Tables.codebook_index ~q_order ~codebook_idx:b) in
  let zero = Array.init scan_orders (fun o ->
      Array.init 3 (fun col ->
          Array.init ng (fun g ->
              (not (dead g))
              && Array.for_all (fun i ->
                  Slice.channel_masked ~macro ~ch:((i land 3) * 3 + col) || sym.(o).(col).(i) = 0)
                groups.(g)))) in
  let palette_group = Array.make ng false in
  let raw_cost = Array.init scan_orders (fun o ->
      Array.init 3 (fun col ->
          Array.init p.Params.table_num (fun b ->
              let s = ref 0 in
              Array.iteri (fun i symbol ->
                  let lane = i land 3 in
                  if not (Slice.channel_masked ~macro ~ch:(lane * 3 + col)) then
                    s := !s + Xcodec.cost (book ~lane ~col ~q_order:o ~b) symbol) sym.(o).(col);
              !s))) in
  let best_book = Array.map (Array.map Rdo.argmin_last) raw_cost in
  let best_cost = Array.mapi (fun o row -> Array.mapi (fun c costs -> costs.(best_book.(o).(c))) row) raw_cost in
  let rlc_num = Array.make candidates 0 and rlc_header = Array.make candidates 0 in
  for idx = 0 to candidates - 1 do
    let q = t.Tables.qorder.(idx).Tables.q_order in
    if q.(col_y) < scan_orders then begin
      let ocg = min q.(col_cg) (scan_orders - 1) and oco = min q.(col_co) (scan_orders - 1) in
      let z = Array.init ng (fun k ->
          zero.(ocg).(col_cg).(k) && zero.(oco).(col_co).(k) && zero.(q.(col_y)).(col_y).(k)) in
      let (num, hdr) = Rdo.rlc_stats ~zero:z ~palette:palette_group in
      rlc_num.(idx) <- num; rlc_header.(idx) <- hdr
    end
  done;
  let target = p.Params.target_data_num in
  let side = macro_side_bits cfg p in
  let cost_vec = Array.init candidates (fun idx ->
      let q = t.Tables.qorder.(idx).Tables.q_order in
      let e = t.Tables.qorder.(idx) in
      let c =
        if e.Tables.macro_mode then best_cost.(q.(col_y)).(col_y) + consts.Rdo.macro_cost ~target + side
        else
          best_cost.(q.(col_y)).(col_y) + best_cost.(q.(col_co)).(col_co) + best_cost.(q.(col_cg)).(col_cg)
          - (consts.Rdo.zero_cost ~rgbg:p.Params.rgbg_mode * rlc_num.(idx)
             - consts.Rdo.escape_cost.(idx) * rlc_header.(idx))
          + (if macro then side else 0) in
      max c 0) in
  let data_num_vec = Array.map (fun c ->
      (c + consts.Rdo.header_bits + consts.Rdo.block_bits - 1) / consts.Rdo.block_bits) cost_vec in
  let flags = Array.map (fun d -> d <= target) data_num_vec in
  let rec first i = if i >= candidates then None else if flags.(i) then Some i else first (i + 1) in
  match first 0 with
  | Some idx ->
    let q = t.Tables.qorder.(idx).Tables.q_order in
    let cb =
      if t.Tables.qorder.(idx).Tables.macro_mode then [| 0; 0; best_book.(q.(col_y)).(col_y) |]
      else [| best_book.(q.(col_cg)).(col_cg); best_book.(q.(col_co)).(col_co);
              best_book.(q.(col_y)).(col_y) |] in
    ({ Slice.q_order_idx = idx; codebook_idx = cb }, data_num_vec.(idx), false)
  | None ->
    ({ Slice.q_order_idx = Bitstream.macro_idx; codebook_idx = [| 0; 0; 0 |] },
     consts.Rdo.fallback_num ~target, true)
