open Types

type constants = {
  header_bits  : int;
  block_bits   : int;
  zero_cost    : rgbg:bool -> int;
  escape_cost  : int array;
  macro_cost   : target:int -> int;
  fallback_num : target:int -> int;
}

let rtl_constants = {
  header_bits = 131;
  block_bits = 96;
  zero_cost = (fun ~rgbg -> if rgbg then 8 else 12);
  escape_cost = [| 11; 11; 11; 11; 10; 11; 10; 10; 10; 10; 0; 0 |];
  macro_cost = (fun ~target -> if target <= 15 then 540 else if target <= 30 then 1080 else 2160);
  fallback_num = (fun ~target -> if target <= 15 then 12 else if target <= 30 then 24 else 46);
}

type trace = {
  raw_cost      : int array array array;
  best_book     : int array array;
  best_cost     : int array array;
  rlc_num       : int array;
  rlc_header    : int array;
  cost_vec      : int array;
  data_num_vec  : int array;
  flags         : bool array;
  cost_overflow : bool;
  chosen_num    : int;
}

let scan_orders = 4          (* scanner datapaths at orders 0..3 *)
let candidates = 12          (* idx 0..11; 12 is the fallback *)

let symbol_cost (cb : Tables.codebook) s = cb.km - cb.ks.(s)

let argmin_last a =
  let best = ref 0 in
  for i = 1 to Array.length a - 1 do if a.(i) <= a.(!best) then best := i done;
  !best

let rlc_stats ~zero ~palette =
  let n = Array.length zero in
  if Array.length palette <> n then spec_violation "rlc_stats: array lengths differ";
  let count = ref 0 and num = ref 0 and hdr = ref 0 in
  for k = 0 to n - 1 do
    if zero.(k) then begin
      incr num;
      if palette.(k) || !count = 0 || !count = 7 || k = 0 then incr hdr;
      count := (if palette.(k) then 0 else if k = 0 || !count = 7 then 1 else !count + 1)
    end
    else count := 0
  done;
  (!num, !hdr)

let decide ?(consts = rtl_constants) (t : Tables.t) (p : Params.t) ~rgb =
  let n = Params.slice_en_num p in
  if Array.length rgb <> 12 * n then
    spec_violation "rdo: expected %d rgb values, got %d" (12 * n) (Array.length rgb);
  let rgb_groups = Array.init n (fun k -> Array.sub rgb (12 * k) 12) in
  let ycocg = Array.map (fun g -> Color.group_to_ycocg ~rgb:g) rgb_groups in
  let column col = Array.init (4 * n) (fun i -> ycocg.(i / 4).(i mod 4 * 3 + col)) in
  let palette = Rlc.palette_flags (Params.palette_cfg p) ~slice_en_num:n ~rgb_groups in
  let macro = p.rgbg_mode in
  let dead k = Rlc.dead ~rlc_mode:p.rlc_mode ~slice_en_num:n k in
  (* scanner datapaths: symbols per (order, col) *)
  let sym = Array.init scan_orders (fun o ->
      Array.init 3 (fun col ->
          fst (Slice.column_encode t ~col ~q_order:o ~macro ~zero:palette ~pix:(column col)))) in
  let zero = Array.init scan_orders (fun o ->
      Array.init 3 (fun col ->
          Array.init n (fun k ->
              (not (dead k))
              && sym.(o).(col).(4 * k) = 0 && sym.(o).(col).(4 * k + 1) = 0
              && sym.(o).(col).(4 * k + 2) = 0 && sym.(o).(col).(4 * k + 3) = 0))) in
  let palette_group = Array.init n (fun k -> palette.(k) && not (dead k)) in
  (* cost evaluator *)
  let overflow = ref false in
  let raw_cost = Array.init scan_orders (fun o ->
      Array.init 3 (fun col ->
          Array.init p.table_num (fun b ->
              let cb = t.codebooks.(Tables.codebook_index ~q_order:o ~codebook_idx:b) in
              let s = ref 0 in
              Array.iteri (fun i symbol ->
                  if not (Slice.channel_masked ~macro ~ch:(i mod 4 * 3 + col)) then
                    s := !s + symbol_cost cb symbol) sym.(o).(col);
              if !s >= 1 lsl 13 then overflow := true;
              !s))) in
  let best_book = Array.map (Array.map argmin_last) raw_cost in
  let best_cost = Array.mapi (fun o row -> Array.mapi (fun c costs -> costs.(best_book.(o).(c))) row) raw_cost in
  let rlc_num = Array.make candidates 0 and rlc_header = Array.make candidates 0 in
  for idx = 0 to candidates - 1 do
    let q = t.qorder.(idx).Tables.q_order in
    if q.(col_y) < scan_orders then begin
      let ocg = min q.(col_cg) (scan_orders - 1) and oco = min q.(col_co) (scan_orders - 1) in
      let z = Array.init n (fun k ->
          zero.(ocg).(col_cg).(k) && zero.(oco).(col_co).(k) && zero.(q.(col_y)).(col_y).(k)) in
      let (num, hdr) = rlc_stats ~zero:z ~palette:palette_group in
      rlc_num.(idx) <- num; rlc_header.(idx) <- hdr
    end
  done;
  (* arbiter *)
  let target = p.target_data_num in
  let cost_vec = Array.init candidates (fun idx ->
      let q = t.qorder.(idx).Tables.q_order in
      let e = t.qorder.(idx) in
      let c =
        if e.Tables.macro_mode then best_cost.(q.(col_y)).(col_y) + consts.macro_cost ~target
        else
          best_cost.(q.(col_y)).(col_y) + best_cost.(q.(col_co)).(col_co) + best_cost.(q.(col_cg)).(col_cg)
          - (consts.zero_cost ~rgbg:p.rgbg_mode * rlc_num.(idx) - consts.escape_cost.(idx) * rlc_header.(idx)) in
      if c >= 1 lsl 14 then overflow := true;
      max c 0) in
  let data_num_vec = Array.map (fun c ->
      (c + consts.header_bits + consts.block_bits - 1) / consts.block_bits) cost_vec in
  let flags = Array.map (fun d -> d <= target) data_num_vec in
  let rec first i = if i >= candidates then None else if flags.(i) then Some i else first (i + 1) in
  let decision, chosen_num =
    match first 0 with
    | Some idx ->
      let q = t.qorder.(idx).Tables.q_order in
      let cb =
        if t.qorder.(idx).Tables.macro_mode then [| 0; 0; best_book.(q.(col_y)).(col_y) |]
        else [| best_book.(q.(col_cg)).(col_cg); best_book.(q.(col_co)).(col_co);
                best_book.(q.(col_y)).(col_y) |] in
      ({ Slice.q_order_idx = idx; codebook_idx = cb }, data_num_vec.(idx))
    | None ->
      ({ Slice.q_order_idx = Bitstream.macro_idx; codebook_idx = [| 0; 0; 0 |] },
       consts.fallback_num ~target) in
  (decision,
   { raw_cost; best_book; best_cost; rlc_num; rlc_header; cost_vec; data_num_vec; flags;
     cost_overflow = !overflow; chosen_num })
