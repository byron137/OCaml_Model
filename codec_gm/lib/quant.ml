open Types

type result = { level : int; sign : bool; q_res : int }

let zero_result = { level = 0; sign = false; q_res = 0 }

let piped_table (t : Tables.t) ~q_order = t.quant.(min q_order 5)

let quantize_anchor (t : Tables.t) ~q_order ~headroom ~res =
  let tbl = Tables.anchor_tbl ~q_order in
  let e = t.anchor.(tbl).(Tables.anchor_addr ~res) in
  if e.level < 0 then
    spec_violation "quant: anchor table %d has no entry for res %d (truncated .inc?)" tbl res;
  let lvl_hi = e.level in
  let lvl_lo = if lvl_hi = 0 then 0 else lvl_hi - 1 in
  (* RTL: (v_hi > +headroom) || (-headroom > v_hi) *)
  let ovf = e.v_hi > headroom || e.v_hi < -headroom in
  { level = (if ovf then lvl_lo else lvl_hi);
    sign = res < 0;
    q_res = (if ovf then e.v_lo else e.v_hi) }

let quantize_piped (t : Tables.t) ~q_order ~headroom ~res =
  let q = piped_table t ~q_order in
  let a = abs res in
  (* thermometer + priority encoder: the highest i whose threshold is met *)
  let raw = ref 0 in
  for i = 1 to Array.length q.dec - 1 do
    if q.dec.(i) <> 0 && a >= q.dec.(i) then raw := i
  done;
  let raw = !raw in
  let dn = if raw = 0 then 0 else raw - 1 in
  let hi = q.res.(raw) and lo = q.res.(dn) in
  let ovf = hi > headroom in
  let level, qa = if ovf then (dn, lo) else (raw, hi) in
  { level; sign = res < 0; q_res = (if res < 0 then -qa else qa) }

let quantize t ~lane ~q_order ~zero ~headroom ~res =
  if res < -255 || res > 255 then spec_violation "quant: res %d outside -255..255" res;
  if headroom < 0 || headroom > 255 then spec_violation "quant: headroom %d" headroom;
  if zero then zero_result
  else if lane = 0 then quantize_anchor t ~q_order ~headroom ~res
  else quantize_piped t ~q_order ~headroom ~res

let dequantize (t : Tables.t) ~lane ~q_order ~pos =
  let v =
    if lane = 0 then t.anchor_res.(Tables.anchor_tbl ~q_order).(pos)
    else (piped_table t ~q_order).res.(pos) in
  v land 0xFF

let recon_values (t : Tables.t) ~lane ~q_order =
  let tbl = if lane = 0 then t.anchor_res.(Tables.anchor_tbl ~q_order)
    else (piped_table t ~q_order).res in
  (* level 0 is always valid; further levels are valid while nonzero *)
  let vals = ref [ 0 ] in
  let i = ref 1 in
  while !i < Array.length tbl && tbl.(!i) <> 0 do vals := tbl.(!i) :: !vals; incr i done;
  Array.of_list (List.rev !vals)

let max_gap t ~lane ~q_order =
  let v = recon_values t ~lane ~q_order in
  let g = ref 0 in
  for i = 1 to Array.length v - 1 do g := max !g (v.(i) - v.(i - 1)) done;
  max !g (256 - v.(Array.length v - 1))

let anchor_available (t : Tables.t) ~q_order ~res =
  (t.anchor.(Tables.anchor_tbl ~q_order).(Tables.anchor_addr ~res)).level >= 0

let max_abs_error t ~lane ~q_order =
  let m = ref 0 in
  for res = -255 to 255 do
    (* entries absent from a truncated anchor_fullcase.inc are skipped *)
    if lane <> 0 || anchor_available t ~q_order ~res then begin
      let r = quantize t ~lane ~q_order ~zero:false ~headroom:255 ~res in
      m := max !m (abs (r.q_res - res))
    end
  done;
  !m
