open Types

type stream = { state : int }

let init = { state = 0 }

let check_state (cb : Tables.codebook) s =
  if s < 0 || s >= 1 lsl cb.km then
    spec_violation "rans: state %d outside 0..%d (km=%d)" s ((1 lsl cb.km) - 1) cb.km

let encode (cb : Tables.codebook) st ~symbol =
  if symbol < 0 || symbol >= Array.length cb.ks then
    spec_violation "rans.encode: symbol %d outside book" symbol;
  check_state cb st.state;
  let ks = cb.ks.(symbol) and cdf = cb.cdf.(symbol) in
  let len = cb.km - ks in
  if len < 0 then spec_violation "rans.encode: ks %d > km %d" ks cb.km;
  let cw = st.state lsr ks in
  let rem = st.state land ((1 lsl ks) - 1) in
  let s' = cdf + rem in
  check_state cb s';
  ({ state = s' }, cw, len)

type decoder = {
  book : Tables.codebook;
  inv  : (int * int * int) array;  (* state -> (symbol, ks, rem); symbol = -1 if uncovered *)
}

let make_decoder (cb : Tables.codebook) ~alphabet =
  let total = 1 lsl cb.km in
  let inv = Array.make total (-1, 0, 0) in
  for sym = 0 to alphabet - 1 do
    let lo = cb.cdf.(sym) and ks = cb.ks.(sym) in
    let hi = min total (lo + (1 lsl ks)) in
    for s = lo to hi - 1 do
      (* First writer wins; overlapping ranges are reported by Tables.check. *)
      let (prev, _, _) = inv.(s) in
      if prev < 0 then inv.(s) <- (sym, ks, s - lo)
    done
  done;
  { book = cb; inv }

let inverse d state =
  check_state d.book state;
  let (sym, ks, rem) = d.inv.(state) in
  if sym < 0 then spec_violation "rans.inverse: state %d not covered by any symbol" state;
  (sym, ks, rem)

let decode d st ~take_bits =
  let (sym, ks, rem) = inverse d st.state in
  let len = d.book.km - ks in
  let di = take_bits len in
  if di < 0 || di >= 1 lsl len then
    spec_violation "rans.decode: take_bits %d returned %d" len di;
  let s = (di lsl ks) + rem in
  check_state d.book s;
  ({ state = s }, sym)
