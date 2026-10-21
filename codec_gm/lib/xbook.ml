(* Offline dyadic codebook construction from a symbol histogram (spec 2.4,
   E2.2 / E2.4: lane-3 and odd/even codebooks "built offline from the
   residual distribution").

   A book is {km; ks; cdf} with freq(s) = 2^ks(s), sum = 2^km, ks <= 7
   (3-bit field), every symbol including the escape at least 2^0. km is
   the one the RTL uses for the order pair (8 / 7 / 5). Construction: start
   from floor(log2(p * 2^km)) per symbol (at least 0), then spend the
   remaining budget one doubling at a time on the symbol whose doubling
   saves the most bits (p * 1 bit) among those whose doubling still fits;
   the budget is always spendable because the smallest frequency is 1. *)

let km_for_alphabet = function 33 -> 8 | 17 -> 7 | 9 -> 5
  | a -> Types.spec_violation "xbook: alphabet %d" a

let build ~(hist : int array) : Tables.codebook =
  let a = Array.length hist in
  let km = km_for_alphabet a in
  let total = 1 lsl km in
  let n = Array.fold_left (+) 0 hist in
  let p s = if n = 0 then 1.0 /. float a else float hist.(s) /. float n in
  let ks = Array.init a (fun s ->
      let f = p s *. float total in
      let k = if f < 1.0 then 0 else int_of_float (floor (log f /. log 2.0)) in
      max 0 (min 7 k)) in
  let sum () = Array.fold_left (fun acc k -> acc + (1 lsl k)) 0 ks in
  (* floor can overshoot through rounding only if p*total is exactly a power
     of two for everyone; lower the largest until it fits *)
  while sum () > total do
    let i = ref 0 in
    Array.iteri (fun s k -> if k > ks.(!i) then i := s) ks;
    ks.(!i) <- ks.(!i) - 1
  done;
  let budget = ref (total - sum ()) in
  while !budget > 0 do
    let best = ref (-1) and gain = ref (-1.0) in
    Array.iteri (fun s k ->
        if k < 7 && (1 lsl k) <= !budget then begin
          let g = p s in
          if g > !gain then begin gain := g; best := s end
        end) ks;
    if !best < 0 then Types.spec_violation "xbook: cannot place remaining budget %d" !budget;
    budget := !budget - (1 lsl ks.(!best));
    ks.(!best) <- ks.(!best) + 1
  done;
  let cdf = Array.make a 0 in
  let acc = ref 0 in
  for s = 0 to a - 1 do cdf.(s) <- !acc; acc := !acc + (1 lsl ks.(s)) done;
  { Tables.km; ks; cdf }

let cost (cb : Tables.codebook) s = cb.Tables.km - cb.Tables.ks.(s)

(* average bits/symbol of a book over a histogram *)
let avg_bits (cb : Tables.codebook) ~(hist : int array) =
  let n = Array.fold_left (+) 0 hist in
  if n = 0 then 0.0
  else float (Array.fold_left (+) 0 (Array.mapi (fun s c -> c * cost cb s) hist)) /. float n

let to_string (cb : Tables.codebook) =
  Printf.sprintf "km=%d ks=[%s]" cb.Tables.km
    (String.concat "," (Array.to_list (Array.map string_of_int cb.Tables.ks)))
