open Types

type kernel = Linear | QuadL | QuadR | DD4 | Wiener of int | Auto

let kernel_to_string = function
  | Linear -> "linear" | QuadL -> "quadl" | QuadR -> "quadr" | DD4 -> "dd4"
  | Wiener k -> Printf.sprintf "wiener%d" k | Auto -> "auto"

let kernel_of_string s =
  match String.lowercase_ascii s with
  | "linear" -> Linear | "quadl" -> QuadL | "quadr" -> QuadR | "dd4" -> DD4 | "auto" -> Auto
  | s when String.length s > 6 && String.sub s 0 6 = "wiener" ->
    let k = int_of_string (String.sub s 6 (String.length s - 6)) in
    if k < -4 || k > 3 then failwith ("interp: wiener k out of -4..3: " ^ s);
    Wiener k
  | _ -> failwith ("interp: unknown kernel " ^ s)

let lin a b = (a + b + 1) asr 1

let ge rec_ i =
  let n = Array.length rec_ in
  if i land 1 <> 0 then spec_violation "interp.ge: odd index %d" i;
  let i = if i < 0 then 0 else if i > n - 2 then n - 2 else i in
  rec_.(i)

let clamp_between a b v =
  let lo = min a b and hi = max a b in
  if v < lo then lo else if v > hi then hi else v

let auto_select rec_ n =
  let e_3 = ge rec_ (n - 3) and e_1 = ge rec_ (n - 1)
  and e1 = ge rec_ (n + 1) and e3 = ge rec_ (n + 3) in
  let c_l = abs (e_3 - 2 * e_1 + e1) and c_r = abs (e_1 - 2 * e1 + e3) in
  if c_l + c_r <= 4 then DD4
  else if 2 * (1 + c_l) < 1 + c_r then QuadL
  else if 2 * (1 + c_r) < 1 + c_l then QuadR
  else Linear

let rec predict_odd k rec_ n =
  if n land 1 = 0 then spec_violation "interp.predict_odd: even index %d" n;
  let e_1 = ge rec_ (n - 1) and e1 = ge rec_ (n + 1) in
  match k with
  | Linear -> lin e_1 e1
  | QuadL -> (- (ge rec_ (n - 3)) + 6 * e_1 + 3 * e1 + 4) asr 3
  | QuadR -> (3 * e_1 + 6 * e1 - ge rec_ (n + 3) + 4) asr 3
  | DD4 ->
    clamp_between e_1 e1 ((9 * (e_1 + e1) - (ge rec_ (n - 3) + ge rec_ (n + 3)) + 8) asr 4)
  | Wiener k ->
    let outer = ge rec_ (n - 3) + ge rec_ (n + 3) in
    clamp_between e_1 e1 ((16 * (e_1 + e1) + k * (outer - (e_1 + e1)) + 16) asr 5)
  | Auto -> clamp_between e_1 e1 (predict_odd (auto_select rec_ n) rec_ n)

let beta_half = [| 0; 1; 2; 4; 8; -2; -4; -8 |]

let delta_y y_rec n = y_rec.(n) - lin (ge y_rec (n - 1)) (ge y_rec (n + 1))

let predict_chroma_beta ~beta_idx ~c_rec ~y_rec n =
  if beta_idx < 0 || beta_idx > 7 then spec_violation "interp: beta_idx %d" beta_idx;
  lin (ge c_rec (n - 1)) (ge c_rec (n + 1)) + ((beta_half.(beta_idx) * delta_y y_rec n) asr 1)

let predict_chroma_beta_u ~beta_idx ~c_rec ~y_rec n =
  if beta_idx < 0 || beta_idx > 7 then spec_violation "interp: beta_idx %d" beta_idx;
  let v = lin (ge c_rec (n - 1)) (ge c_rec (n + 1)) + ((beta_half.(beta_idx) * delta_y y_rec n) asr 2) in
  if v < 0 then 0 else if v > 255 then 255 else v

let choose_beta ~c_orig ~c_rec ~y_rec ~seg_start ~seg_len =
  let best = ref 0 and best_sad = ref max_int in
  for idx = 0 to 7 do
    let sad = ref 0 in
    let n = ref (seg_start lor 1) in
    while !n < seg_start + seg_len do
      sad := !sad + abs (c_orig.(!n) - predict_chroma_beta_u ~beta_idx:idx ~c_rec ~y_rec !n);
      n := !n + 2
    done;
    if !sad < !best_sad then begin best_sad := !sad; best := idx end
  done;
  !best
