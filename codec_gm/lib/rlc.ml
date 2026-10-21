open Types

type palette_cfg = { enabled : bool; tf : int; dead_lo : int; dead_hi : int }

let palette_default ~enabled = { enabled; tf = 8; dead_lo = 7; dead_hi = 9 }

let nonflat ~tf ~rgb =
  if Array.length rgb <> 3 * lane_num then
    spec_violation "rlc.nonflat: expected %d values" (3 * lane_num);
  let r = ref false in
  for lane = 1 to lane_num - 1 do
    for c = 0 to 2 do
      let d = rgb.(3 * lane + c) - rgb.(c) in
      if d > tf || d < -tf then r := true
    done
  done;
  !r

let palette_flags cfg ~slice_en_num ~rgb_groups =
  let n = Array.length rgb_groups in
  if n <> slice_en_num then
    spec_violation "rlc.palette_flags: %d groups, slice_en_num %d" n slice_en_num;
  Array.init n (fun k ->
      cfg.enabled
      && k + 1 < n
      && rgb_groups.(k + 1) = rgb_groups.(k)
      && nonflat ~tf:cfg.tf ~rgb:rgb_groups.(k)
      && k >= cfg.dead_lo
      && k < n - cfg.dead_hi)

let dead ~rlc_mode ~slice_en_num k =
  (not rlc_mode) || k < 2 || k >= slice_en_num - 2

let classify ~rlc_mode ~slice_en_num ~all_zero ~palette =
  let n = slice_en_num in
  if Array.length all_zero <> n || Array.length palette <> n then
    spec_violation "rlc.classify: array lengths differ from slice_en_num %d" n;
  let modes = Array.make n Normal in
  let zero k = all_zero.(k) && not (dead ~rlc_mode ~slice_en_num k) && not palette.(k) in
  let count = ref 0 in
  for k = 0 to n - 1 do
    if zero k then begin
      count := (if !count = 7 then 1 else !count + 1);
      let run_ends = (!count = 7) || (k + 1 >= n) || not (zero (k + 1)) in
      modes.(k) <- (if run_ends then Escape !count else Bypass);
      if run_ends then count := 0
    end
    else begin
      count := 0;
      if palette.(k) && not (dead ~rlc_mode ~slice_en_num k) then modes.(k) <- Palette
    end
  done;
  modes

type stats = { zero_groups : int; escapes : int; palettes : int }

let stats modes =
  Array.fold_left (fun s m ->
      match m with
      | Normal -> s
      | Bypass -> { s with zero_groups = s.zero_groups + 1 }
      | Escape _ -> { s with zero_groups = s.zero_groups + 1; escapes = s.escapes + 1 }
      | Palette -> { s with palettes = s.palettes + 1 })
    { zero_groups = 0; escapes = 0; palettes = 0 } modes
