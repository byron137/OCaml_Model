open Types

let rgb_to_ycocg_u ~r ~g ~b =
  let co = r - b in
  let tmp = b + co / 2 in
  let cg = g - tmp in
  let y = tmp + cg / 2 in
  let co_u = co / 2 + 128 in
  let cg_u = cg / 2 + 128 in
  (y land 0xFF, co_u land 0xFF, cg_u land 0xFF)

let clamp8 x = if x < 0 then 0 else if x > 255 then 255 else x

let ycocg_u_to_rgb ~y ~co_u ~cg_u =
  let co = co_u - 128 and cg = cg_u - 128 in
  let tmp = y - cg in
  let g = tmp + 2 * cg in
  let b = tmp - co in
  let r = b + 2 * co in
  (clamp8 r, clamp8 g, clamp8 b)

let group_to_ycocg ~rgb =
  if Array.length rgb <> 3 * lane_num then
    spec_violation "group_to_ycocg: expected %d values, got %d" (3 * lane_num) (Array.length rgb);
  let out = Array.make ch_num 0 in
  for lane = 0 to lane_num - 1 do
    let (y, co, cg) =
      rgb_to_ycocg_u ~r:rgb.(3 * lane) ~g:rgb.(3 * lane + 1) ~b:rgb.(3 * lane + 2) in
    out.(lane * 3 + col_y) <- y;
    out.(lane * 3 + col_co) <- co;
    out.(lane * 3 + col_cg) <- cg
  done;
  out

let group_to_rgb (g : group) =
  let out = Array.make (3 * lane_num) 0 in
  for lane = 0 to lane_num - 1 do
    let (r, gg, b) =
      ycocg_u_to_rgb ~y:g.(lane * 3 + col_y) ~co_u:g.(lane * 3 + col_co)
        ~cg_u:g.(lane * 3 + col_cg) in
    out.(3 * lane) <- r;
    out.(3 * lane + 1) <- gg;
    out.(3 * lane + 2) <- b
  done;
  out

let max_error = 1
