open Types

type col_state = { pred : int }

let init ~first_pix =
  if first_pix < 0 || first_pix > 255 then spec_violation "dpcm.init: pixel %d" first_pix;
  { pred = first_pix }

let residual st ~pix =
  let res = pix - st.pred in
  let headroom = if res < 0 then st.pred else 255 - st.pred in
  (res, headroom)

let reconstruct st ~q_res =
  let r = st.pred + q_res in
  if r < 0 || r > 255 then
    spec_violation "dpcm.reconstruct: pred %d + q_res %d = %d overflows" st.pred q_res r;
  { pred = r }

let reconstruct_reverse ~next_pix ~q_res_mod = (next_pix + q_res_mod) land 0xFF
