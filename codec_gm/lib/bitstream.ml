open Types

type header = {
  q_order_idx  : int;
  codebook_idx : int array;
  last_pixel   : int array;
  states       : int array;
}

let block_width = 96
let block_words = 3
let macro_idx = 12
let state_bits ~q_order_idx = if q_order_idx = macro_idx then 3 else 8
let header_length ~q_order_idx = 1 + 4 + 6 + 24 + ch_num * state_bits ~q_order_idx

let check_header h =
  if h.q_order_idx < 0 || h.q_order_idx > macro_idx then
    spec_violation "header: q_order_idx %d" h.q_order_idx;
  if Array.length h.codebook_idx <> 3 || Array.length h.last_pixel <> 3
     || Array.length h.states <> ch_num then spec_violation "header: field lengths";
  Array.iter (fun v -> if v < 0 || v > 3 then spec_violation "header: codebook_idx %d" v) h.codebook_idx;
  Array.iter (fun v -> if v < 0 || v > 255 then spec_violation "header: last_pixel %d" v) h.last_pixel;
  let sb = state_bits ~q_order_idx:h.q_order_idx in
  Array.iter (fun v -> if v < 0 || v >= 1 lsl sb then
                 spec_violation "header: state %d does not fit %d bits" v sb) h.states

module Writer = struct
  type t = { bv : Bitvec.t; mutable payload : int; mutable has_header : bool }

  let create () = { bv = Bitvec.create (); payload = -1; has_header = false }

  let push t ~value ~len =
    if t.has_header then spec_violation "bitstream.push after header";
    Bitvec.push_int t.bv ~value ~len

  let push_header t h =
    if t.has_header then spec_violation "bitstream: header pushed twice";
    check_header h;
    t.payload <- Bitvec.length t.bv;
    let sb = state_bits ~q_order_idx:h.q_order_idx in
    Array.iter (fun s -> Bitvec.push_int t.bv ~value:s ~len:sb) h.states;      (* ch0 lowest *)
    Array.iter (fun p -> Bitvec.push_int t.bv ~value:p ~len:8) h.last_pixel;   (* cg, co, y *)
    Array.iter (fun c -> Bitvec.push_int t.bv ~value:c ~len:2) h.codebook_idx; (* cg, co, y *)
    Bitvec.push_int t.bv ~value:h.q_order_idx ~len:4;
    Bitvec.push_bit t.bv 1;
    t.has_header <- true

  let payload_bits t =
    if not t.has_header then spec_violation "bitstream.payload_bits before header";
    t.payload

  let finish t =
    if not t.has_header then spec_violation "bitstream.finish before header";
    Bitvec.to_blocks t.bv ~width:block_width ~words:block_words
end

module Reader = struct
  type t = { bv : Bitvec.t; mutable top : int; mutable payload : int }
  (* top = number of unread payload bits *)

  let of_blocks blocks = { bv = Bitvec.of_blocks blocks; top = -1; payload = -1 }

  let pop_header t =
    let marker = match Bitvec.highest_set t.bv with
      | Some m -> m
      | None -> spec_violation "bitstream: no header marker in slice" in
    if marker < 4 then spec_violation "bitstream: marker at bit %d, header truncated" marker;
    let q_order_idx = Bitvec.read_int t.bv ~pos:(marker - 4) ~len:4 in
    let len = header_length ~q_order_idx in
    let start = marker + 1 - len in
    if start < 0 then spec_violation "bitstream: header of %d bits does not fit" len;
    let sb = state_bits ~q_order_idx in
    let pos = ref start in
    let rd n = let v = Bitvec.read_int t.bv ~pos:!pos ~len:n in pos := !pos + n; v in
    let states = Array.init ch_num (fun _ -> rd sb) in
    let last_pixel = Array.init 3 (fun _ -> rd 8) in
    let codebook_idx = Array.init 3 (fun _ -> rd 2) in
    let idx' = rd 4 in
    if idx' <> q_order_idx || rd 1 <> 1 then spec_violation "bitstream: header parse mismatch";
    t.top <- start;
    t.payload <- start;
    { q_order_idx; codebook_idx; last_pixel; states }

  let take t n =
    if t.top < 0 then spec_violation "bitstream.take before pop_header";
    if n < 0 || n > t.top then spec_violation "bitstream.take: %d bits requested, %d left" n t.top;
    t.top <- t.top - n;
    Bitvec.read_int t.bv ~pos:t.top ~len:n

  let remaining t = if t.top < 0 then 0 else t.top
  let payload_bits t = t.payload
end
