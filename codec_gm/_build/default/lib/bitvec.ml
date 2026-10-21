open Types

type t = { mutable bits : Bytes.t; mutable len : int }

let create () = { bits = Bytes.make 1024 '\000'; len = 0 }
let length t = t.len

let get t i =
  if i < 0 || i >= t.len then spec_violation "bitvec.get: index %d outside 0..%d" i (t.len - 1);
  Char.code (Bytes.unsafe_get t.bits i)

let ensure t n =
  if n > Bytes.length t.bits then begin
    let nb = Bytes.make (max n (2 * Bytes.length t.bits)) '\000' in
    Bytes.blit t.bits 0 nb 0 t.len;
    t.bits <- nb
  end

let push_bit t b =
  ensure t (t.len + 1);
  Bytes.unsafe_set t.bits t.len (Char.unsafe_chr (b land 1));
  t.len <- t.len + 1

let push_int t ~value ~len =
  if len < 0 || len > 62 then spec_violation "bitvec.push_int: len %d" len;
  if value < 0 || (len < 62 && value >= 1 lsl len) then
    spec_violation "bitvec.push_int: value %d does not fit in %d bits" value len;
  for i = 0 to len - 1 do push_bit t ((value lsr i) land 1) done

let read_int t ~pos ~len =
  if pos < 0 || pos + len > t.len then
    spec_violation "bitvec.read_int: [%d, %d) outside length %d" pos (pos + len) t.len;
  let v = ref 0 in
  for i = len - 1 downto 0 do v := (!v lsl 1) lor get t (pos + i) done;
  !v

let highest_set t =
  let rec go i = if i < 0 then None else if get t i = 1 then Some i else go (i - 1) in
  go (t.len - 1)

let copy t = { bits = Bytes.copy t.bits; len = t.len }
let of_bits a = let t = create () in Array.iter (push_bit t) a; t
let to_bits t = Array.init t.len (get t)

let to_blocks t ~width ~words =
  if words * 32 < width then spec_violation "bitvec.to_blocks: %d words cannot hold %d bits" words width;
  let n = (t.len + width - 1) / width in
  Array.init n (fun j ->
      Array.init words (fun w ->
          let v = ref 0 in
          for i = 31 downto 0 do
            let p = j * width + w * 32 + i in
            let b = if p < t.len && w * 32 + i < width then get t p else 0 in
            v := (!v lsl 1) lor b
          done;
          !v))

let of_blocks blocks =
  let t = create () in
  Array.iter (fun blk ->
      Array.iter (fun w -> for i = 0 to 31 do push_bit t ((w lsr i) land 1) done) blk) blocks;
  t

let block_to_hex blk =
  let b = Buffer.create 32 in
  for w = Array.length blk - 1 downto 0 do Buffer.add_string b (Printf.sprintf "%08x" blk.(w)) done;
  Buffer.contents b

let block_of_hex ~words s =
  if String.length s <> 8 * words then spec_violation "block_of_hex: expected %d hex digits" (8 * words);
  Array.init words (fun w ->
      let msw = words - 1 - w in
      int_of_string ("0x" ^ String.sub s (8 * msw) 8))
