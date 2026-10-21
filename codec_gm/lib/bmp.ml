open Types

let u16 s o = Char.code (Bytes.get s o) lor (Char.code (Bytes.get s (o + 1)) lsl 8)
let u32 s o = u16 s o lor (u16 s (o + 2) lsl 16)
let s32 s o = let v = u32 s o in if v land 0x80000000 <> 0 then v - (1 lsl 32) else v

let read path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let b = Bytes.create len in
  really_input ic b 0 len;
  close_in ic;
  if len < 54 || Bytes.get b 0 <> 'B' || Bytes.get b 1 <> 'M' then failwith (path ^ ": not a BMP file");
  let offset = u32 b 10 in
  let dib = u32 b 14 in
  if dib < 40 then failwith (path ^ ": unsupported DIB header");
  let w = s32 b 18 and h_raw = s32 b 22 in
  let bpp = u16 b 28 and compression = u32 b 30 in
  if w <= 0 || h_raw = 0 then failwith (path ^ ": bad dimensions");
  if not (bpp = 24 || bpp = 32) then failwith (Printf.sprintf "%s: %d bpp not supported (24/32 only)" path bpp);
  if not (compression = 0 || (compression = 3 && bpp = 32)) then
    failwith (path ^ ": compressed BMP not supported");
  let top_down = h_raw < 0 in
  let h = abs h_raw in
  let bytes_pp = bpp / 8 in
  let stride = (w * bytes_pp + 3) land (lnot 3) in
  if offset + stride * h > len then failwith (path ^ ": truncated pixel data");
  let data = Array.make (3 * w * h) 0 in
  for row = 0 to h - 1 do
    let y = if top_down then row else h - 1 - row in
    let base = offset + row * stride in
    for x = 0 to w - 1 do
      let o = base + x * bytes_pp in
      let i = 3 * (y * w + x) in
      data.(i) <- Char.code (Bytes.get b (o + 2));       (* R *)
      data.(i + 1) <- Char.code (Bytes.get b (o + 1));   (* G *)
      data.(i + 2) <- Char.code (Bytes.get b o)          (* B *)
    done
  done;
  { w; h; data }

let write path (img : image) =
  let stride = (img.w * 3 + 3) land (lnot 3) in
  let size = 54 + stride * img.h in
  let b = Bytes.make size '\000' in
  let set16 o v = Bytes.set b o (Char.chr (v land 0xFF)); Bytes.set b (o + 1) (Char.chr ((v lsr 8) land 0xFF)) in
  let set32 o v = set16 o (v land 0xFFFF); set16 (o + 2) ((v lsr 16) land 0xFFFF) in
  Bytes.set b 0 'B'; Bytes.set b 1 'M';
  set32 2 size; set32 10 54; set32 14 40; set32 18 img.w; set32 22 img.h;
  set16 26 1; set16 28 24; set32 30 0; set32 34 (stride * img.h);
  set32 38 2835; set32 42 2835;
  for y = 0 to img.h - 1 do
    let row = img.h - 1 - y in
    let base = 54 + row * stride in
    for x = 0 to img.w - 1 do
      let i = 3 * (y * img.w + x) and o = base + 3 * x in
      Bytes.set b o (Char.chr (img.data.(i + 2) land 0xFF));
      Bytes.set b (o + 1) (Char.chr (img.data.(i + 1) land 0xFF));
      Bytes.set b (o + 2) (Char.chr (img.data.(i) land 0xFF))
    done
  done;
  let oc = open_out_bin path in
  output_bytes oc b;
  close_out oc
