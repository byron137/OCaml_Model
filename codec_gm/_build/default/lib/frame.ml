open Types

type slice_record = {
  y         : int;
  slice_idx : int;
  decision  : Slice.decision;
  blocks    : int array array;
  stats     : Slice.stats;
  trace     : Rdo.trace option;
}

let check_geometry (p : Params.t) (img : image) =
  let count = Params.slice_count p in
  if img.w <> p.res_h then
    spec_violation "frame: image width %d <> res_h %d" img.w p.res_h;
  if p.res_h mod (4 * count) <> 0 then
    spec_violation "frame: res_h %d is not a multiple of %d" p.res_h (4 * count);
  if Array.length img.data <> 3 * img.w * img.h then
    spec_violation "frame: image data length %d <> 3*%d*%d" (Array.length img.data) img.w img.h

let slice_pixels p = 4 * Params.slice_en_num p

let slice_rgb (p : Params.t) (img : image) ~y ~slice_idx =
  let np = slice_pixels p in
  let x0 = slice_idx * np in
  Array.sub img.data (3 * (y * img.w + x0)) (3 * np)

let blit_slice (p : Params.t) (img : image) ~y ~slice_idx rgb =
  let np = slice_pixels p in
  if Array.length rgb <> 3 * np then spec_violation "blit_slice: %d values" (Array.length rgb);
  Array.blit rgb 0 img.data (3 * (y * img.w + slice_idx * np)) (3 * np)

let encode ?consts (t : Tables.t) (p : Params.t) (img : image) =
  check_geometry p img;
  let count = Params.slice_count p in
  let recon = { w = img.w; h = img.h; data = Array.make (Array.length img.data) 0 } in
  let records = Array.make (img.h * count) None in
  for y = 0 to img.h - 1 do
    for s = 0 to count - 1 do
      let rgb = slice_rgb p img ~y ~slice_idx:s in
      let (decision, trace) = Rdo.decide ?consts t p ~rgb in
      let (blocks, stats, rc) = Slice.encode_slice t p decision ~rgb in
      blit_slice p recon ~y ~slice_idx:s rc;
      records.(y * count + s) <- Some { y; slice_idx = s; decision; blocks; stats; trace = Some trace }
    done
  done;
  (Array.map (function Some r -> r | None -> assert false) records, recon)

let decode (t : Tables.t) (p : Params.t) ~w ~h slices =
  let count = Params.slice_count p in
  let img = { w; h; data = Array.make (3 * w * h) 0 } in
  check_geometry p img;
  let seen = Array.make (h * count) false in
  let records = Array.make (h * count) None in
  Array.iter (fun (y, s, blocks) ->
      if y < 0 || y >= h || s < 0 || s >= count then
        spec_violation "frame.decode: slice (y=%d, s=%d) outside %dx%d" y s h count;
      if seen.(y * count + s) then spec_violation "frame.decode: slice (y=%d, s=%d) given twice" y s;
      seen.(y * count + s) <- true;
      let (rgb, decision, stats) = Slice.decode_slice t p blocks in
      blit_slice p img ~y ~slice_idx:s rgb;
      records.(y * count + s) <- Some { y; slice_idx = s; decision; blocks; stats; trace = None })
    slices;
  Array.iteri (fun i b -> if not b then
                  spec_violation "frame.decode: slice (y=%d, s=%d) missing" (i / count) (i mod count)) seen;
  (img, Array.map (function Some r -> r | None -> assert false) records)
