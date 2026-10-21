open Types

type decision = { q_order_idx : int; codebook_idx : int array }

type resolved = {
  q_order    : Types.q_order;
  macro      : bool;
  rlc        : bool;
  escape_sym : int;
  books      : Tables.codebook array;
  masked     : bool array;
}

(* RTL w_macro_bus = 12'b011_000_011_000: Cg/Co of lanes 1 and 3 *)
let channel_masked ~macro ~ch = macro && ch mod 3 <> col_y && (ch / 3) land 1 = 1

let resolve (t : Tables.t) (p : Params.t) d =
  if d.q_order_idx < 0 || d.q_order_idx > Bitstream.macro_idx then
    spec_violation "slice: q_order_idx %d" d.q_order_idx;
  if Array.length d.codebook_idx <> 3 then spec_violation "slice: codebook_idx length";
  Array.iter (fun c -> if c < 0 || c >= p.table_num then
                 spec_violation "slice: codebook_idx %d not below table_num %d" c p.table_num)
    d.codebook_idx;
  let e = t.qorder.(d.q_order_idx) in
  let macro = e.macro_mode || p.rgbg_mode in
  let books = Array.init ch_num (fun ch ->
      let col = ch mod 3 in
      let b = Tables.codebook_index ~q_order:e.q_order.(col) ~codebook_idx:d.codebook_idx.(col) in
      (* An inconsistent book silently desynchronises encoder and decoder
         (wrong renormalisation lengths), so refuse it up front. The macro
         book (12) never codes an escape, so its escape entry is not checked. *)
      (match Tables.book_issues t ~check_escape:(b <> Bitstream.macro_idx) ~book_idx:b with
       | [] -> ()
       | m :: _ -> spec_violation "slice: codebook %d (column %d, codebook_idx %d) is inconsistent: %s"
                     b col d.codebook_idx.(col) m);
      t.codebooks.(b)) in
  { q_order = e.q_order; macro; rlc = e.rlc_mode && p.rlc_mode;
    escape_sym = Tables.level_max ~q_order:e.q_order.(col_cg); books;
    masked = Array.init ch_num (fun ch -> channel_masked ~macro ~ch) }

(* ------------------------------------------------------------------ *)
(* Lossy core, one column                                              *)
(* ------------------------------------------------------------------ *)

let column_encode (t : Tables.t) ~col ~q_order ~macro ~zero ~pix =
  let n = Array.length pix in
  if n = 0 || n mod lane_num <> 0 then spec_violation "column_encode: %d samples" n;
  let symbols = Array.make n 0 and recon = Array.make n 0 in
  let st = ref (Dpcm.init ~first_pix:pix.(0)) in
  for i = 0 to n - 1 do
    let lane = i mod lane_num and group = i / lane_num in
    let z = zero.(group) || channel_masked ~macro ~ch:(lane * 3 + col) in
    let (res, headroom) = Dpcm.residual !st ~pix:pix.(i) in
    let r = Quant.quantize t ~lane ~q_order ~zero:z ~headroom ~res in
    st := Dpcm.reconstruct !st ~q_res:r.Quant.q_res;
    recon.(i) <- (!st).Dpcm.pred;
    symbols.(i) <- Mapper.map ~q_order ~level:r.Quant.level ~sign:r.Quant.sign
  done;
  (symbols, recon)

(* ------------------------------------------------------------------ *)
(* Statistics                                                          *)
(* ------------------------------------------------------------------ *)

type stats = {
  modes        : Types.group_mode array;
  payload_bits : int;
  data_num     : int;
  mode_hist    : int array;
  symbol_hist  : int array array;
  bits_per_ch  : int array;
}

let mode_hist modes =
  let h = Array.make 4 0 in
  Array.iter (function
      | Normal -> h.(0) <- h.(0) + 1
      | Bypass -> h.(1) <- h.(1) + 1
      | Escape _ -> h.(2) <- h.(2) + 1
      | Palette -> h.(3) <- h.(3) + 1) modes;
  h

(* ------------------------------------------------------------------ *)
(* Encoder                                                             *)
(* ------------------------------------------------------------------ *)

let split_rgb ~n rgb =
  if Array.length rgb <> 12 * n then
    spec_violation "slice: expected %d rgb values, got %d" (12 * n) (Array.length rgb);
  Array.init n (fun k -> Array.sub rgb (12 * k) 12)

let apply_palette_copies modes (rgb_groups : int array array) =
  (* LIFO: group k copies the output of group k+1, which may itself be a copy *)
  let n = Array.length modes in
  for k = n - 2 downto 0 do
    if modes.(k) = Palette then rgb_groups.(k) <- Array.copy rgb_groups.(k + 1)
  done

let encode_slice (t : Tables.t) (p : Params.t) d ~rgb =
  let n = Params.slice_en_num p in
  let r = resolve t p d in
  let rgb_groups = split_rgb ~n rgb in
  (* color transform, column-major *)
  let ycocg = Array.map (fun g -> Color.group_to_ycocg ~rgb:g) rgb_groups in
  let column col = Array.init (4 * n) (fun i -> ycocg.(i / 4).(i mod 4 * 3 + col)) in
  (* palette flags force zero residuals in the quantizer (sliding_palette TAP 0) *)
  let palette = Rlc.palette_flags (Params.palette_cfg p) ~slice_en_num:n ~rgb_groups in
  let sym = Array.make ch_num [||] and recon = Array.make 3 [||] in
  for col = 0 to 2 do
    let (s, rc) = column_encode t ~col ~q_order:r.q_order.(col) ~macro:r.macro ~zero:palette
        ~pix:(column col) in
    recon.(col) <- rc;
    for lane = 0 to 3 do
      sym.(lane * 3 + col) <- Array.init n (fun k -> s.(4 * k + lane))
    done
  done;
  let all_zero = Array.init n (fun k -> Array.for_all (fun s -> s.(k) = 0) sym) in
  let modes = Rlc.classify ~rlc_mode:r.rlc ~slice_en_num:n ~all_zero ~palette in
  (* entropy coding *)
  let w = Bitstream.Writer.create () in
  let st = Array.make ch_num Rans.init in
  let bits_per_ch = Array.make ch_num 0 in
  let symbol_hist = Array.init ch_num (fun _ -> Array.make 33 0) in
  let code ch symbol =
    let (s, cw, len) = Rans.encode r.books.(ch) st.(ch) ~symbol in
    st.(ch) <- s;
    Bitstream.Writer.push w ~value:cw ~len;
    bits_per_ch.(ch) <- bits_per_ch.(ch) + len;
    symbol_hist.(ch).(symbol) <- symbol_hist.(ch).(symbol) + 1 in
  Array.iteri (fun k m ->
      match m with
      | Bypass -> ()
      | Normal -> for ch = 0 to ch_num - 1 do if not r.masked.(ch) then code ch sym.(ch).(k) done
      | Escape _ | Palette ->
        let rlc_num = (match m with Escape l -> l | _ -> 0) in
        code 0 r.escape_sym;
        Bitstream.Writer.push w ~value:rlc_num ~len:3;
        bits_per_ch.(1) <- bits_per_ch.(1) + 3) modes;
  let last = 4 * n - 1 in
  let header = { Bitstream.q_order_idx = d.q_order_idx; codebook_idx = Array.copy d.codebook_idx;
                 last_pixel = Array.init 3 (fun col -> recon.(col).(last));
                 states = Array.map (fun s -> s.Rans.state) st } in
  Bitstream.Writer.push_header w header;
  let payload_bits = Bitstream.Writer.payload_bits w in
  let blocks = Bitstream.Writer.finish w in
  (* encoder-side reconstruction in RGB, with palette copies *)
  let recon_groups = Array.init n (fun k ->
      Color.group_to_rgb (Array.init ch_num (fun ch -> recon.(ch mod 3).(4 * k + ch / 3)))) in
  apply_palette_copies modes recon_groups;
  let stats = { modes; payload_bits; data_num = Array.length blocks; mode_hist = mode_hist modes;
                symbol_hist; bits_per_ch } in
  (blocks, stats, Array.concat (Array.to_list recon_groups))

(* ------------------------------------------------------------------ *)
(* Deblock filter                                                      *)
(* ------------------------------------------------------------------ *)

let deblock_clamp ~q_order_idx = if (q_order_idx lsr 1) land 3 = 0 then 16 else 32

let clamp8 x = if x < 0 then 0 else if x > 255 then 255 else x

let deblock ~clamp ~edge_th rgb =
  let n_pix = Array.length rgb / 3 in
  let src = Array.copy rgb in   (* neighbours are unfiltered *)
  let k = ref 1 in
  while 4 * !k + 1 < n_pix do
    let i = 4 * !k in
    let edge = ref false in
    for c = 0 to 2 do
      if abs (src.(3 * (i - 1) + c) - src.(3 * (i + 1) + c)) > edge_th then edge := true
    done;
    if not !edge then
      for c = 0 to 2 do
        let prev = src.(3 * (i - 1) + c) and cur = src.(3 * i + c) and next = src.(3 * (i + 1) + c) in
        let avg = (prev + next + 1) asr 1 in
        let delta = avg - cur in
        let delta = if delta > clamp then clamp else if delta < -clamp then -clamp else delta in
        rgb.(3 * i + c) <- clamp8 (cur + delta)
      done;
    incr k
  done

(* ------------------------------------------------------------------ *)
(* Decoder (independent LIFO implementation)                           *)
(* ------------------------------------------------------------------ *)

let decode_slice (t : Tables.t) (p : Params.t) blocks =
  let n = Params.slice_en_num p in
  let rd = Bitstream.Reader.of_blocks blocks in
  let h = Bitstream.Reader.pop_header rd in
  let d = { q_order_idx = h.Bitstream.q_order_idx; codebook_idx = Array.copy h.Bitstream.codebook_idx } in
  let r = resolve t p d in
  let dec = Array.mapi (fun ch b ->
      Rans.make_decoder b ~alphabet:(Tables.level_max ~q_order:r.q_order.(ch mod 3) + 1)) r.books in
  let ds = Array.map (fun s -> { Rans.state = s }) h.Bitstream.states in
  let take = Bitstream.Reader.take rd in
  let sym = Array.init ch_num (fun _ -> Array.make n 0) in
  let modes = Array.make n Normal in
  let bits_per_ch = Array.make ch_num 0 in
  let symbol_hist = Array.init ch_num (fun _ -> Array.make 33 0) in
  let decode ch =
    let s0 = ds.(ch).Rans.state in
    let (s, symbol) = Rans.decode dec.(ch) ds.(ch) ~take_bits:take in
    ds.(ch) <- s;
    let (_, ks, _) = Rans.inverse dec.(ch) s0 in
    bits_per_ch.(ch) <- bits_per_ch.(ch) + (r.books.(ch).km - ks);
    symbol_hist.(ch).(symbol) <- symbol_hist.(ch).(symbol) + 1;
    symbol in
  let k = ref (n - 1) in
  while !k >= 0 do
    let (sym0, _, _) = Rans.inverse dec.(0) ds.(0).Rans.state in
    if sym0 = r.escape_sym then begin
      let rlc_num = take 3 in
      bits_per_ch.(1) <- bits_per_ch.(1) + 3;
      ignore (decode 0);
      modes.(!k) <- (if rlc_num = 0 then Palette else Escape rlc_num);
      decr k;
      for _ = 2 to rlc_num do
        if !k < 0 then spec_violation "decode_slice: zero run overflows the slice start";
        modes.(!k) <- Bypass; decr k
      done
    end else begin
      for ch = ch_num - 1 downto 0 do
        if not r.masked.(ch) then sym.(ch).(!k) <- decode ch
      done;
      decr k
    end
  done;
  if Bitstream.Reader.remaining rd <> 0 then
    spec_violation "decode_slice: %d payload bits left over" (Bitstream.Reader.remaining rd);
  Array.iter (fun s -> if s.Rans.state <> 0 then
                 spec_violation "decode_slice: final state %d <> 0" s.Rans.state) ds;
  (* reverse DPCM per column *)
  let total = 4 * n in
  let ycocg = Array.make_matrix 3 total 0 in
  for col = 0 to 2 do
    let q_order = r.q_order.(col) in
    ycocg.(col).(total - 1) <- h.Bitstream.last_pixel.(col);
    for i = total - 2 downto 0 do
      let j = i + 1 in
      let pos = Mapper.unmap ~q_order ~symbol:sym.(j mod 4 * 3 + col).(j / 4) in
      let m = Quant.dequantize t ~lane:(j mod 4) ~q_order ~pos in
      ycocg.(col).(i) <- Dpcm.reconstruct_reverse ~next_pix:ycocg.(col).(j) ~q_res_mod:m
    done
  done;
  let rgb_groups = Array.init n (fun k ->
      Color.group_to_rgb (Array.init ch_num (fun ch -> ycocg.(ch mod 3).(4 * k + ch / 3)))) in
  apply_palette_copies modes rgb_groups;
  let rgb = Array.concat (Array.to_list rgb_groups) in
  if p.deblock_mode then
    deblock ~clamp:(deblock_clamp ~q_order_idx:d.q_order_idx) ~edge_th:p.deblock_edge_th rgb;
  let stats = { modes; payload_bits = Bitstream.Reader.payload_bits rd; data_num = Array.length blocks; mode_hist = mode_hist modes;
                symbol_hist; bits_per_ch } in
  (rgb, d, stats)
