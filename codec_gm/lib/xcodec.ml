open Types

type structure = Serial | Trunk4 | VariantA | VariantB
type m1 = M1a | M1b | M1c

let structure_to_string = function
  | Serial -> "serial" | Trunk4 -> "trunk4" | VariantA -> "a" | VariantB -> "b"
let structure_of_string s = match String.lowercase_ascii s with
  | "serial" | "current" -> Serial | "trunk4" -> Trunk4 | "a" | "varianta" -> VariantA
  | "b" | "variantb" -> VariantB | _ -> failwith ("xcodec: unknown structure " ^ s)
let m1_to_string = function M1a -> "m1a" | M1b -> "m1b" | M1c -> "m1c"
let m1_of_string s = match String.lowercase_ascii s with
  | "m1a" | "a" -> M1a | "m1b" | "b" -> M1b | "m1c" | "c" -> M1c
  | _ -> failwith ("xcodec: unknown m1 variant " ^ s)

type config = {
  structure : structure;
  m1        : m1;
  seg_len   : int;
  book_for  : (lane:int -> col:int -> q_order:int -> codebook_idx:int -> Tables.codebook) option;
}

let default_config = { structure = Serial; m1 = M1a; seg_len = 20; book_for = None }

(* ------------------------------------------------------------------ *)
(* Emission order and predictors                                       *)
(* ------------------------------------------------------------------ *)

let emission_groups st ~n_groups =
  let n = n_groups in
  match st with
  | Serial | Trunk4 -> Array.init n (fun k -> [| 4 * k; 4 * k + 1; 4 * k + 2; 4 * k + 3 |])
  | VariantA ->
    Array.init (n + 1) (fun g ->
        if g = 0 then [| 0; 1; 2 |]
        else if g = n then [| 4 * (n - 1) + 3 |]
        else [| 4 * g; 4 * g + 1; 4 * g + 2; 4 * (g - 1) + 3 |])
  | VariantB ->
    Array.init (n + 1) (fun g ->
        if g = 0 then [| 0; 2 |]
        else if g = n then [| 4 * (n - 1) + 1; 4 * (n - 1) + 3 |]
        else [| 4 * g; 4 * g + 2; 4 * (g - 1) + 1; 4 * (g - 1) + 3 |])

(* lin(recon 4k+2, recon 4k+4), or recon(4k+2) when 4k+4 is past the slice *)
let lin_right rec_ ~n_total i =
  if i + 1 < n_total then Interp.lin rec_.(i - 1) rec_.(i + 1) else rec_.(i - 1)

let pred st ~rec_ ~n_total i =
  if i <= 0 then spec_violation "xcodec.pred: index %d" i;
  match st, i land 3 with
  | Serial, _ -> rec_.(i - 1)
  | Trunk4, 0 -> rec_.(i - 4)
  | Trunk4, _ -> rec_.(i - 1)
  | VariantA, 0 -> rec_.(i - 4)
  | VariantA, 3 -> lin_right rec_ ~n_total i
  | VariantA, _ -> rec_.(i - 1)
  | VariantB, 0 -> rec_.(i - 4)
  | VariantB, 2 -> rec_.(i - 2)
  | VariantB, 1 -> Interp.lin rec_.(i - 1) rec_.(i + 1)
  | VariantB, _ -> lin_right rec_ ~n_total i

(* ------------------------------------------------------------------ *)
(* One column, closed loop                                             *)
(* ------------------------------------------------------------------ *)

let column_encode cfg (t : Tables.t) ~col ~q_order ~macro ~pix =
  let n = Array.length pix in
  if n = 0 || n mod lane_num <> 0 then spec_violation "xcodec.column_encode: %d samples" n;
  let symbols = Array.make n 0 and recon = Array.make n (-1) in
  let groups = emission_groups cfg.structure ~n_groups:(n / 4) in
  Array.iter (fun g ->
      Array.iter (fun i ->
          let lane = i land 3 in
          let p = if i = 0 then pix.(0) else pred cfg.structure ~rec_:recon ~n_total:n i in
          if p < 0 || p > 255 then spec_violation "xcodec: pred %d at %d" p i;
          let z = Slice.channel_masked ~macro ~ch:(lane * 3 + col) in
          let res = pix.(i) - p in
          let headroom = if res < 0 then p else 255 - p in
          let r = Quant.quantize t ~lane ~q_order ~zero:z ~headroom ~res in
          let rc = p + r.Quant.q_res in
          if rc < 0 || rc > 255 then spec_violation "xcodec: recon %d at %d" rc i;
          recon.(i) <- rc;
          symbols.(i) <- Mapper.map ~q_order ~level:r.Quant.level ~sign:r.Quant.sign) g) groups;
  (symbols, recon)

(* ------------------------------------------------------------------ *)
(* Codebooks and costs                                                 *)
(* ------------------------------------------------------------------ *)

let books_of cfg (t : Tables.t) (r : Slice.resolved) (d : Slice.decision) =
  Array.init ch_num (fun ch ->
      let lane = ch / 3 and col = ch mod 3 in
      match cfg.book_for with
      | Some f -> f ~lane ~col ~q_order:r.Slice.q_order.(col) ~codebook_idx:d.Slice.codebook_idx.(col)
      | None -> r.Slice.books.(ch))

let cost (cb : Tables.codebook) s = cb.Tables.km - cb.Tables.ks.(s)

(* ------------------------------------------------------------------ *)
(* Improvement 1: odd chroma output                                    *)
(* ------------------------------------------------------------------ *)

type token = Sym of int * int | Esc of int

type side = { first_pix : int array; betas : int array array }

type encoded = {
  decision   : Slice.decision;
  side       : side;
  tokens     : token array;
  symbols    : int array array;
  chain      : int array array;
  out_ycocg  : int array array;
  recon_rgb  : int array;
  modes      : Types.group_mode array;
  sym_bits   : int;
  side_bits  : int;
  bits       : int;
  data_num   : int;
}

let segments cfg ~n_total = (n_total + cfg.seg_len - 1) / cfg.seg_len

(* chain -> output values of the chroma columns under macro; betas are
   per (col, segment) and only read for M1c *)
let m1_output cfg ~macro ~(chain : int array array) ~betas =
  let n_total = Array.length chain.(0) in
  let out = Array.map Array.copy chain in
  if macro && cfg.m1 <> M1a then
    for col = 0 to 1 do
      let i = ref 1 in
      while !i < n_total do
        out.(col).(!i) <-
          (match cfg.m1 with
           | M1a -> chain.(col).(!i)
           | M1b -> Interp.lin (Interp.ge chain.(col) (!i - 1)) (Interp.ge chain.(col) (!i + 1))
           | M1c ->
             Interp.predict_chroma_beta_u ~beta_idx:betas.(col).(!i / cfg.seg_len)
               ~c_rec:chain.(col) ~y_rec:chain.(col_y) !i);
        i := !i + 2
      done
    done;
  out

let to_rgb ~(ycocg : int array array) ~n_total =
  let rgb = Array.make (3 * n_total) 0 in
  for i = 0 to n_total - 1 do
    let (r, g, b) = Color.ycocg_u_to_rgb ~y:ycocg.(col_y).(i) ~co_u:ycocg.(col_co).(i)
        ~cg_u:ycocg.(col_cg).(i) in
    rgb.(3 * i) <- r; rgb.(3 * i + 1) <- g; rgb.(3 * i + 2) <- b
  done;
  rgb

(* ------------------------------------------------------------------ *)
(* Encoder                                                             *)
(* ------------------------------------------------------------------ *)

let encode_slice cfg (t : Tables.t) (p : Params.t) d ~rgb =
  let n = Params.slice_en_num p in
  let n_total = 4 * n in
  let r = Slice.resolve t p d in
  if Array.length rgb <> 3 * n_total then
    spec_violation "xcodec: expected %d rgb values, got %d" (3 * n_total) (Array.length rgb);
  let ycocg = Array.make_matrix 3 n_total 0 in
  for i = 0 to n_total - 1 do
    let (y, co, cg) = Color.rgb_to_ycocg_u ~r:rgb.(3 * i) ~g:rgb.(3 * i + 1) ~b:rgb.(3 * i + 2) in
    ycocg.(col_y).(i) <- y; ycocg.(col_co).(i) <- co; ycocg.(col_cg).(i) <- cg
  done;
  let sym_px = Array.make 3 [||] and chain = Array.make 3 [||] in
  for col = 0 to 2 do
    let (s, rc) = column_encode cfg t ~col ~q_order:r.Slice.q_order.(col) ~macro:r.Slice.macro
        ~pix:ycocg.(col) in
    sym_px.(col) <- s; chain.(col) <- rc
  done;
  let books = books_of cfg t r d in
  (* improvement 1 side information *)
  let nseg = segments cfg ~n_total in
  let betas =
    if r.Slice.macro && cfg.m1 = M1c then
      Array.init 2 (fun col ->
          Array.init nseg (fun s ->
              Interp.choose_beta ~c_orig:ycocg.(col) ~c_rec:chain.(col) ~y_rec:chain.(col_y)
                ~seg_start:(s * cfg.seg_len) ~seg_len:(min cfg.seg_len (n_total - s * cfg.seg_len))))
    else [||] in
  let side_bits = if Array.length betas > 0 then 6 * nseg else 0 in
  (* RLC over emission groups *)
  let groups = emission_groups cfg.structure ~n_groups:n in
  let ng = Array.length groups in
  let masked ch = r.Slice.masked.(ch) in
  let all_zero = Array.map (fun g ->
      Array.for_all (fun i ->
          let lane = i land 3 in
          List.for_all (fun col -> masked (lane * 3 + col) || sym_px.(col).(i) = 0) [ 0; 1; 2 ]) g)
      groups in
  let palette = Array.make ng false in
  let modes = Rlc.classify ~rlc_mode:r.Slice.rlc ~slice_en_num:ng ~all_zero ~palette in
  (* tokens and bits *)
  let toks = ref [] and sym_bits = ref 0 in
  Array.iteri (fun g m ->
      match m with
      | Bypass -> ()
      | Normal ->
        Array.iter (fun i ->
            let lane = i land 3 in
            for col = 0 to 2 do
              let ch = lane * 3 + col in
              if not (masked ch) then begin
                let s = sym_px.(col).(i) in
                toks := Sym (ch, s) :: !toks;
                sym_bits := !sym_bits + cost books.(ch) s
              end
            done) groups.(g)
      | Escape l ->
        toks := Esc l :: !toks;
        sym_bits := !sym_bits + cost books.(0) r.Slice.escape_sym + 3
      | Palette -> spec_violation "xcodec: palette mode is not supported") modes;
  let tokens = Array.of_list (List.rev !toks) in
  let bits = !sym_bits + side_bits in
  let data_num = (bits + Bitstream.header_length ~q_order_idx:d.Slice.q_order_idx
                  + Bitstream.block_width - 1) / Bitstream.block_width in
  let out_ycocg = m1_output cfg ~macro:r.Slice.macro ~chain ~betas in
  let symbols = Array.init ch_num (fun ch ->
      Array.init n (fun k -> sym_px.(ch mod 3).(4 * k + ch / 3))) in
  { decision = d;
    side = { first_pix = Array.init 3 (fun col -> chain.(col).(0)); betas };
    tokens; symbols; chain; out_ycocg;
    recon_rgb = to_rgb ~ycocg:out_ycocg ~n_total;
    modes; sym_bits = !sym_bits; side_bits; bits; data_num }

(* ------------------------------------------------------------------ *)
(* Decoder (forward, independent)                                      *)
(* ------------------------------------------------------------------ *)

let decode_slice cfg (t : Tables.t) (p : Params.t) d side tokens =
  let n = Params.slice_en_num p in
  let n_total = 4 * n in
  let r = Slice.resolve t p d in
  let groups = emission_groups cfg.structure ~n_groups:n in
  let ng = Array.length groups in
  let rec_ = Array.make_matrix 3 n_total (-1) in
  let pos = ref 0 in
  let next () =
    if !pos >= Array.length tokens then spec_violation "xcodec.decode: token stream exhausted";
    let tk = tokens.(!pos) in incr pos; tk in
  let zero_left = ref 0 in   (* groups of the current zero run still to output *)
  for g = 0 to ng - 1 do
    let zero_group =
      if !zero_left > 0 then (decr zero_left; true)
      else if !pos < Array.length tokens && (match tokens.(!pos) with Esc _ -> true | _ -> false) then
        (match next () with Esc l -> zero_left := l - 1; true | _ -> assert false)
      else false in
    Array.iter (fun i ->
        let lane = i land 3 in
        for col = 0 to 2 do
          let ch = lane * 3 + col in
          let q_order = r.Slice.q_order.(col) in
          let pr = if i = 0 then side.first_pix.(col) else pred cfg.structure ~rec_:rec_.(col) ~n_total i in
          let v =
            if zero_group || r.Slice.masked.(ch) then pr
            else begin
              let s = (match next () with
                  | Sym (ch', s) when ch' = ch -> s
                  | Sym (ch', _) -> spec_violation "xcodec.decode: expected ch %d, got %d" ch ch'
                  | Esc _ -> spec_violation "xcodec.decode: unexpected escape") in
              let m = Quant.dequantize t ~lane ~q_order ~pos:(Mapper.unmap ~q_order ~symbol:s) in
              (pr - m) land 0xFF
            end in
          rec_.(col).(i) <- v
        done) groups.(g)
  done;
  if !pos <> Array.length tokens then spec_violation "xcodec.decode: %d tokens left over" (Array.length tokens - !pos);
  let out = m1_output cfg ~macro:r.Slice.macro ~chain:rec_ ~betas:side.betas in
  to_rgb ~ycocg:out ~n_total

let lane_class_symbols e ~lane ~col =
  let n = Array.length e.symbols.(0) in
  Array.init n (fun k -> e.symbols.(lane * 3 + col).(k))
