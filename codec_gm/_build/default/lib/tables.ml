open Types

type quant_tbl = { res : int array; dec : int array }
type anchor_entry = { v_hi : int; v_lo : int; level : int }
type codebook = { km : int; ks : int array; cdf : int array }
type qorder_entry = { q_order : Types.q_order; macro_mode : bool; rlc_mode : bool }

type t = {
  quant          : quant_tbl array;
  anchor         : anchor_entry array array;
  anchor_res     : int array array;
  anchor_missing : (int * int) list;
  codebooks      : codebook array;
  qorder         : qorder_entry array;
}

let threshold_num = 32
let anchor_tbl_num = 3
let anchor_size = 512
let codebook_num = 13
let code_num = threshold_num + 1

(* ------------------------------------------------------------------ *)
(* Tokenizer                                                           *)
(* ------------------------------------------------------------------ *)

let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '`'
let is_ident_char c = is_ident_start c || is_digit c

let strip_comment s =
  match String.index_opt s '/' with
  | Some i when i + 1 < String.length s && s.[i + 1] = '/' -> String.sub s 0 i
  | _ -> s

let ints_of_line (line : string) : int list =
  let s = strip_comment line in
  let n = String.length s in
  let acc = ref [] in
  let i = ref 0 in
  let skip_spaces k = let k = ref k in while !k < n && s.[!k] = ' ' do incr k done; !k in
  let read_with pred base k =
    let k0 = skip_spaces k in
    let k = ref k0 in
    while !k < n && pred s.[!k] do incr k done;
    if !k = k0 then None
    else begin
      let txt = String.sub s k0 (!k - k0) in
      let v = match base with
        | `Dec -> int_of_string txt
        | `Hex -> int_of_string ("0x" ^ txt)
        | `Bin -> int_of_string ("0b" ^ txt) in
      Some (v, !k)
    end in
  while !i < n do
    let c = s.[!i] in
    if is_ident_start c then begin
      while !i < n && is_ident_char s.[!i] do incr i done
    end
    else if c = '-' || is_digit c then begin
      let neg = (c = '-') in
      let j = if neg then !i + 1 else !i in
      match read_with is_digit `Dec j with
      | None -> incr i
      | Some (v, k) ->
        if k < n && s.[k] = '\'' then begin
          (* Verilog sized literal: <width>'[s]<base><digits> *)
          let k = k + 1 in
          let k = if k < n && (s.[k] = 's' || s.[k] = 'S') then k + 1 else k in
          let base_c = if k < n then s.[k] else 'd' in
          let k = k + 1 in
          let r = match base_c with
            | 'd' | 'D' -> read_with is_digit `Dec k
            | 'h' | 'H' -> read_with is_hex `Hex k
            | 'b' | 'B' -> read_with (fun c -> c = '0' || c = '1') `Bin k
            | _ -> failwith (Printf.sprintf "unsupported literal base '%c'" base_c) in
          (match r with
           | None -> failwith (Printf.sprintf "malformed sized literal in: %s" line)
           | Some (v, k') -> acc := (if neg then -v else v) :: !acc; i := k')
        end else begin
          acc := (if neg then -v else v) :: !acc; i := k
        end
    end
    else incr i
  done;
  List.rev !acc

(* ------------------------------------------------------------------ *)
(* File helpers                                                        *)
(* ------------------------------------------------------------------ *)

let read_lines path =
  let ic = open_in_bin path in
  let rec go acc =
    match input_line ic with
    | l -> go (l :: acc)
    | exception End_of_file -> close_in ic; List.rev acc in
  go []

let fail path lineno msg =
  failwith (Printf.sprintf "%s:%d: %s" path lineno msg)

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  m = 0 || go 0

(* ------------------------------------------------------------------ *)
(* Parsers                                                             *)
(* ------------------------------------------------------------------ *)

(* "<name>[ 3] = 16;" style, one entry per line. *)
let parse_vec path ~name ~size =
  let tbl = Array.make size 0 in
  let seen = Array.make size false in
  List.iteri (fun ln l ->
      if contains l name then
        match ints_of_line l with
        | [idx; v] when idx >= 0 && idx < size -> tbl.(idx) <- v; seen.(idx) <- true
        | _ -> fail path (ln + 1) "expected '<name>[idx] = value;'")
    (read_lines path);
  Array.iteri (fun i s -> if not s then fail path 0 (Printf.sprintf "missing entry %d" i)) seen;
  tbl

let parse_anchor path =
  let tbl = Array.init anchor_tbl_num (fun _ ->
      Array.make anchor_size { v_hi = 0; v_lo = 0; level = -1 }) in
  let lines = read_lines path in
  let last = List.length lines in
  List.iteri (fun ln l ->
      if contains l "w_anchor_fullcase" then
        match ints_of_line l with
        | [t; off; v_hi; v_lo; level]
          when t >= 0 && t < anchor_tbl_num && off >= 0 && off < anchor_size ->
          tbl.(t).(off) <- { v_hi; v_lo; level }
        | _ ->
          (* A truncated trailing line is tolerated here and surfaced by [check]. *)
          if ln + 1 < last then
            fail path (ln + 1) "expected '{v_hi, v_lo, level}' entry")
    lines;
  let missing = ref [] in
  Array.iteri (fun t a ->
      Array.iteri (fun off e -> if e.level < 0 then missing := (t, off) :: !missing) a) tbl;
  tbl, List.rev !missing

let parse_codebooks dir =
  let km_path = Filename.concat dir "codebook_km.inc"
  and ks_path = Filename.concat dir "codebook_ks.inc"
  and cdf_path = Filename.concat dir "codebook_cdf.inc" in
  let km = parse_vec km_path ~name:"w_lookup_table_km" ~size:codebook_num in
  let parse_rows path name =
    let rows = Array.make codebook_num [||] in
    List.iteri (fun ln l ->
        if contains l name then
          match ints_of_line l with
          | idx :: entries when idx >= 0 && idx < codebook_num ->
            if List.length entries <> code_num then
              fail path (ln + 1) (Printf.sprintf "expected %d entries, got %d"
                                    code_num (List.length entries));
            (* The RTL concatenation lists index 32 first; store index 0 first. *)
            rows.(idx) <- Array.of_list (List.rev entries)
          | _ -> fail path (ln + 1) "expected '<name>[idx] = {...};'")
      (read_lines path);
    Array.iteri (fun i r -> if Array.length r = 0 then
                    fail path 0 (Printf.sprintf "missing book %d" i)) rows;
    rows in
  let ks = parse_rows ks_path "w_lookup_table_ks" in
  let cdf = parse_rows cdf_path "w_lookup_table_cdf" in
  Array.init codebook_num (fun i -> { km = km.(i); ks = ks.(i); cdf = cdf.(i) })

(* compressor_ctrl.v quant_order_idx_2_bus, stored as [|cg; co; y|]. *)
let qorder_table =
  let e y c ~macro ~rlc = { q_order = [| c; c; y |]; macro_mode = macro; rlc_mode = rlc } in
  [|
    e 0 0 ~macro:false ~rlc:true;   (* 0  *)
    e 0 1 ~macro:false ~rlc:true;   (* 1  *)
    e 1 0 ~macro:false ~rlc:true;   (* 2  *)
    e 1 1 ~macro:false ~rlc:true;   (* 3  *)
    e 1 2 ~macro:false ~rlc:true;   (* 4  *)
    e 2 1 ~macro:false ~rlc:true;   (* 5  *)
    e 2 2 ~macro:false ~rlc:true;   (* 6  *)
    e 2 3 ~macro:false ~rlc:true;   (* 7  *)
    e 3 2 ~macro:false ~rlc:true;   (* 8  *)
    e 3 3 ~macro:false ~rlc:true;   (* 9  *)
    e 2 6 ~macro:true  ~rlc:false;  (* 10 *)
    e 3 6 ~macro:true  ~rlc:false;  (* 11 *)
    e 6 6 ~macro:true  ~rlc:false;  (* 12 (default branch) *)
  |]

let load ~inc_dir =
  let p f = Filename.concat inc_dir f in
  let quant = Array.init 6 (fun i ->
      { res = parse_vec (p (Printf.sprintf "quant_table_res_%02d.inc" i))
            ~name:"w_res_table_vec" ~size:threshold_num;
        dec = parse_vec (p (Printf.sprintf "quant_table_dec_%02d.inc" i))
            ~name:"w_dec_table_vec" ~size:threshold_num }) in
  let anchor, anchor_missing = parse_anchor (p "anchor_fullcase.inc") in
  let anchor_res = Array.init anchor_tbl_num (fun i ->
      parse_vec (p (Printf.sprintf "quant_anchor_%02d.inc" i))
        ~name:"w_res_table_vec" ~size:threshold_num) in
  { quant; anchor; anchor_res; anchor_missing;
    codebooks = parse_codebooks inc_dir; qorder = qorder_table }

(* ------------------------------------------------------------------ *)
(* Index helpers                                                       *)
(* ------------------------------------------------------------------ *)

let anchor_addr ~res =
  if res < -256 || res > 255 then spec_violation "anchor_addr: res %d out of 9-bit range" res;
  let sign = if res < 0 then 1 else 0 in
  ((1 - sign) lsl 8) lor (res land 0xFF)

let anchor_tbl ~q_order =
  let p = q_order lsr 1 in if p >= 2 then 2 else p

let codebook_index ~q_order ~codebook_idx =
  let idx = ((q_order lsr 1) lsl 2) lor codebook_idx in
  if idx >= codebook_num then
    spec_violation "codebook_index: q_order %d codebook_idx %d -> %d exceeds %d books"
      q_order codebook_idx idx codebook_num;
  idx

let level_max ~q_order =
  match q_order with 0 | 1 -> 32 | 2 | 3 -> 16 | _ -> 8

let alphabet_size ~book_idx =
  let pair = book_idx / 4 in
  1 + (match pair with 0 -> 32 | 1 -> 16 | _ -> 8)

(* ------------------------------------------------------------------ *)
(* Consistency check                                                   *)
(* ------------------------------------------------------------------ *)

let book_issues ?(check_escape = true) t ~book_idx:b =
  let cb = t.codebooks.(b) in
  let issues = ref [] in
  let add fmt = Printf.ksprintf (fun s -> issues := s :: !issues) fmt in
  let total = 1 lsl cb.km in
  let n = alphabet_size ~book_idx:b in
  (* symbols 0 .. n-2 are regular, n-1 is the escape *)
  for s = 0 to n - 2 do
    let next = if s + 1 < n then cb.cdf.(s + 1) else total in
    if cb.cdf.(s) + (1 lsl cb.ks.(s)) <> next then
      add "book %d: symbol %d cdf %d + 2^%d <> next cdf %d" b s cb.cdf.(s) cb.ks.(s) next
  done;
  let e = n - 1 in
  if check_escape && cb.cdf.(e) + (1 lsl cb.ks.(e)) > total then
    add "book %d: escape symbol %d not encodable (cdf %d + 2^%d > 2^%d)"
      b e cb.cdf.(e) cb.ks.(e) cb.km;
  List.rev !issues

let check t =
  let issues = ref [] in
  let add fmt = Printf.ksprintf (fun s -> issues := s :: !issues) fmt in
  (match t.anchor_missing with
   | [] -> ()
   | l ->
     let (t0, a0) = List.hd l and (t1, a1) = List.nth l (List.length l - 1) in
     add "anchor_fullcase: %d missing entries, first (tbl %d, addr %d), last (tbl %d, addr %d)"
       (List.length l) t0 a0 t1 a1);
  let per_book = List.concat (List.init codebook_num (fun b -> book_issues t ~book_idx:b)) in
  List.rev !issues @ per_book
