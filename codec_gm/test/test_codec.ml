(* Stage 3 tests: table parsing and rANS. Needs CODEC_INC_DIR pointing at the
   RTL image_codec directory that holds the .inc files. *)
open Codec_gm

let inc_dir =
  match Sys.getenv_opt "CODEC_INC_DIR" with
  | Some d -> d
  | None -> failwith "set CODEC_INC_DIR to the directory containing the .inc files"

let tables = Tables.load ~inc_dir

(* ---------------- tokenizer ---------------- *)

let test_tokenizer =
  QCheck.Test.make ~count:1 ~name:"ints_of_line examples" QCheck.unit (fun () ->
      let eq l s = Tables.ints_of_line s = l in
      eq [3; 16] "w_res_table_vec[ 3] = 16;"
      && eq [0; 0; -255; -253; 31]
        "assign w_anchor_fullcase [0 * `MAX_RES_NUM +   0] = {-9'sd255, -9'sd253, 5'd31};"
      && eq [1; 511; 255; 253; 15]
        "assign w_anchor_fullcase [1 * `MAX_RES_NUM + 511] = { 9'sd255,  9'sd253, 5'd15};"
      && eq [0; 8] "assign w_lookup_table_km[ 0] = 4'd8;"
      && eq [0; 128; 0; 7; 0; 0]
        "(w_r >= 8'd0   && w_r < 8'd128) : begin w_symbol = 5'd 0; w_ks = 3'd7; w_rem = w_r - 8'd0  ; w_escape = 1'd0; end"
      && eq [255] "8'hFF"
      && eq [] "// Order (0,1)   32 31 30")

(* ---------------- table consistency ---------------- *)

let test_check =
  QCheck.Test.make ~count:1 ~name:"Tables.check: only known issues" QCheck.unit (fun () ->
      let issues = Tables.check tables in
      List.iter print_endline issues;
      (* Known/accepted: book 12 (macro) cannot encode an escape; anchor entries
         may be missing if the .inc copy is truncated. Anything else fails. *)
      List.for_all (fun s ->
          String.length s >= 8 && (String.sub s 0 8 = "book 12:" || String.sub s 0 6 = "anchor"))
        issues)

let test_freq_dyadic =
  QCheck.Test.make ~count:1 ~name:"codebooks: frequencies are 2^ks and sum to 2^km" QCheck.unit
    (fun () ->
       Array.to_list tables.Tables.codebooks
       |> List.mapi (fun b (cb : Tables.codebook) -> (b, cb))
       |> List.for_all (fun (b, (cb : Tables.codebook)) ->
           let n = Tables.alphabet_size ~book_idx:b in
           let n = if b = 12 then n - 1 else n in   (* macro book: escape excluded *)
           let sum = ref 0 in
           for s = 0 to n - 1 do sum := !sum + (1 lsl cb.ks.(s)) done;
           !sum = 1 lsl cb.km))

(* ---------------- rANS round trip ---------------- *)

(* LIFO bit buffer: head of the list is the most recently pushed bit. *)
let push bits ~value ~len =
  for i = 0 to len - 1 do bits := ((value lsr i) land 1) :: !bits done

let take bits n =
  let v = ref 0 in
  for j = n - 1 downto 0 do
    match !bits with
    | b :: rest -> v := !v lor (b lsl j); bits := rest
    | [] -> failwith "bit buffer underflow"
  done;
  !v

let symbols_gen ~book_idx =
  let n = Tables.alphabet_size ~book_idx in
  let n = if book_idx = 12 then n - 1 else n in
  QCheck.Gen.(list_size (int_bound 300) (int_bound (n - 1)))

let roundtrip book_idx =
  let cb = tables.Tables.codebooks.(book_idx) in
  let dec = Rans.make_decoder cb ~alphabet:(Tables.alphabet_size ~book_idx) in
  QCheck.Test.make ~count:200
    ~name:(Printf.sprintf "rans round trip, book %d" book_idx)
    (QCheck.make ~print:QCheck.Print.(list int) (symbols_gen ~book_idx))
    (fun syms ->
       let bits = ref [] in
       let st = List.fold_left (fun st sym ->
           let (st', cw, len) = Rans.encode cb st ~symbol:sym in
           push bits ~value:cw ~len; st') Rans.init syms in
       (* decode in reverse, starting from the final state *)
       let st, decoded = List.fold_left (fun (st, acc) _ ->
           let (st', sym) = Rans.decode dec st ~take_bits:(take bits) in
           (st', sym :: acc)) (st, []) syms in
       decoded = syms && st.Rans.state = 0 && !bits = [])

let roundtrips = List.init 13 roundtrip

(* ---------------- inverse vs codebook_r.inc ---------------- *)

(* Parses the nested case statement of codebook_r.inc into
   (book_idx, lo, hi, symbol, ks, escape) tuples. The RTL encodes the escape
   as symbol 0 with w_escape = 1; ranges are "w_r >= lo && w_r < hi" or
   "w_r == v". Order 6 (book 12) is a bit-slice, not a table, and is skipped. *)
let parse_codebook_r path =
  let ic = open_in path in
  let order_pair = ref 0 and cb_idx = ref 0 and acc = ref [] in
  let has s sub =
    let n = String.length s and m = String.length sub in
    let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in go 0 in
  let add lo hi sym ks esc =
    if !order_pair < 3 then acc := (!order_pair * 4 + !cb_idx, lo, hi, sym, ks, esc) :: !acc in
  (try
     while true do
       let l = input_line ic in
       if has l "w_r >=" then
         (match Tables.ints_of_line l with
          | lo :: hi :: sym :: ks :: _ :: esc :: _ -> add lo hi sym ks esc
          | _ -> failwith ("codebook_r: unexpected range line: " ^ l))
       else if has l "w_r ==" then
         (match Tables.ints_of_line l with
          | v :: sym :: ks :: _ :: esc :: _ -> add v (v + 1) sym ks esc
          | _ -> failwith ("codebook_r: unexpected point line: " ^ l))
       else if has l ": begin" && not (has l "default") then
         (match Tables.ints_of_line l with
          | o :: _ when has l "3'd" -> order_pair := o lsr 1
          | c :: _ when has l "2'd" -> cb_idx := c
          | _ -> ())
     done
   with End_of_file -> close_in ic);
  List.rev !acc

let test_inverse_vs_rtl =
  QCheck.Test.make ~count:1 ~name:"Rans.inverse matches codebook_r.inc" QCheck.unit (fun () ->
      let ranges = parse_codebook_r (Filename.concat inc_dir "codebook_r.inc") in
      let decs = Array.mapi (fun b cb ->
          Rans.make_decoder cb ~alphabet:(Tables.alphabet_size ~book_idx:b)) tables.Tables.codebooks in
      Printf.printf "codebook_r.inc: %d ranges\n" (List.length ranges);
      ranges <> [] &&
      List.for_all (fun (b, lo, hi, sym, ks, esc) ->
          let level_max = Tables.alphabet_size ~book_idx:b - 1 in
          let expect_sym = if esc = 1 then level_max else sym in
          let ok = ref true in
          for s = lo to hi - 1 do
            let (sym', ks', rem') = Rans.inverse decs.(b) s in
            if not (sym' = expect_sym && ks' = ks && rem' = s - lo) then begin
              Printf.printf "book %d state %d: rtl (sym %d, ks %d, esc %d) model (sym %d, ks %d, rem %d)\n"
                b s sym ks esc sym' ks' rem';
              ok := false
            end
          done;
          !ok) ranges)

let () =
  QCheck_base_runner.run_tests_main
    ([ test_tokenizer; test_check; test_freq_dyadic; test_inverse_vs_rtl ] @ roundtrips)
