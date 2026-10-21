(* Stage 5 tests: Bitvec, Bitstream (header + blocks), Rlc, and an
   entropy-layer round trip (Rlc + Rans + Bitstream over 12 channels). *)
open Codec_gm

let inc_dir =
  match Sys.getenv_opt "CODEC_INC_DIR" with
  | Some d -> d
  | None -> failwith "set CODEC_INC_DIR to the directory containing the .inc files"

let tables = Tables.load ~inc_dir
let unit_test name f = QCheck.Test.make ~count:1 ~name QCheck.unit f

(* ---------------- Bitvec ---------------- *)

let gen_fields = QCheck.Gen.(list_size (int_bound 200) (int_bound 8 >>= fun len ->
    map (fun v -> (v, len)) (int_bound ((1 lsl len) - 1))))

let test_bitvec_roundtrip =
  QCheck.Test.make ~count:300 ~name:"bitvec: push_int / read_int / blocks / hex round trip"
    (QCheck.make ~print:QCheck.Print.(list (pair int int)) gen_fields)
    (fun fields ->
       let bv = Bitvec.create () in
       List.iter (fun (v, len) -> Bitvec.push_int bv ~value:v ~len) fields;
       let pos = ref 0 in
       let ok = List.for_all (fun (v, len) ->
           let r = Bitvec.read_int bv ~pos:!pos ~len in pos := !pos + len; r = v) fields in
       let blocks = Bitvec.to_blocks bv ~width:96 ~words:3 in
       let hex = Array.map Bitvec.block_to_hex blocks in
       let blocks' = Array.map (Bitvec.block_of_hex ~words:3) hex in
       let bv' = Bitvec.of_blocks blocks' in
       let n = Bitvec.length bv in
       let prefix_ok = ref true in
       for i = 0 to n - 1 do if Bitvec.get bv' i <> Bitvec.get bv i then prefix_ok := false done;
       let pad_ok = ref true in
       for i = n to Bitvec.length bv' - 1 do if Bitvec.get bv' i <> 0 then pad_ok := false done;
       ok && blocks = blocks' && !prefix_ok && !pad_ok
       && Bitvec.length bv' = 96 * Array.length blocks
       && Array.length blocks = (n + 95) / 96)

(* ---------------- Bitstream header ---------------- *)

let gen_header = QCheck.Gen.(
    int_bound 12 >>= fun q_order_idx ->
    let sb = if q_order_idx = 12 then 3 else 8 in
    let states = array_size (return 12) (int_bound ((1 lsl sb) - 1)) in
    map3 (fun states last_pixel codebook_idx ->
        { Bitstream.q_order_idx; codebook_idx; last_pixel; states })
      states (array_size (return 3) (int_bound 255)) (array_size (return 3) (int_bound 3)))

let print_header (h : Bitstream.header) =
  Printf.sprintf "idx=%d cb=%s px=%s st=%s" h.q_order_idx
    (QCheck.Print.(array int) h.codebook_idx) (QCheck.Print.(array int) h.last_pixel)
    (QCheck.Print.(array int) h.states)

let test_bitstream_roundtrip =
  QCheck.Test.make ~count:500 ~name:"bitstream: payload + header written, header parsed, payload read back LIFO"
    (QCheck.make ~print:(QCheck.Print.pair print_header QCheck.Print.(list (pair int int)))
       QCheck.Gen.(pair gen_header gen_fields))
    (fun (h, fields) ->
       let w = Bitstream.Writer.create () in
       List.iter (fun (v, len) -> Bitstream.Writer.push w ~value:v ~len) fields;
       Bitstream.Writer.push_header w h;
       let payload = Bitstream.Writer.payload_bits w in
       let blocks = Bitstream.Writer.finish w in
       let r = Bitstream.Reader.of_blocks blocks in
       let h' = Bitstream.Reader.pop_header r in
       let back = List.rev_map (fun (_, len) -> (Bitstream.Reader.take r len, len)) (List.rev fields) in
       (* [back] was produced from the reversed list, so it is in original order again *)
       h' = h && back = fields && Bitstream.Reader.remaining r = 0
       && Array.length blocks
          = (payload + Bitstream.header_length ~q_order_idx:h.q_order_idx + 95) / 96)

(* ---------------- Rlc ---------------- *)

let gen_slice = QCheck.Gen.(
    int_range 8 45 >>= fun n ->
    triple (return n)
      (array_size (return n) (map (fun x -> x < 60) (int_bound 99)))   (* 60 % zero groups *)
      (array_size (return n) (map (fun x -> x < 10) (int_bound 99))))  (* 10 % palette flags *)
let print_slice = QCheck.Print.(triple int (array bool) (array bool))

(* Register-level transcription of rlc_detector.v: decisions taken when group
   k arrives apply to slot k-1; one extra step flushes the last slot. *)
let rtl_classify ~rlc_mode ~n ~all_zero ~palette =
  let dead k = Rlc.dead ~rlc_mode ~slice_en_num:n k in
  let modes = Array.make n Types.Normal in
  let count = ref 0 and curr_is_zero = ref false in
  for k = 0 to n do
    let next_is_zero = k < n && all_zero.(k) && not (dead k) && not palette.(k) in
    let palette_prev = k >= 1 && palette.(k - 1) && not (dead (k - 1)) in
    let escape = (!curr_is_zero && not next_is_zero) || !count = 7 || (!count = 0 && palette_prev) in
    let bypass = !count > 0 && not escape in
    if k >= 1 then
      modes.(k - 1) <-
        (if escape then (if !count = 0 then Types.Palette else Types.Escape !count)
         else if bypass then Types.Bypass else Types.Normal);
    count := (if not next_is_zero then 0 else if !count = 7 then 1 else !count + 1);
    curr_is_zero := next_is_zero
  done;
  modes

let test_rlc_vs_rtl =
  QCheck.Test.make ~count:1000 ~name:"rlc: classify equals register-level rlc_detector transcription"
    (QCheck.make ~print:print_slice gen_slice)
    (fun (n, all_zero, palette) ->
       List.for_all (fun rlc_mode ->
           Rlc.classify ~rlc_mode ~slice_en_num:n ~all_zero ~palette
           = rtl_classify ~rlc_mode ~n ~all_zero ~palette) [ true; false ])

let test_rlc_invariants =
  QCheck.Test.make ~count:1000 ~name:"rlc: dead zone, chunk lengths, run termination"
    (QCheck.make ~print:print_slice gen_slice)
    (fun (n, all_zero, palette) ->
       let modes = Rlc.classify ~rlc_mode:true ~slice_en_num:n ~all_zero ~palette in
       let ok = ref true in
       let run = ref 0 in
       Array.iteri (fun k m ->
           let dead = Rlc.dead ~rlc_mode:true ~slice_en_num:n k in
           (match m with
            | Types.Normal -> if !run <> 0 then ok := false
            | Types.Bypass -> if dead then ok := false; incr run; if !run > 6 then ok := false
            | Types.Escape len ->
              if dead || len < 1 || len > 7 || len <> !run + 1 then ok := false; run := 0
            | Types.Palette -> if dead || !run <> 0 || not palette.(k) then ok := false))
         modes;
       !ok && !run = 0
       && Rlc.classify ~rlc_mode:false ~slice_en_num:n ~all_zero ~palette
          = Array.make n Types.Normal)

let test_palette_flags =
  unit_test "rlc: palette flags need equality, non-flat pattern and dead zone" (fun () ->
      let flat = Array.make 12 100 in
      let pattern = Array.init 12 (fun i -> if i >= 3 && i mod 3 = 0 then 200 else 100) in   (* R of lanes 1..3 differs from lane 0 *)
      let n = 40 in
      let groups = Array.init n (fun k ->
          if k = 20 || k = 21 then pattern else if k = 30 || k = 31 then flat else Array.make 12 k) in
      let cfg = Rlc.palette_default ~enabled:true in
      let f = Rlc.palette_flags cfg ~slice_en_num:n ~rgb_groups:groups in
      let expect = Array.init n (fun k -> k = 20) in
      f = expect
      && not (Rlc.nonflat ~tf:8 ~rgb:flat) && Rlc.nonflat ~tf:8 ~rgb:pattern
      && Rlc.palette_flags (Rlc.palette_default ~enabled:false) ~slice_en_num:n ~rgb_groups:groups
         = Array.make n false)

(* ---------------- entropy layer round trip ---------------- *)

(* Uses codebook_idx 3 for every column: those books are consistent with
   codebook_cdf.inc in the current .inc snapshot; switch to idx 0..2 once
   codebook_ks.inc is corrected. *)
let cb_idx = 3

let gen_groups = QCheck.Gen.(
    int_bound 9 >>= fun q_order_idx ->
    let q = tables.Tables.qorder.(q_order_idx).Tables.q_order in
    int_range 8 45 >>= fun n ->
    (* 50 % all-zero groups, otherwise random symbols below level_max per channel *)
    let group =
      bool >>= fun zero ->
      if zero then return (Array.make 12 0)
      else map (fun rs -> Array.mapi (fun ch r -> r mod Tables.level_max ~q_order:q.(ch mod 3)) rs)
          (array_size (return 12) (int_bound 1000)) in
    map (fun gp -> (q_order_idx, n, gp))
      (pair (array_size (return n) group)
         (array_size (return n) (map (fun x -> x < 10) (int_bound 99)))))

let print_groups = QCheck.Print.(triple int int (pair (array (array int)) (array bool)))

let test_entropy_layer =
  QCheck.Test.make ~count:300 ~name:"entropy layer: Rlc + 12-stream Rans + Bitstream round trip"
    (QCheck.make ~print:print_groups gen_groups)
    (fun (q_order_idx, n, (groups, palette)) ->
       let q = tables.Tables.qorder.(q_order_idx).Tables.q_order in
       let book ch = tables.Tables.codebooks.(Tables.codebook_index ~q_order:q.(ch mod 3) ~codebook_idx:cb_idx) in
       let esc = Tables.level_max ~q_order:q.(0) in
       let all_zero = Array.map (fun g -> Array.for_all (fun s -> s = 0) g) groups in
       let modes = Rlc.classify ~rlc_mode:true ~slice_en_num:n ~all_zero ~palette in
       (* encode *)
       let w = Bitstream.Writer.create () in
       let st = Array.make 12 Rans.init in
       Array.iteri (fun k m ->
           match m with
           | Types.Bypass -> ()
           | Types.Normal ->
             for ch = 0 to 11 do
               let (s, cw, len) = Rans.encode (book ch) st.(ch) ~symbol:groups.(k).(ch) in
               st.(ch) <- s; Bitstream.Writer.push w ~value:cw ~len
             done
           | Types.Escape _ | Types.Palette ->
             let rlc_num = (match m with Types.Escape l -> l | _ -> 0) in
             let (s, cw, l0) = Rans.encode (book 0) st.(0) ~symbol:esc in
             st.(0) <- s; Bitstream.Writer.push w ~value:cw ~len:l0;
             Bitstream.Writer.push w ~value:rlc_num ~len:3) modes;
       let header = { Bitstream.q_order_idx; codebook_idx = Array.make 3 cb_idx;
                      last_pixel = [| 1; 2; 3 |]; states = Array.map (fun s -> s.Rans.state) st } in
       Bitstream.Writer.push_header w header;
       let blocks = Bitstream.Writer.finish w in
       (* decode, LIFO, without looking at [modes] *)
       let r = Bitstream.Reader.of_blocks blocks in
       let h = Bitstream.Reader.pop_header r in
       let dec = Array.mapi (fun ch b -> Rans.make_decoder b ~alphabet:(Tables.level_max ~q_order:q.(ch mod 3) + 1))
           (Array.init 12 book) in
       let ds = Array.map (fun s -> { Rans.state = s }) h.Bitstream.states in
       let out_groups = Array.make n [||] and out_modes = Array.make n Types.Normal in
       let k = ref (n - 1) in
       while !k >= 0 do
         let (sym0, _, _) = Rans.inverse dec.(0) ds.(0).Rans.state in
         if sym0 = esc then begin
           let rlc_num = Bitstream.Reader.take r 3 in
           let (s, _) = Rans.decode dec.(0) ds.(0) ~take_bits:(Bitstream.Reader.take r) in
           ds.(0) <- s;
           out_groups.(!k) <- Array.make 12 0;
           out_modes.(!k) <- (if rlc_num = 0 then Types.Palette else Types.Escape rlc_num);
           decr k;
           for _ = 2 to rlc_num do
             out_groups.(!k) <- Array.make 12 0; out_modes.(!k) <- Types.Bypass; decr k
           done
         end else begin
           let g = Array.make 12 0 in
           for ch = 11 downto 0 do
             let (s, sym) = Rans.decode dec.(ch) ds.(ch) ~take_bits:(Bitstream.Reader.take r) in
             ds.(ch) <- s; g.(ch) <- sym
           done;
           out_groups.(!k) <- g; decr k
         end
       done;
       let expect = Array.mapi (fun k g -> if modes.(k) = Types.Normal then g else Array.make 12 0) groups in
       h.Bitstream.q_order_idx = q_order_idx
       && out_modes = modes && out_groups = expect
       && Bitstream.Reader.remaining r = 0
       && Array.for_all (fun s -> s.Rans.state = 0) ds)

let () =
  QCheck_base_runner.run_tests_main
    [ test_bitvec_roundtrip; test_bitstream_roundtrip; test_rlc_vs_rtl; test_rlc_invariants;
      test_palette_flags; test_entropy_layer ]
