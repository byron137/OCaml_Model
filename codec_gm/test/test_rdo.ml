(* Stage 7 tests: Rdo (scanner + cost evaluator + arbiter) against the RTL
   transcriptions and against the real encoder's bit count. *)
open Codec_gm

let inc_dir =
  match Sys.getenv_opt "CODEC_INC_DIR" with
  | Some d -> d
  | None -> failwith "set CODEC_INC_DIR to the directory containing the .inc files"

(* Test-only patch: make every codebook's ks agree with its cdf spacing, so
   books 0..2 are usable even with the known-inconsistent codebook_ks.inc.
   The RTL copy must be fixed for real; this keeps the tests meaningful. *)
let tables =
  let t = Tables.load ~inc_dir in
  Array.iteri (fun b (cb : Tables.codebook) ->
      let n = Tables.alphabet_size ~book_idx:b in
      for s = 0 to n - 1 do
        let next = if s + 1 < n then cb.cdf.(s + 1) else 1 lsl cb.km in
        let gap = next - cb.cdf.(s) in
        if gap > 0 && gap land (gap - 1) = 0 then begin
          let k = ref 0 in
          while 1 lsl !k < gap do incr k done;
          cb.ks.(s) <- !k
        end
      done) t.Tables.codebooks;
  t

let unit_test name f = QCheck.Test.make ~count:1 ~name QCheck.unit f

(* ---------------- min4 tie-break ---------------- *)

let min4_rtl a =
  (* literal transcription of min4 for NUM = Array.length a *)
  let pick i j = if a.(i) < a.(j) then i else j in
  match Array.length a with
  | 4 -> let x = pick 0 1 and y = pick 2 3 in if a.(x) < a.(y) then x else y
  | 3 -> let x = pick 0 1 in if a.(x) < a.(2) then x else 2
  | 2 -> pick 0 1
  | _ -> 0

let test_argmin =
  QCheck.Test.make ~count:2000 ~name:"argmin_last equals the min4 tree for NUM 1..4"
    (QCheck.make ~print:QCheck.Print.(array int)
       QCheck.Gen.(int_range 1 4 >>= fun n -> array_size (return n) (int_bound 5)))
    (fun a -> Rdo.argmin_last a = min4_rtl a)

(* ---------------- rlc_accumulator ---------------- *)

let rlc_rtl ~zero ~palette =
  let n = Array.length zero in
  let count = ref 0 and num = ref 0 and hdr = ref 0 in
  for k = 0 to n - 1 do
    let ssync = (k = 0) in
    let is_zero = zero.(k) in
    let new_rlc = is_zero && (palette.(k) || !count = 0 || !count = 7 || ssync) in
    let next_count = if (not is_zero) || palette.(k) then 0
      else if ssync || !count = 7 then 1 else !count + 1 in
    num := (if ssync then (if is_zero then 1 else 0) else !num + (if is_zero then 1 else 0));
    hdr := (if ssync then (if new_rlc then 1 else 0) else !hdr + (if new_rlc then 1 else 0));
    count := next_count
  done;
  (!num, !hdr)

let test_rlc_stats =
  QCheck.Test.make ~count:2000 ~name:"rlc_stats equals the rlc_accumulator transcription"
    (QCheck.make ~print:QCheck.Print.(pair (array bool) (array bool))
       QCheck.Gen.(int_range 1 60 >>= fun n ->
                   pair (array_size (return n) (map (fun x -> x < 70) (int_bound 99)))
                        (array_size (return n) (map (fun x -> x < 15) (int_bound 99)))))
    (fun (zero, palette) -> Rdo.rlc_stats ~zero ~palette = rlc_rtl ~zero ~palette)

(* ---------------- against the encoder ---------------- *)

let gen_rgb n = QCheck.Gen.(
    let noise = array_size (return 12) (int_bound 255) in
    let smooth base = map (fun d -> Array.init 12 (fun i -> (base.(i mod 3) + d.(i) - 8) land 255))
        (array_size (return 12) (int_bound 16)) in
    list_size (return n) (int_bound 9) >>= fun kinds ->
    let rec build prev acc = function
      | [] -> return (List.rev acc)
      | kind :: tl ->
        let g =
          if kind < 2 then noise
          else if kind < 6 then (match prev with Some g -> return (Array.init 12 (fun i -> g.(9 + i mod 3))) | None -> return (Array.make 12 77))
          else if kind < 8 then (match prev with Some g -> smooth g | None -> noise)
          else (match prev with Some g -> return g | None -> noise) in
        g >>= fun g -> build (Some g) (g :: acc) tl in
    build None [] kinds >>= fun groups -> return (Array.concat groups))

(* target_data_num classes are tied to the slice layout in the RTL
   (macro_cost and the fallback data_num are keyed on the target: <=15 for 4
   slices, <=30 for 2, else 1), so only consistent pairs are generated. *)
let gen_case = QCheck.Gen.(
    oneofl [ (Params.S4, 15); (Params.S4, 8); (Params.S4, 12); (Params.S2, 30); (Params.S2, 20);
             (Params.S1, 60); (Params.S1, 40) ]
    >>= fun (slice_num, target) ->
    int_range 1 4 >>= fun table_num ->
    bool >>= fun rlc_mode -> bool >>= fun palette_mode -> bool >>= fun rgbg_mode ->
    let p = { Params.default with slice_num; target_data_num = target; table_num;
                                  rlc_mode; palette_mode; rgbg_mode } in
    map (fun rgb -> (p, rgb)) (gen_rgb (Params.slice_en_num p)))

let print_case (p, rgb) =
  Printf.sprintf "n=%d target=%d table_num=%d rlc=%b palette=%b rgbg=%b px=%d"
    (Params.slice_en_num p) p.Params.target_data_num p.Params.table_num p.Params.rlc_mode
    p.Params.palette_mode p.Params.rgbg_mode (Array.length rgb / 3)

let test_exact_without_rlc =
  QCheck.Test.make ~count:200 ~name:"rdo: with rlc and palette off, every candidate's data_num is exact"
    (QCheck.make ~print:print_case gen_case)
    (fun (p, rgb) ->
       let p = { p with Params.rlc_mode = false; palette_mode = false } in
       let (_, tr) = Rdo.decide tables p ~rgb in
       let ok = ref true in
       for idx = 0 to 11 do
         let q = tables.Tables.qorder.(idx).Tables.q_order in
         let cb = if tables.Tables.qorder.(idx).Tables.macro_mode
           then [| 0; 0; tr.Rdo.best_book.(q.(2)).(2) |]
           else Array.init 3 (fun c -> tr.Rdo.best_book.(q.(c)).(c)) in
         let (_, st, _) = Slice.encode_slice tables p { Slice.q_order_idx = idx; codebook_idx = cb } ~rgb in
         if st.Slice.data_num <> tr.Rdo.data_num_vec.(idx) then begin
           Printf.printf "idx %d: rdo %d encoder %d (cost %d, payload %d)\n" idx
             tr.Rdo.data_num_vec.(idx) st.Slice.data_num tr.Rdo.cost_vec.(idx) st.Slice.payload_bits;
           ok := false
         end
       done;
       !ok)

let test_budget =
  QCheck.Test.make ~count:300 ~name:"rdo: chosen decision fits the target (estimate is conservative)"
    (QCheck.make ~print:print_case gen_case)
    (fun (p, rgb) ->
       let (d, tr) = Rdo.decide tables p ~rgb in
       let (_, st, _) = Slice.encode_slice tables p d ~rgb in
       if d.Slice.q_order_idx = Bitstream.macro_idx then
         Array.for_all not tr.Rdo.flags && st.Slice.data_num = tr.Rdo.chosen_num
       else begin
         let idx = d.Slice.q_order_idx in
         tr.Rdo.flags.(idx)
         && Array.for_all not (Array.sub tr.Rdo.flags 0 idx)
         && st.Slice.data_num <= tr.Rdo.data_num_vec.(idx)
         && st.Slice.data_num <= p.Params.target_data_num
       end)

let test_best_book_is_min =
  QCheck.Test.make ~count:100 ~name:"rdo: best_book minimises the raw column cost"
    (QCheck.make ~print:print_case gen_case)
    (fun (p, rgb) ->
       let (_, tr) = Rdo.decide tables p ~rgb in
       let ok = ref true in
       Array.iteri (fun o row -> Array.iteri (fun c costs ->
           let m = Array.fold_left min max_int costs in
           if costs.(tr.Rdo.best_book.(o).(c)) <> m || tr.Rdo.best_cost.(o).(c) <> m then ok := false)
           row) tr.Rdo.raw_cost;
       !ok)

let () =
  QCheck_base_runner.run_tests_main
    [ test_argmin; test_rlc_stats; test_exact_without_rlc; test_budget; test_best_book_is_min ]
