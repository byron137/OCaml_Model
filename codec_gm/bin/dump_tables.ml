(* Loads every .inc from a directory, prints the consistency report and a
   short summary of each table. Usage: dump_tables <inc_dir> *)
open Codec_gm

let () =
  let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  let t = Tables.load ~inc_dir:dir in
  Printf.printf "== Tables.check ==\n";
  (match Tables.check t with
   | [] -> print_endline "clean"
   | l -> List.iter (fun s -> Printf.printf "  %s\n" s) l);
  Printf.printf "== quant tables (res / dec) ==\n";
  Array.iteri (fun i (q : Tables.quant_tbl) ->
      let valid = Array.fold_left (fun n v -> if v <> 0 then n + 1 else n) 1 q.res in
      Printf.printf "  order %d: %d levels, max res %d\n" i valid
        (Array.fold_left max 0 q.res))
    t.Tables.quant;
  Printf.printf "== codebooks ==\n";
  Array.iteri (fun b (cb : Tables.codebook) ->
      let n = Tables.alphabet_size ~book_idx:b in
      (* ideal cost in bits of each symbol = km - ks *)
      let costs = List.init n (fun s -> cb.km - cb.ks.(s)) in
      Printf.printf "  book %2d: km=%d alphabet=%d cost(sym0..)=%s\n" b cb.km n
        (String.concat "," (List.map string_of_int costs)))
    t.Tables.codebooks;
  Printf.printf "== anchor fullcase: %d missing entries ==\n"
    (List.length t.Tables.anchor_missing)
