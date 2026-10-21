(* build_books <hist_file> <out_file> [--merge lane3|oddeven]
   Reads the histogram file written by `exp --dump-hist` (lines
   "lane col q_order count..."), merges lanes into classes and writes one
   dyadic codebook per (class, col, q_order) as "class col q_order km ks...".
     lane3   : class 3 = lane 3 only (E2.2)
     oddeven : class 0 = lanes 0,2 ; class 1 = lanes 1,3 (E2.4) *)
open Codec_gm

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let merge = ref "lane3" in
  let rec strip acc = function
    | "--merge" :: v :: tl -> merge := v; strip acc tl
    | x :: tl -> strip (x :: acc) tl
    | [] -> List.rev acc in
  let input, output = match strip [] args with
    | [ a; b ] -> (a, b)
    | _ -> prerr_endline "usage: build_books <hist> <out> [--merge lane3|oddeven]"; exit 2 in
  let classes = Hashtbl.create 32 in
  let ic = open_in input in
  (try while true do
       match List.map int_of_string (String.split_on_char ' ' (String.trim (input_line ic))) with
       | lane :: col :: q :: h ->
         let cls = match !merge with
           | "lane3" -> if lane = 3 then Some 3 else None
           | "oddeven" -> Some (lane land 1)
           | m -> failwith ("unknown merge " ^ m) in
         (match cls with
          | None -> ()
          | Some c ->
            let h = Array.of_list h in
            match Hashtbl.find_opt classes (c, col, q) with
            | Some acc -> Array.iteri (fun i v -> acc.(i) <- acc.(i) + v) h
            | None -> Hashtbl.replace classes (c, col, q) h)
       | _ -> ()
     done with End_of_file -> close_in ic);
  let oc = open_out output in
  Hashtbl.iter (fun (c, col, q) h ->
      let cb = Xbook.build ~hist:h in
      Printf.fprintf oc "%d %d %d %d %s\n" c col q cb.Tables.km
        (String.concat " " (Array.to_list (Array.map string_of_int cb.Tables.ks)));
      Printf.printf "class %d col %d q %d: n=%d %s avg=%.3f bits\n" c col q (Array.fold_left (+) 0 h)
        (Xbook.to_string cb) (Xbook.avg_bits cb ~hist:h)) classes;
  close_out oc
