type slice_num = S1 | S2 | S4

type t = {
  res_h           : int;
  slice_num       : slice_num;
  target_data_num : int;
  table_num       : int;
  rlc_mode        : bool;
  palette_mode    : bool;
  palette_tf      : int;
  palette_dead_lo : int;
  palette_dead_hi : int;
  rgbg_mode       : bool;
  deblock_mode    : bool;
  deblock_edge_th : int;
}

let default = {
  res_h = 720; slice_num = S4; target_data_num = 15; table_num = 1;
  rlc_mode = true; palette_mode = true; palette_tf = 8;
  palette_dead_lo = 7; palette_dead_hi = 9;
  rgbg_mode = false; deblock_mode = false; deblock_edge_th = 255;
}

let slice_count p = match p.slice_num with S1 -> 1 | S2 -> 2 | S4 -> 4
let slice_en_num p = p.res_h / (4 * slice_count p)

let palette_cfg p =
  { Rlc.enabled = p.palette_mode; tf = p.palette_tf;
    dead_lo = p.palette_dead_lo; dead_hi = p.palette_dead_hi }

let of_args p args =
  let bool_of = function
    | "on" | "1" | "true" -> true
    | "off" | "0" | "false" -> false
    | s -> failwith ("expected on|off, got " ^ s) in
  let int_of s = try int_of_string s with _ -> failwith ("expected an integer, got " ^ s) in
  let rec go p rest = function
    | "--slice" :: v :: tl ->
      let sn = (match int_of v with 1 -> S1 | 2 -> S2 | 4 -> S4 | _ -> failwith "--slice 1|2|4") in
      go { p with slice_num = sn } rest tl
    | "--target" :: v :: tl -> go { p with target_data_num = int_of v } rest tl
    | "--table-num" :: v :: tl ->
      let n = int_of v in
      if n < 1 || n > 4 then failwith "--table-num 1..4";
      go { p with table_num = n } rest tl
    | "--rlc" :: v :: tl -> go { p with rlc_mode = bool_of v } rest tl
    | "--palette" :: v :: tl -> go { p with palette_mode = bool_of v } rest tl
    | "--palette-tf" :: v :: tl -> go { p with palette_tf = int_of v } rest tl
    | "--rgbg" :: v :: tl -> go { p with rgbg_mode = bool_of v } rest tl
    | "--deblock" :: v :: tl -> go { p with deblock_mode = bool_of v } rest tl
    | "--deblock-th" :: v :: tl -> go { p with deblock_edge_th = int_of v } rest tl
    | s :: _ when String.length s > 2 && String.sub s 0 2 = "--" -> failwith ("unknown option " ^ s)
    | s :: tl -> go p (s :: rest) tl
    | [] -> (p, List.rev rest) in
  go p [] args
