let col_cg = 0
let col_co = 1
let col_y  = 2
let lane_num = 4
let ch_num   = 12

type group = int array
type q_order = int array
type group_mode = Normal | Bypass | Escape of int | Palette
type image = { w : int; h : int; data : int array }

exception Spec_violation of string

let spec_violation fmt = Printf.ksprintf (fun s -> raise (Spec_violation s)) fmt
