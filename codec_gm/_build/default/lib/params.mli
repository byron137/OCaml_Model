(** Fixed parameters of one run (the RTL regfile inputs and elaboration
    parameters). *)

type slice_num = S1 | S2 | S4
(** RTL i_slice_num: 2'b00 -> 4 slices (res_h >> 4 groups each), 2'b01 -> 2,
    otherwise 1. The RTL encoding is inverted with respect to intuition, so
    a variant is used. *)

type t = {
  res_h           : int;        (** line width in pixels, 720 *)
  slice_num       : slice_num;
  target_data_num : int;        (** 96-bit block budget per slice (scanner i_target_data_num, 7 bits) *)
  table_num       : int;        (** codebooks selectable per column, 1..4 (RTL `TABLE_NUM`, currently 1) *)
  rlc_mode        : bool;       (** regfile i_rlc_mode *)
  palette_mode    : bool;       (** regfile i_palette_mode *)
  palette_tf      : int;        (** sliding_palette i_palette_tf, bound to 8 *)
  palette_dead_lo : int;        (** see Rlc.palette_cfg *)
  palette_dead_hi : int;
  rgbg_mode       : bool;       (** forces macro mode on every q_order_idx *)
  deblock_mode    : bool;       (** uncompressor i_deblock_mode *)
  deblock_edge_th : int;        (** uncompressor i_deblock_edge_th, 8 bits *)
}

val default : t
(** 720 / S4 / target 15 / table_num 1 / rlc on / palette on, tf 8 /
    rgbg off / deblock off, edge_th 255. *)

val slice_en_num : t -> int
(** Groups (4-pixel enables) per slice = res_h >> (4 | 3 | 2). *)

val slice_count : t -> int
(** Slices per line = 4 | 2 | 1. *)

val palette_cfg : t -> Rlc.palette_cfg

val of_args : t -> string list -> t * string list
(** Parses recognised options ([--slice 1|2|4], [--target N], [--table-num N],
    [--rlc on|off], [--palette on|off], [--palette-tf N], [--rgbg on|off],
    [--deblock on|off], [--deblock-th N]) and returns the remaining
    positional arguments. Raises [Failure] on a malformed option. *)
