(** L4: frame iteration. Every line is cut into Params.slice_count independent
    slices of 4*slice_en_num pixels, left to right; each slice is decided by
    Rdo and coded by Slice on its own. *)

type slice_record = {
  y         : int;
  slice_idx : int;
  decision  : Slice.decision;
  blocks    : int array array;      (** 96-bit blocks, [|lo; mid; hi|] words *)
  stats     : Slice.stats;
  trace     : Rdo.trace option;     (** None for records read back from a .data file *)
}

val check_geometry : Params.t -> Types.image -> unit
(** Raises [Types.Spec_violation] unless image.w = res_h and res_h is a
    multiple of 4 * slice_count. *)

val slice_rgb : Params.t -> Types.image -> y:int -> slice_idx:int -> int array
(** The 12*slice_en_num R,G,B values of one slice. *)

val blit_slice : Params.t -> Types.image -> y:int -> slice_idx:int -> int array -> unit

val encode : ?consts:Rdo.constants -> Tables.t -> Params.t -> Types.image ->
  slice_record array * Types.image
(** Records in raster order (y major, slice_idx minor) and the encoder-side
    reconstruction, which the decoder must reproduce bit-exactly (before its
    optional deblock filter). *)

val decode : Tables.t -> Params.t -> w:int -> h:int ->
  (int * int * int array array) array -> Types.image * slice_record array
(** Input: (y, slice_idx, blocks) per slice, any order. Every (y, slice_idx)
    of the frame must be present exactly once. *)
