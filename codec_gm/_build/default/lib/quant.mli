(** L1: nonlinear quantizer with headroom back-off and two table families.
    RTL nonlinear_quantizer.v (encoder) / distribution_unmapper.v (decoder).

    - lane 0 (the anchor, first pixel of each group): level_activation_feedback.
      Looks up the fullcase LUT [Tables.anchor], table chosen by
      [Tables.anchor_tbl q_order], indexed by [Tables.anchor_addr res].
      The entry gives level, v_hi (reconstruction) and v_lo (one level down).
    - lanes 1..3: level_activation_piped. level_raw = highest i with
      dec.(i) <> 0 && |res| >= dec.(i); reconstruction = res.(level_raw).
      Tables: [Tables.quant q_order] with q_order 5 and 6 sharing table 5.
    - both: if |reconstruction| > headroom, drop one level (never below 0)
      and use that level's reconstruction instead.
    - [zero] (macro-mode chroma or a palette group): level 0, sign false,
      q_res 0.

    Decoder side ([dequantize]) is index-by-position and modular: it returns
    the 8-bit value res.(pos), congruent to -q_res mod 256, that
    Dpcm.reconstruct_reverse adds.

    Worst-case |q_res - res| without back-off (exhaustive):
      anchor lane : order 0,1 -> 19; 2,3 -> 37; 4..6 -> 63
      lanes 1..3  : order 0,1 -> 15; 2,3 -> 31; 4..6 -> 63 *)

type result = {
  level : int;    (** 0..level_max-1 *)
  sign  : bool;   (** res < 0 *)
  q_res : int;    (** signed reconstruction fed back into Dpcm *)
}

val quantize :
  Tables.t -> lane:int -> q_order:int -> zero:bool -> headroom:int -> res:int -> result
(** Raises [Types.Spec_violation] on a missing anchor entry. *)

val dequantize : Tables.t -> lane:int -> q_order:int -> pos:int -> int
(** Modular 8-bit residual for position [pos] (see Mapper): lane 0 reads
    [Tables.anchor_res], other lanes read the quant res table. *)

val max_abs_error : Tables.t -> lane:int -> q_order:int -> int
(** Worst |q_res - res| over res in -255..255 with headroom 255 (no back-off),
    obtained by exhaustive scan. Under back-off the error is bounded by the
    largest gap between consecutive reconstruction values instead. *)

val max_gap : Tables.t -> lane:int -> q_order:int -> int

val anchor_available : Tables.t -> q_order:int -> res:int -> bool
(** False when the fullcase entry is missing (truncated .inc). *)
