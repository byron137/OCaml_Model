(** Offline dyadic codebook construction (see xbook.ml). *)

val build : hist:int array -> Tables.codebook
(** [hist] has alphabet-size entries (33 / 17 / 9, escape last). *)

val avg_bits : Tables.codebook -> hist:int array -> float
val to_string : Tables.codebook -> string
