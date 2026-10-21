(** L1: symmetric mapping between a quantizer level and an unsigned rANS
    symbol. RTL distribution_mapper.v (encoder) / distribution_unmapper.v
    (decoder).

    The level space is modular: with [max = Tables.level_max q_order], a
    negative residual at magnitude level k and a positive residual at level
    max-k share the same position [pos], because every reconstruction table
    satisfies res.(max-k) = 256 - res.(k) (checked for all six quant tables
    and all three anchor tables). The decoder therefore never needs a sign:
    res.(pos) is congruent to -q_res mod 256 and Dpcm adds it in 8-bit
    arithmetic.

    Encoder:
      pos    = (sign || level = 0) ? level : max - level
      mid    = max / 2
      symbol = pos < mid ? 2*pos : max - 2*(pos - mid) - 1
    Decoder:
      pos    = symbol even ? symbol/2 : max - 1 - symbol/2 *)

val pos_of : q_order:int -> level:int -> sign:bool -> int
val map    : q_order:int -> level:int -> sign:bool -> int
(** Raises [Types.Spec_violation] if [level >= max]. *)

val unmap  : q_order:int -> symbol:int -> int
(** symbol -> pos. [unmap (map level sign) = pos_of level sign] and
    [unmap] restricted to 0..max-1 is a bijection onto 0..max-1. *)
