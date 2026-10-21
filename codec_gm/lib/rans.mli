(** L1: dyadic rANS with an 8-bit state and no multiplier.
    RTL: entropy_encoder.v (rans_encoder) / entropy_decoder.v (rans_decoder).

    Encode one symbol on one stream (book [cb]):
      cw   = state lsr ks
      len  = km - ks                       (number of output bits, 0..8)
      s'   = cdf + (state land (2^ks - 1))
    Decode (reverse order; [di] = the last [len] bits of the bitstream):
      sym, ks, rem = inverse s'            (cdf.(sym) <= s' < cdf.(sym+1), rem = s' - cdf.(sym))
      s    = (di lsl ks) + rem

    There are 12 independent streams (one per channel). Every stream starts a
    slice at state 0; the 12 final states are written to the slice header and
    the decoder starts from them, consuming the bitstream from its end.
    Spec boundary: a state must stay in 0 .. 2^km - 1, otherwise
    [Types.Spec_violation] is raised. *)

type stream = { state : int }

val init : stream

val encode : Tables.codebook -> stream -> symbol:int -> stream * int * int
(** [encode cb st ~symbol] returns [(st', codeword, len)]. [symbol] may be the
    escape index (= level_max of the order pair). *)

type decoder
(** A codebook plus its precomputed inverse table (2^km entries). *)

val make_decoder : Tables.codebook -> alphabet:int -> decoder
(** [alphabet] = number of valid symbols including the escape
    (see [Tables.alphabet_size]); higher indices in the book are ignored. *)

val inverse : decoder -> int -> int * int * int
(** [inverse d state] = (symbol, ks, rem). Raises [Spec_violation] for a state
    that no symbol covers (only possible with an inconsistent book). *)

val decode : decoder -> stream -> take_bits:(int -> int) -> stream * int
(** [take_bits n] must return the last [n] bits of the bitstream as an integer
    whose MSB is the very last bit (RTL data_unpacker: di = window >> (8-len)). *)
