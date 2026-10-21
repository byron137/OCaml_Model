(** L1: bit concatenation, 96-bit blocks and the slice header.
    RTL data_packer.v / compressor_ctrl.v (encoder), data_unpacker.v (decoder).

    Stream layout of one slice, LSB-first (bit 0 = first bit emitted):
      [payload][header][zero padding up to a multiple of 96]
    Payload: for every group in forward order, the codewords of ch0..ch11 in
    that order (data_packer offsets); an [Escape n] group contributes ch0's
    codeword followed by the raw 3-bit n; [Bypass] contributes nothing;
    macro-mode chroma channels contribute nothing.
    Header (RTL concatenation, listed MSB first, so [states] sits right after
    the payload):
      {1'b1, q_order_idx[3:0], codebook_idx[5:0] = {Y,Co,Cg} 2 bits each,
       last_pixel[23:0] = {Y,Co,Cg}, states}
      states = 12 x 8 bits, ch0 lowest (131 bits total), or 12 x 3 bits when
      q_order_idx = 12 (71 bits).
    The decoder finds the header by the topmost 1 bit (data_unpacker lzd),
    reads q_order_idx from the 4 bits below it to know the header width,
    and consumes the payload downward from the header's start. *)

type header = {
  q_order_idx  : int;
  codebook_idx : int array;   (** [|cg; co; y|], each 0..3 *)
  last_pixel   : int array;   (** [|cg; co; y|], reconstructed YCoCg of the slice's last pixel *)
  states       : int array;   (** 12 final rANS states, index = ch *)
}

val block_width : int          (** 96 *)
val block_words : int          (** 3 *)
val header_length : q_order_idx:int -> int   (** 131 or 71, including the marker bit *)
val macro_idx : int            (** 12 *)

module Writer : sig
  type t
  val create : unit -> t
  val push : t -> value:int -> len:int -> unit
  val push_header : t -> header -> unit
  (** Raises [Types.Spec_violation] on out-of-range fields. Must be called once. *)
  val payload_bits : t -> int    (** bits pushed before the header *)
  val finish : t -> int array array
  (** Blocks of [block_words] 32-bit words (word 0 = lowest bits), zero padded. *)
end

module Reader : sig
  type t
  val of_blocks : int array array -> t
  val pop_header : t -> header
  (** Locates and parses the header; afterwards [take] reads the payload
      from its end downward. *)
  val take : t -> int -> int
  (** Next [n] bits from the top of the remaining payload, MSB = topmost bit
      (what [Rans.decode]'s [take_bits] expects). *)
  val remaining : t -> int
  val payload_bits : t -> int   (** payload length, valid after [pop_header] *)
end
