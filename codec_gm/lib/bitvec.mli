(** Growable bit vector, LSB-first. Bit 0 is the first bit the encoder
    emitted; the decoder consumes from the top. Values pushed with
    [push_int] are placed with their bit 0 at the current length
    (RTL data_packer: code << offset). *)

type t

val create : unit -> t
val length : t -> int
val get : t -> int -> int                       (** 0 or 1 *)
val push_bit : t -> int -> unit
val push_int : t -> value:int -> len:int -> unit
(** [len] may be 0. Raises [Types.Spec_violation] if [value] does not fit. *)

val read_int : t -> pos:int -> len:int -> int
(** Bits [pos .. pos+len-1], bit [pos] being the LSB. *)

val highest_set : t -> int option
(** Index of the topmost 1 bit. *)

val copy : t -> t
val of_bits : int array -> t
val to_bits : t -> int array

val to_blocks : t -> width:int -> words:int -> int array array
(** Zero-pads to a multiple of [width] and cuts into blocks; each block is
    an array of [words] 32-bit words, word 0 holding the lowest bits. *)

val of_blocks : int array array -> t
val block_to_hex : int array -> string        (** 8 hex digits per word, most significant word first *)
val block_of_hex : words:int -> string -> int array
