(** L5: Windows BMP, stdlib only. Reads uncompressed 24-bit and 32-bit
    (BGRx) files, bottom-up or top-down; writes 24-bit bottom-up with a
    40-byte BITMAPINFOHEADER. *)

val read  : string -> Types.image
(** Raises [Failure] with a reason for anything it cannot read. *)

val write : string -> Types.image -> unit
