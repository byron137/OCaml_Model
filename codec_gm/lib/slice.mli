(** L2: slice-level codec, transaction level (untimed).
    [encode_slice] follows compressor.v's datapath for a given decision;
    [decode_slice] is an independent LIFO implementation of uncompressor.v.

    Channel masking in macro mode (entropy_encoder w_macro_bus =
    12'b011_000_011_000): the Cg/Co channels of lanes 1 and 3 carry no bits
    and their residual is forced to zero, i.e. 2:1 horizontal chroma
    subsampling; lanes 0 and 2 keep full chroma. macro = qorder table entry
    || Params.rgbg_mode. *)

type decision = {
  q_order_idx  : int;         (** 0..12 *)
  codebook_idx : int array;   (** [|cg; co; y|], each < Params.table_num *)
}

type resolved = {
  q_order    : Types.q_order;
  macro      : bool;
  rlc        : bool;                  (** qorder table rlc_mode && Params.rlc_mode *)
  escape_sym : int;                   (** Tables.level_max of column Cg *)
  books      : Tables.codebook array; (** per channel *)
  masked     : bool array;            (** per channel: no bits, residual forced to 0 *)
}

val resolve : Tables.t -> Params.t -> decision -> resolved

val channel_masked : macro:bool -> ch:int -> bool

val column_encode :
  Tables.t -> col:int -> q_order:int -> macro:bool -> zero:bool array ->
  pix:int array -> int array * int array
(** Closed-loop DPCM + quantizer + mapper for one column of a slice.
    [zero.(k)] forces group k's residuals to zero (palette). [pix] has
    4*slice_en_num samples. Returns (symbols, reconstruction). Shared with
    Rdo, whose cost is a pure function of the symbols. *)

type stats = {
  modes        : Types.group_mode array;
  payload_bits : int;
  data_num     : int;                  (** 96-bit blocks including the header *)
  mode_hist    : int array;            (** [|normal; bypass; escape; palette|] *)
  symbol_hist  : int array array;      (** .(ch).(symbol), 33 bins, coded symbols only *)
  bits_per_ch  : int array;            (** rANS bits per channel; raw rlc_num bits counted on ch1 *)
}

val encode_slice :
  Tables.t -> Params.t -> decision -> rgb:int array -> int array array * stats * int array
(** [rgb] has 12*slice_en_num values (R,G,B per pixel). Returns
    (blocks, stats, recon_rgb) where recon_rgb is what the decoder must
    reproduce exactly (before its optional deblock filter), palette copies
    applied. *)

val decode_slice : Tables.t -> Params.t -> int array array -> int array * decision * stats
(** Blocks -> RGB (12*slice_en_num values). Independent of the encoder:
    parses the header, decodes LIFO detecting escapes from the ch0 state,
    walks the DPCM chain backwards with modular dequantisation, copies
    palette groups from the previously output group, then applies the
    anchor deblock filter when Params.deblock_mode is set. *)

val deblock_clamp : q_order_idx:int -> int
(** RTL uncompressor.v: q_order_idx[2:1] = 0 -> 16, else 32 (kept as-is,
    see the pending clamp ruling). *)

val deblock : clamp:int -> edge_th:int -> int array -> unit
(** In-place anchor filter on a slice's RGB output (forward pixel order):
    for every anchor pixel (index 4k, k >= 1) and each of R,G,B, unless some
    channel has |prev - next| > edge_th:
      avg = (prev + next + 1) asr 1, delta = clamp(avg - cur, +-clamp),
      cur = clamp8(cur + delta). Neighbours are unfiltered pixels. *)
