(** Experimental slice codec for the spec "水平降取樣與插值預測改進"
    (improvements 1 and 2). Symbol level: it shares Color, Quant, Mapper,
    Rlc and the codebook cost model with the golden model but does not
    produce an rANS bitstream. Bits are the exact dyadic rANS payload
    (km - ks per coded symbol, as Rdo uses) plus escape/rlc raw bits plus
    the new side information; blocks are rounded to 96 bits after the
    131-bit header exactly as the arbiter does.

    The decoder is a forward (not LIFO) implementation that consumes the
    token stream in emission order and must reproduce the encoder's
    reconstruction bit-exactly; this is the round-trip property the spec's
    deliverable (a) asks for.

    Prediction structures (spec 0.1 and 2.1 / 2.2), lane = n mod 4:
      Serial   : pred(n) = recon(n-1)                 (what the model does today)
      Trunk4   : pred(4k) = recon(4k-4); lanes 1..3 = recon(n-1)
                                                      (the spec's stated baseline)
      VariantA : Trunk4, but pred(4k+3) = lin(recon 4k+2, recon 4k+4)
      VariantB : pred(4k+2) = recon(4k); pred(4k+1) = lin(4k, 4k+2);
                 pred(4k+3) = lin(4k+2, 4k+4)
    In every structure pred(0) = x(0). A lane-3 (or lane-1 in B) sample of
    the last group has no 4k+4: pred = recon(4k+2).

    Emission order (= processing order; every predictor reads only samples
    emitted before it) is a list of emission groups:
      Serial/Trunk4 : [4k; 4k+1; 4k+2; 4k+3]
      VariantA      : [0;1;2], [4k; 4k+1; 4k+2; 4(k-1)+3] ..., [4(n-1)+3]
      VariantB      : [0;2],   [4k; 4k+2; 4(k-1)+1; 4(k-1)+3] ..., [4(n-1)+1; 4(n-1)+3]
    RLC (zero-run) and the 12 rANS streams work on emission groups and on
    lane = n mod 4 respectively, so A and B have one more group than the
    model's slice_en_num.

    Macro mode masks Cg/Co of lanes 1 and 3 (residual 0, recon = pred) as
    in Slice. Improvement 1 changes only the odd chroma value handed to the
    colour inverse transform:
      M1a : the chain value (= e(n-1) in Serial/Trunk4/A-lane-1; already the
            linear interpolation in B)
      M1b : lin(e(n-1), e(n+1))
      M1c : M1b + luma-guided beta per segment and chroma channel
            (Interp.predict_chroma_beta_u), 6 bits of side information per
            segment, chosen by the encoder with Interp.choose_beta.
    The chain itself (what later predictors read) is never changed. *)

type structure = Serial | Trunk4 | VariantA | VariantB
type m1 = M1a | M1b | M1c

val structure_of_string : string -> structure
val structure_to_string : structure -> string
val m1_of_string : string -> m1
val m1_to_string : m1 -> string

type config = {
  structure : structure;
  m1        : m1;
  seg_len   : int;     (** L: pixels per beta segment (12 | 20 | 36) *)
  book_for  : (lane:int -> col:int -> q_order:int -> codebook_idx:int -> Tables.codebook) option;
  (** Codebook override per (lane, col, q_order); None = the table's book
      Tables.codebook_index q_order codebook_idx, shared by all lanes. *)
}

val default_config : config

val emission_groups : structure -> n_groups:int -> int array array
(** Pixel indices of every emission group, in order. *)

val pred : structure -> rec_:int array -> n_total:int -> int -> int
(** Prediction for pixel index i > 0 from already reconstructed samples. *)

val column_encode :
  config -> Tables.t -> col:int -> q_order:int -> macro:bool -> pix:int array ->
  int array * int array
(** (symbols, recon) indexed by pixel. [pix] has 4*n_groups samples. *)

type token = Sym of int * int  (** (ch, symbol) *) | Esc of int  (** escape + rlc_num (0 = none) *)

type side = {
  first_pix : int array;        (** per column, 8 bits each (replaces the LIFO last_pixel) *)
  betas     : int array array;  (** .(col).(segment) for col 0,1 when m1 = M1c; else [||] *)
}

type encoded = {
  decision   : Slice.decision;
  side       : side;
  tokens     : token array;
  symbols    : int array array;      (** .(ch).(pixel index / 4) as Slice, for statistics *)
  chain      : int array array;      (** .(col).(pixel): DPCM chain reconstruction, YCoCg_u *)
  out_ycocg  : int array array;      (** .(col).(pixel): value sent to the colour inverse *)
  recon_rgb  : int array;
  modes      : Types.group_mode array;
  sym_bits   : int;                  (** rANS payload: coded symbols + escapes + 3-bit rlc_num *)
  side_bits  : int;                  (** beta side information *)
  bits       : int;                  (** sym_bits + side_bits *)
  data_num   : int;                  (** ceil ((bits + 131) / 96) *)
}

val encode_slice : config -> Tables.t -> Params.t -> Slice.decision -> rgb:int array -> encoded

val decode_slice :
  config -> Tables.t -> Params.t -> Slice.decision -> side -> token array -> int array
(** Forward decoder: returns recon_rgb. Independent of [encode_slice]'s
    internal state; shares only the predictors and tables. *)

val books_of : config -> Tables.t -> Slice.resolved -> Slice.decision -> Tables.codebook array
(** Per channel ch = lane*3 + col. *)

val lane_class_symbols : encoded -> lane:int -> col:int -> int array
(** Symbols of one (lane, col) over the slice, for entropy and histograms. *)

val cost : Tables.codebook -> int -> int
(** km - ks.(symbol), the dyadic rANS bit count of one symbol. *)

val segments : config -> n_total:int -> int
(** Number of beta segments in a slice of n_total pixels. *)
