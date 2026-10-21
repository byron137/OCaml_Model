(** L0: single source of truth for every LUT. The RTL [.inc] files are parsed
    directly; nothing is transcribed by hand.

    RTL files consumed:
    - quant_table_res_0[0-5].inc / quant_table_dec_0[0-5].inc
        (nonlinear_quantizer.v, 32 entries each, signed 9-bit)
    - anchor_fullcase.inc
        (anchor_fullcase_lut, 3*512 entries of {v_hi, v_lo, level})
    - quant_anchor_0[0-2].inc
        (anchor reconstruction values, used by the decoder's
         anchor_dequant_table_selector, 32 entries each)
    - codebook_km.inc / codebook_ks.inc / codebook_cdf.inc
        (entropy_encoder.v codebook_rom, 13 books x 33 entries)
    - codebook_r.inc is NOT parsed by the model; the inverse mapping is
      derived from cdf/ks in [Rans] and cross-checked against it in tests.
    - quant_order_idx_2_bus (compressor_ctrl.v, 13 entries) is hard-coded
      in the RTL rather than an .inc, so it is mirrored here in [qorder]. *)

type quant_tbl = {
  res : int array;   (** 32 entries: level -> |reconstructed residual| (0 = unused level) *)
  dec : int array;   (** 32 entries: thermometer threshold; |res| >= dec.(i) && dec.(i) <> 0 -> level >= i *)
}

type anchor_entry = { v_hi : int; v_lo : int; level : int }
(** One fullcase-LUT entry: [level] for this residual, [v_hi] its signed
    reconstruction, [v_lo] the one-level-down reconstruction used when
    [v_hi] overflows the headroom. A missing entry has [level = -1]. *)

type codebook = {
  km  : int;         (** total state bits K_M (4-bit field) *)
  ks  : int array;   (** 33 entries (32 symbols + escape): K_s of each symbol, freq = 2^ks *)
  cdf : int array;   (** 33 entries: CDF of each symbol (8-bit) *)
}

type qorder_entry = {
  q_order    : Types.q_order;  (** [|cg; co; y|] *)
  macro_mode : bool;
  rlc_mode   : bool;
}

type t = {
  quant          : quant_tbl array;        (** index = q_order 0..5 *)
  anchor         : anchor_entry array array;
  (** anchor.(tbl).(addr): tbl = min (q_order lsr 1) 2, addr = [anchor_addr], 512 entries *)
  anchor_res     : int array array;        (** anchor_res.(tbl): 32-entry reconstruction table *)
  anchor_missing : (int * int) list;       (** (tbl, addr) pairs absent from anchor_fullcase.inc *)
  codebooks      : codebook array;         (** index = [codebook_index], 13 books *)
  qorder         : qorder_entry array;     (** index = q_order_idx 0..12 *)
}

val load : inc_dir:string -> t
(** Reads every .inc under [inc_dir]. Raises [Failure] with file name and line
    number on a parse error. Missing anchor entries are tolerated and listed
    in [anchor_missing]; use [check] to make them visible. *)

val book_issues : ?check_escape:bool -> t -> book_idx:int -> string list
(** Consistency of one codebook: every regular symbol's range
    [cdf, cdf + 2^ks) must abut the next one and the escape must fit.
    An inconsistent book makes encoder and decoder disagree on bit counts. *)

val check : t -> string list
(** Consistency report (empty = clean): missing anchor entries, non-monotone
    or non-dyadic codebooks, escape symbols that are not encodable. *)

val anchor_addr : res:int -> int
(** RTL: w_addr = {~res[8], res[7:0]} within one 512-entry table. *)

val anchor_tbl : q_order:int -> int
(** RTL: (q_order[2:1] == 3) ? 2 : q_order[2:1]. *)

val codebook_index : q_order:int -> codebook_idx:int -> int
(** RTL: w_lookup_table_idx = {q_order[2:1], codebook_idx}. Raises
    [Spec_violation] if the index exceeds the 13 available books. *)

val level_max : q_order:int -> int
(** RTL distribution_mapper.v: order 0,1 -> 32; 2,3 -> 16; else -> 8.
    Also the escape symbol index and, +1, the alphabet size of the codebook. *)

val alphabet_size : book_idx:int -> int
(** level_max + 1 for the order pair the book belongs to. *)

val ints_of_line : string -> int list
(** Tokenizer used by the parsers, exposed for tests. Returns every integer
    token in the line; Verilog sized literals such as [9'sd255], [-9'sd255],
    [5'd 31], [8'hFF] yield their value only. Identifiers are skipped as a
    whole so that digits inside names are ignored. Text after [//] is ignored. *)
