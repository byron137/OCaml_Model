(** L3: forward-looking rate decision. RTL scanner.v (4 parallel
    comp_data_path instances at orders 0..3), cost_evaluator.v and
    priority_arbiter.v. Untimed: the scanner's datapath is Slice.column_encode,
    whose symbols are a pure function of the column and its order, and the
    dyadic rANS bit count of a symbol is km - ks.(symbol), independent of the
    coder state, so every cost is a plain sum over the slice.

    Scanner / cost evaluator:
      raw.(o).(c).(b) = sum over the slice of (km - ks.(sym)) for the 4 lanes
                        of column c at order o with codebook b, masked channels
                        (macro, i.e. Params.rgbg_mode) contributing 0; no RLC
                        or escape is applied (the datapath encoders run with
                        bypass = escape = 0).
      zero.(o).(c).(k) = all 4 lane symbols of column c at order o are 0 in
                        group k, and k is not in the RLC dead zone.
      palette.(k)      = Rlc palette flag and not dead.
      rlc_num.(idx) / rlc_header.(idx) : rlc_accumulator over the groups, with
                        the three columns taken at idx's orders (see [rlc_stats]).
    Arbiter:
      best book per (o, c) = argmin over b < table_num, ties to the HIGHEST
                        index (min4 tree: `a < b ? a : b`).
      cost.(idx<10)    = sum of the three best column costs at idx's orders
                         - (zero_cost * rlc_num - escape_cost.(idx) * rlc_header)
      cost.(10|11)     = best Y cost at order 2|3 + macro_cost
      data_num.(idx)   = ceil ((cost + header_bits) / block_bits)
      decision         = first idx in 0..11 with data_num <= target, else 12
                         with the fixed fallback data_num and codebooks 0.
    All constants live in [constants] so experiments can change them. *)

type constants = {
  header_bits  : int;                 (** 131, used for every idx *)
  block_bits   : int;                 (** 96 *)
  zero_cost    : rgbg:bool -> int;    (** 12, or 8 under rgbg (8 unmasked channels) *)
  escape_cost  : int array;           (** per idx 0..11: 11 when Cg is at order 0/1, 10 at 2/3 *)
  macro_cost   : target:int -> int;   (** chroma bits of idx 10/11: 540 / 1080 / 2160 by target *)
  fallback_num : target:int -> int;   (** idx 12 data_num: 12 / 24 / 46 by target *)
}

val rtl_constants : constants

type trace = {
  raw_cost      : int array array array;  (** .(order 0..3).(col).(book) *)
  best_book     : int array array;        (** .(order).(col) *)
  best_cost     : int array array;
  rlc_num       : int array;              (** per idx 0..11 *)
  rlc_header    : int array;
  cost_vec      : int array;              (** 12 candidates, ideal integers *)
  data_num_vec  : int array;
  flags         : bool array;
  cost_overflow : bool;
  (** true when a column cost needs more than the RTL's 13 bits or a
      candidate cost more than its 14 bits; the decision above is the ideal
      one and the RTL would differ. Reachable only in 1-slice mode. *)
  chosen_num    : int;                    (** arbiter's data_num for the decision *)
}

val decide : ?consts:constants -> Tables.t -> Params.t -> rgb:int array -> Slice.decision * trace
(** [rgb] = 12 * slice_en_num values, as for Slice.encode_slice. *)

val argmin_last : int array -> int
(** Index of the minimum; among equal minima the largest index (RTL min4). *)

val rlc_stats : zero:bool array -> palette:bool array -> int * int
(** rlc_accumulator: (rlc_num, rlc_header_num). [zero.(k)] = group k is
    all-zero at the candidate's orders and not dead; [palette.(k)] = palette
    group and not dead. A palette group counts as a zero group that starts
    its own run; a run restarts after 7 groups. *)

val symbol_cost : Tables.codebook -> int -> int
(** km - ks.(symbol) *)

val scan_orders : int   (** 4: scanner datapaths at orders 0..3 *)
val candidates : int    (** 12: idx 0..11; 12 is the fallback *)
