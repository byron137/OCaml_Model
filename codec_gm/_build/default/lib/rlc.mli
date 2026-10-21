(** L1: zero-run and palette classification of the groups of one slice
    (forward direction). RTL rlc_detector.v + sliding_palette.v, with the
    statistics of cost_evaluator.v's rlc_accumulator.

    Input per group k (0-based, N = slice_en_num groups): whether all 12
    mapped symbols are zero, and the palette flag. Output: a
    [Types.group_mode] per group.

    RLC dead zone (RTL w_dead_zone, 1-based counter <= 2 or +1 >= N):
      !rlc_mode || k < 2 || k >= N - 2.
    zero(k) = all_zero(k) && !dead(k) && !palette(k).
    A maximal run of zero groups is cut into chunks of at most 7
    (RTL r_rlc_count wraps 7 -> 1). Within a chunk every group but the last
    is [Bypass] (no bits); the last is [Escape len], len = chunk length,
    1..7. Because the last two groups are always dead, a run never reaches
    the end of the slice, so every chunk is terminated.
    palette(k) && !dead(k) -> [Palette] (same wire format as [Escape 0]).
    Anything else -> [Normal].

    Palette flag (sliding_palette.v, RGB domain; ruling: the flag belongs to
    G_k, the group the decoder reconstructs after G_k+1 in LIFO order):
      P_k = enabled && G_k+1 = G_k (all 12 RGB values equal)
            && nonflat(G_k)
            && dead_lo <= k && k < N - dead_hi
    nonflat(G) = some lane 1..3 has a subpixel differing from lane 0's by
    more than tf. The RTL dead zone is evaluated on its 1-based counter at
    the time G_k+1 is presented, giving dead_lo = 7 and dead_hi = 9; the
    exact +-1 placement is still to be confirmed on a waveform, hence the
    two fields are parameters. *)

type palette_cfg = { enabled : bool; tf : int; dead_lo : int; dead_hi : int }

val palette_default : enabled:bool -> palette_cfg
(** tf = 8, dead_lo = 7, dead_hi = 9 (RTL binding). *)

val nonflat : tf:int -> rgb:int array -> bool
(** [rgb] = one group as 12 values, R,G,B per lane. *)

val palette_flags :
  palette_cfg -> slice_en_num:int -> rgb_groups:int array array -> bool array

val dead : rlc_mode:bool -> slice_en_num:int -> int -> bool

val classify :
  rlc_mode:bool -> slice_en_num:int -> all_zero:bool array -> palette:bool array ->
  Types.group_mode array

type stats = {
  zero_groups : int;   (** groups coded as Bypass or Escape n (n > 0) *)
  escapes     : int;   (** number of Escape n (n > 0) groups *)
  palettes    : int;   (** number of Palette groups *)
}

val stats : Types.group_mode array -> stats
