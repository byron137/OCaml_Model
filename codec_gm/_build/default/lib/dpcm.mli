(** L1: temporal decorrelation, first-order closed-loop DPCM chained across
    the 4 lanes of a group. RTL time_decorrelator.v (pipline_dpcm) and
    time_correlator.v.

    Forward prediction chain (each column independent):
      pred[lane 0] = reconstruction of the previous group's lane 3;
                     for the first group of a slice, pred = the pixel itself
                     (ssync path), so the first residual is always 0
      pred[lane k] = reconstruction of lane k-1 of the same group
      res      = pix - pred                       (9-bit signed)
      headroom = res < 0 ? pred : 255 - pred      (RTL: sign ? pred : ~pred)
      recon    = pred + q_res                     (q_res chosen by Quant)

    Reverse reconstruction (decoder, LIFO). The residual arrives as an 8-bit
    modular value m = table.(pos) (see Quant.dequantize and Mapper) which is
    congruent to -q_res mod 256, so one 8-bit addition undoes the prediction:
      p[i] = (p[i+1] + m[i+1]) mod 256,  starting from the header's last pixel.
    The headroom rule on the encoder side guarantees the modular result is
    the exact reconstruction (verified exhaustively for every lane/order
    over all 256x256 (pred, pix) pairs). *)

type col_state = { pred : int }
(** Per-column state carried across groups (= RTL r_curr_pred). *)

val init : first_pix:int -> col_state

val residual : col_state -> pix:int -> int * int
(** Returns (res, headroom). *)

val reconstruct : col_state -> q_res:int -> col_state
(** recon = pred + q_res. Raises [Types.Spec_violation] if the result leaves
    0..255: the headroom mechanism must prevent that, so an overflow here is
    a table or logic bug, never a wrap. *)

val reconstruct_reverse : next_pix:int -> q_res_mod:int -> int
(** (next_pix + q_res_mod) land 0xFF. *)
