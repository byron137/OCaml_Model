(** Experimental (spec "水平降取樣與插值預測改進", section 0.2 / 4):
    odd-sample predictors over reconstructed EVEN samples.

    All functions read only even positions of [rec_]; an even index outside
    the slice is clamped to [0, n-2] (n = Array.length rec_, even), so
    neither the previous line nor another slice is ever used. For the last
    pixel of a slice (odd, index n-1) every kernel that needs e(n+1) reads
    e(n-2) instead, so Linear degenerates to e(n-2) as the spec requires.

    Ideal integers, no bit-width limits. *)

type kernel = Linear | QuadL | QuadR | DD4 | Wiener of int | Auto

val kernel_of_string : string -> kernel
val kernel_to_string : kernel -> string

val lin : int -> int -> int
(** (a + b + 1) asr 1 *)

val ge : int array -> int -> int
(** [ge rec_ i]: even sample i, index clamped to [0, n-2]. [i] must be even. *)

val predict_odd : kernel -> int array -> int -> int
(** [predict_odd k rec_ n]: value for odd index n from the even neighbours
    e(n-3), e(n-1), e(n+1), e(n+3) of [rec_]. DD4, Wiener and Auto results
    are clamped to [min(e(n-1), e(n+1)), max(e(n-1), e(n+1))]. *)

val auto_select : int array -> int -> kernel
(** The kernel Auto resolves to at odd index n (curvature rule of 0.2). *)

val beta_half : int array
(** [|0; 1; 2; 4; 8; -2; -4; -8|] = 2 * beta for idx 0..7
    (beta = 0, +1/2, +1, +2, +4, -1, -2, -4). *)

val predict_chroma_beta : beta_idx:int -> c_rec:int array -> y_rec:int array -> int -> int
(** M1c in the true YCoCg-R domain, as written in the spec:
    lin(C(n-1), C(n+1)) + (beta_half * dY) asr 1,
    dY = Y(n) - lin(Y(n-1), Y(n+1)). No clamp (the caller's domain decides). *)

val predict_chroma_beta_u : beta_idx:int -> c_rec:int array -> y_rec:int array -> int -> int
(** The same correction applied in the model's stored chroma domain
    Co_u = Co/2 + 128 (Cg likewise): the chroma axis is halved, so the
    correction is (beta_half * dY) asr 2, and the result is clamped to
    0..255. Equivalent to [predict_chroma_beta] on the un-halved values. *)

val choose_beta : c_orig:int array -> c_rec:int array -> y_rec:int array ->
  seg_start:int -> seg_len:int -> int
(** Encoder decision for one segment and one chroma channel, in the u domain:
    the idx (0..7) minimising SAD over the odd n of the segment between
    [c_orig.(n)] and [predict_chroma_beta_u]; ties to the lowest idx. *)
