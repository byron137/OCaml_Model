(** Arbiter replica for the experimental codec (see xrdo.ml). *)

val macro_side_bits : Xcodec.config -> Params.t -> int
(** 6 bits per segment under M1c, else 0. *)

val decide : ?consts:Rdo.constants -> Xcodec.config -> Tables.t -> Params.t -> rgb:int array ->
  Slice.decision * int * bool
(** (decision, arbiter data_num, fallback). With [Xcodec.default_config] and
    Params.palette_mode = false this equals Rdo.decide. *)
