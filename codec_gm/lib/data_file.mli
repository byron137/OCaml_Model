(** L5: the .data text format (what a UVM scoreboard reads) and the stats
    sidecar.

    .data layout, one slice per section:
      # codec_gm v1 w=720 h=1280 res_h=720 slice_num=4 target=15 table_num=1 rlc=1 palette=1 palette_tf=8 rgbg=0 deblock=0 deblock_th=255
      S y=0 s=0 idx=3 cb=0,0,0 n=7
      <24 hex digits>            block 0 = first block the compressor emits, hex = bits [95:0]
      ...
    Lines starting with '#' after the first are comments. [n] must equal the
    number of block lines of the section. *)

val params_to_kv : Params.t -> string
val params_of_kv : string list -> Params.t -> Params.t
(** Applies key=value tokens to a base; unknown keys raise [Failure]. *)

val write : string -> Params.t -> w:int -> h:int -> Frame.slice_record array -> unit

val read : string -> Params.t * int * int * (int * int * Slice.decision * int array array) array
(** (params, w, h, slices as (y, slice_idx, header decision, blocks)). *)

val write_stats : string -> Params.t -> w:int -> h:int -> Frame.slice_record array -> unit
(** JSON: frame summary (block count, bits per pixel, compression ratio,
    q_order_idx histogram) and per slice: decision, data_num, payload bits,
    mode histogram, bits per channel, per-channel symbol histogram and its
    empirical entropy, and the Rdo trace when present. *)

val entropy_bits : int array -> float
(** Empirical entropy (bits/symbol) of a histogram; 0 for an empty one. *)
