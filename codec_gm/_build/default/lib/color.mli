(** L1: color decorrelation. RTL color_decorrelator.v / color_correlator.v.

    This is NOT lossless YCoCg-R: Co and Cg are stored as [x/2 + 128] in
    8 bits, dropping their LSB. It is the first error source of the codec.
    Exhaustive check over all 2^24 inputs: |R err| <= 1, |G err| <= 1,
    B is exact, and the inverse never needs the clamp. *)

val rgb_to_ycocg_u : r:int -> g:int -> b:int -> int * int * int
(** Returns (y, co_u, cg_u). RTL:
      co = r - b; tmp = b + co/2; cg = g - tmp; y = tmp + cg/2;
      co_u = co/2 + 128; cg_u = cg/2 + 128.
    Every [/2] is a Verilog signed division (truncation toward zero, same as
    OCaml's [/]), not an arithmetic shift. All results are in 0..255. *)

val ycocg_u_to_rgb : y:int -> co_u:int -> cg_u:int -> int * int * int
(** RTL color_correlator: co = co_u-128; cg = cg_u-128; tmp = y - cg;
    g = tmp + 2cg; b = tmp - co; r = b + 2co; each clamped to 0..255. *)

val group_to_ycocg : rgb:int array -> Types.group
(** [rgb] holds 4 pixels as R,G,B triples (length 12, lane-major);
    the result is indexed by ch = lane*3 + col (col 0 = Cg, 1 = Co, 2 = Y). *)

val group_to_rgb : Types.group -> int array

val max_error : int
(** = 1, the worst-case per-channel round-trip error. *)
