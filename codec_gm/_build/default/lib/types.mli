(** Basic types shared by the whole model. All arithmetic is on ideal
    integers; bit widths are only enforced at spec boundaries. *)

(** Channel index convention (matches the RTL BUS2VEC layout):
    ch = lane*3 + col, with col 0 = Cg, 1 = Co, 2 = Y.
    RTL: pixel_ycocg_bus = {Y, Co_u, Cg_u}; w_pix_vec[i], i = lane*COL_NUM+col. *)
val col_cg : int   (** = 0 *)
val col_co : int   (** = 1 *)
val col_y  : int   (** = 2 *)

val lane_num : int  (** = 4 pixels per compressor enable *)
val ch_num   : int  (** = 12 *)

(** The 12 values of one 4-pixel group in some domain, indexed by ch. *)
type group = int array

(** q_order per column, indexed by col. RTL: q_order_bus = {3'dY, 3'dCo, 3'dCg}. *)
type q_order = int array

(** Coding mode of one group (rlc_detector / entropy_encoder bypass+escape combos).
    - [Normal]   : all 12 channels rANS-coded
    - [Bypass]   : inside a zero run, emits no bits
    - [Escape n] : last group of a zero run; ch0 codes the escape symbol,
                   ch1 emits raw 3-bit n (1..7)
    - [Palette]  : same wire format as [Escape 0]; decoder copies the
                   previously *output* group (see Rlc) *)
type group_mode = Normal | Bypass | Escape of int | Palette

(** 24-bit RGB image, data length = w*h*3, order R,G,B, top-down row-major. *)
type image = { w : int; h : int; data : int array }

exception Spec_violation of string
(** Raised only at spec-defined boundaries (rANS state range, symbol range,
    header field widths, missing table entries). *)

val spec_violation : ('a, unit, string, 'b) format4 -> 'a
(** [spec_violation "fmt" ...] raises [Spec_violation] with a formatted message. *)
