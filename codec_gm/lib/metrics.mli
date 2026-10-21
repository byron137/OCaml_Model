(** L6: quality metrics. The only floating-point module; not on the golden
    path. *)

type t = {
  psnr        : float;            (** all three channels, peak 255; infinity when identical *)
  psnr_ch     : float array;      (** R, G, B *)
  mse         : float;
  max_err     : int;
  max_err_pos : int * int * int;  (** x, y, channel (0 = R) of the first maximum *)
  vif         : float;            (** pixel-domain VIF (Sheikh & Bovik), 4 scales, on luma *)
}

val compare : orig:Types.image -> recon:Types.image -> t

val vif : orig:float array -> dist:float array -> w:int -> h:int -> float
(** Pixel-domain VIF on one plane. Gaussian windows of size 2^(4-s+1)+1 and
    sigma N/5 at scales s = 1..4, 'valid' filtering, downsampling by 2 between
    scales, sigma_nsq = 2. 1.0 for identical planes. *)

val luma : Types.image -> float array
(** Rec. 601: 0.299 R + 0.587 G + 0.114 B. *)

val error_heatmap : ?gain:int -> orig:Types.image -> recon:Types.image -> unit -> Types.image
(** Per-pixel max over R,G,B of |orig - recon|, mapped through a black ->
    red -> yellow -> white ramp. [gain] is the error value that saturates the
    ramp; default = the image's max error (so the hottest pixel is white). *)

val to_json : t -> string
