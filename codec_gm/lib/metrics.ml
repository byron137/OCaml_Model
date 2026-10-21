open Types

type t = {
  psnr        : float;
  psnr_ch     : float array;
  mse         : float;
  max_err     : int;
  max_err_pos : int * int * int;
  vif         : float;
}

let check_same (a : image) (b : image) =
  if a.w <> b.w || a.h <> b.h then
    spec_violation "metrics: image sizes differ (%dx%d vs %dx%d)" a.w a.h b.w b.h

let psnr_of_mse mse = if mse <= 0.0 then infinity else 10.0 *. log10 (255.0 *. 255.0 /. mse)

let luma (img : image) =
  Array.init (img.w * img.h) (fun i ->
      0.299 *. float img.data.(3 * i) +. 0.587 *. float img.data.(3 * i + 1)
      +. 0.114 *. float img.data.(3 * i + 2))

(* ------------------------------------------------------------------ VIF -- *)

let gaussian n sd =
  let k = Array.init n (fun i -> let x = float (i - n / 2) in exp (-. x *. x /. (2.0 *. sd *. sd))) in
  let s = Array.fold_left (+.) 0.0 k in
  Array.map (fun v -> v /. s) k

(* separable 'valid' convolution: (w,h) -> (w-n+1, h-n+1) *)
let conv_valid src ~w ~h k =
  let n = Array.length k in
  let w2 = w - n + 1 and h2 = h - n + 1 in
  if w2 <= 0 || h2 <= 0 then None
  else begin
    let tmp = Array.make (w2 * h) 0.0 in
    for y = 0 to h - 1 do
      for x = 0 to w2 - 1 do
        let s = ref 0.0 in
        for i = 0 to n - 1 do s := !s +. k.(i) *. src.(y * w + x + i) done;
        tmp.(y * w2 + x) <- !s
      done
    done;
    let out = Array.make (w2 * h2) 0.0 in
    for y = 0 to h2 - 1 do
      for x = 0 to w2 - 1 do
        let s = ref 0.0 in
        for i = 0 to n - 1 do s := !s +. k.(i) *. tmp.((y + i) * w2 + x) done;
        out.(y * w2 + x) <- !s
      done
    done;
    Some (out, w2, h2)
  end

let downsample2 a ~w ~h =
  let w2 = (w + 1) / 2 and h2 = (h + 1) / 2 in
  (Array.init (w2 * h2) (fun i -> let y = i / w2 and x = i mod w2 in a.((2 * y) * w + 2 * x)), w2, h2)

let vif ~orig ~dist ~w ~h =
  if Array.length orig <> w * h || Array.length dist <> w * h then spec_violation "vif: plane size";
  let eps = 1e-10 and sigma_nsq = 2.0 in
  let num = ref 0.0 and den = ref 0.0 in
  let r = ref orig and d = ref dist and cw = ref w and ch = ref h in
  (try
     for scale = 1 to 4 do
       let n = (1 lsl (4 - scale + 1)) + 1 in
       let k = gaussian n (float n /. 5.0) in
       if scale > 1 then begin
         match conv_valid !r ~w:!cw ~h:!ch k, conv_valid !d ~w:!cw ~h:!ch k with
         | Some (r', w', h'), Some (d', _, _) ->
           let (r2, w2, h2) = downsample2 r' ~w:w' ~h:h' in
           let (d2, _, _) = downsample2 d' ~w:w' ~h:h' in
           r := r2; d := d2; cw := w2; ch := h2
         | _ -> raise Exit
       end;
       let w0 = !cw and h0 = !ch in
       let sq a b = Array.init (w0 * h0) (fun i -> a.(i) *. b.(i)) in
       match conv_valid !r ~w:w0 ~h:h0 k, conv_valid !d ~w:w0 ~h:h0 k,
             conv_valid (sq !r !r) ~w:w0 ~h:h0 k, conv_valid (sq !d !d) ~w:w0 ~h:h0 k,
             conv_valid (sq !r !d) ~w:w0 ~h:h0 k with
       | Some (mu1, _, _), Some (mu2, _, _), Some (rr, _, _), Some (dd, _, _), Some (rd, _, _) ->
         Array.iteri (fun i m1 ->
             let m2 = mu2.(i) in
             let s1 = max 0.0 (rr.(i) -. m1 *. m1) and s2 = max 0.0 (dd.(i) -. m2 *. m2) in
             let s12 = rd.(i) -. m1 *. m2 in
             let g = ref (s12 /. (s1 +. eps)) in
             let sv = ref (s2 -. !g *. s12) in
             let s1 = ref s1 in
             if !s1 < eps then begin g := 0.0; sv := s2; s1 := 0.0 end;
             if s2 < eps then begin g := 0.0; sv := 0.0 end;
             if !g < 0.0 then begin sv := s2; g := 0.0 end;
             if !sv < eps then sv := eps;
             num := !num +. log10 (1.0 +. !g *. !g *. !s1 /. (!sv +. sigma_nsq));
             den := !den +. log10 (1.0 +. !s1 /. sigma_nsq)) mu1
       | _ -> raise Exit
     done
   with Exit -> ());
  if !den = 0.0 then 1.0 else !num /. !den

(* -------------------------------------------------------------- compare -- *)

let compare ~orig ~recon =
  check_same orig recon;
  let n = orig.w * orig.h in
  let se = Array.make 3 0.0 in
  let max_err = ref 0 and pos = ref (0, 0, 0) in
  for i = 0 to n - 1 do
    for c = 0 to 2 do
      let d = recon.data.(3 * i + c) - orig.data.(3 * i + c) in
      se.(c) <- se.(c) +. float (d * d);
      if abs d > !max_err then begin max_err := abs d; pos := (i mod orig.w, i / orig.w, c) end
    done
  done;
  let mse_ch = Array.map (fun s -> s /. float n) se in
  let mse = (se.(0) +. se.(1) +. se.(2)) /. float (3 * n) in
  { psnr = psnr_of_mse mse; psnr_ch = Array.map psnr_of_mse mse_ch; mse;
    max_err = !max_err; max_err_pos = !pos;
    vif = vif ~orig:(luma orig) ~dist:(luma recon) ~w:orig.w ~h:orig.h }

let error_heatmap ?gain ~orig ~recon () =
  check_same orig recon;
  let n = orig.w * orig.h in
  let err = Array.init n (fun i ->
      let m = ref 0 in
      for c = 0 to 2 do m := max !m (abs (recon.data.(3 * i + c) - orig.data.(3 * i + c))) done;
      !m) in
  let gain = match gain with Some g -> max 1 g | None -> max 1 (Array.fold_left max 0 err) in
  let out = Array.make (3 * n) 0 in
  Array.iteri (fun i e ->
      let v = min 765 (e * 765 / gain) in            (* 0..765 across three ramps *)
      let r = min 255 v and g = max 0 (min 255 (v - 255)) and b = max 0 (v - 510) in
      out.(3 * i) <- r; out.(3 * i + 1) <- g; out.(3 * i + 2) <- b) err;
  { w = orig.w; h = orig.h; data = out }

let to_json m =
  let f x = if x = infinity then "\"inf\"" else Printf.sprintf "%.4f" x in
  let (x, y, c) = m.max_err_pos in
  Printf.sprintf
    "{\"psnr\": %s, \"psnr_rgb\": [%s, %s, %s], \"mse\": %s, \"max_err\": %d, \"max_err_pos\": {\"x\": %d, \"y\": %d, \"ch\": %d}, \"vif\": %s}"
    (f m.psnr) (f m.psnr_ch.(0)) (f m.psnr_ch.(1)) (f m.psnr_ch.(2)) (f m.mse) m.max_err x y c (f m.vif)

(* ------------------------------------------------------------ CIEDE2000 -- *)

let srgb_to_linear c =
  let c = float c /. 255.0 in
  if c <= 0.04045 then c /. 12.92 else ((c +. 0.055) /. 1.055) ** 2.4

let rgb_to_lab (r, g, b) =
  let r = srgb_to_linear r and g = srgb_to_linear g and b = srgb_to_linear b in
  let x = (0.4124564 *. r +. 0.3575761 *. g +. 0.1804375 *. b) /. 0.95047
  and y = (0.2126729 *. r +. 0.7151522 *. g +. 0.0721750 *. b) /. 1.00000
  and z = (0.0193339 *. r +. 0.1191920 *. g +. 0.9503041 *. b) /. 1.08883 in
  let f t = if t > 0.008856 then t ** (1.0 /. 3.0) else 7.787 *. t +. 16.0 /. 116.0 in
  let fx = f x and fy = f y and fz = f z in
  (116.0 *. fy -. 16.0, 500.0 *. (fx -. fy), 200.0 *. (fy -. fz))

let pi = 4.0 *. atan 1.0
let deg x = x *. 180.0 /. pi
let rad x = x *. pi /. 180.0

let ciede2000_lab (l1, a1, b1) (l2, a2, b2) =
  let c1 = sqrt (a1 *. a1 +. b1 *. b1) and c2 = sqrt (a2 *. a2 +. b2 *. b2) in
  let cm = (c1 +. c2) /. 2.0 in
  let cm7 = cm ** 7.0 in
  let g = 0.5 *. (1.0 -. sqrt (cm7 /. (cm7 +. 25.0 ** 7.0))) in
  let a1' = (1.0 +. g) *. a1 and a2' = (1.0 +. g) *. a2 in
  let c1' = sqrt (a1' *. a1' +. b1 *. b1) and c2' = sqrt (a2' *. a2' +. b2 *. b2) in
  let h' a b = if a = 0.0 && b = 0.0 then 0.0 else
      let h = deg (atan2 b a) in if h < 0.0 then h +. 360.0 else h in
  let h1' = h' a1' b1 and h2' = h' a2' b2 in
  let dl = l2 -. l1 and dc = c2' -. c1' in
  let dh =
    if c1' *. c2' = 0.0 then 0.0
    else let d = h2' -. h1' in
      if abs_float d <= 180.0 then d else if d > 180.0 then d -. 360.0 else d +. 360.0 in
  let dH = 2.0 *. sqrt (c1' *. c2') *. sin (rad (dh /. 2.0)) in
  let lm = (l1 +. l2) /. 2.0 and cm' = (c1' +. c2') /. 2.0 in
  let hm =
    if c1' *. c2' = 0.0 then h1' +. h2'
    else let s = h1' +. h2' in
      if abs_float (h1' -. h2') <= 180.0 then s /. 2.0
      else if s < 360.0 then (s +. 360.0) /. 2.0 else (s -. 360.0) /. 2.0 in
  let t = 1.0 -. 0.17 *. cos (rad (hm -. 30.0)) +. 0.24 *. cos (rad (2.0 *. hm))
          +. 0.32 *. cos (rad (3.0 *. hm +. 6.0)) -. 0.20 *. cos (rad (4.0 *. hm -. 63.0)) in
  let dtheta = 30.0 *. exp (-. (((hm -. 275.0) /. 25.0) ** 2.0)) in
  let cm7' = cm' ** 7.0 in
  let rc = 2.0 *. sqrt (cm7' /. (cm7' +. 25.0 ** 7.0)) in
  let lm50 = (lm -. 50.0) ** 2.0 in
  let sl = 1.0 +. 0.015 *. lm50 /. sqrt (20.0 +. lm50) in
  let sc = 1.0 +. 0.045 *. cm' and sh = 1.0 +. 0.015 *. cm' *. t in
  let rt = -. sin (rad (2.0 *. dtheta)) *. rc in
  sqrt ((dl /. sl) ** 2.0 +. (dc /. sc) ** 2.0 +. (dH /. sh) ** 2.0
        +. rt *. (dc /. sc) *. (dH /. sh))

let ciede2000_pixel p q = ciede2000_lab (rgb_to_lab p) (rgb_to_lab q)

let ciede2000 ~orig ~recon =
  check_same orig recon;
  let n = orig.w * orig.h in
  let px (img : image) i = (img.data.(3 * i), img.data.(3 * i + 1), img.data.(3 * i + 2)) in
  let de = Array.init n (fun i -> ciede2000_pixel (px orig i) (px recon i)) in
  let mean = Array.fold_left (+.) 0.0 de /. float (max 1 n) in
  let sorted = Array.copy de in
  Array.sort Stdlib.compare sorted;
  let p99 = if n = 0 then 0.0 else sorted.(min (n - 1) (int_of_float (ceil (0.99 *. float n)) - 1)) in
  (mean, p99)

(* -------------------------------------------------------------- entropy -- *)

let residual_entropy a =
  let n = Array.length a in
  if n = 0 then 0.0
  else begin
    let h = Hashtbl.create 64 in
    Array.iter (fun s -> Hashtbl.replace h s (1 + try Hashtbl.find h s with Not_found -> 0)) a;
    Hashtbl.fold (fun _ c acc ->
        let p = float c /. float n in acc -. p *. log p /. log 2.0) h 0.0
  end

let heatmap32 ~orig ~recon = error_heatmap ~gain:32 ~orig ~recon ()
