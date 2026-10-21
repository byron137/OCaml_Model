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
