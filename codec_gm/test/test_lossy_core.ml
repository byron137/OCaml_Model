(* Stage 4 tests: Color, Dpcm, Mapper, Quant and their composition. *)
open Codec_gm

let inc_dir =
  match Sys.getenv_opt "CODEC_INC_DIR" with
  | Some d -> d
  | None -> failwith "set CODEC_INC_DIR to the directory containing the .inc files"

let tables = Tables.load ~inc_dir

let unit_test name f = QCheck.Test.make ~count:1 ~name QCheck.unit f

(* ---------------- Color ---------------- *)

let test_color_exhaustive =
  unit_test "color: exhaustive range and round-trip error" (fun () ->
      let ok = ref true in
      for r = 0 to 255 do
        for g = 0 to 255 do
          for b = 0 to 255 do
            let (y, co, cg) = Color.rgb_to_ycocg_u ~r ~g ~b in
            let in_range v = v >= 0 && v <= 255 in
            let (r', g', b') = Color.ycocg_u_to_rgb ~y ~co_u:co ~cg_u:cg in
            if not (in_range y && in_range co && in_range cg
                    && abs (r' - r) <= Color.max_error
                    && abs (g' - g) <= Color.max_error
                    && b' = b) then ok := false
          done
        done
      done;
      !ok)

(* ---------------- Mapper ---------------- *)

let test_mapper =
  unit_test "mapper: unmap (map) = pos and map is a bijection" (fun () ->
      let ok = ref true in
      for q_order = 0 to 6 do
        let max = Tables.level_max ~q_order in
        let hit = Array.make max false in
        for level = 0 to max - 1 do
          List.iter (fun sign ->
              let sym = Mapper.map ~q_order ~level ~sign in
              let pos = Mapper.pos_of ~q_order ~level ~sign in
              if sym < 0 || sym >= max || Mapper.unmap ~q_order ~symbol:sym <> pos then ok := false;
              if sign then hit.(sym) <- true)
            [ true; false ]
        done;
        if not (Array.for_all (fun b -> b) hit) then ok := false
      done;
      !ok)

(* ---------------- Quant ---------------- *)

let anchor_entries_consistent =
  unit_test "anchor fullcase: v_hi/v_lo agree with quant_anchor_0x.inc" (fun () ->
      let ok = ref true in
      Array.iteri (fun tbl entries ->
          Array.iteri (fun addr (e : Tables.anchor_entry) ->
              if e.level >= 0 then begin
                let res = addr - 256 in
                let rv = tables.Tables.anchor_res.(tbl) in
                let expect_hi = if res < 0 then - rv.(e.level) else rv.(e.level) in
                let lo_level = if e.level = 0 then 0 else e.level - 1 in
                let expect_lo = if res < 0 then - rv.(lo_level) else rv.(lo_level) in
                if e.v_hi <> expect_hi || e.v_lo <> expect_lo then begin
                  Printf.printf "tbl %d res %d: entry (%d,%d,L%d) expected (%d,%d)\n"
                    tbl res e.v_hi e.v_lo e.level expect_hi expect_lo;
                  ok := false
                end
              end) entries) tables.Tables.anchor;
      !ok)

let gen_case =
  QCheck.Gen.(quad (int_bound 3) (int_bound 6) (int_bound 255) (int_bound 255))
let print_case = QCheck.Print.(quad int int int int)

let test_quant_dequant =
  QCheck.Test.make ~count:5000 ~name:"quant: recon in range, modular dequant matches, error bounded"
    (QCheck.make ~print:print_case gen_case)
    (fun (lane, q_order, pred, pix) ->
       let st = Dpcm.init ~first_pix:pred in
       let (res, headroom) = Dpcm.residual st ~pix in
       QCheck.assume (lane <> 0 || Quant.anchor_available tables ~q_order ~res);
       let r = Quant.quantize tables ~lane ~q_order ~zero:false ~headroom ~res in
       let st' = Dpcm.reconstruct st ~q_res:r.Quant.q_res in   (* raises on overflow *)
       let pos = Mapper.pos_of ~q_order ~level:r.Quant.level ~sign:r.Quant.sign in
       let m = Quant.dequantize tables ~lane ~q_order ~pos in
       let back = Dpcm.reconstruct_reverse ~next_pix:st'.Dpcm.pred ~q_res_mod:m in
       back = pred
       && abs (r.Quant.q_res - res) <= Quant.max_gap tables ~lane ~q_order
       && r.Quant.level < Tables.level_max ~q_order)

let test_quant_zero =
  unit_test "quant: zero flag yields level 0 / q_res 0 for every input" (fun () ->
      let ok = ref true in
      for lane = 0 to 3 do
        for res = -255 to 255 do
          let r = Quant.quantize tables ~lane ~q_order:2 ~zero:true ~headroom:255 ~res in
          if r.Quant.level <> 0 || r.Quant.q_res <> 0 || r.Quant.sign then ok := false
        done
      done;
      !ok)

(* ---------------- Dpcm chain: forward encode, reverse decode ---------------- *)

let gen_column =
  QCheck.Gen.(pair (int_bound 6) (list_size (map (fun k -> 4 * (k + 1)) (int_bound 40)) (int_bound 255)))
let print_column = QCheck.Print.(pair int (list int))

let test_dpcm_chain =
  QCheck.Test.make ~count:500 ~name:"dpcm: forward chain reconstructed exactly by reverse LIFO walk"
    (QCheck.make ~print:print_column gen_column)
    (fun (q_order, pixels) ->
       let n = List.length pixels in
       let pix = Array.of_list pixels in
       (* forward: encoder-side reconstruction and positions *)
       let recon = Array.make n 0 and pos = Array.make n 0 in
       let st = ref (Dpcm.init ~first_pix:pix.(0)) in
       let skipped = ref false in
       for i = 0 to n - 1 do
         let lane = i mod 4 in
         let (res, headroom) = Dpcm.residual !st ~pix:pix.(i) in
         if lane = 0 && not (Quant.anchor_available tables ~q_order ~res) then skipped := true
         else begin
           let r = Quant.quantize tables ~lane ~q_order ~zero:false ~headroom ~res in
           st := Dpcm.reconstruct !st ~q_res:r.Quant.q_res;
           recon.(i) <- (!st).Dpcm.pred;
           pos.(i) <- Mapper.pos_of ~q_order ~level:r.Quant.level ~sign:r.Quant.sign
         end
       done;
       QCheck.assume (not !skipped);
       (* reverse: start from the last reconstruction (header last_pixel) *)
       let dec = Array.make n 0 in
       dec.(n - 1) <- recon.(n - 1);
       for i = n - 2 downto 0 do
         let m = Quant.dequantize tables ~lane:((i + 1) mod 4) ~q_order ~pos:pos.(i + 1) in
         dec.(i) <- Dpcm.reconstruct_reverse ~next_pix:dec.(i + 1) ~q_res_mod:m
       done;
       dec = recon)

let () =
  QCheck_base_runner.run_tests_main
    [ test_color_exhaustive; test_mapper; anchor_entries_consistent;
      test_quant_dequant; test_quant_zero; test_dpcm_chain ]
