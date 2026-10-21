# codec_gm — Codec V1.6 golden model（OCaml 4.14, stdlib only）

All modules implemented (Stages 2-8). Library is stdlib-only (OCaml 4.14); tests need qcheck-core.

| 模組 | RTL 對應 | 備註 |
|---|---|---|
| Tables | *.inc, compressor_ctrl.quant_order_idx_2_bus | 唯一真相源，直接 parse .inc |
| Params | regfile inputs (i_slice_num, i_target_data_num, `TABLE_NUM`…) | slice_num 用 variant 消除 RTL 編碼歧義 |
| Color | color_decorrelator / color_correlator | 有損（Co/Cg 截位），signed 除法 truncate |
| Dpcm | time_decorrelator / time_correlator | 首 pixel pred = p₀；反向以 last_pixel 起算 |
| Quant | nonlinear_quantizer (feedback = lane0, piped = lane1..3) | 雙表 + headroom 降階 |
| Mapper | distribution_mapper / unmapper | level_max 32/16/8 |
| Rlc | rlc_detector + sliding_palette | palette 語意見 rlc.mli，需波形確認一次 |
| Rans | entropy_encoder.rans_encoder / entropy_decoder.rans_decoder | 12 stream、state 0..255 |
| Bitstream | data_packer / compressor_ctrl header / data_unpacker | LSB-first，header 131/71 bit |
| Slice | compressor datapath / uncompressor | decode 獨立實作 LIFO |
| Rdo | scanner + cost_evaluator + priority_arbiter | 常數集中於 `Rdo.constants` |
| Frame / Data_file / Bmp / Metrics | — | I/O 與品質指標 |

## Build
```
dune build          # 需 vendored dune 與 qcheck-core（test 目錄可先拿掉）
dune exec bin/enc.exe -- <inc_dir> in.bmp out.data --stats out.json --recon enc_recon.bmp --slice 4 --target 15
dune exec bin/dec.exe -- <inc_dir> out.data out.bmp --ref in.bmp --heatmap err.bmp --metrics m.json --deblock on
```

## Layout (Stage 3)
- `lib/`  — implemented modules with tests: `Types`, `Tables`, `Rans` (Stage 3); `Color`, `Dpcm`, `Mapper`, `Quant` (Stage 4); `Bitvec`, `Bitstream`, `Rlc` (Stage 5); `Params`, `Slice` (Stage 6); `Rdo` (Stage 7); `Frame`, `Data_file`, `Bmp`, `Metrics` + `enc`/`dec` (Stage 8)
- `bin/dump_tables.ml` — loads the `.inc` directory, prints `Tables.check` and per-table summaries
- `bin/slice_trace.ml` — one slice end to end: raw RGB text in, decoded RGB text out, every
  stage printed (color transform, symbols, modes, blocks, header, per-pixel diff)
- `test/test_codec.ml` — QCheck: tokenizer, table consistency, rANS round trip per book,
  `Rans.inverse` vs `codebook_r.inc`
- `test/test_lossy_core.ml` — exhaustive color check, mapper bijection, anchor LUT vs
  `quant_anchor_0x.inc`, quant/dequant modular consistency, forward/reverse DPCM chain
- `test/test_stream.ml` — bitvec/block/hex round trip, header write/parse, `Rlc.classify` vs a
  register-level transcription of `rlc_detector.v`, run invariants, palette flags, and an
  entropy-layer round trip (Rlc + 12-stream Rans + Bitstream, decoded LIFO)
- `test/test_slice.ml` — full slice encode/decode round trip over every q_order_idx, slice
  layouts 1/2/4, palette/rgbg/rlc on and off; error bound; deblock locality
- `test/test_rdo.ml` — min4 tie-break, rlc_accumulator transcription, candidate data_num exact
  when RLC is off, chosen decision always fits the target (tables patched in-test so that
  ks agrees with cdf; see the .inc package MANIFEST)
- `test/test_frame.ml` — BMP round trip, frame encode -> .data -> decode == encoder reconstruction,
  metrics sanity

```
CODEC_INC_DIR=/path/to/rtl/image_codec dune test
dune exec bin/dump_tables.exe -- /path/to/rtl/image_codec
dune exec bin/slice_trace.exe -- /path/to/rtl/image_codec --idx 3 --cb 3 --table-num 4
dune exec bin/slice_trace.exe -- /path/to/rtl/image_codec --in my_slice.txt --out dec.txt --idx 1
```
