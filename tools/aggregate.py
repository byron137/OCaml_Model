#!/usr/bin/env python3
"""Aggregate experiment outputs (results/<name>/per_image.csv, per_slice.csv,
symbols.csv) into results/REPORT.md with the spec 0.6 tables, the 1.4 / 2.6
pass criteria and 2x crop comparisons (results/crops/*.png).

Usage: aggregate.py <results_dir> <images_test_dir>
"""
import csv, os, sys, statistics as st
from collections import defaultdict

RES = sys.argv[1]
IMG = sys.argv[2]
CATS = ["ui", "text", "photo", "gradient", "noise"]


def load_images(name):
    p = os.path.join(RES, name, "per_image.csv")
    if not os.path.exists(p):
        return None
    with open(p) as f:
        return list(csv.DictReader(f))


def load_slices(name):
    with open(os.path.join(RES, name, "per_slice.csv")) as f:
        return list(csv.DictReader(f))


def load_symbols(name):
    with open(os.path.join(RES, name, "symbols.csv")) as f:
        return {(int(r["lane"]), int(r["col"])): r for r in csv.DictReader(f)}


def mean(xs):
    xs = list(xs)
    return st.fmean(xs) if xs else float("nan")


def summarize(rows):
    """per category + all: bpp, bits_raw/pixel, psnr, max_err, dE, fallback, over"""
    out = {}
    for cat in CATS + ["all"]:
        rs = [r for r in rows if cat == "all" or r["category"] == cat]
        if not rs:
            continue
        px = sum(int(r["pixels"]) for r in rs)
        out[cat] = dict(
            n=len(rs),
            bpp=96.0 * sum(int(r["blocks"]) for r in rs) / px,
            raw_bpp=sum(int(r["bits_raw"]) for r in rs) / px,
            side_bpp=sum(int(r["side_bits"]) for r in rs) / px,
            psnr=mean(min(99.0, float(r["psnr"])) for r in rs),   # inf (exact) capped at 99 dB
            max_err=max(int(r["max_err"]) for r in rs),
            de=mean(float(r["de_mean"]) for r in rs),
            de99=mean(float(r["de_p99"]) for r in rs),
            fb=mean(float(r["fallback_rate"]) for r in rs),
            macro=mean(float(r["macro_rate"]) for r in rs),
            over=mean(float(r["over_target_rate"]) for r in rs),
        )
    return out


def pct(a, b):
    return 100.0 * (a - b) / b if b else float("nan")


def table(title, names, baseline, prev_of, note=""):
    """one markdown table: rows = (experiment, category)"""
    lines = [f"### {title}", ""]
    if note:
        lines += [note, ""]
    lines.append("| exp | category | n | bpp (blocks) | raw bpp | side bpp | PSNR | max err | ΔE00 mean | ΔE00 p99 | fallback | over target | Δbits% vs prev | ΔPSNR vs prev | Δbits% vs base | ΔPSNR vs base |")
    lines.append("|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|")
    sums = {n: summarize(load_images(n)) for n in names + [baseline] if load_images(n)}
    for n in names:
        if n not in sums:
            continue
        for cat in CATS + ["all"]:
            s = sums[n].get(cat)
            if not s:
                continue
            b = sums[baseline].get(cat)
            p = sums.get(prev_of.get(n, baseline), {}).get(cat)
            dbp = pct(s["raw_bpp"], p["raw_bpp"]) if p else float("nan")
            dpp = s["psnr"] - p["psnr"] if p else float("nan")
            dbb = pct(s["raw_bpp"], b["raw_bpp"]) if b else float("nan")
            dpb = s["psnr"] - b["psnr"] if b else float("nan")
            bold = "**" if cat == "all" else ""
            lines.append(f"| {n} | {bold}{cat}{bold} | {s['n']} | {s['bpp']:.3f} | {s['raw_bpp']:.3f} | {s['side_bpp']:.3f} | {s['psnr']:.2f} | {s['max_err']} | {s['de']:.3f} | {s['de99']:.3f} | {s['fb']*100:.1f}% | {s['over']*100:.1f}% | {dbp:+.2f}% | {dpp:+.3f} | {dbb:+.2f}% | {dpb:+.3f} |")
    lines.append("")
    return "\n".join(lines), sums


def macro_slice_compare(names):
    """E1.4: compare only on slices that actually ended in macro under the
    reference run (same decisions for m1a/m1b; m1c may differ because of the
    side-info cost, so the intersection by (image, y, slice) of macro=true is used)."""
    sl = {n: load_slices(n) for n in names}
    key = lambda r: (r["image"], r["y"], r["slice_idx"])
    macro = None
    for n in names:
        m = {key(r) for r in sl[n] if r["macro"] == "true"}
        macro = m if macro is None else macro & m
    lines = ["### E1.4 — normal arbiter, slices that ended in macro mode under every variant", "",
             f"Macro slices common to all variants: {len(macro)} of {len(sl[names[0]])} "
             f"({100.0*len(macro)/len(sl[names[0]]):.1f}%). Per-slice PSNR/max err over that set, by category:", "",
             "| exp | category | slices | mean slice PSNR | max err | mean bits/slice |", "|---|---|---|---|---|---|"]
    for n in names:
        rs = [r for r in sl[n] if key(r) in macro]
        for cat in CATS + ["all"]:
            c = [r for r in rs if cat == "all" or r["category"] == cat]
            if not c:
                continue
            lines.append(f"| {n} | {cat} | {len(c)} | {mean(float(r['psnr']) for r in c):.2f} | {max(int(r['max_err']) for r in c)} | {mean(int(r['bits']) for r in c):.1f} |")
    # macro share per category under each run
    lines += ["", "Macro share of all slices (per category):", "", "| exp | " + " | ".join(CATS) + " | all |", "|---|" + "---|" * (len(CATS) + 1)]
    for n in names:
        row = []
        for cat in CATS + ["all"]:
            c = [r for r in sl[n] if cat == "all" or r["category"] == cat]
            row.append(f"{100.0*sum(r['macro']=='true' for r in c)/len(c):.1f}%")
        lines.append(f"| {n} | " + " | ".join(row) + " |")
    lines.append("")
    return "\n".join(lines)


def symbols_table(names):
    lines = ["### Residual statistics per (lane, channel): entropy bits/symbol, share of level 0 and of ±1", "",
             "| exp | lane | Cg H | Cg 0 | Cg ±1 | Co H | Co 0 | Co ±1 | Y H | Y 0 | Y ±1 |", "|---|---|---|---|---|---|---|---|---|---|---|"]
    for n in names:
        if not os.path.exists(os.path.join(RES, n, "symbols.csv")):
            continue
        s = load_symbols(n)
        for lane in range(4):
            cells = []
            for col in range(3):
                r = s[(lane, col)]
                cells.append(f"{float(r['entropy']):.3f} | {float(r['zero_share'])*100:.0f}% | {float(r['pm1_share'])*100:.0f}%")
            lines.append(f"| {n} | {lane} | " + " | ".join(cells) + " |")
    lines.append("")
    return "\n".join(lines)


def beta_hist(names):
    lines = ["### β index histogram (M1c runs)", "", "| exp | " + " | ".join(f"idx{i}" for i in range(8)) + " | share idx0 |", "|---|" + "---|" * 9]
    for n in names:
        rows = load_images(n)
        if not rows:
            continue
        h = [0] * 8
        for r in rows:
            for i, v in enumerate(r["beta_hist"].split(";")):
                h[i] += int(v)
        tot = sum(h) or 1
        lines.append(f"| {n} | " + " | ".join(str(x) for x in h) + f" | {100.0*h[0]/tot:.1f}% |")
    lines.append("")
    return "\n".join(lines)


# ----------------------------------------------------------------- crops --
def crops(pairs):
    try:
        from PIL import Image, ImageDraw
    except ImportError:
        return "PIL not available: no crops.\n"
    os.makedirs(os.path.join(RES, "crops"), exist_ok=True)
    made = []
    for cat in CATS:
        src = os.path.join(IMG, cat, f"{cat}_02.bmp")
        if not os.path.exists(src):
            continue
        orig = Image.open(src).convert("RGB")
        x0, x1 = 240, 360
        for base, exp, tag in pairs:
            bp = os.path.join(RES, base, f"{cat}_{cat}_02.bmp")
            ep = os.path.join(RES, exp, f"{cat}_{cat}_02.bmp")
            if not (os.path.exists(bp) and os.path.exists(ep)):
                continue
            tiles = []
            for label, path in [("orig", src), (base, bp), (exp, ep), (base + " heat", bp[:-4] + "_heat.bmp"), (exp + " heat", ep[:-4] + "_heat.bmp")]:
                im = Image.open(path).convert("RGB").crop((x0, 0, x1, 96)).resize(((x1 - x0) * 2, 192), Image.NEAREST)
                canvas = Image.new("RGB", (im.width, im.height + 14), (40, 40, 40))
                canvas.paste(im, (0, 14))
                ImageDraw.Draw(canvas).text((2, 1), label, fill=(255, 255, 255))
                tiles.append(canvas)
            w = sum(t.width for t in tiles) + 4 * (len(tiles) - 1)
            sheet = Image.new("RGB", (w, tiles[0].height), (40, 40, 40))
            x = 0
            for t in tiles:
                sheet.paste(t, (x, 0)); x += t.width + 4
            out = os.path.join(RES, "crops", f"{tag}_{cat}.png")
            sheet.save(out)
            made.append(out)
    return "\n".join(f"- `{os.path.relpath(m, RES)}`" for m in made) + "\n"


# ---------------------------------------------------------------- report --
def main():
    R = []
    R.append("# Experiment results — improvements 1 and 2\n")
    R.append("Generated by tools/aggregate.py from results/*/per_image.csv. Conditions: 2-slice (360 px), target 24 blocks = 6.4 bpp (3.75x), table_num 4, RLC on, palette off, synthetic test set (6 images x 5 categories, 720x96, images/test), codebooks for E2.2/E2.4 trained on images/train. 'raw bpp' = exact rANS payload + side info per pixel (no header, no block rounding); 'bpp (blocks)' = 96 x data_num per pixel including the 131-bit header. PSNR of an exactly reconstructed image (grayscale gradients under macro) is capped at 99 dB before averaging. Bits are the exact dyadic-rANS payload (km - ks per symbol), not a codeword-length proxy.\n")

    # improvement 1
    t, s1 = table("E1.0–E1.3 — macro chroma reconstruction, all slices forced to macro (rgbg mode)",
                  ["E1.0", "E1.1", "E1.2", "E1.3_L12", "E1.3_L36"], "E1.0",
                  {"E1.1": "E1.0", "E1.2": "E1.1", "E1.3_L12": "E1.2", "E1.3_L36": "E1.2"},
                  "E1.0 = M1a (copy), E1.1 = M1b (linear), E1.2 = M1c (linear + β, L = 20), E1.3 = M1c at L = 12 / 36. Bits are identical for M1a/M1b; M1c adds 6 bits per segment and chroma channel pair.")
    R.append("## Improvement 1 — macro mode odd chroma\n")
    R.append(t)
    R.append(beta_hist(["E1.2", "E1.3_L12", "E1.3_L36", "E1.4_m1c"]))
    R.append(macro_slice_compare(["E1.4_m1a", "E1.4_m1b", "E1.4_m1c"]))
    # criteria 1.4
    R.append("### Pass criteria (spec 1.4)\n")
    for cat in CATS + ["all"]:
        a, b, c = s1["E1.0"][cat], s1["E1.1"][cat], s1["E1.2"][cat]
        R.append(f"- {cat}: E1.1 vs E1.0 max err {a['max_err']} → {b['max_err']}, PSNR {a['psnr']:.2f} → {b['psnr']:.2f} ({'OK' if b['psnr'] >= a['psnr'] - 1e-9 else 'FAIL: PSNR below baseline'}); "
                 f"E1.2 vs E1.1 ΔE00 p99 {b['de99']:.3f} → {c['de99']:.3f} ({pct(c['de99'], b['de99']):+.1f}%), PSNR {c['psnr'] - b['psnr']:+.3f} dB")
    ui, tx = s1["E1.2"]["ui"], s1["E1.2"]["text"]
    ui1, tx1 = s1["E1.1"]["ui"], s1["E1.1"]["text"]
    ph, ph1 = s1["E1.2"]["photo"], s1["E1.1"]["photo"]
    R.append(f"\n- E1.2 criterion (UI and text ΔE00 p99 down ≥ 20 %, photo not worse than 0.1 dB): UI {pct(ui['de99'], ui1['de99']):+.1f}%, text {pct(tx['de99'], tx1['de99']):+.1f}%, photo ΔPSNR {ph['psnr'] - ph1['psnr']:+.3f} dB → "
             f"{'PASS' if pct(ui['de99'], ui1['de99']) <= -20 and pct(tx['de99'], tx1['de99']) <= -20 and ph['psnr'] - ph1['psnr'] >= -0.1 else 'NOT MET'}\n")

    # improvement 2
    R.append("## Improvement 2 — interpolative prediction in full mode\n")
    t, s2 = table("E2.0–E2.4 — structures and codebooks, normal arbiter",
                  ["E2.0", "E2.0_palette", "E2.0_trunk4", "E2.1", "E2.2", "E2.3", "E2.4", "E2.4_serial_oddeven"], "E2.0",
                  {"E2.0_trunk4": "E2.0", "E2.1": "E2.0", "E2.2": "E2.1", "E2.3": "E2.1", "E2.4": "E2.3", "E2.4_serial_oddeven": "E2.0"},
                  "E2.0 = serial chain (the model today), E2.0_palette = same with palette on (reference), E2.0_trunk4 = stride-4 trunk (the spec's stated baseline), E2.1 = variant A shared books, E2.2 = A + lane-3 book, E2.3 = variant B shared books, E2.4 = B + odd/even books, E2.4_serial_oddeven = serial + odd/even books (isolates the codebook effect from the structure).")
    R.append(t)
    R.append(symbols_table(["E2.0", "E2.0_trunk4", "E2.1", "E2.3"]))
    R.append("### Pass criteria (spec 2.6)\n")
    a0, a1, a3 = s2["E2.0"]["all"], s2["E2.1"]["all"], s2["E2.3"]["all"]
    R.append(f"- E2.1 (all-category Δbits ≤ −2 %, PSNR within ±0.1 dB): Δbits {pct(a1['raw_bpp'], a0['raw_bpp']):+.2f}%, ΔPSNR {a1['psnr'] - a0['psnr']:+.3f} dB → "
             f"{'PASS' if pct(a1['raw_bpp'], a0['raw_bpp']) <= -2 and abs(a1['psnr'] - a0['psnr']) <= 0.1 else 'NOT MET'}")
    t4 = s2["E2.0_trunk4"]["all"]
    R.append(f"- same, relative to the Trunk4 baseline the spec assumed: Δbits {pct(a1['raw_bpp'], t4['raw_bpp']):+.2f}%, ΔPSNR {a1['psnr'] - t4['psnr']:+.3f} dB")
    sy = load_symbols("E2.3")
    def share(lane, col):
        r = sy[(lane, col)]; return float(r["zero_share"]) + float(r["pm1_share"])
    chroma = min(share(l, c) for l in (1, 3) for c in (0, 1))
    luma = min(share(l, 2) for l in (1, 3))
    R.append(f"- E2.3 (bits within ±1.5 % of E2.1; lanes 1,3 share of 0/±1 ≥ 60 % chroma, ≥ 40 % Y): Δbits vs E2.1 {pct(a3['raw_bpp'], a1['raw_bpp']):+.2f}%, min chroma share {chroma*100:.1f}%, min Y share {luma*100:.1f}% → "
             f"{'PASS' if abs(pct(a3['raw_bpp'], a1['raw_bpp'])) <= 1.5 and chroma >= 0.6 and luma >= 0.4 else 'NOT MET'}\n")
    # E2.5 per category A vs B
    R.append("### E2.5 — A vs B per category (text: lane-2 stride-2 cost)\n")
    R.append("| category | A raw bpp | B raw bpp | B−A % | A PSNR | B PSNR |\n|---|---|---|---|---|---|")
    for cat in CATS + ["all"]:
        a, b = s2["E2.1"][cat], s2["E2.3"][cat]
        R.append(f"| {cat} | {a['raw_bpp']:.3f} | {b['raw_bpp']:.3f} | {pct(b['raw_bpp'], a['raw_bpp']):+.2f}% | {a['psnr']:.2f} | {b['psnr']:.2f} |")
    R.append("")
    # E2.6 appendix
    t, _ = table("E2.6 (appendix) — 1-slice (target 48) and 4-slice (target 12) at 6.4 bpp",
                 ["E2.6_s1_serial", "E2.6_s1_a", "E2.6_s1_b", "E2.6_s4_serial", "E2.6_s4_a", "E2.6_s4_b"], "E2.0",
                 {"E2.6_s1_a": "E2.6_s1_serial", "E2.6_s1_b": "E2.6_s1_serial", "E2.6_s4_a": "E2.6_s4_serial", "E2.6_s4_b": "E2.6_s4_serial"},
                 "'vs base' columns compare with the 2-slice serial run and are only indicative.")
    R.append(t)
    R.append("## Visual checks (2x crops, x = 240..360 of image _02 per category: orig / baseline / experiment / heatmaps 0..32)\n")
    R.append(crops([("E1.0", "E1.2", "E1"), ("E2.0", "E2.1", "E2A"), ("E2.0", "E2.4", "E2B")]))
    with open(os.path.join(RES, "REPORT.md"), "w") as f:
        f.write("\n".join(R))
    print("wrote", os.path.join(RES, "REPORT.md"))


main()
