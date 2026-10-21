#!/usr/bin/env python3
"""Generate a synthetic test-image set for the image codec experiment.

All images are SYNTHETIC (procedurally generated); there are no real
photographs.  Output: 24-bit uncompressed BMP, 720x96, written to
    /home/user/ocaml_model/images/{train,test}/{ui,text,photo,gradient,noise}/
Train uses seed base 1000, test uses seed base 2000; each image gets its own
numpy RandomState so generation is fully deterministic.

Dependencies: Pillow, numpy, standard library.
"""
import hashlib
import os
import sys
import numpy as np
from PIL import Image, ImageDraw, ImageFont, ImageFilter

W, H = 720, 96
ROOT = "/home/user/ocaml_model/images"
CATEGORIES = ["ui", "text", "photo", "gradient", "noise"]
SETS = {"train": 1000, "test": 2000}
N_PER_CAT = 6

FONTS = {
    "sans": "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    "serif": "/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf",
    "mono": "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
    "free": "/usr/share/fonts/truetype/freefont/FreeSans.ttf",
}
FONT_LIST = list(FONTS.values())

WORDS = ("alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu "
         "xi omicron pi rho sigma tau upsilon phi chi psi omega codec entropy "
         "quantize predict residual block pixel sample channel bitrate lossless "
         "transform wavelet context model arithmetic range coder buffer frame "
         "the quick brown fox jumps over the lazy dog 0123456789 OCaml GM").split()


def seed_for(set_base, cat_idx, img_idx):
    return set_base + cat_idx * 100 + img_idx


def rand_words(rng, n):
    return " ".join(WORDS[int(i)] for i in rng.randint(0, len(WORDS), n))


def to_img(arr):
    return Image.fromarray(np.clip(np.round(arr), 0, 255).astype(np.uint8), "RGB")


# ---------------------------------------------------------------- helpers
SATURATED = [(255, 0, 0), (0, 0, 255), (0, 200, 0), (255, 0, 255), (255, 200, 0),
             (0, 220, 220), (255, 90, 0), (120, 0, 255), (0, 128, 0), (220, 0, 90)]
PASTEL = [(255, 200, 210), (200, 220, 255), (210, 245, 210), (250, 235, 200),
          (230, 210, 250), (205, 240, 245), (245, 245, 245), (225, 225, 225)]


def pick(rng, lst):
    return lst[int(rng.randint(0, len(lst)))]


def value_noise(rng, w, h, cell, resample=Image.BICUBIC):
    """Random grid of size ~(w/cell, h/cell) upsampled with bicubic/bilinear interpolation."""
    gw, gh = max(2, int(np.ceil(w / cell)) + 1), max(2, int(np.ceil(h / cell)) + 1)
    g = rng.rand(gh, gw).astype(np.float32)
    im = Image.fromarray(g, "F").resize((gw * cell, gh * cell), resample)
    return np.asarray(im, dtype=np.float32)[:h, :w]


def fbm(rng, w, h, base_cell=64, octaves=4, persistence=0.5):
    """Multi-octave value noise in [0,1]."""
    out = np.zeros((h, w), np.float32)
    amp, cell, total = 1.0, base_cell, 0.0
    for _ in range(octaves):
        res = Image.BICUBIC if cell >= 8 else Image.BILINEAR
        out += amp * value_noise(rng, w, h, max(2, cell), res)
        total += amp
        amp *= persistence
        cell = max(2, cell // 2)
    out /= total
    lo, hi = out.min(), out.max()
    return (out - lo) / max(hi - lo, 1e-6)


# ---------------------------------------------------------------- UI
def gen_ui(rng, k):
    bg = pick(rng, PASTEL) if k % 2 == 0 else pick(rng, [(250, 250, 250), (30, 34, 40), (240, 244, 248)])
    img = Image.new("RGB", (W, H), bg)
    d = ImageDraw.Draw(img)
    font = ImageFont.truetype(pick(rng, FONT_LIST), int(rng.randint(10, 15)))
    # Left panel with hard saturated edge pairs (guaranteed red/blue + green/magenta)
    pairs = [((255, 0, 0), (0, 0, 255)), ((0, 200, 0), (255, 0, 255))]
    rng.shuffle(pairs)
    x = 0
    pw = int(rng.randint(40, 80))
    for a, b in pairs:
        d.rectangle([x, 0, x + pw - 1, H // 2 - 1], fill=a)       # horizontal edge at H/2
        d.rectangle([x, H // 2, x + pw - 1, H - 1], fill=b)
        d.rectangle([x + pw, 0, x + 2 * pw - 1, H - 1], fill=b)   # vertical edge at x+pw
        d.rectangle([x + pw, H // 2, x + 2 * pw - 1, H - 1], fill=a)
        x += 2 * pw
    # Cards / buttons
    n_cards = int(rng.randint(3, 6))
    cx = x + 8
    for i in range(n_cards):
        cw = int(rng.randint(70, 130))
        if cx + cw > W - 4:
            break
        card_col = pick(rng, PASTEL) if rng.rand() < 0.6 else pick(rng, SATURATED)
        border = (0, 0, 0) if rng.rand() < 0.5 else pick(rng, SATURATED)
        y0 = int(rng.randint(4, 20))
        y1 = H - int(rng.randint(4, 20))
        d.rectangle([cx, y0, cx + cw - 1, y1], fill=card_col, outline=border, width=1)
        # Button inside card
        bcol = pick(rng, SATURATED)
        d.rounded_rectangle([cx + 6, y1 - 24, cx + cw - 7, y1 - 6], radius=int(rng.randint(2, 7)),
                            fill=bcol, outline=(0, 0, 0), width=1)
        tcol = (255, 255, 255) if sum(bcol) < 400 else (0, 0, 0)
        d.text((cx + 10, y1 - 22), pick(rng, ["OK", "Cancel", "Save", "Run", "Open", "Apply"]),
               fill=tcol, font=font)
        # Icon
        icon = int(rng.randint(0, 4))
        ic = pick(rng, SATURATED)
        ix, iy = cx + 6, y0 + 6
        if icon == 0:
            d.ellipse([ix, iy, ix + 16, iy + 16], fill=ic, outline=(0, 0, 0))
        elif icon == 1:
            d.polygon([(ix, iy + 16), (ix + 8, iy), (ix + 16, iy + 16)], fill=ic, outline=(0, 0, 0))
        elif icon == 2:
            d.rounded_rectangle([ix, iy, ix + 16, iy + 16], radius=4, fill=ic, outline=(0, 0, 0))
        else:
            d.line([(ix, iy + 9), (ix + 6, iy + 15), (ix + 16, iy + 2)], fill=ic, width=3)
        # Label text (grayscale AA via PIL)
        lcol = (20, 20, 20) if sum(card_col) > 380 else (240, 240, 240)
        d.text((ix + 22, y0 + 5), pick(rng, ["Settings", "Profile", "Inbox", "Report", "Status", "Files"]),
               fill=lcol, font=font)
        cx += cw + int(rng.randint(4, 12))
    # Fill any remainder with a thin-bordered panel and separators
    if cx < W - 10:
        d.rectangle([cx, 2, W - 3, H - 3], fill=pick(rng, PASTEL), outline=(80, 80, 80), width=1)
        for yy in range(10, H - 10, int(rng.randint(12, 20))):
            d.line([(cx + 4, yy), (W - 7, yy)], fill=pick(rng, SATURATED), width=1)
    return img


# ---------------------------------------------------------------- TEXT
def render_text_subpixel(rng, lines, fg, bg, fonts_sizes):
    """ClearType-style: render at 3x horizontal resolution in grayscale ('L'),
    then map triplets of horizontal samples to R,G,B of one output pixel."""
    hi = Image.new("L", (W * 3, H), 0)
    d = ImageDraw.Draw(hi)
    y = 2
    for text, (fpath, size) in zip(lines, fonts_sizes):
        f = ImageFont.truetype(fpath, size * 3)  # scale font 3x horizontally...
        # ...PIL cannot scale non-uniformly, so render at 3x in both axes then squash vertically.
        tmp = Image.new("L", (W * 3, size * 3 + 12), 0)
        ImageDraw.Draw(tmp).text((6, 0), text, fill=255, font=f)
        tmp = tmp.resize((W * 3, size + 4), Image.BOX)
        hi.paste(tmp, (0, y), tmp)
        y += size + 4 + int(rng.randint(0, 3))
        if y > H:
            break
    cov = np.asarray(hi, dtype=np.float32).reshape(H, W, 3) / 255.0  # (H,W,3): per-subpixel coverage
    fg = np.asarray(fg, np.float32)[None, None, :]
    bg = np.asarray(bg, np.float32)[None, None, :]
    return to_img(bg + (fg - bg) * cov)


def render_text_gray(rng, lines, fg, bg, fonts_sizes):
    img = Image.new("RGB", (W, H), bg)
    d = ImageDraw.Draw(img)
    y = 2
    for text, (fpath, size) in zip(lines, fonts_sizes):
        d.text((2, y), text, fill=fg, font=ImageFont.truetype(fpath, size))
        y += size + 4 + int(rng.randint(0, 3))
        if y > H:
            break
    return img


def gen_text(rng, k):
    n_lines = int(rng.randint(4, 7))
    sizes = [int(rng.randint(10, 23)) for _ in range(n_lines)]
    fonts_sizes = [(pick(rng, FONT_LIST), s) for s in sizes]
    lines = [rand_words(rng, int(rng.randint(8, 22))) for _ in range(n_lines)]
    # k: 0 black-on-white subpixel, 1 white-on-dark subpixel, 2,3 coloured-on-coloured subpixel,
    #    4,5 plain grayscale AA (black/white, white/dark)
    if k == 0:
        return render_text_subpixel(rng, lines, (0, 0, 0), (255, 255, 255), fonts_sizes)
    if k == 1:
        return render_text_subpixel(rng, lines, (255, 255, 255), (int(rng.randint(10, 40)),) * 3, fonts_sizes)
    if k in (2, 3):
        combos = [((255, 255, 0), (0, 0, 160)), ((0, 80, 0), (255, 230, 200)), ((200, 0, 0), (220, 240, 255)),
                  ((255, 255, 255), (0, 90, 180)), ((30, 30, 120), (255, 240, 120))]
        fg, bg = combos[(k + int(rng.randint(0, 3))) % len(combos)]
        return render_text_subpixel(rng, lines, fg, bg, fonts_sizes)
    if k == 4:
        return render_text_gray(rng, lines, (0, 0, 0), (255, 255, 255), fonts_sizes)
    return render_text_gray(rng, lines, (255, 255, 255), (int(rng.randint(10, 50)),) * 3, fonts_sizes)


# ---------------------------------------------------------------- PHOTO
PALETTES = [
    # sky top, sky bottom, ground a, ground b
    ((110, 160, 230), (200, 220, 245), (90, 120, 50), (150, 130, 70)),
    ((250, 160, 90), (255, 220, 170), (60, 50, 40), (120, 90, 60)),
    ((40, 60, 110), (120, 140, 190), (30, 60, 90), (70, 100, 120)),
    ((180, 200, 220), (235, 235, 230), (140, 130, 110), (200, 190, 170)),
    ((90, 140, 200), (170, 200, 230), (60, 110, 60), (120, 150, 80)),
    ((230, 120, 100), (250, 200, 150), (80, 70, 90), (150, 120, 130)),
]


def gen_photo(rng, k):
    yy, xx = np.mgrid[0:H, 0:W].astype(np.float32)
    sky0, sky1, g0, g1 = PALETTES[k % len(PALETTES)]
    sky0, sky1, g0, g1 = (np.asarray(c, np.float32) for c in (sky0, sky1, g0, g1))
    horizon = int(rng.randint(30, 70))
    # sky gradient with slow noise modulation (clouds)
    t = np.clip(yy / max(horizon, 1), 0, 1)[..., None]
    sky = sky0 + (sky1 - sky0) * t
    cloud = fbm(rng, W, H, base_cell=96, octaves=4)[..., None]
    sky = sky + (cloud - 0.5) * 40
    # ground texture: fbm with finer octaves
    gnoise = fbm(rng, W, H, base_cell=int(rng.randint(16, 48)), octaves=5, persistence=0.55)[..., None]
    ground = g0 + (g1 - g0) * gnoise
    ground *= (1.0 - 0.3 * np.clip((yy - horizon) / 60.0, 0, 1))[..., None]  # darkens toward bottom
    # soft horizon boundary (slightly wavy)
    wave = (fbm(rng, W, 1, base_cell=120, octaves=2)[0] - 0.5) * 10
    hline = horizon + wave[None, :]
    mix = np.clip((yy - hline) / 2.0 + 0.5, 0, 1)[..., None]
    img = sky * (1 - mix) + ground * mix
    # shaded ellipses (soft object boundaries, radial shading)
    for _ in range(int(rng.randint(1, 4))):
        cx, cy = rng.randint(40, W - 40), rng.randint(15, H - 15)
        rx, ry = rng.randint(15, 60), rng.randint(10, 40)
        col = np.asarray(pick(rng, [(200, 60, 40), (240, 200, 60), (60, 90, 170), (150, 150, 150),
                                    (80, 140, 60), (230, 230, 230)]), np.float32) * (0.8 + 0.4 * rng.rand())
        r = np.sqrt(((xx - cx) / rx) ** 2 + ((yy - cy) / ry) ** 2)
        inside = np.clip((1.0 - r) / 0.06, 0, 1)[..., None]  # soft edge
        lx, ly = rng.uniform(-1, 1), rng.uniform(-1, 1)
        shade = 0.55 + 0.45 * np.clip(1.0 - r + 0.5 * ((xx - cx) / rx * lx + (yy - cy) / ry * ly), 0, 1)
        img = img * (1 - inside) + (col * shade[..., None]) * inside
    im = to_img(img).filter(ImageFilter.GaussianBlur(radius=0.6 + 1.2 * rng.rand()))
    arr = np.asarray(im, np.float32)
    sigma = 1.0 + 2.0 * rng.rand()
    arr += rng.normal(0, sigma, arr.shape).astype(np.float32)
    return to_img(arr)


# ---------------------------------------------------------------- GRADIENT
def gen_gradient(rng, k):
    yy, xx = np.mgrid[0:H, 0:W].astype(np.float32)
    if k == 0:   # horizontal, full 0..255 range over 720 px (step ~0.35), saturated -> unsaturated
        t = xx / (W - 1)
        if rng.rand() < 0.5:
            t = 1.0 - t
        c0 = np.asarray(pick(rng, SATURATED), np.float32)
        c1 = np.asarray(pick(rng, [(255, 255, 255), (0, 0, 0), (128, 128, 128)]), np.float32)
        return to_img(c0[None, None] + (c1 - c0)[None, None] * t[..., None])
    if k == 1:   # vertical, black -> saturated blue (steps of 255/95 per row)
        t = yy / (H - 1)
        if rng.rand() < 0.5:
            t = 1.0 - t
        c0 = np.asarray(pick(rng, [(0, 0, 0), (255, 255, 255), (64, 64, 64)]), np.float32)
        c1 = np.asarray(pick(rng, SATURATED), np.float32)
        return to_img(c0 + (c1 - c0) * t[..., None])
    if k == 2:   # diagonal, 3 colour stops, full range
        t = (xx / (W - 1) * 0.7 + yy / (H - 1) * 0.3)
        stops = [np.asarray(pick(rng, SATURATED), np.float32), np.asarray((128, 128, 128), np.float32),
                 np.asarray(pick(rng, SATURATED), np.float32)]
        a = np.where(t < 0.5, t * 2, (t - 0.5) * 2)[..., None]
        lo = np.where((t < 0.5)[..., None], stops[0], stops[1])
        hi = np.where((t < 0.5)[..., None], stops[1], stops[2])
        return to_img(lo + (hi - lo) * a)
    if k == 3:   # radial, 2 stops, banded (quantised to small steps)
        cx, cy = rng.randint(100, W - 100), rng.randint(20, H - 20)
        r = np.sqrt((xx - cx) ** 2 + (yy - cy) ** 2) / 400.0
        t = np.clip(r, 0, 1)
        step = int(rng.randint(4, 12))
        t = np.floor(t * (255 / step)) * step / 255.0
        c0, c1 = np.asarray(pick(rng, PASTEL), np.float32), np.asarray(pick(rng, SATURATED), np.float32)
        return to_img(c0 + (c1 - c0) * t[..., None])
    if k == 4:   # very low-contrast horizontal gradient (range 40 levels) grey
        base = int(rng.randint(60, 180))
        t = xx / (W - 1)
        v = base + 40 * t
        if rng.rand() < 0.5:   # small banded steps
            v = np.floor(v / 2) * 2
        return to_img(np.stack([v, v, v], -1))
    # k == 5: horizontal 3-stop saturated gradient with banded steps + vertical modulation
    t = xx / (W - 1)
    s = [np.asarray(c, np.float32) for c in (pick(rng, SATURATED), pick(rng, PASTEL), pick(rng, SATURATED))]
    a = np.where(t < 0.5, t * 2, (t - 0.5) * 2)[..., None]
    lo = np.where((t < 0.5)[..., None], s[0], s[1])
    hi = np.where((t < 0.5)[..., None], s[1], s[2])
    img = lo + (hi - lo) * a
    img = img * (0.7 + 0.3 * yy / (H - 1))[..., None]
    step = int(rng.randint(3, 8))
    img = np.floor(img / step) * step
    return to_img(img)


# ---------------------------------------------------------------- NOISE
def gen_noise(rng, k):
    if k == 0:   # colour uniform noise, R,G,B independent, full range
        return to_img(rng.randint(0, 256, (H, W, 3)))
    if k == 1:   # gaussian around mid-gray sigma 20 (left) and 60 (right); grayscale
        g = np.empty((H, W), np.float32)
        g[:, :W // 2] = rng.normal(128, 20, (H, W // 2))
        g[:, W // 2:] = rng.normal(128, 60, (H, W - W // 2))
        return to_img(np.stack([g] * 3, -1))
    if k == 2:   # fine checkerboards and 1-px stripes (vertical / horizontal / diagonal), various contrasts
        yy, xx = np.mgrid[0:H, 0:W]
        out = np.zeros((H, W), np.float32)
        bands = [(0, 180), (180, 360), (360, 540), (540, 720)]
        rng.shuffle(bands)
        pats = [((xx + yy) % 2), (xx % 2), (yy % 2), ((xx - yy) % 2)]   # checker, vstripe, hstripe, diag
        for (x0, x1), p in zip(bands, pats):
            lo, hi = int(rng.randint(0, 80)), int(rng.randint(176, 256))
            out[:, x0:x1] = np.where(p[:, x0:x1] == 1, hi, lo)
        # 2-px variants on a thin top strip to vary frequency
        out[:8, :] = np.where(((xx[:8, :] // 2) + (yy[:8, :] // 2)) % 2 == 1, 255, 0)
        c = np.asarray(pick(rng, [(255, 255, 255), (255, 240, 200), (200, 220, 255)]), np.float32) / 255.0
        return to_img(out[..., None] * c[None, None, :])
    if k == 3:   # halftone-style dot patterns with varying dot pitch / size (simulated tone ramp)
        yy, xx = np.mgrid[0:H, 0:W].astype(np.float32)
        pitch = int(rng.randint(4, 8))
        ang = rng.uniform(0, np.pi / 4)
        u = xx * np.cos(ang) + yy * np.sin(ang)
        v = -xx * np.sin(ang) + yy * np.cos(ang)
        du = (u / pitch) - np.floor(u / pitch) - 0.5
        dv = (v / pitch) - np.floor(v / pitch) - 0.5
        r = np.sqrt(du ** 2 + dv ** 2)
        tone = xx / (W - 1)    # 0..1 ramp across image controls dot radius
        dot = (r < tone * 0.72).astype(np.float32)
        ink = np.asarray(pick(rng, [(0, 0, 0), (0, 160, 230), (230, 0, 120), (255, 220, 0)]), np.float32)
        paper = np.asarray((250, 250, 245), np.float32)
        return to_img(paper + (ink - paper) * dot[..., None])
    if k == 4:   # textured fabric: white noise filtered with small anisotropic kernel, warm tint
        n = rng.normal(0, 1, (H + 8, W + 8)).astype(np.float32)
        if rng.rand() < 0.5:
            kern = np.array([[1, 2, 4, 2, 1]], np.float32)          # horizontal threads
            kern2 = np.array([[1], [3], [1]], np.float32)
        else:
            kern = np.array([[1], [2], [4], [2], [1]], np.float32)  # vertical threads
            kern2 = np.array([[1, 3, 1]], np.float32)
        from numpy.lib.stride_tricks import sliding_window_view as swv
        def conv2(a, kk):
            kh, kw = kk.shape
            win = swv(a, (kh, kw))
            return np.einsum("ijkl,kl->ij", win, kk) / kk.sum()
        f = conv2(conv2(n, kern), kern2)[:H, :W]
        f = (f - f.mean()) / (f.std() + 1e-6)
        weave = ((np.arange(W)[None, :] // 2 + np.arange(H)[:, None] // 2) % 2) * 0.4 - 0.2
        v = 128 + 45 * f + 25 * weave
        tint = np.asarray(pick(rng, [(1.0, 0.92, 0.80), (0.85, 0.9, 1.0), (0.95, 1.0, 0.9)]), np.float32)
        return to_img(v[..., None] * tint[None, None, :])
    # k == 5: mixed strip: colour gaussian noise sigma 20 | grayscale uniform | per-channel independent
    #         gaussian sigma 60 | 1-px diagonal colour stripes
    out = np.empty((H, W, 3), np.float32)
    out[:, 0:180] = rng.normal(128, 20, (H, 180, 3))
    g = rng.randint(0, 256, (H, 180)).astype(np.float32)
    out[:, 180:360] = g[..., None]
    out[:, 360:540] = rng.normal(128, 60, (H, 180, 3))
    yy, xx = np.mgrid[0:H, 0:180]
    d = (xx + yy) % 3
    cols = np.asarray([pick(rng, SATURATED), pick(rng, SATURATED), (0, 0, 0)], np.float32)
    out[:, 540:720] = cols[d]
    return to_img(out)


GENERATORS = {"ui": gen_ui, "text": gen_text, "photo": gen_photo,
              "gradient": gen_gradient, "noise": gen_noise}


# ---------------------------------------------------------------- main
def stats(path):
    im = Image.open(path)
    im.load()
    a = np.asarray(im, np.float32)
    hdiff = np.abs(a[:, 1:, :] - a[:, :-1, :]).mean()
    return im.mode, im.size, float(a.mean()), float(a.std()), float(hdiff)


def main():
    manifest = []
    hashes = {}
    errors = []
    for set_name, base in SETS.items():
        for ci, cat in enumerate(CATEGORIES):
            outdir = os.path.join(ROOT, set_name, cat)
            os.makedirs(outdir, exist_ok=True)
            for k in range(N_PER_CAT):
                seed = seed_for(base, ci, k)
                rng = np.random.RandomState(seed)
                img = GENERATORS[cat](rng, k)
                assert img.mode == "RGB" and img.size == (W, H), (cat, k, img.mode, img.size)
                path = os.path.join(outdir, f"{cat}_{k:02d}.bmp")
                img.save(path, format="BMP")
                # verify
                mode, size, mean, std, hdiff = stats(path)
                fsize = os.path.getsize(path)
                expected = 54 + W * H * 3
                if mode != "RGB" or size != (W, H) or fsize != expected:
                    errors.append(f"{path}: mode={mode} size={size} bytes={fsize} expected={expected}")
                with open(path, "rb") as fh:
                    hdr = fh.read(54)
                    bpp = int.from_bytes(hdr[28:30], "little")
                    comp = int.from_bytes(hdr[30:34], "little")
                    if bpp != 24 or comp != 0:
                        errors.append(f"{path}: bpp={bpp} compression={comp}")
                    fh.seek(0)
                    h = hashlib.sha256(fh.read()).hexdigest()
                if h in hashes:
                    errors.append(f"{path}: identical to {hashes[h]}")
                hashes[h] = path
                manifest.append(f"{path} {size[0]} {size[1]} mean={mean:.2f} std={std:.2f} hdiff={hdiff:.2f}")
    header = (f"# SYNTHETIC test-image set (procedurally generated, no real photographs). "
              f"{W}x{H} 24-bit BMP. Seeds: train base {SETS['train']}, test base {SETS['test']}; "
              f"per-image seed = base + category_index*100 + image_index "
              f"(categories: {', '.join(CATEGORIES)}).")
    print(header)
    for line in manifest:
        print(line)
    with open(os.path.join(ROOT, "MANIFEST.txt"), "w") as fh:
        fh.write(header + "\n")
        fh.write("\n".join(manifest) + "\n")
    print(f"\n{len(manifest)} images written; {len(errors)} verification errors")
    for e in errors:
        print("ERROR:", e)
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
