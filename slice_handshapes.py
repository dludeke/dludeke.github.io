#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Slice manual-alphabet charts into one image per letter.

Each chart is a regular grid, so a chart is described by its margins, its
number of rows and columns, and the reading order of its cells. Reading order
matters: the Arabic and Hebrew charts run right to left, so cell (0,0) is the
top *right* letter, not the top left.

Cells are listed per chart as the sequence of letter glyphs in reading order,
with None for a cell that is blank or holds something that is not a letter.
Output goes to images/handshapes/<system>/<codepoint>.png so filenames stay
safe regardless of script.

    python3 slice_handshapes.py            # slice every configured chart
    python3 slice_handshapes.py arabic     # just one
"""
import json
import os
import sys

try:
    from PIL import Image
except ImportError:
    sys.exit("Pillow is required:  python3 -m pip install --user Pillow")

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = os.path.expanduser("~/Downloads/handshapes")
OUT_ROOT = os.path.join(HERE, "images", "handshapes")
DATA_DIR = os.path.join(HERE, "assets", "data", "scripts")

AR = "ابتثجحخدذرزسشصضطظعغفقكلمنهوي"
CSL30 = [chr(65 + i) for i in range(26)] + ["ZH", "CH", "SH", "NG"]
RU = "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"


GEEZ33 = "ሀለሐመሠረሰሸቀበተቸኀነኘአከኸወዐዘዠየደጀገጠጨጰጸፀፈፐ"
HANGUL24 = "ㄱㄴㄷㄹㅁㅂㅅㅇㅈㅊㅋㅌㅍㅎㅏㅑㅓㅕㅗㅛㅜㅠㅡㅣ"

# Hebrew: 4 per row read right to left. The chart also shows the five final
# forms and a sin/shin variant, which are not separate letters here.
HEB_CELLS = [
    "א", "ב", "ג", "ד",
    "ה", "ו", "ז", "ח",
    "ט", "י", "כ", None,      # None = final kaf
    "ל", "מ", None, "נ",      # final mem
    None, "ס", "ע", "פ",      # final nun
    None, "צ", None, "ק",     # final pe, final tsadi
    "ר", "ש", None, "ת",      # sin variant
]

# Kana: two blocks of five gojuon columns side by side, read left to right.
KANA_CELLS = [
    "あ","い","う","え","お", "は","ひ","ふ","へ","ほ",
    "か","き","く","け","こ", "ま","み","む","め","も",
    "さ","し","す","せ","そ", "や",None,"ゆ",None,"よ",
    "た","ち","つ","て","と", "ら","り","る","れ","ろ",
    "な","に","ぬ","ね","の", "わ",None,"を",None,"ん",
]

CHARTS = {
    # slug of the script this chart belongs to, the chart file, the grid, and
    # the letters in reading order.
    "geez": {
        "file": "EthSL.PNG",
        "crop": (88, 79, 616, 697),
        "rows": 6, "cols": 6,
        "rtl": False,
        # the final row of three is centred, not left-aligned
        "cells": list(GEEZ33)[:30] + [None] + list(GEEZ33)[30:] + [None, None],
        "isolate": "trim",
        "trim": (0.05, 0.05, 0.05, 0.22),   # label sits inside, bottom right
        "field": "handshape",
    },
    "hangul": {
        "file": "KSL_2.JPG",
        "crop": (6, 6, 566, 536),
        "rows": 5, "cols": 7,
        "rtl": False,
        # 31 numbered cells; 25-31 are extra vowels this reference does not list
        "cells": list(HANGUL24) + [None] * 11,
        "isolate": "trim",
        "trim": (0.04, 0.04, 0.04, 0.30),   # white label strip along the bottom
        "field": "handshape",
    },
    "hebrew": {
        "file": "ISL.JPG",
        "crop": (8, 95, 552, 788),
        "rows": 7, "cols": 4,
        "rtl": True,
        "cells": HEB_CELLS,
        "isolate": "trim",
        "trim": (0.04, 0.04, 0.26, 0.04),   # label to the left of each hand (RTL)
        "field": "handshape",
    },
    "kana": {
        "file": "JSL_2.JPG",
        "crop": (0, 0, 454, 340),
        "rows": 5, "cols": 10,
        "rtl": False,
        "cells": KANA_CELLS,
        "isolate": "trim",
        "trim": (0.02, 0.02, 0.02, 0.30),   # kana label under each hand
        "field": "handshape",
    },
    "farsi": {
        "file": "ZEI_3.PNG",
        "crop": (6, 6, 500, 566),
        "rows": 7, "cols": 5,
        "rtl": True,
        # Top row holds vowel forms; only the first maps to a letter here.
        # This chart omits ر entirely, so re has no image.
        "cells": [
            "ا", None, None, None, None,
            "ب", "پ", "ت", "ث", "ج",
            "چ", "ح", "خ", "د", "ذ",
            "ز", "ژ", "س", "ش", "ص",
            "ض", "ط", "ظ", "ع", "غ",
            "ف", "ق", "ک", "گ", "ل",
            "م", "ن", "و", "ه", "ی",
        ],
        "isolate": "trim",
        "trim": (0.03, 0.03, 0.03, 0.30),   # label strip under each hand
        "field": "handshape",
    },
    "thai": {
        "file": "ThSL.PNG",
        "crop": None,
        "rows": 6, "cols": 7,
        # measured borders; the columns are not equal widths
        "colx": [0, 97, 216, 359, 508, 660, 786, 848],
        "rowy": [0, 113, 215, 311, 404, 497, 580],
        "rtl": False,
        # This is a research figure keyed by phonetic code ("ก"=K, "ข"=K1), so
        # the order is by sound, not by Thai alphabetical order. Several cells
        # hold a motion sequence of two or three hands rather than one shape.
        # The two obsolete letters ฃ and ฅ are absent from the chart.
        "cells": [
            "ก", "ข", "ค", "ฆ", "ล", "ฬ", "ร",
            "ต", "ถ", "ฐ", "ฒ", "ท", "ฏ", "ม",
            "ส", "ศ", "ษ", "ซ", "ห", "ฮ", "ว",
            "พ", "ป", "ผ", "ภ", "ด", "ฎ", "บ",
            "ฟ", "ฝ", "ย", "ญ", "ณ", "ง", "น",
            "จ", "ฑ", "ธ", "ฌ", "ช", "ฉ", "อ",
        ],
        "isolate": "trim",
        "trim": (0.03, 0.03, 0.03, 0.22),   # phonetic-code label under each cell
        "field": "handshape",
    },
    "arabic": {
        "file": "ArSL_1.PNG",
        "crop": (28, 28, 706, 622),   # strip the white border
        "rows": 4, "cols": 7,
        "rtl": True,
        # 28 letters, 7 per row, right to left
        "cells": list(AR),
        "isolate": "panel",   # hand sits on a grey panel, label below on white
        # The panel detector swallows ق's label because it prints hard against
        # the panel edge; drop the band it leaves behind.
        "overrides": {"ق": (0.0, 0.0, 0.0, 0.34)},
        "field": "handshape",
    },
    "pinyin": {
        "file": "CSL_1.JPG",
        "crop": (72, 229, 1597, 2405),   # grid bounds from border detection
        "isolate": "trim",               # label sits inside the box, bottom-left
        "trim": (0.06, 0.03, 0.04, 0.20),  # l, t, r, b as fractions
        "rows": 6, "cols": 5,
        "rtl": False,
        "cells": CSL30,
        "field": "handshape",
    },
    "cyrillic": {
        "file": "RSL_1.PNG",
        "crop": (28, 88, 648, 628),
        "rows": 6, "cols": 6,
        "rtl": False,
        # 33 letters across 36 cells; the last three hold a caption
        "cells": list(RU) + [None, None, None],
        "isolate": "red",   # glyphs are red, labels grey: crop to the red ink
        "field": "signwriting_image",
    },
}


def isolate(cell, cfg):
    """Reduce a sliced cell to just the handshape, dropping its printed label."""
    mode = cfg.get("isolate")
    if mode == "trim":
        l, t, r, b = cfg.get("trim", (0, 0, 0, 0))
        w, h = cell.size
        return cell.crop((round(w * l), round(h * t),
                          round(w * (1 - r)), round(h * (1 - b))))
    if mode == "red":
        # SignWriting glyphs are printed red while the letter labels are grey
        # or blue, so the glyph is exactly the bounding box of red pixels.
        import numpy as np
        a = np.asarray(cell.convert("RGB"), dtype=np.int16)
        r, g, b = a[..., 0], a[..., 1], a[..., 2]
        red = (r > 110) & (r - g > 50) & (r - b > 50)
        rows = np.where(red.any(axis=1))[0]
        cols = np.where(red.any(axis=0))[0]
        if len(rows) and len(cols):
            pad = cfg.get("pad", 6)
            w, h = cell.size
            return cell.crop((max(0, int(cols[0]) - pad), max(0, int(rows[0]) - pad),
                              min(w, int(cols[-1]) + 1 + pad), min(h, int(rows[-1]) + 1 + pad)))
        return cell

    if mode == "panel":
        # The photo sits on a uniform grey panel against white; find the panel.
        import numpy as np
        a = np.asarray(cell.convert("L"), dtype=np.uint8)
        mask = a < 225
        rows = np.where(mask.mean(axis=1) > 0.5)[0]
        cols = np.where(mask.mean(axis=0) > 0.5)[0]
        if len(rows) and len(cols):
            return cell.crop((int(cols[0]), int(rows[0]),
                              int(cols[-1]) + 1, int(rows[-1]) + 1))
    return cell



# Devanagari comes from a table rather than a grid of pictures: the sign images
# sit in two of eight unequal columns, so the columns are given explicitly.
NSL_BLOCKS = [
    # file, (x0, x1) of each image column, y0, y1, rows, letters down each column
    ("NSL_part1.jpg", [(296, 392), (660, 756)], 78, 600, 7,
     [["अ", "आ", "इ", "ई", "उ", "ऊ", "ऋ"],
      ["ए", "ऐ", "ओ", "औ", None, None, None]]),       # last two are अं and अः
    ("NSL_part2.jpg", [(300, 421), (689, 790)], 123, 1035, 12,
     [["क", "ख", "ग", "घ", "ङ", "च", "छ", "ज", "झ", "ञ", "ट", "ठ"],
      ["ड", "ढ", "ण", "त", "थ", "द", "ध", "न", "प", "फ", "ब", "भ"]]),
    ("NSL_part2.jpg", [(300, 421), (689, 790)], 1077, 1545, 6,
     [["म", "य", "र", "ल", "व", "श"],
      ["ष", "स", "ह", None, None, None]]),            # last three are conjuncts
]


def slice_nsl(verbose=True):
    mapping = {}
    out_dir = os.path.join(OUT_ROOT, "devanagari")
    raw_dir = os.path.join(out_dir, "raw")
    os.makedirs(raw_dir, exist_ok=True)
    for fname, xcols, y0, y1, nrows, columns in NSL_BLOCKS:
        path = os.path.join(SRC, fname)
        if not os.path.exists(path):
            print(f"  devanagari: missing {fname}")
            continue
        im = Image.open(path).convert("RGB")
        rh = (y1 - y0) / nrows
        for (cx0, cx1), letters in zip(xcols, columns):
            for r, letter in enumerate(letters):
                if letter is None:
                    continue
                box = (cx0, round(y0 + r * rh), cx1, round(y0 + (r + 1) * rh))
                raw = im.crop(box)
                name = f"{ord(letter):04x}"
                raw.save(os.path.join(raw_dir, f"{name}.png"))
                rel = f"images/handshapes/devanagari/{name}.png"
                # trim the caption printed under each little sign picture
                w, h = raw.size
                raw.crop((2, 2, w - 2, round(h * 0.88))).save(os.path.join(HERE, rel))
                mapping[letter] = rel
    if verbose:
        print(f"  devanagari: {len(mapping)} cells from NSL tables")
    return mapping


def apply_override(cell, cfg, letter):
    """Per-letter correction for cells the chart-wide rule gets wrong."""
    o = (cfg.get("overrides") or {}).get(letter)
    if not o:
        return cell
    l, t, r, b = o
    w, h = cell.size
    return cell.crop((round(w * l), round(h * t), round(w * (1 - r)), round(h * (1 - b))))


def slice_chart(slug, cfg, verbose=True):
    path = os.path.join(SRC, cfg["file"])
    if not os.path.exists(path):
        print(f"  {slug}: missing {cfg['file']}")
        return {}

    im = Image.open(path).convert("RGB")
    if cfg.get("crop"):
        im = im.crop(cfg["crop"])
    W, H = im.size
    rows, cols = cfg["rows"], cfg["cols"]
    # Some charts have unequal columns (Thai widens the cells that hold a
    # motion sequence), so explicit boundaries can be given instead.
    xs = cfg.get("colx") or [round(i * W / cols) for i in range(cols + 1)]
    ys = cfg.get("rowy") or [round(i * H / rows) for i in range(rows + 1)]

    out_dir = os.path.join(OUT_ROOT, slug)
    os.makedirs(out_dir, exist_ok=True)

    mapping, i = {}, 0
    for r in range(rows):
        order = range(cols - 1, -1, -1) if cfg.get("rtl") else range(cols)
        for c in order:
            if i >= len(cfg["cells"]):
                break
            letter = cfg["cells"][i]
            i += 1
            if letter is None:
                continue
            box = (xs[c], ys[r], xs[c + 1], ys[r + 1])
            raw = im.crop(box)
            name = "-".join(f"{ord(ch_):04x}" for ch_ in letter)

            # Keep the uncropped cell so re-cropping is always non-destructive:
            # crop.html edits against raw/, never against the published image.
            raw_dir = os.path.join(out_dir, "raw")
            os.makedirs(raw_dir, exist_ok=True)
            raw.save(os.path.join(raw_dir, f"{name}.png"))

            rel = f"images/handshapes/{slug}/{name}.png"
            apply_override(isolate(raw, cfg), cfg, letter).save(os.path.join(HERE, rel))
            mapping[letter] = rel

    if verbose:
        print(f"  {slug}: {len(mapping)} cells from {cfg['file']} ({W}x{H}, {cols}x{rows})")
    return mapping


def apply_to_data(slug, cfg, mapping):
    """Write the sliced paths into the script's data file."""
    path = os.path.join(DATA_DIR, f"{slug}.json")
    if not os.path.exists(path):
        print(f"  {slug}: no data file")
        return 0
    with open(path, encoding="utf-8") as fh:
        doc = json.load(fh)
    field = cfg["field"]
    n = 0
    for L in doc["letters"]:
        rel = mapping.get(L["glyph"])
        if rel and L.get(field) != rel:
            L[field] = rel
            n += 1
    with open(path, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, ensure_ascii=False, indent=2)
        fh.write("\n")
    return n


def main(argv):
    wanted = argv[1:] or (list(CHARTS) + ["devanagari"])
    total = 0
    if "devanagari" in wanted:
        m = slice_nsl()
        if m:
            total += apply_to_data("devanagari", {"field": "handshape"}, m)
            print(f"    wrote paths into devanagari.json (handshape)")
        wanted = [w for w in wanted if w != "devanagari"]
    for slug in wanted:
        cfg = CHARTS.get(slug)
        if not cfg:
            print(f"  {slug}: no chart configured")
            continue
        mapping = slice_chart(slug, cfg)
        if mapping:
            n = apply_to_data(slug, cfg, mapping)
            print(f"    wrote {n} paths into {slug}.json ({cfg['field']})")
            total += n
    print(f"\n{total} letters given an image")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
