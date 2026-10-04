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

CHARTS = {
    # slug of the script this chart belongs to, the chart file, the grid, and
    # the letters in reading order.
    "arabic": {
        "file": "ArSL_1.PNG",
        "crop": (28, 28, 706, 622),   # strip the white border
        "rows": 4, "cols": 7,
        "rtl": True,
        # 28 letters, 7 per row, right to left
        "cells": list(AR),
        "isolate": "panel",   # hand sits on a grey panel, label below on white
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
    cw, ch = W / cols, H / rows

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
            box = (round(c * cw), round(r * ch), round((c + 1) * cw), round((r + 1) * ch))
            raw = im.crop(box)
            name = "-".join(f"{ord(ch_):04x}" for ch_ in letter)

            # Keep the uncropped cell so re-cropping is always non-destructive:
            # crop.html edits against raw/, never against the published image.
            raw_dir = os.path.join(out_dir, "raw")
            os.makedirs(raw_dir, exist_ok=True)
            raw.save(os.path.join(raw_dir, f"{name}.png"))

            rel = f"images/handshapes/{slug}/{name}.png"
            isolate(raw, cfg).save(os.path.join(HERE, rel))
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
    wanted = argv[1:] or list(CHARTS)
    total = 0
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
