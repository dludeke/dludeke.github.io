#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Render Sutton SignWriting symbols to PNG.

The SignWriting font carries all 37,811 ISWA 2010 symbols, but it is nearly
8MB, which is far too much to make every visitor download. So symbols are
rendered here at build time and the site serves small PNGs instead.

A symbol is identified by a Formal SignWriting key: "S" then a three hex digit
base (100-38b), then a fill digit (0-5), then a rotation digit (0-f). The 261
hand symbols are the bases from S100 to S204. The codepoint is

    0x40001 + (base - 0x100) * 96 + fill * 16 + rotation

and the font maps only the combinations that actually exist, so membership in
its cmap is the test of whether a key is valid.

    python3 render_signwriting.py bases     # the picker's symbol grid
    python3 render_signwriting.py assigned  # whatever the data files reference
"""
import json
import glob
import os
import sys
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(HERE, "assets", "data", "scripts")
OUT = os.path.join(HERE, "images", "signwriting")
FONT_URL = ("https://cdn.jsdelivr.net/npm/@sutton-signwriting/font-ttf/"
            "font/SuttonSignWritingOneD.ttf")
FONT_PATH = os.path.join(HERE, "SuttonSignWritingOneD.ttf")

HAND_FIRST, HAND_LAST = 0x100, 0x204
INK = (200, 16, 16)


def font_file():
    if not os.path.exists(FONT_PATH):
        print(f"fetching {FONT_URL}")
        urllib.request.urlretrieve(FONT_URL, FONT_PATH)
    return FONT_PATH


def codepoint(base, fill=0, rot=0):
    return 0x40001 + (base - 0x100) * 96 + fill * 16 + rot


def parse_key(key):
    """'S1f721' -> (0x1f7, 2, 1).  Returns None if malformed."""
    if not key or len(key) != 6 or key[0].upper() != "S":
        return None
    try:
        return int(key[1:4], 16), int(key[4], 16), int(key[5], 16)
    except ValueError:
        return None


def render(key, path, size=96, pad=6):
    from PIL import Image, ImageDraw, ImageFont
    parsed = parse_key(key)
    if not parsed:
        return False
    cp = codepoint(*parsed)
    font = ImageFont.truetype(font_file(), size)
    img = Image.new("RGBA", (size * 2, size * 2), (255, 255, 255, 0))
    d = ImageDraw.Draw(img)
    d.text((size // 2, size // 2), chr(cp), font=font, fill=INK + (255,))
    box = img.getbbox()
    if not box:
        return False
    img = img.crop((max(0, box[0] - pad), max(0, box[1] - pad),
                    box[2] + pad, box[3] + pad))
    os.makedirs(os.path.dirname(path), exist_ok=True)
    img.save(path)
    return True


def valid_keys():
    """Every hand base the font actually has a glyph for."""
    from fontTools.ttLib import TTFont
    cmap = set(TTFont(font_file(), lazy=True).getBestCmap())
    return [b for b in range(HAND_FIRST, HAND_LAST + 1) if codepoint(b) in cmap]


def do_bases():
    bases = valid_keys()
    index = []
    for b in bases:
        key = f"S{b:03x}00"
        rel = f"images/signwriting/base/{key}.png"
        if render(key, os.path.join(HERE, rel), size=72):
            index.append({"key": key, "base": f"{b:03x}", "image": rel})
    out = os.path.join(HERE, "assets", "data", "signwriting-hands.json")
    with open(out, "w", encoding="utf-8") as fh:
        json.dump({"note": "ISWA 2010 hand symbols, base S100-S204, "
                           "shown at fill 0 rotation 0",
                   "symbols": index}, fh, ensure_ascii=False, indent=2)
        fh.write("\n")
    print(f"rendered {len(index)} hand symbols -> {out}")


def do_assigned():
    n = 0
    for f in sorted(glob.glob(os.path.join(DATA_DIR, "*.json"))):
        if f.endswith("index.json"):
            continue
        with open(f, encoding="utf-8") as fh:
            doc = json.load(fh)
        changed = False
        for L in doc["letters"]:
            key = L.get("signwriting")
            if not key:
                continue
            rel = f"images/signwriting/{doc['slug']}/{key}.png"
            if render(key, os.path.join(HERE, rel)):
                if L.get("signwriting_image") != rel:
                    L["signwriting_image"] = rel
                    changed = True
                n += 1
        if changed:
            with open(f, "w", encoding="utf-8") as fh:
                json.dump(doc, fh, ensure_ascii=False, indent=2)
                fh.write("\n")
    print(f"rendered {n} assigned symbols")


def main(argv):
    what = argv[1] if len(argv) > 1 else "bases"
    if what == "bases":
        do_bases()
    elif what == "assigned":
        do_assigned()
    else:
        print(__doc__)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
