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
import re
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


def draw(key, size=96, ink=INK):
    """One symbol on a transparent canvas, cropped to its ink."""
    from PIL import Image, ImageDraw, ImageFont
    parsed = parse_key(key)
    if not parsed:
        return None
    font = ImageFont.truetype(font_file(), size)
    # The draw origin decides where the glyph's edges land between pixels, so
    # it stays where it was: moving it rerenders every symbol in the repo
    # with different antialiasing and nothing else to show for it.
    img = Image.new("RGBA", (size * 3, size * 3), (255, 255, 255, 0))
    ImageDraw.Draw(img).text((size // 2, size // 2), chr(codepoint(*parsed)),
                             font=font, fill=tuple(ink) + (255,))
    box = img.getbbox()
    return img.crop(box) if box else None


def render(key, path, size=96, pad=6, ink=INK):
    from PIL import Image
    img = draw(key, size, ink)
    if img is None:
        return False
    out = Image.new("RGBA", (img.width + pad * 2, img.height + pad * 2),
                    (255, 255, 255, 0))
    # copied, not composited: pasting through the image as its own mask
    # blends every half-transparent edge pixel toward the canvas colour
    out.paste(img, (pad, pad))
    os.makedirs(os.path.dirname(path), exist_ok=True)
    out.save(path)
    return True


# A Formal SignWriting string places every symbol on a 1000x1000 field whose
# centre is 500,500, in units that are pixels when the font is set at 30.
FSW_UNIT = 30
SYMBOL_RE = re.compile(r"S([0-9a-f]{3}[0-9a-f]{2})(\d{3})x(\d{3})")


def fsw_symbols(fsw):
    """[(key, x, y)] for a Formal SignWriting string."""
    return [("S" + k, int(x), int(y)) for k, x, y in SYMBOL_RE.findall(fsw or "")]


def symbol_size(key):
    """(width, height) of a symbol in FSW units."""
    img = draw(key, FSW_UNIT)
    return (img.width, img.height) if img else (0, 0)


def fsw_box(symbols):
    """The M prefix for a set of placed symbols.

    The sign box is symmetric about 500,500, so each edge is as far out as
    the furthest symbol edge on either side of the centre.
    """
    mx = my = 500
    for key, x, y in symbols:
        w, h = symbol_size(key)
        mx = max(mx, x + w, 1000 - x)
        my = max(my, y + h, 1000 - y)
    return "M%dx%d" % (mx, my)


def fsw_join(symbols):
    return fsw_box(symbols) + "".join("%s%03dx%03d" % (k, x, y)
                                      for k, x, y in symbols)


def render_fsw(fsw, path, size=96, pad=6, ink=INK):
    """Render a whole string, every symbol where the string puts it."""
    from PIL import Image
    syms = fsw_symbols(fsw)
    if not syms:
        return False
    scale = size / FSW_UNIT
    drawn = [(draw(k, size, ink), x, y) for k, x, y in syms]
    drawn = [(im, x, y) for im, x, y in drawn if im is not None]
    if not drawn:
        return False
    x0 = min(x for _im, x, _y in drawn)
    y0 = min(y for _im, _x, y in drawn)
    places = [(im, round((x - x0) * scale), round((y - y0) * scale))
              for im, x, y in drawn]
    W = max(px + im.width for im, px, _py in places)
    H = max(py + im.height for im, _px, py in places)
    out = Image.new("RGBA", (W + pad * 2, H + pad * 2), (255, 255, 255, 0))
    for im, px, py in places:
        out.paste(im, (px + pad, py + pad), im)
    os.makedirs(os.path.dirname(path), exist_ok=True)
    out.save(path)
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
    """Render every symbol the data files reference.

    A letter can carry more than one transcription: Latin holds an ASL and a
    BSL string, and the page can show either, so both their handshapes need a
    picture. A transcription with a movement in it is more than one symbol, and
    the hand alone is not the sign, so those are drawn whole and filed under
    the letter rather than under a symbol key.
    """
    n = 0
    for f in sorted(glob.glob(os.path.join(DATA_DIR, "*.json"))):
        if f.endswith("index.json"):
            continue
        with open(f, encoding="utf-8") as fh:
            doc = json.load(fh)
        slug, changed = doc["slug"], False
        for L in doc["letters"]:
            strings = [L.get("signwriting")] + \
                      list((L.get("signwriting_variants") or {}).values())
            for fsw in strings:
                for key, _x, _y in fsw_symbols(fsw) or ([(fsw, 0, 0)] if fsw else []):
                    base = parse_key(key)
                    if not base or not HAND_FIRST <= base[0] <= HAND_LAST:
                        continue
                    if render(key, os.path.join(HERE,
                              f"images/signwriting/{slug}/{key}.png")):
                        n += 1

            fsw = L.get("signwriting")
            if not fsw:
                continue
            syms = fsw_symbols(fsw)
            if len(syms) > 1:
                rel = f"images/signwriting/{slug}/{ord(L['glyph']):04x}.png"
                ok = render_fsw(fsw, os.path.join(HERE, rel))
            else:
                key = syms[0][0] if syms else fsw
                rel = f"images/signwriting/{slug}/{key}.png"
                ok = render(key, os.path.join(HERE, rel))
            if ok and L.get("signwriting_image") != rel:
                L["signwriting_image"] = rel
                changed = True
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
