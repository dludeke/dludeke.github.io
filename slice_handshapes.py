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
        "clean": "ink",
        "trim_px": (5, 5, 9, 22), "tighten": "ink",
        "crop": (88, 79, 616, 697),
        "rows": 6, "cols": 6,
        "rtl": False,
        # the final row of three is centred, not left-aligned
        "cells": list(GEEZ33)[:30] + [None] + list(GEEZ33)[30:] + [None, None],
        "isolate": "trim",
        "field": "handshape",
    },
    "hebrew": {
        "file": "ISL.JPG",
        "trim_px": (4, 4, 40, 4), 
        "crop": (8, 95, 552, 788),
        "rows": 7, "cols": 4,
        "rtl": True,
        "cells": HEB_CELLS,
        "isolate": "trim",
        "field": "handshape",
    },
    "kana": {
        "file": "JSL_2.JPG",
        "trim_px": (1, 1, 1, 15), "tighten": "ink",
        "crop": (0, 0, 454, 340),
        "rows": 5, "cols": 10,
        "rtl": False,
        "cells": KANA_CELLS,
        "isolate": "trim",
        "field": "handshape",
    },
    "farsi": {
        "file": "ZEI_3.PNG",
        "trim_px": (4, 4, 4, 26), 
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
        "field": "handshape",
    },
    "thai": {
        "file": "ThSL.PNG",
        "clean": "ink",
        "trim_px": (4, 4, 4, 26), "tighten": "ink",
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
        # CSL_2 is a quarter the resolution of CSL_1 but printed clean; CSL_1 is
        # a photo of a book page whose reverse side shows through behind every
        # hand, which no amount of levels work removes.
        "file": "CSL_2.jpg",
        "clean": "ink",
        "crop": (3, 3, 327, 426),
        "rows": 6, "cols": 5,
        "rtl": False,
        "cells": CSL30,
        "trim_px": (6, 4, 7, 22), "tighten": "ink",
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



# Japanese: the dakuten, the handakuten, the small kana and the long-vowel
# bar are not shapes of their own. JSL signs the base kana and moves the hand
# -- sideways for the dakuten, up for the handakuten, back toward the signer
# for a small kana, down for the chōonpu -- so each of those kana is drawn as
# its base hand with the movement printed under it. The arrows are Sutton
# SignWriting movement symbols, which is what lets the same ones go into the
# transcriptions as well. The chōonpu has no base kana of its own: it moves
# whichever hand came before it, so it gets the arrow and nothing else.
KANA_ARROWS = {
    "side": "S26606",   # wall plane, straight, to the right
    "up":   "S26600",   # wall plane, straight, up
    "down": "S26604",   # wall plane, straight, down
    "back": "S22b04",   # floor plane, straight, toward the signer
}
# the arrow's longest side, as a fraction of the base image's height. Scaling
# by height alone makes the sideways arrow, which is wider than it is tall,
# come out twice the size of the other two.
KANA_ARROW_SCALE = 0.45
KANA_UPSCALE = 4        # the JSL chart cells are only ~36px; the arrow is not


def kana_moves():
    """glyph -> (base kana, which arrow), for every kana signed with movement."""
    import unicodedata
    from script_content import KANA_SMALL_BASE, KANA_VOICED
    out = {g: (b, "back") for g, b in KANA_SMALL_BASE.items()}
    for row in KANA_VOICED:
        g = row[0]
        d = unicodedata.normalize("NFD", g)
        out[g] = (d[0], "side" if d[1] == "\u3099" else "up")
    return out


def compose_kana(mapping, verbose=True):
    """Base hand plus arrow, as a picture and as an FSW string.

    Returns (handshapes, signwriting), both keyed by glyph. The FSW places
    the arrow under the hand and recomputes the sign box, so the string is a
    real transcription rather than two keys stuck together.
    """
    from render_signwriting import (draw, fsw_symbols, fsw_join, symbol_size,
                                    render_fsw, FSW_UNIT)
    out_dir = os.path.join(OUT_ROOT, "kana")
    os.makedirs(out_dir, exist_ok=True)

    # the three arrows on their own, for the Reference page's movement badge
    for name, key in KANA_ARROWS.items():
        img = draw(key, 96, ink=(0, 0, 0))
        if img:
            img.save(os.path.join(HERE, f"images/handshapes/kana/move-{name}.png"))

    path = os.path.join(DATA_DIR, "kana.json")
    with open(path, encoding="utf-8") as fh:
        doc = json.load(fh)
    base_fsw = {L["glyph"]: L.get("signwriting") for L in doc["letters"]}

    shapes, fsws = {}, {}
    for glyph, (base, which) in sorted(kana_moves().items()):
        key = KANA_ARROWS[which]
        name = f"{ord(glyph):04x}"

        src = mapping.get(base)
        if src and os.path.exists(os.path.join(HERE, src)):
            hand = Image.open(os.path.join(HERE, src)).convert("RGB")
            w, h = hand.width * KANA_UPSCALE, hand.height * KANA_UPSCALE
            hand = hand.resize((w, h), Image.Resampling.LANCZOS)
            arrow = draw(key, 96, ink=(0, 0, 0))
            k = max(8, round(h * KANA_ARROW_SCALE)) / max(arrow.width, arrow.height)
            arrow = arrow.resize((max(1, round(arrow.width * k)),
                                  max(1, round(arrow.height * k))),
                                 Image.Resampling.LANCZOS)
            gap = max(2, round(h * 0.06))
            sheet = Image.new("RGB", (max(w, arrow.width), h + gap + arrow.height),
                              (255, 255, 255))
            sheet.paste(hand, ((sheet.width - w) // 2, 0))
            sheet.paste(arrow, ((sheet.width - arrow.width) // 2, h + gap), arrow)
            rel = f"images/handshapes/kana/{name}.png"
            sheet.save(os.path.join(HERE, rel))
            shapes[glyph] = rel

        bf = base_fsw.get(base)
        if bf:
            syms = fsw_symbols(bf)
            if syms:
                low = max(y + symbol_size(k)[1] for k, _x, y in syms)
                aw, _ah = symbol_size(key)
                syms.append((key, 500 - aw // 2, low + 3))
                fsws[glyph] = fsw_join(syms)
                render_fsw(fsws[glyph],
                           os.path.join(HERE, f"images/signwriting/kana/{name}.png"))
    if verbose:
        print(f"  kana: {len(shapes)} hands and {len(fsws)} transcriptions "
              f"given a movement arrow")
    return shapes, fsws


def apply_kana_moves(shapes, fsws):
    path = os.path.join(DATA_DIR, "kana.json")
    with open(path, encoding="utf-8") as fh:
        doc = json.load(fh)
    n = 0
    for L in doc["letters"]:
        g = L["glyph"]
        if g in shapes:
            L["handshape"] = shapes[g]
        if g in fsws:
            L["signwriting"] = fsws[g]
            L["signwriting_image"] = f"images/signwriting/kana/{ord(g):04x}.png"
        if g in shapes or g in fsws:
            n += 1
    with open(path, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, ensure_ascii=False, indent=2)
        fh.write("\n")
    return n


# Korean comes from three screenshots rather than the single KSL_2 chart,
# which is a quarter of the resolution and stops at the ten basic vowels.
# 1a is the consonants, 1b and 1c the vowels: seventeen of them, including
# the y- and w-forms. ssang siot is the one cell holding two frames, since
# it is the only Korean letter signed with a movement; cropping the whole
# cell keeps both.
KSL_BLOCKS = [
    ("KSL_1a.jpg", [(379, 572), (672, 864), (964, 1156)],
     [(38, 182), (190, 334), (342, 486), (494, 638), (646, 790)],
     ["\u3131", "\u3134", "\u3137", "\u3139", "\u3141",
      "\u3142", "\u3145", "\u3146", "\u3147", "\u3148",
      "\u314a", "\u314b", "\u314c", "\u314d", "\u314e"]),
    ("KSL_1b.jpg", [(159, 402), (502, 745)],
     [(38, 220), (228, 410), (418, 600), (608, 790)],
     ["\u314f", "\u3150", "\u3151", "\u3152",
      "\u3153", "\u3154", "\u3155", "\u3156"]),
    ("KSL_1c.jpg", [(29, 272), (372, 615), (715, 958)],
     [(38, 220), (228, 410), (418, 600)],
     ["\u3157", "\u315a", "\u315b",
      "\u315c", "\u315f", "\u3160",
      "\u3161", "\u3162", "\u3163"]),
]

# The two ssang siot frames are inset in their cell and its caption is printed
# inside the band rather than under it, so that one cell is given outright.
KSL_BOXES = {"\u3146": (342, 714, 486, 822)}


# Every Korean jamo written out of two others. The chart has a shape of its
# own for ㅆ, ㅚ, ㅟ and ㅢ, so those are not here; the rest are spelled by
# signing their parts in order, and are drawn as the two hands side by side.
HANGUL_PARTS = {
    "ㄲ": "ㄱㄱ", "ㄸ": "ㄷㄷ", "ㅃ": "ㅂㅂ", "ㅉ": "ㅈㅈ",
    "ㅘ": "ㅗㅏ", "ㅙ": "ㅗㅐ", "ㅝ": "ㅜㅓ", "ㅞ": "ㅜㅔ",
    "ㄳ": "ㄱㅅ", "ㄵ": "ㄴㅈ", "ㄶ": "ㄴㅎ",
    "ㄺ": "ㄹㄱ", "ㄻ": "ㄹㅁ", "ㄼ": "ㄹㅂ",
    "ㄽ": "ㄹㅅ", "ㄾ": "ㄹㅌ", "ㄿ": "ㄹㅍ",
    "ㅀ": "ㄹㅎ", "ㅄ": "ㅂㅅ",
}
HANGUL_PART_GAP = 10


def compose_hangul(mapping, verbose=True):
    """Draw a two-part jamo as its two component hands, left to right."""
    out = {}
    for glyph, parts in sorted(HANGUL_PARTS.items()):
        srcs = [mapping.get(p) for p in parts]
        if not all(srcs):
            continue
        ims = [Image.open(os.path.join(HERE, p)).convert("RGB") for p in srcs]
        h = max(i.height for i in ims)
        ims = [i if i.height == h else
               i.resize((round(i.width * h / i.height), h), Image.Resampling.LANCZOS)
               for i in ims]
        w = sum(i.width for i in ims) + HANGUL_PART_GAP * (len(ims) - 1)
        sheet = Image.new("RGB", (w, h), (255, 255, 255))
        x = 0
        for i in ims:
            sheet.paste(i, (x, 0))
            x += i.width + HANGUL_PART_GAP
        rel = f"images/handshapes/hangul/{ord(glyph):04x}.png"
        sheet.save(os.path.join(HERE, rel))
        out[glyph] = rel
    if verbose:
        print(f"  hangul: {len(out)} two-part jamo drawn from their components")
    return out


def trim_white(cell, limit=238):
    """Trim a near-white margin, leaving the photograph itself alone.

    The ink-based tighten is wrong for photographs: its threshold sits inside
    the picture and crops away everything but the darkest part of the hand.
    This only looks for paper white, so a cell that is all photograph comes
    back unchanged and the two-frame ssang siot cell loses its surround.
    """
    import numpy as np
    a = np.asarray(cell.convert("L"), dtype=np.int16)
    mask = a < limit
    rows = np.where(mask.any(axis=1))[0]
    cols = np.where(mask.any(axis=0))[0]
    if not len(rows) or not len(cols):
        return cell
    return cell.crop((int(cols[0]), int(rows[0]),
                      int(cols[-1]) + 1, int(rows[-1]) + 1))


def slice_ksl(verbose=True):
    """Slice the three Korean screenshots into one image per jamo."""
    mapping = {}
    out_dir = os.path.join(OUT_ROOT, "hangul")
    raw_dir = os.path.join(out_dir, "raw")
    os.makedirs(raw_dir, exist_ok=True)
    for fname, bands, xs, letters in KSL_BLOCKS:
        path = os.path.join(SRC, fname)
        if not os.path.exists(path):
            print(f"  hangul: missing {fname}")
            continue
        im = Image.open(path).convert("RGB")
        i = 0
        for (y0, y1) in bands:
            for (x0, x1) in xs:
                if i >= len(letters):
                    break
                letter = letters[i]; i += 1
                if letter is None:
                    continue
                raw = im.crop(KSL_BOXES.get(letter) or (x0, y0, x1, y1))
                name = f"{ord(letter):04x}"
                raw.save(os.path.join(raw_dir, f"{name}.png"))
                rel = f"images/handshapes/hangul/{name}.png"
                trim_white(raw).save(os.path.join(HERE, rel))
                mapping[letter] = rel
    if verbose:
        print(f"  hangul: {len(mapping)} cells from the three KSL screenshots")
    return mapping


# Devanagari comes from a table rather than a grid of pictures: the sign images
# sit in two of eight unequal columns, so the columns are given explicitly.
NSL_BLOCKS = [
    # file, (x0, x1) of each image column, y0, y1, rows, letters down each column
    ("NSL_part1.jpg", [(296, 392), (660, 756)], 78, 600, 7,
     [["अ", "आ", "इ", "ई", "उ", "ऊ", "ऋ"],
      # rows 12 and 13 are printed as अं and अः, which is the mark on the bare
      # vowel; the sign is the mark's, so it is filed under the mark
      ["ए", "ऐ", "ओ", "औ", "ं", "ः", None]]),
    ("NSL_part2.jpg", [(300, 421), (689, 790)], 123, 1035, 12,
     [["क", "ख", "ग", "घ", "ङ", "च", "छ", "ज", "झ", "ञ", "ट", "ठ"],
      ["ड", "ढ", "ण", "त", "थ", "द", "ध", "न", "प", "फ", "ब", "भ"]]),
    ("NSL_part2.jpg", [(300, 421), (689, 790)], 1077, 1545, 6,
     [["म", "य", "र", "ल", "व", "श"],
      ["ष", "स", "ह", None, None, None]]),            # last three are conjuncts
]


# A matra is the same vowel as the independent letter, written as a sign on
# the consonant rather than standing on its own, and NSL signs it the same
# way. The chart only prints the independent column, so the matras had no
# handshape at all and words like \u0926\u093e\u0932 came apart with a blank in the middle.
NSL_MATRA_OF = {
    "\u093e": "\u0906", "\u093f": "\u0907", "\u0940": "\u0908", "\u0941": "\u0909",
    "\u0942": "\u090a", "\u0943": "\u090b", "\u0947": "\u090f", "\u0948": "\u0910",
    "\u094b": "\u0913", "\u094c": "\u0914",
}


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
                raw.crop((3, 2, w - 9, round(h * 0.86))).save(os.path.join(HERE, rel))
                mapping[letter] = rel
    matras = 0
    for matra, vowel in NSL_MATRA_OF.items():
        if vowel in mapping:
            mapping[matra] = mapping[vowel]
            matras += 1
    if verbose:
        print(f"  devanagari: {len(mapping) - matras} cells from NSL tables, "
              f"{matras} matras taking their vowel's sign")
    return mapping


# The EthSL chart does not stop at the 33 base shapes. Under them sit a
# legend of six arrows, one per vowel order, and a worked row showing ሀ
# taking each of them. That is how the alphabet reaches all 231 cells:
# a consonant's handshape plus the movement for its order. Slicing only
# the grid left 198 syllables with nothing.
ETHSL_COLS = [93, 181, 273, 356, 437, 523, 608]
ETHSL_ARROWS = (706, 752)     # the arrow alone, below the rule and above the captions
ETHSL_EXAMPLES = (805, 892)   # ሀ in the 2nd to 7th orders
ETHSL_ORDER_ROWS = [
    (1, "kaʽeb",  "ካዕብ", "an arc to the left"),
    (2, "salis",       "ሣልስ", "straight to the right"),
    (3, "rabʽe",  "ራብዕ", "straight down"),
    (4, "hames",       "ኃምስ", "up and hooking over"),
    (5, "sadis",       "ሳድስ", "down, with a shake"),
    (6, "sabʽe",  "ሳብዕ", "a loop"),
]


def slice_ethsl_movements(verbose=True):
    """The six movement arrows, and ሀ shown taking each of them."""
    path = os.path.join(SRC, "EthSL.PNG")
    if not os.path.exists(path):
        print("  geez: missing EthSL.PNG")
        return {}, {}
    im = Image.open(path).convert("RGB")
    out_dir = os.path.join(OUT_ROOT, "geez")
    raw_dir = os.path.join(out_dir, "raw")
    os.makedirs(raw_dir, exist_ok=True)

    moves, examples = {}, {}
    for i, (order, rom, amh, desc) in enumerate(ETHSL_ORDER_ROWS):
        x0, x1 = ETHSL_COLS[i], ETHSL_COLS[i + 1]
        for label, (y0, y1), store in (("move", ETHSL_ARROWS, moves),
                                       ("form", ETHSL_EXAMPLES, examples)):
            cell = im.crop((x0 + 3, y0, x1 - 2, y1))
            name = "%s-%d" % (label, order + 1)
            cell.save(os.path.join(raw_dir, name + ".png"))
            rel = "images/handshapes/geez/%s.png" % name
            trim_edges(cell).save(os.path.join(HERE, rel))
            store[order] = rel
    if verbose:
        print("  geez: %d movement arrows, %d worked forms" % (len(moves), len(examples)))
    return moves, examples


def trim_edges(cell):
    """Trim the blank margin around a sliced cell.

    No rule-detection here. The bands are cut inside the printed rules
    instead, because the 3rd form's arrow is a horizontal line and any
    test that spots a rule spots that too, and ate it.
    """
    a = cell.convert("L").point(lambda v: 255 if v < 170 else 0)
    bb = a.getbbox()
    return cell.crop(bb) if bb and bb[3] - bb[1] > 4 else cell


def tighten(cell, cfg):
    """Trim the printed label, then shrink to the drawing itself.

    The label band is a constant pixel height on these charts while the cells
    are not all the same size, so a fractional trim under-cuts the tall rows
    and over-cuts the short ones. trim_px is given in pixels for that reason.
    The content box is then found against whichever background the chart uses.
    """
    import numpy as np
    l, t, r, b = cfg.get("trim_px", (0, 0, 0, 0))
    w, h = cell.size
    cell = cell.crop((l, t, max(l + 1, w - r), max(t + 1, h - b)))

    if cfg.get("clean") == "ink":
        # These are photocopies: text printed on the reverse of the page shows
        # through as pale grey. Pull anything lighter than the paper up to
        # white, which removes the bleed without touching the drawn lines.
        import numpy as np
        g = np.asarray(cell.convert("L"), dtype=np.float32)
        lo, hi = float(np.percentile(g, 2)), float(np.percentile(g, 72))
        if hi > lo + 8:
            g = np.clip((g - lo) * (255.0 / (hi - lo)), 0, 255)
            cell = Image.fromarray(g.astype("uint8"), "L").convert("RGB")

    if not cfg.get("tighten"):
        return cell

    a = np.asarray(cell.convert("L"), dtype=np.int16)
    mode = cfg["tighten"]
    if mode == "ink":                  # dark drawing on pale paper
        mask = a < (int(np.median(a)) - 28)
    elif mode == "light_on_dark":       # pale hand on a dark panel
        mask = a > (int(np.median(a)) + 28)
    else:
        return cell

    rows = np.where(mask.sum(axis=1) > max(1, mask.shape[1] * 0.012))[0]
    cols = np.where(mask.sum(axis=0) > max(1, mask.shape[0] * 0.012))[0]
    if not len(rows) or not len(cols):
        return cell
    pad = cfg.get("pad", 4)
    H, W = a.shape
    return cell.crop((max(0, int(cols[0]) - pad), max(0, int(rows[0]) - pad),
                      min(W, int(cols[-1]) + 1 + pad), min(H, int(rows[-1]) + 1 + pad)))


def apply_override(cell, cfg, letter):
    """Per-letter correction for cells the chart-wide rule gets wrong."""
    o = (cfg.get("overrides") or {}).get(letter)
    if not o:
        return cell
    l, t, r, b = o
    w, h = cell.size
    return cell.crop((round(w * l), round(h * t), round(w * (1 - r)), round(h * (1 - b))))



# Latin is the one script with two manual alphabets worth showing side by side:
# BSL is two-handed, ASL one-handed. Both are sliced and the page toggles.
#
# BSL chart: CC BY-SA 3.0, User:Cowplopmorris via Wikimedia Commons.
# ASL chart: public domain, User:Ds13 (Gallaudet font) via Wikimedia Commons.
LATIN_VARIANTS = {
    "BSL": {
        # Measured bands: each drawing row alternates with a row of big letters.
        "file": "BSL_wikimedia.png",
        "bands": [
            (19, 94, [(22, 107), (145, 246), (282, 345), (380, 458), (495, 589), (627, 716)], "ABCDEF"),
            (271, 342, [(22, 104), (142, 215), (252, 352), (390, 478), (518, 588), (628, 710)], "GHIJKL"),
            (520, 597, [(22, 92), (131, 203), (244, 323), (361, 431), (470, 549), (589, 661)], "MNOPQR"),
            (770, 847, [(22, 97), (135, 214), (252, 349), (389, 460), (500, 592), (632, 713)], "STUVWX"),
            (1021, 1093, [(23, 90), (128, 202)], "YZ"),
        ],
    },
    "ASL": {
        # Rows hold 7, 6, 6 and 7 cells and do not share a column grid, so each
        # row is given as a y band with its own measured cell boundaries.
        "file": "ASL_gallaudet.png",
        "bands": [
            (31, 179, [(129, 194), (277, 333), (418, 525), (600, 661), (744, 806), (884, 954), (1005, 1130)], "ABCDEFG"),
            (400, 542, [(135, 273), (353, 426), (508, 617), (695, 755), (838, 946), (1024, 1104)], "HIJKLM"),
            (768, 906, [(133, 220), (300, 385), (468, 624), (705, 809), (888, 946), (1025, 1091)], "NOPQRS"),
            (1090, 1269, [(131, 194), (274, 334), (415, 476), (553, 623), (704, 785), (868, 991), (1030, 1143)], "TUVWXYZ"),
        ],
    },
}


def slice_latin(verbose=True):
    """Slice both Latin manual alphabets into variant folders."""
    import json as _json
    out = {}
    for name, cfg in LATIN_VARIANTS.items():
        path = os.path.join(SRC, cfg["file"])
        if not os.path.exists(path):
            print(f"  latin/{name}: missing {cfg['file']}")
            continue
        im = Image.open(path).convert("RGB")
        folder = os.path.join(OUT_ROOT, "latin", name.lower())
        raw_dir = os.path.join(folder, "raw")
        os.makedirs(raw_dir, exist_ok=True)
        got = {}

        def store(letter, cell):
            nm = f"{ord(letter):04x}"
            cell.save(os.path.join(raw_dir, f"{nm}.png"))
            rel = f"images/handshapes/latin/{name.lower()}/{nm}.png"
            tighten(cell, {"tighten": "ink", "pad": 5}).save(os.path.join(HERE, rel))
            got[letter] = rel

        if "bands" in cfg:
            for y0, y1, xs, letters in cfg["bands"]:
                for (x0, x1), letter in zip(xs, letters):
                    pad = 6
                    store(letter, im.crop((max(0, x0 - pad), y0 - pad, x1 + pad, y1 + pad)))
        else:
            im = im.crop(cfg["crop"])
            cols, rows = cfg["grid"]
            W, H = im.size
            cw, ch = W / cols, H / rows
            l, t, r, b = cfg["trim_px"]
            i = 0
            for rr in range(rows):
                for cc in range(cols):
                    if i >= len(cfg["cells"]):
                        break
                    letter = cfg["cells"][i]; i += 1
                    if letter is None:
                        continue
                    box = (round(cc * cw) + l, round(rr * ch) + t,
                           round((cc + 1) * cw) - r, round((rr + 1) * ch) - b)
                    store(letter, im.crop(box))
        out[name] = got
        if verbose:
            print(f"  latin/{name}: {len(got)} cells from {cfg['file']}")
    return out


def apply_latin(variants):
    path = os.path.join(DATA_DIR, "latin.json")
    with open(path, encoding="utf-8") as fh:
        doc = json.load(fh)
    n = 0
    for L in doc["letters"]:
        v = {k: m[L["glyph"]] for k, m in variants.items() if L["glyph"] in m}
        if not v:
            continue
        L["handshape_variants"] = v
        # default shown when the page has no preference stored
        L["handshape"] = v.get("BSL") or next(iter(v.values()))
        n += 1
    with open(path, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, ensure_ascii=False, indent=2)
        fh.write("\n")
    return n


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
            cell = apply_override(isolate(raw, cfg), cfg, letter)
            tighten(cell, cfg).save(os.path.join(HERE, rel))
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
    wanted = argv[1:] or (list(CHARTS) + ["devanagari", "latin", "hangul"])
    total = 0
    if "hangul" in wanted:
        m = slice_ksl()
        if m:
            m.update(compose_hangul(m))
            total += apply_to_data("hangul", {"field": "handshape"}, m)
            print("    wrote paths into hangul.json (handshape)")
        wanted = [w for w in wanted if w != "hangul"]
    if "latin" in wanted:
        v = slice_latin()
        if v:
            print(f"    wrote {apply_latin(v)} letters into latin.json (handshape_variants)")
        wanted = [w for w in wanted if w != "latin"]
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
        if slug == "kana" and mapping:
            shapes, fsws = compose_kana(mapping)
            n = apply_kana_moves(shapes, fsws)
            print(f"    wrote {n} moved kana into kana.json")
            total += n
    print(f"\n{total} letters given an image")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
