#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Apply a handshape-crops.json export from crop.html to the sliced images.

crop.html stores a crop rectangle per letter as fractions of the *uncropped*
slice kept in images/handshapes/<slug>/raw/. Cropping is therefore always
non-destructive: re-cropping wider simply re-reads the raw cell, and running
slice_handshapes.py again regenerates raw without losing these edits.

    python3 apply_crops.py ~/Downloads/handshape-crops.json
"""
import json
import os
import sys

try:
    from PIL import Image
except ImportError:
    sys.exit("Pillow is required:  python3 -m pip install --user Pillow")

HERE = os.path.dirname(os.path.abspath(__file__))


def main(argv):
    if len(argv) != 2:
        print(__doc__)
        return 2

    path = os.path.expanduser(argv[1])
    if not os.path.exists(path):
        print(f"No such file: {path}\n")
        print("That file is produced by the cropping tool, so export it first:")
        print("  1. open crop.html on the local server")
        print("  2. adjust the crops you want")
        print("  3. press 'Export JSON' (it saves to your Downloads)")
        print("  4. run this command again")
        return 1

    try:
        with open(path, encoding="utf-8") as fh:
            crops = json.load(fh)
    except ValueError as e:
        print(f"{path} is not valid JSON: {e}")
        return 1
    if not isinstance(crops, dict) or not crops:
        print(f"{path} has no crops in it. Adjust at least one letter, then export again.")
        return 1

    applied, missing = 0, []
    for slug, letters in crops.items():
        for glyph, r in letters.items():
            name = "-".join(f"{ord(c):04x}" for c in glyph)
            raw = os.path.join(HERE, "images", "handshapes", slug, "raw", f"{name}.png")
            out = os.path.join(HERE, "images", "handshapes", slug, f"{name}.png")
            if not os.path.exists(raw):
                missing.append(f"{slug}/{glyph}")
                continue
            im = Image.open(raw)
            W, H = im.size
            box = (max(0, round(r["x"] * W)), max(0, round(r["y"] * H)),
                   min(W, round((r["x"] + r["w"]) * W)),
                   min(H, round((r["y"] + r["h"]) * H)))
            if box[2] <= box[0] or box[3] <= box[1]:
                missing.append(f"{slug}/{glyph} (empty rect)")
                continue
            im.crop(box).save(out)
            applied += 1
        print(f"{slug}: {len(letters)} crops")

    print(f"\napplied {applied} crops")
    if missing:
        print("skipped (no raw slice): " + ", ".join(missing))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
