#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Apply a signwriting-keys.json export from signwriting.html.

The export maps script slug -> letter glyph -> Formal SignWriting key, e.g.

    {"latin": {"A": "S1f720", "B": "S14c20"}}

Keys are written into each letter's `signwriting` field and the matching
symbol is rendered to images/signwriting/<slug>/<key>.png.

    python3 apply_signwriting.py ~/Downloads/signwriting-keys.json
"""
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(HERE, "assets", "data", "scripts")

sys.path.insert(0, HERE)
from render_signwriting import render, parse_key  # noqa: E402

SYMBOL = re.compile(r"S([0-9a-f]{3})([0-9a-f])([0-9a-f])")
HAND_FIRST, HAND_LAST = 0x100, 0x204


def swap_hand(fsw, key):
    """Replace the handshape inside an FSW string, keeping everything else.

    A correction names a handshape, not a whole sign. Overwriting the string
    with the bare key would throw away the rest of the transcription, which
    for letters with movement (ASL j and z) includes the arrow.
    """
    if not fsw:
        return key
    def sub(m):
        base = int(m.group(1), 16)
        return key if HAND_FIRST <= base <= HAND_LAST else m.group(0)
    out, n = SYMBOL.subn(sub, fsw, count=0)
    return out if n else fsw


def main(argv):
    if len(argv) != 2:
        print(__doc__)
        return 2

    path = os.path.expanduser(argv[1])
    if not os.path.exists(path):
        print(f"No such file: {path}\n")
        print("That file comes from the transcriber, so export it first:")
        print("  1. open signwriting.html on the local server")
        print("  2. assign symbols to the letters you want")
        print("  3. press 'Export JSON' (it saves to your Downloads)")
        print("  4. run this command again")
        return 1

    with open(path, encoding="utf-8") as fh:
        keys = json.load(fh)
    if not isinstance(keys, dict) or not keys:
        print(f"{path} has no transcriptions in it.")
        return 1

    applied, bad, unknown = 0, [], []
    for slug, letters in keys.items():
        data_file = os.path.join(DATA_DIR, f"{slug}.json")
        if not os.path.exists(data_file):
            unknown.append(slug)
            continue
        with open(data_file, encoding="utf-8") as fh:
            doc = json.load(fh)
        by_glyph = {L["glyph"]: L for L in doc["letters"]}
        touched = False
        for glyph, key in letters.items():
            L = by_glyph.get(glyph)
            if L is None:
                unknown.append(f"{slug}/{glyph}")
                continue
            if not parse_key(key):
                bad.append(f"{slug}/{glyph}={key}")
                continue
            rel = f"images/signwriting/{slug}/{key}.png"
            if not render(key, os.path.join(HERE, rel)):
                bad.append(f"{slug}/{glyph}={key} (no glyph in font)")
                continue
            new_fsw = swap_hand(L.get("signwriting"), key)
            if L.get("signwriting") != new_fsw or L.get("signwriting_image") != rel:
                L["signwriting"] = new_fsw
                L["signwriting_image"] = rel
                touched = True
            applied += 1
        if touched:
            with open(data_file, "w", encoding="utf-8") as fh:
                json.dump(doc, fh, ensure_ascii=False, indent=2)
                fh.write("\n")
        print(f"{slug}: {len(letters)} transcriptions")

    print(f"\napplied {applied}")
    if bad:
        print("rejected: " + ", ".join(bad))
    if unknown:
        print("unrecognised: " + ", ".join(unknown))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
