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
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(HERE, "assets", "data", "scripts")

sys.path.insert(0, HERE)
from render_signwriting import render, parse_key  # noqa: E402


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
            if L.get("signwriting") != key or L.get("signwriting_image") != rel:
                L["signwriting"] = key
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
