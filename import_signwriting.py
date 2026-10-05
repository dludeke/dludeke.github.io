#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Import SignWriting fingerspelling transcriptions.

Source: sign-language-processing/signwriting (MIT), whose fingerspelling data
is taken from signwriting.org's fingerspelling keys and was transcribed by
Sutthikhun Phaengphongsai, funded by sign.mt ltd -- the same transcriber who
authored the Russian chart already in this repo.

Each entry is a Formal SignWriting string such as

    M507x507S1f720487x492

which is a sign box followed by positioned symbols. The handshape is the
symbol whose base lies in the hand range S100-S204; that key is what gets
rendered, while the whole FSW string is kept alongside it, since letters with
movement (ASL j and z) carry an arrow symbol the handshape alone loses.

    python3 import_signwriting.py            # fetch and apply
    python3 import_signwriting.py --dry-run  # report coverage only
"""
import json
import os
import re
import sys
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(HERE, "assets", "data", "scripts")
RAW = ("https://raw.githubusercontent.com/sign-language-processing/signwriting/"
       "main/signwriting/fingerspelling/data/{}.json")
CACHE = os.path.join(HERE, "fingerspelling-cache")

# which sign language's fingerspelling belongs to which script here
SOURCES = {
    "greek": [("gss", None)],          # Greek Sign Language
    "hebrew": [("isr", None)],         # Israeli Sign Language
    "kana": [("jsl", None)],           # Japanese Sign Language
    "hangul": [("ko", None)],          # Korean Sign Language
    "thai": [("tsq", None)],           # Thai Sign Language
    "pinyin": [("csl", None)],         # Chinese Sign Language
    "latin": [("ase", "ASL"), ("bfi", "BSL")],
}

SYMBOL = re.compile(r"S([0-9a-f]{3})([0-9a-f])([0-9a-f])")
HAND_FIRST, HAND_LAST = 0x100, 0x204


def hand_key(fsw):
    """The handshape symbol in an FSW string, or None."""
    for base, fill, rot in SYMBOL.findall(fsw):
        if HAND_FIRST <= int(base, 16) <= HAND_LAST:
            return f"S{base}{fill}{rot}"
    return None


def fetch(code):
    os.makedirs(CACHE, exist_ok=True)
    path = os.path.join(CACHE, f"{code}.json")
    if not os.path.exists(path):
        urllib.request.urlretrieve(RAW.format(code), path)
    with open(path, encoding="utf-8") as fh:
        return json.load(fh)


def lookup(table, glyph, alt):
    """Match a letter against the source table, trying sensible variants."""
    for cand in (glyph, glyph.lower(), alt, (alt or "").lower()):
        if cand and cand in table:
            return table[cand]
    return None


def main(argv):
    dry = "--dry-run" in argv
    from render_signwriting import render

    total_matched = total_letters = 0
    for slug, sources in SOURCES.items():
        path = os.path.join(DATA_DIR, f"{slug}.json")
        if not os.path.exists(path):
            continue
        with open(path, encoding="utf-8") as fh:
            doc = json.load(fh)

        tables = {(label or code): fetch(code) for code, label in sources}
        matched = 0
        for L in doc["letters"]:
            total_letters += 1
            found = {}
            for label, table in tables.items():
                entries = lookup(table, L["glyph"], L.get("alt"))
                if entries:
                    found[label] = entries[0]
            if not found:
                continue
            matched += 1

            if len(sources) > 1:
                L["signwriting_variants"] = found
                primary = found.get("ASL") or next(iter(found.values()))
            else:
                primary = next(iter(found.values()))
            L["signwriting"] = primary

            key = hand_key(primary)
            if key and not dry:
                rel = f"images/signwriting/{slug}/{key}.png"
                if render(key, os.path.join(HERE, rel)):
                    L["signwriting_image"] = rel

        total_matched += matched
        print(f"{slug:<10} {matched:>3}/{len(doc['letters']):<3} "
              f"from {', '.join(c for c, _ in sources)}")
        if not dry:
            with open(path, "w", encoding="utf-8") as fh:
                json.dump(doc, fh, ensure_ascii=False, indent=2)
                fh.write("\n")

    print(f"\nmatched {total_matched} letters")
    if dry:
        print("(dry run, nothing written)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
