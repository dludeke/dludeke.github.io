#!/usr/bin/env python3
"""Fold a sign-timestamps.json export from signs.html into the data files.

The page captures start/end timestamps into browser localStorage while you
review, because a static site cannot write its own JSON. Exporting produces a
file shaped like:

    {"mx": {"start_seconds": 31.2, "end_seconds": 37.0}, ...}

Run this with that file to make the timestamps permanent:

    python3 apply_timestamps.py ~/Downloads/sign-timestamps.json

Values written here survive regeneration: generate_country_signs.py reads
hand-edited fields back off the existing files and carries them forward.
"""
import glob
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(HERE, "assets", "data")
FIELDS = ("start_seconds", "end_seconds")


def main(argv):
    if len(argv) != 2:
        print(__doc__)
        return 2
    with open(argv[1], encoding="utf-8") as fh:
        export = json.load(fh)
    if not isinstance(export, dict):
        print("Expected a JSON object keyed by ISO2 country code.")
        return 1

    applied, unknown = 0, set(export)
    for path in sorted(glob.glob(os.path.join(DATA_DIR, "country-signs-*.json"))):
        with open(path, encoding="utf-8") as fh:
            doc = json.load(fh)
        touched = False
        for c in doc["countries"]:
            patch = export.get(c["iso2"])
            if not patch:
                continue
            unknown.discard(c["iso2"])
            for field in FIELDS:
                val = patch.get(field)
                if val is None:
                    continue
                if not isinstance(val, (int, float)) or val < 0:
                    print(f"  skipping {c['name_en']}: bad {field} {val!r}")
                    continue
                if c["sign"].get(field) != val:
                    c["sign"][field] = val
                    touched = True
            applied += 1
        if touched:
            with open(path, "w", encoding="utf-8") as fh:
                json.dump(doc, fh, ensure_ascii=False, indent=2)
                fh.write("\n")
            print(f"updated {os.path.basename(path)}")

    print(f"applied timestamps for {applied} countries")
    if unknown:
        print(f"unrecognised ISO2 codes ignored: {', '.join(sorted(unknown))}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
