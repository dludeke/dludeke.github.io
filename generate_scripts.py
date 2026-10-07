#!/usr/bin/env python3
"""Generate the per-script reference data files for reference.html / script.html.

Each script gets assets/data/scripts/<slug>.json holding its letters. Letters
are enumerated here because that part is fixed and known; the per-letter
content to be filled in later (IPA values, handshape image, example noun and
its picture) is emitted empty.

Hand-edited fields are read back off any existing file and carried forward, so
re-running this never discards work. Same contract as
generate_country_signs.py.
"""
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from script_content import (WORDS, IPA as IPA_TABLE, WORD_IPA, NOTES,
                            LINKS, GLOSSARY, IPA_LINKS,
                            GEEZ_ORDERS, GEEZ_CONSONANTS, GEEZ_EXTRA_WORDS,
                            GEEZ_RARE_ROWS, THAI_VOWELS, THAI_TONES,
                            THAI_MODIFIERS, thai_display)

HERE = os.path.dirname(os.path.abspath(__file__))
OUT_DIR = os.path.join(HERE, "assets", "data", "scripts")

# Per-letter fields a human fills in; preserved across regeneration.
HAND_EDITED = ("ipa", "handshape", "example_word", "example_gloss",
               "example_image", "example_emoji", "example_ipa", "notes",
               "romanization", "signwriting", "signwriting_image",
               "signwriting_variants", "handshape_variants")

# slug, display name, representative letter, sign language, languages, note
SCRIPTS = [
    ("greek", "Greek", "α", "GSL", "Ελληνική νοηματική γλώσσα",
     ["Greek"], "Alphabet: 24 letters, each with an upper and lower case."),
    ("latin", "Latin", "A", "ASL, BSL", "American / British Sign Language",
     ["English", "and most of Europe"], "Alphabet: 26 letters. ASL and BSL are different manual alphabets, not variants of one: ASL fingerspells with one hand, BSL with two. Use the toggle to switch."),
    ("hebrew", "Hebrew", "א", "ISL", "שפת סימנים ישראלית",
     ["Hebrew", "Ladino", "Yiddish"], "Abjad: 22 consonants, written right to left. Five have final forms."),
    ("geez", "Ge'ez", "ሀ", "EthSL", "የኢትዮጵያ ምልክት ቋንቋ",
     ["Amharic", "Tigrinya", "Ge'ez"], "Abugida: 33 consonants, each written in seven vowel orders, giving 231 syllables. Rows are consonants, columns are vowels; the shape of the base changes slightly in each order rather than taking a separate vowel sign."),
    ("arabic", "Arabic", "ا", "ArSL", "لغة الإشارة العربية",
     ["Arabic"], "Abjad: 28 letters, written right to left, each with initial, medial, final and isolated forms."),
    ("devanagari", "Devanagari", "अ", "NSL", "नेपाली सांकेतिक भाषा",
     ["Hindi", "Sanskrit", "Nepali"], "Abugida: vowels, then consonants carrying an inherent 'a'."),
    ("farsi", "Farsi", "پ", "ZEI", "زبان اشاره ایرانی",
     ["Persian / Farsi"], "Perso-Arabic: the 28 Arabic letters plus four Persian additions. Shown by پ (pe), one of those four, since ا is shared with Arabic."),
    ("kana", "Kana", "あ", "JSL", "日本手話",
     ["Japanese"], "Syllabary: 46 basic hiragana, each with a katakana counterpart."),
    ("cyrillic", "Cyrillic", "Б", "RSL", "Русский жестовый язык",
     ["Russian", "Bulgarian", "Serbian", "and others"], "Alphabet: 33 letters in the Russian inventory; other languages add or drop a few."),
    ("thai", "Thai", "ก", "ThSL", "ภาษามือไทย",
     ["Thai"], "Abugida: 44 consonants, listed here with their acrophonic names."),
    ("hangul", "Hangul", "ㄱ", "KSL", "한국수어",
     ["Korean"], "Featural alphabet: 14 basic consonants and 10 basic vowels, composed into syllable blocks."),
    ("pinyin", "Pinyin", "ā", "CSL", "中国手语",
     ["Mandarin Chinese"], "Romanisation rather than a script. The CSL fingerspelling scheme of 1963 has 30 handshapes: the 26 Latin letters plus the digraphs ZH, CH, SH and NG."),
]

# letters: (glyph, lowercase_or_alt, name)  -- alt is "" when there is no pair
# The Thai marks carry their content in one table rather than five, since
# they were added together; spread it into the per-field tables.
for _g, _n, _i, _w, _wi, _no in THAI_VOWELS + THAI_TONES + THAI_MODIFIERS:
    if _i:
        IPA_TABLE.setdefault("thai", {})[_g] = _i
    WORDS.setdefault("thai", {})[_g] = _w
    WORD_IPA.setdefault("thai", {})[_g] = _wi
    if _no:
        NOTES.setdefault("thai", {})[_g] = _no

SECTIONS = {"thai": ([("Consonants", None)]
                     + [("Vowel signs", g) for g, *_ in THAI_VOWELS]
                     + [("Tone marks", g) for g, *_ in THAI_TONES]
                     + [("Modifiers", g) for g, *_ in THAI_MODIFIERS])}
SECTION_OF = {slug: {g: name for name, g in rows if g}
              for slug, rows in SECTIONS.items()}
SECTION_ORDER = {slug: list(dict.fromkeys(n for n, _ in rows))
                 for slug, rows in SECTIONS.items()}

def geez_syllabary():
    """All 231 Ethiopic syllables as (glyph, alt, name) triples, row by row.

    Each consonant occupies eight consecutive codepoints, the first seven
    being the vowel orders, so every cell is derivable from its base.
    """
    out = []
    for base, rom, _ipa in GEEZ_CONSONANTS:
        for i, (vrom, _v) in enumerate(GEEZ_ORDERS):
            out.append((chr(ord(base) + i), "", f"{rom}{vrom}"))
    return out


LETTERS = {
"greek": [("Α","α","Alpha"),("Β","β","Beta"),("Γ","γ","Gamma"),("Δ","δ","Delta"),
 ("Ε","ε","Epsilon"),("Ζ","ζ","Zeta"),("Η","η","Eta"),("Θ","θ","Theta"),
 ("Ι","ι","Iota"),("Κ","κ","Kappa"),("Λ","λ","Lambda"),("Μ","μ","Mu"),
 ("Ν","ν","Nu"),("Ξ","ξ","Xi"),("Ο","ο","Omicron"),("Π","π","Pi"),
 ("Ρ","ρ","Rho"),("Σ","σ","Sigma"),("Τ","τ","Tau"),("Υ","υ","Upsilon"),
 ("Φ","φ","Phi"),("Χ","χ","Chi"),("Ψ","ψ","Psi"),("Ω","ω","Omega")],

"latin": [(chr(65+i), chr(97+i), chr(65+i)) for i in range(26)],

"hebrew": [("א","","Alef"),("ב","","Bet"),("ג","","Gimel"),("ד","","Dalet"),("ה","","He"),
 ("ו","","Vav"),("ז","","Zayin"),("ח","","Het"),("ט","","Tet"),("י","","Yod"),
 ("כ","ך","Kaf"),("ל","","Lamed"),("מ","ם","Mem"),("נ","ן","Nun"),
 ("ס","","Samekh"),("ע","","Ayin"),("פ","ף","Pe"),("צ","ץ","Tsadi"),
 ("ק","","Qof"),("ר","","Resh"),("ש","","Shin"),("ת","","Tav")],

"geez": geez_syllabary(),

"arabic": [("ا","","Alif"),("ب","","Ba"),("ت","","Ta"),("ث","","Tha"),("ج","","Jim"),
 ("ح","","Ha"),("خ","","Kha"),("د","","Dal"),("ذ","","Dhal"),("ر","","Ra"),
 ("ز","","Zay"),("س","","Sin"),("ش","","Shin"),("ص","","Sad"),("ض","","Dad"),
 ("ط","","Ta (emphatic)"),("ظ","","Za (emphatic)"),("ع","","Ayn"),("غ","","Ghayn"),
 ("ف","","Fa"),("ق","","Qaf"),("ك","","Kaf"),("ل","","Lam"),("م","","Mim"),
 ("ن","","Nun"),("ه","","Ha"),("و","","Waw"),("ي","","Ya")],

"farsi": [("ا","","Alef"),("ب","","Be"),("پ","","Pe"),("ت","","Te"),("ث","","Se"),
 ("ج","","Jim"),("چ","","Che"),("ح","","He"),("خ","","Khe"),("د","","Dal"),
 ("ذ","","Zal"),("ر","","Re"),("ز","","Ze"),("ژ","","Zhe"),("س","","Sin"),
 ("ش","","Shin"),("ص","","Sad"),("ض","","Zad"),("ط","","Ta"),("ظ","","Za"),
 ("ع","","Eyn"),("غ","","Gheyn"),("ف","","Fe"),("ق","","Ghaf"),("ک","","Kaf"),
 ("گ","","Gaf"),("ل","","Lam"),("م","","Mim"),("ن","","Nun"),("و","","Vav"),
 ("ه","","He"),("ی","","Ye")],

"devanagari": [("अ","","a"),("आ","","ā"),("इ","","i"),("ई","","ī"),("उ","","u"),
 ("ऊ","","ū"),("ऋ","","ṛ"),("ए","","e"),("ऐ","","ai"),("ओ","","o"),("औ","","au"),
 ("क","","ka"),("ख","","kha"),("ग","","ga"),("घ","","gha"),("ङ","","ṅa"),
 ("च","","ca"),("छ","","cha"),("ज","","ja"),("झ","","jha"),("ञ","","ña"),
 ("ट","","ṭa"),("ठ","","ṭha"),("ड","","ḍa"),("ढ","","ḍha"),("ण","","ṇa"),
 ("त","","ta"),("थ","","tha"),("द","","da"),("ध","","dha"),("न","","na"),
 ("प","","pa"),("फ","","pha"),("ब","","ba"),("भ","","bha"),("म","","ma"),
 ("य","","ya"),("र","","ra"),("ल","","la"),("व","","va"),
 ("श","","śa"),("ष","","ṣa"),("स","","sa"),("ह","","ha")],

"kana": [("あ","ア","a"),("い","イ","i"),("う","ウ","u"),("え","エ","e"),("お","オ","o"),
 ("か","カ","ka"),("き","キ","ki"),("く","ク","ku"),("け","ケ","ke"),("こ","コ","ko"),
 ("さ","サ","sa"),("し","シ","shi"),("す","ス","su"),("せ","セ","se"),("そ","ソ","so"),
 ("た","タ","ta"),("ち","チ","chi"),("つ","ツ","tsu"),("て","テ","te"),("と","ト","to"),
 ("な","ナ","na"),("に","ニ","ni"),("ぬ","ヌ","nu"),("ね","ネ","ne"),("の","ノ","no"),
 ("は","ハ","ha"),("ひ","ヒ","hi"),("ふ","フ","fu"),("へ","ヘ","he"),("ほ","ホ","ho"),
 ("ま","マ","ma"),("み","ミ","mi"),("む","ム","mu"),("め","メ","me"),("も","モ","mo"),
 ("や","ヤ","ya"),("ゆ","ユ","yu"),("よ","ヨ","yo"),
 ("ら","ラ","ra"),("り","リ","ri"),("る","ル","ru"),("れ","レ","re"),("ろ","ロ","ro"),
 ("わ","ワ","wa"),("を","ヲ","wo"),("ん","ン","n")],

"cyrillic": [("А","а","A"),("Б","б","Be"),("В","в","Ve"),("Г","г","Ge"),
 ("Д","д","De"),("Е","е","Ye"),("Ё","ё","Yo"),("Ж","ж","Zhe"),
 ("З","з","Ze"),("И","и","I"),("Й","й","Short I"),("К","к","Ka"),
 ("Л","л","El"),("М","м","Em"),("Н","н","En"),("О","о","O"),
 ("П","п","Pe"),("Р","р","Er"),("С","с","Es"),("Т","т","Te"),
 ("У","у","U"),("Ф","ф","Ef"),("Х","х","Kha"),("Ц","ц","Tse"),
 ("Ч","ч","Che"),("Ш","ш","Sha"),("Щ","щ","Shcha"),("Ъ","ъ","Hard sign"),
 ("Ы","ы","Yery"),("Ь","ь","Soft sign"),("Э","э","E"),("Ю","ю","Yu"),
 ("Я","я","Ya")],

"thai": [("ก","","ko kai"),("ข","","kho khai"),("ฃ","","kho khuat"),("ค","","kho khwai"),
 ("ฅ","","kho khon"),("ฆ","","kho rakhang"),("ง","","ngo ngu"),("จ","","cho chan"),
 ("ฉ","","cho ching"),("ช","","cho chang"),("ซ","","so so"),("ฌ","","cho choe"),
 ("ญ","","yo ying"),("ฎ","","do chada"),("ฏ","","to patak"),("ฐ","","tho than"),
 ("ฑ","","tho nangmontho"),("ฒ","","tho phuthao"),("ณ","","no nen"),("ด","","do dek"),
 ("ต","","to tao"),("ถ","","tho thung"),("ท","","tho thahan"),("ธ","","tho thong"),
 ("น","","no nu"),("บ","","bo baimai"),("ป","","po pla"),("ผ","","pho phueng"),
 ("ฝ","","fo fa"),("พ","","pho phan"),("ฟ","","fo fan"),("ภ","","pho samphao"),
 ("ม","","mo ma"),("ย","","yo yak"),("ร","","ro ruea"),("ล","","lo ling"),
 ("ว","","wo waen"),("ศ","","so sala"),("ษ","","so ruesi"),("ส","","so suea"),
 ("ห","","ho hip"),("ฬ","","lo chula"),("อ","","o ang"),("ฮ","","ho nokhuk")]
 + [(g, "", n) for g, n, _i, _w, _wi, _no in THAI_VOWELS + THAI_TONES + THAI_MODIFIERS],

"hangul": [("ㄱ","","giyeok"),("ㄴ","","nieun"),("ㄷ","","digeut"),("ㄹ","","rieul"),
 ("ㅁ","","mieum"),("ㅂ","","bieup"),("ㅅ","","siot"),("ㅇ","","ieung"),
 ("ㅈ","","jieut"),("ㅊ","","chieut"),("ㅋ","","kieuk"),("ㅌ","","tieut"),
 ("ㅍ","","pieup"),("ㅎ","","hieut"),
 ("ㅏ","","a"),("ㅑ","","ya"),("ㅓ","","eo"),("ㅕ","","yeo"),("ㅗ","","o"),
 ("ㅛ","","yo"),("ㅜ","","u"),("ㅠ","","yu"),("ㅡ","","eu"),("ㅣ","","i")],

"pinyin": [(chr(65+i), chr(97+i), chr(65+i)) for i in range(26)]
          + [("ZH", "zh", "ZH"), ("CH", "ch", "CH"),
             ("SH", "sh", "SH"), ("NG", "ng", "NG")],
}


# Per-letter content, filled in script by script. IPA values are Modern Greek;
# where a letter's value shifts before front vowels both allophones are listed.
# example_emoji stands in until a real picture is dropped into example_image.
CONTENT = {
"greek": {
 "\u0391": ("a", ["a"], "\u03b1\u03c3\u03c4\u03ad\u03c1\u03b9", "star", "\u2b50"),
 "\u0392": ("v", ["v"], "\u03b2\u03b9\u03b2\u03bb\u03af\u03bf", "book", "\U0001f4d6"),
 "\u0393": ("g", ["\u0263", "\u029d"], "\u03b3\u03ac\u03c4\u03b1", "cat", "\U0001f408"),
 "\u0394": ("d", ["\u00f0"], "\u03b4\u03ad\u03bd\u03c4\u03c1\u03bf", "tree", "\U0001f333"),
 "\u0395": ("e", ["e"], "\u03b5\u03bb\u03ad\u03c6\u03b1\u03bd\u03c4\u03b1\u03c2", "elephant", "\U0001f418"),
 "\u0396": ("z", ["z"], "\u03b6\u03ad\u03b2\u03c1\u03b1", "zebra", "\U0001f993"),
 "\u0397": ("i", ["i"], "\u03ae\u03bb\u03b9\u03bf\u03c2", "sun", "\u2600\ufe0f"),
 "\u0398": ("th", ["\u03b8"], "\u03b8\u03ad\u03b1\u03c4\u03c1\u03bf", "theatre", "\U0001f3ad"),
 "\u0399": ("i", ["i"], "\u03b9\u03c0\u03c0\u03bf\u03c0\u03cc\u03c4\u03b1\u03bc\u03bf\u03c2", "hippopotamus", "\U0001f99b"),
 "\u039a": ("k", ["k", "c"], "\u03ba\u03b1\u03c1\u03b4\u03b9\u03ac", "heart", "\u2764\ufe0f"),
 "\u039b": ("l", ["l", "\u028e"], "\u03bb\u03bf\u03c5\u03bb\u03bf\u03cd\u03b4\u03b9", "flower", "\U0001f33c"),
 "\u039c": ("m", ["m"], "\u03bc\u03ae\u03bb\u03bf", "apple", "\U0001f34e"),
 "\u039d": ("n", ["n", "\u0272"], "\u03bd\u03b5\u03c1\u03cc", "water", "\U0001f4a7"),
 "\u039e": ("x", ["ks"], "\u03be\u03cd\u03bb\u03bf", "wood", "\U0001fab5"),
 "\u039f": ("o", ["o"], "\u03bf\u03c5\u03c1\u03b1\u03bd\u03cc\u03c2", "sky", "\U0001f324\ufe0f"),
 "\u03a0": ("p", ["p"], "\u03c0\u03bf\u03c5\u03bb\u03af", "bird", "\U0001f426"),
 "\u03a1": ("r", ["r"], "\u03c1\u03bf\u03bb\u03cc\u03b9", "clock", "\U0001f570\ufe0f"),
 "\u03a3": ("s", ["s", "z"], "\u03c3\u03c0\u03af\u03c4\u03b9", "house", "\U0001f3e0"),
 "\u03a4": ("t", ["t"], "\u03c4\u03c1\u03ad\u03bd\u03bf", "train", "\U0001f686"),
 "\u03a5": ("y", ["i"], "\u03c5\u03c0\u03bf\u03bb\u03bf\u03b3\u03b9\u03c3\u03c4\u03ae\u03c2", "computer", "\U0001f4bb"),
 "\u03a6": ("f", ["f"], "\u03c6\u03b5\u03b3\u03b3\u03ac\u03c1\u03b9", "moon", "\U0001f319"),
 "\u03a7": ("ch", ["x", "\u00e7"], "\u03c7\u03ad\u03c1\u03b9", "hand", "\u270b"),
 "\u03a8": ("ps", ["ps"], "\u03c8\u03ac\u03c1\u03b9", "fish", "\U0001f41f"),
 "\u03a9": ("o", ["o"], "\u03c9\u03ba\u03b5\u03b1\u03bd\u03cc\u03c2", "ocean", "\U0001f30a"),
},
}


# Source credits for charts that require them. The BSL chart is share-alike,
# so the per-letter crops derived from it carry the same licence.
# Scripts whose letters have a real two-dimensional shape are laid out as a
# grid rather than reflowed. Kana is the gojuon table: five vowel columns, one
# consonant row each, with gaps where a syllable does not exist (yi, ye, wi,
# we). A null is a gap and renders as empty space, keeping the columns aligned
# so each column stays one vowel.
# Contextual forms that are the same letter: Greek writes a different sigma at
# the end of a word, and Hebrew has five final forms. The alt field already
# carries the Hebrew ones, so only the extras go here.
ALIASES = {
    "greek": {"Σ": ["ς"]},
}


# How the reference grid is grouped. Pinyin is a romanisation rather than an
# alphabet, but it behaves like one here: one letter, one sound, no inherent
# vowel. Kana is a syllabary rather than an abugida; they share a row because
# both write a consonant and vowel as a single unit.
FAMILY = {
    "greek": "Alphabet", "latin": "Alphabet", "cyrillic": "Alphabet",
    "pinyin": "Alphabet", "hangul": "Alphabet",
    "geez": "Abugida and syllabary", "devanagari": "Abugida and syllabary",
    "thai": "Abugida and syllabary", "kana": "Abugida and syllabary",
    "arabic": "Abjad", "hebrew": "Abjad", "farsi": "Abjad",
}

FAMILY_ORDER = ["Alphabet", "Abugida and syllabary", "Abjad"]

def geez_layout():
    rows, cells = [], geez_syllabary()
    for r in range(0, len(cells), 7):
        rows.append([c[0] for c in cells[r:r + 7]])
    return {"cols": 7,
            "col_labels": [f"{i+1}  {v[0]}" for i, v in enumerate(GEEZ_ORDERS)],
            "rows": rows}


LAYOUTS = {
    "geez": None,   # filled in below, since it is generated
    "kana": {
        "cols": 5,
        "col_labels": ["a", "i", "u", "e", "o"],
        "rows": [
            ["あ", "い", "う", "え", "お"],
            ["か", "き", "く", "け", "こ"],
            ["さ", "し", "す", "せ", "そ"],
            ["た", "ち", "つ", "て", "と"],
            ["な", "に", "ぬ", "ね", "の"],
            ["は", "ひ", "ふ", "へ", "ほ"],
            ["ま", "み", "む", "め", "も"],
            ["や", None, "ゆ", None, "よ"],
            ["ら", "り", "る", "れ", "ろ"],
            ["わ", None, None, None, "を"],
            ["ん", None, None, None, None],
        ],
    },
}


LAYOUTS["geez"] = geez_layout()

ATTRIBUTION = {
    "latin": [
        {"what": "BSL handshapes",
         "credit": "User:Cowplopmorris, Wikimedia Commons",
         "licence": "CC BY-SA 3.0",
         "url": "https://commons.wikimedia.org/wiki/File:British_Sign_Language_chart.png",
         "note": "Cropped into one image per letter; as a derivative of a "
                 "share-alike work these crops carry the same licence."},
        {"what": "ASL handshapes",
         "credit": "User:Ds13, Wikimedia Commons (Gallaudet font)",
         "licence": "Public domain",
         "url": "https://commons.wikimedia.org/wiki/File:Asl_alphabet_gallaudet.svg",
         "note": "Cropped into one image per letter."},
    ],
}


def load_hand_edits(path):
    """Map letter glyph -> hand-edited fields from an existing file."""
    if not os.path.exists(path):
        return {}
    try:
        with open(path, encoding="utf-8") as fh:
            doc = json.load(fh)
    except (ValueError, OSError):
        return {}
    kept = {}
    for L in doc.get("letters", []):
        edits = {k: L[k] for k in HAND_EDITED if L.get(k) not in (None, "", [])}
        if edits:
            kept[L.get("glyph")] = edits
    return kept


def existing_handshapes(slug):
    """Reuse manual-alphabet images the repo already holds.

    The practice area sliced the Greek Sign Language chart into one image per
    letter, so Greek starts with its handshape column already filled in.
    """
    if slug != "greek":
        return {}
    path = os.path.join(HERE, "gsl-handshapes-mapping.json")
    if not os.path.exists(path):
        return {}
    with open(path, encoding="utf-8") as fh:
        data = json.load(fh)
    out = {}
    for v in (data.get("handshapes") or {}).values():
        if isinstance(v, dict) and v.get("image") and v.get("greek"):
            out[v["greek"]] = v["image"]
    return out


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    index, preserved_total = [], 0

    for slug, name, rep, sl_abbr, sl_name, languages, note in SCRIPTS:
        path = os.path.join(OUT_DIR, f"{slug}.json")
        preserved = load_hand_edits(path)
        shapes = existing_handshapes(slug)

        letters = []
        for glyph, alt, label in LETTERS[slug]:
            entry = {
                "glyph": glyph,
                "alt": alt,            # lowercase, katakana, or final form
                "name": label,
                # a combining mark drawn on a dotted circle, so the card shows
                # where it sits rather than floating it on nothing
                "display_glyph": thai_display(glyph) if slug == "thai" and
                                 glyph in SECTION_OF.get("thai", {}) else None,
                "section": SECTION_OF.get(slug, {}).get(glyph),
                "romanization": None,
                "ipa": [],             # possible IPA values, filled in later
                "handshape": None,     # path to the manual-alphabet image
                "example_word": None,  # noun in the language using this letter
                "example_gloss": None, # its meaning in English
                "example_image": None, # picture of that noun
                "example_emoji": None, # stand-in until a real picture exists
                # How the example word is pronounced, so it can be read
                # before its letters are known.
                "example_ipa": None,
                # Sutton SignWriting transcription of the handshape, as a
                # Formal SignWriting (FSW) string e.g. "S1f720". Rendered from
                # the Unicode block U+1D800-1DAAF.
                "signwriting": None,
                # Sliced SignWriting glyph, where a chart provides one.
                "signwriting_image": None,
                "aliases": ALIASES.get(slug, {}).get(glyph, []),
                "notes": "",
            }
            if slug == "geez":
                # The Ethiopic block has consonants this reference does not
                # list sitting between the ones it does, so the base cannot be
                # found by dividing the codepoint: look it up.
                cp = ord(glyph)
                found = None
                for b, rom_c, ipa_c in GEEZ_CONSONANTS:
                    off = cp - ord(b)
                    if 0 <= off < len(GEEZ_ORDERS):
                        found = (ipa_c, off)
                        break
                if found:
                    cons_ipa, oi = found
                    entry["ipa"] = [cons_ipa + GEEZ_ORDERS[oi][1]]
                    entry["romanization"] = label
                w = GEEZ_EXTRA_WORDS.get(glyph) or WORDS.get(slug, {}).get(glyph)
                if w:
                    word, gloss, emoji = w
                    entry.update({"example_word": word, "example_gloss": gloss,
                                  "example_emoji": emoji})
            # geez derives its own IPA per syllable; the old per-consonant
            # table wrote the transliteration ä where the vowel is really /ə/.
            ipa_vals = None if slug == "geez" else IPA_TABLE.get(slug, {}).get(glyph)
            if ipa_vals:
                entry["ipa"] = list(ipa_vals)
            # named letter_note, not note: the script's own note comes from
            # the loop header and was being clobbered by this
            letter_note = NOTES.get(slug, {}).get(glyph)
            if letter_note:
                entry["notes"] = letter_note
            # Say so on the row header where a consonant barely supplies
            # words, rather than leaving six blank cells unexplained.
            if slug == "geez" and not letter_note and glyph in GEEZ_RARE_ROWS:
                letter_note = GEEZ_RARE_ROWS[glyph]
                entry["notes"] = letter_note
            wi = WORD_IPA.get(slug, {}).get(glyph)
            if wi:
                entry["example_ipa"] = wi
            w = WORDS.get(slug, {}).get(glyph)
            if w:
                word, gloss, emoji = w
                entry.update({"example_word": word, "example_gloss": gloss,
                              "example_emoji": emoji})
            got = CONTENT.get(slug, {}).get(glyph)
            if got:
                rom, ipa, word, gloss, emoji = got
                entry.update({"romanization": rom, "ipa": list(ipa),
                              "example_word": word, "example_gloss": gloss,
                              "example_emoji": emoji})
            if shapes.get(glyph):
                entry["handshape"] = shapes[glyph]
            # Preserve only what the tables do not supply. Without this the
            # values just written would be read back as "hand edits" on the
            # next run and win over the tables, so editing script_content.py
            # would silently do nothing.
            from_tables = set()
            # Values the geez block derives are table output, not hand edits;
            # without this a bad earlier run would be preserved over the fix.
            if slug == "geez":
                from_tables.update(("ipa", "romanization"))
                if GEEZ_EXTRA_WORDS.get(glyph):
                    from_tables.update(("example_word", "example_gloss",
                                        "example_emoji"))
            if ipa_vals:
                from_tables.add("ipa")
            if w:
                from_tables.update(("example_word", "example_gloss", "example_emoji"))
            if wi:
                from_tables.add("example_ipa")
            if letter_note:
                from_tables.add("notes")
            if got:
                from_tables.update(("romanization", "ipa", "example_word",
                                    "example_gloss", "example_emoji"))
            keep = {k: v for k, v in preserved.get(glyph, {}).items()
                    if k not in from_tables}
            if keep:
                entry.update(keep)
                preserved_total += 1
            letters.append(entry)

        doc = {
            "slug": slug,
            "name": name,
            "representative": rep,
            "sign_language": {"abbr": sl_abbr, "name": sl_name},
            "languages": languages,
            "note": note,
            "status": "skeleton",
            "attribution": ATTRIBUTION.get(slug),
            "layout": LAYOUTS.get(slug),
            "sections": SECTION_ORDER.get(slug),
            "links": [{"label": l, "url": u} for l, u in LINKS.get(slug, [])],
            "glossary": GLOSSARY,
            "ipa_links": {k: v for k, v in IPA_LINKS.items() if v},
            "letters": letters,
        }
        with open(path, "w", encoding="utf-8") as fh:
            json.dump(doc, fh, ensure_ascii=False, indent=2)
            fh.write("\n")

        index.append({
            "slug": slug, "name": name, "representative": rep,
            "family": FAMILY.get(slug, "Other"),
            "sign_language": sl_abbr, "languages": languages,
            "letter_count": len(letters),
        })
        print(f"{name:<12} {len(letters):>3} letters -> {slug}.json")

    with open(os.path.join(OUT_DIR, "index.json"), "w", encoding="utf-8") as fh:
        json.dump({"families": FAMILY_ORDER, "scripts": index},
                  fh, ensure_ascii=False, indent=2)
        fh.write("\n")

    print(f"\n{len(index)} scripts, {sum(s['letter_count'] for s in index)} letters total")
    print(f"hand-edited letters preserved: {preserved_total}")


if __name__ == "__main__":
    sys.exit(main())
