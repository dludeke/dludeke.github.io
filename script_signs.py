# -*- coding: utf-8 -*-
"""Signs the alphabets leave out, found by auditing each script against
what its language actually writes.

Every entry is (glyph, alt, name, [ipa], (word, gloss, emoji), word_ipa,
note) — the same shape the Thai and Hangul tables in script_content use,
so generate_scripts can spread them into the per-field tables without
knowing which script they came from.

Two kinds of gap turned up. Most were a blank tile: a letter simply not
listed, so a word containing it could not be broken down. The worse kind
was a sign that resolved to a *different* letter and read as that one —
Ge'ez ጧ showing as ṭä, Farsi آ as Alef, pinyin ü as U — which teaches
something false rather than nothing.
"""

# --- Korean final clusters --------------------------------------------
# Only one of the pair is heard when the syllable ends there. They are in
# very ordinary words: 값, 닭, 읽다, 없다, 앉다.
HANGUL_FINALS = [
 ("ㄳ", "", "gieok-siot", ["k̚"], ("몫", "share", "\U0001f9fe"), "mok̚",
  "A final cluster. Only one of the two letters is pronounced when the syllable "
  "ends there; the other is heard again when a vowel follows."),
 ("ㄵ", "", "nieun-jieut", ["n"], ("앉다", "to sit", "\U0001fa91"), "an.t͈a", None),
 ("ㄶ", "", "nieun-hieuh", ["n"], ("많다", "to be many", "➕"), "man.tʰa", None),
 ("ㄺ", "", "rieul-gieok", ["k̚"], ("닭", "chicken", "\U0001f414"), "tak̚", None),
 ("ㄻ", "", "rieul-mieum", ["m"], ("삶", "life", "\U0001f331"), "sam", None),
 ("ㄼ", "", "rieul-bieup", ["l"], ("넓다", "to be wide", "↔️"), "nʌl.t͈a", None),
 ("ㄽ", "", "rieul-siot", ["l"], None, None,
  "Rare: it reaches only a couple of words."),
 ("ㄾ", "", "rieul-tieut", ["l"], ("핥다", "to lick", "\U0001f445"), "hal.t͈a", None),
 ("ㄿ", "", "rieul-pieup", ["p̚"], ("읊다", "to recite", "\U0001f4dc"), "ɯp̚.t͈a", None),
 ("ㅀ", "", "rieul-hieuh", ["l"], ("싫다", "to dislike", "\U0001f645"), "sil.tʰa", None),
 ("ㅄ", "", "bieup-siot", ["p̚"], ("값", "price", "\U0001f4b0"), "kap̚", None),
]

# --- Hebrew vowel points ----------------------------------------------
HEBREW_NIQQUD = [
 ("ַ", "", "patach", ["a"], ("בַיִת", "house", "\U0001f3e0"), "ba.jit",
  "Ordinary Hebrew is written with consonants alone and leaves the points out. "
  "They are taught first, then dropped, and kept in dictionaries, poetry, "
  "children's books and the Bible."),
 ("ָ", "", "kamatz", ["a", "o"], ("יָד", "hand", "✋"), "jad",
  "Usually a, but o in a closed unstressed syllable."),
 ("ֵ", "", "tzere", ["e"], ("סֵפֶר", "book", "\U0001f4d6"), "se.feʁ", None),
 ("ֶ", "", "segol", ["e"], ("אֶרֶץ", "land", "\U0001f30d"), "e.ʁets", None),
 ("ִ", "", "hiriq", ["i"], ("עִיר", "city", "\U0001f3d9️"), "iʁ", None),
 ("ֹ", "", "holam", ["o"], ("יוֹם", "day", "\U0001f4c5"), "jom", None),
 ("ֻ", "", "kubutz", ["u"], ("שֻׁלְחָן", "table", "\U0001fa91"), "ʃul.ħan", None),
 ("ְ", "", "sheva", ["ə"], ("בְרָכָה", "blessing", "\U0001f64f"), "bə.ʁa.хa",
  "A very short e, or no vowel at all, depending on where it falls."),
 ("ֱ", "", "hataf segol", ["e"], ("אֱמֶת", "truth", "✔️"), "e.met",
  "The three hataf vowels are shortened forms, used under the guttural letters."),
 ("ֲ", "", "hataf patach", ["a"], ("אֲנִי", "I", "\U0001f64b"), "a.ni", None),
 ("ֳ", "", "hataf kamatz", ["o"], ("צָהֳרַיִם", "noon", "\U0001f55b"), "tso.ho.ʁa.jim", None),
 ("ּ", "", "dagesh", [], ("דָּג", "fish", "\U0001f41f"), "dag",
  "Hardens ב כ פ to b, k and p, and doubles the other letters."),
 ("ׁ", "", "shin dot", ["ʃ"], ("שֶׁמֶשׁ", "sun", "☀️"), "ʃe.meʃ",
  "The dot tells ש apart: to the right for sh, to the left for s."),
 ("ׂ", "", "sin dot", ["s"], ("שָׂדֶה", "field", "\U0001f33e"), "sa.de", None),
]

# --- Persian hamza carriers and vowel marks ---------------------------
# Farsi had none of these although Arabic now does, and the four carriers
# were resolving to plain Alef and Vav.
FARSI_FORMS = [
 ("آ", "", "alef-e madd", ["ɒː"], ("آینه", "mirror", "\U0001fa9e"), "ɒːj.ne",
  "Alef carrying madda: a long a at the start of a word."),
 ("ء", "", "hamze", ["ʔ"], ("جزء", "part", "\U0001f9e9"), "dʒozʔ", None),
 ("أ", "", "alef with hamze above", ["ʔa"], ("تأثیر", "effect", "✨"), "tæʔ.siːر",
  "Mostly in words taken from Arabic."),
 ("إ", "", "alef with hamze below", ["ʔe"], None, None, None),
 ("ؤ", "", "vav-e hamze", ["ʔ"], ("سؤال", "question", "❓"), "soʔɒːl", None),
 ("ئ", "", "ye-ye hamze", ["ʔ"], ("مسئله", "problem", "❗"), "mæsʔæ.le", None),
 ("ة", "", "te-ye gerd", ["t", "e"], None, None,
  "Only in words taken from Arabic; Persian usually writes ه instead."),
 ("ۀ", "", "he-ye havvaz", ["je"], ("خانۀ", "house of", "\U0001f3e0"), "xɒː.ne.je",
  "Marks the ezafe, the linker that joins a noun to what follows it."),
]

FARSI_HARAKAT = [
 ("َ", "", "zebar", ["æ"], ("سَگ", "dog", "\U0001f415"), "sæg",
  "Persian leaves the three short vowels out of everyday writing, as Arabic does."),
 ("ِ", "", "zir", ["e"], ("دِل", "heart", "❤️"), "del", None),
 ("ُ", "", "pish", ["o"], ("گُل", "flower", "\U0001f337"), "gol", None),
 ("ّ", "", "tashdid", [], ("اَوّل", "first", "\U0001f947"), "æv.væl",
  "Doubles the letter it sits on."),
 ("ْ", "", "sokun", [], ("مِنْ", "from", "➡️"), "men",
  "Marks a consonant with no vowel after it."),
 ("ً", "", "tanvin", ["æn"], ("لطفاً", "please", "\U0001f64f"), "lot.fæn",
  "Only on adverbs borrowed from Arabic."),
]

# --- Devanagari vowels for English loanwords --------------------------
DEVANAGARI_CANDRA = [
 ("ॉ", "", "candra o matra", ["ɒ"], ("डॉक्टर", "doctor", "\U0001f468‍⚕️"), "ɖɒkʈəɾ",
  "Added to write the English o of doctor and college, a sound Hindi "
  "otherwise has no letter for."),
 ("ऑ", "", "candra O", ["ɒ"], ("ऑफ़िस", "office", "\U0001f3e2"), "ɒfis", None),
 ("ॅ", "", "candra e matra", ["æ"], None, None,
  "Chiefly Marathi; Hindi usually writes the English a of bank with ै."),
 ("ऍ", "", "candra E", ["æ"], None, None, None),
 ("ऽ", "", "avagraha", [], None, None,
  "Marks an a that has dropped out, almost only in Sanskrit."),
]

# --- Thai marks that are not vowels -----------------------------------
THAI_SIGNS = [
 ("ๆ", "", "mai yamok", [], ("เด็กๆ", "children", "\U0001f9d2"), "dèk.dèk",
  "Repeat the word before it. Very common in ordinary writing."),
 ("ฯ", "", "paiyannoi", [], ("กรุงเทพฯ", "Bangkok", "\U0001f3d9️"), "kruŋ.tʰeːp",
  "Shortens a long name, the way a full stop does in an abbreviation."),
 ("ํ", "", "nikhahit", ["ŋ"], None, None,
  "A nasal, in Pali and Sanskrit. ํ over a consonant with า after it is what ำ is built from."),
 ("ฺ", "", "phinthu", [], None, None,
  "Silences the consonant beneath it, when Pali is written in Thai letters."),
 ("ฦ", "", "lu", ["lɯ"], None, None,
  "The l to ฤ's r, taken from Sanskrit. Obsolete; it no longer appears in modern Thai."),
]

# --- Pinyin: the tones, and the vowel ü --------------------------------
# ü was resolving to U, which loses the difference between lǜ and lù.
PINYIN_TONES = [
 ("ü", "Ü", "u umlaut", ["y"], ("绿 (lǜ)", "green", "\U0001f49a"), "ly˥˩",
  "A separate vowel, not u. It is written plain after j, q, x and y, where "
  "no u can follow anyway."),
 ("̄", "", "first tone", ["˥˥"], ("妈 (mā)", "mother", "\U0001f469"), "ma˥˥",
  "The four marks sit over the main vowel and are part of the spelling, not "
  "an optional accent: mā, má, mǎ and mà are four words."),
 ("́", "", "second tone", ["˧˥"], ("麻 (má)", "hemp", "\U0001fab4"), "ma˧˥", None),
 ("̌", "", "third tone", ["˨˩˧"], ("马 (mǎ)", "horse", "\U0001f40e"), "ma˨˩˧", None),
 ("̀", "", "fourth tone", ["˥˩"], ("骂 (mà)", "to scold", "\U0001f5e3️"), "ma˥˩", None),
]

# --- Arabic: two marks held over from classical spelling ---------------
ARABIC_CLASSICAL = [
 ("ٰ", "", "dagger alef", ["aː"], ("هٰذا", "this", "\U0001f449"), "haː.ðaː",
  "A long a written as a stroke rather than a letter, kept in a handful of "
  "very old spellings such as هٰذا and الله."),
 ("ٱ", "", "alef wasla", ["ː"], None, None,
  "An alef that is not pronounced when a word runs on from the one before. "
  "Written mostly in the Quran."),
]


def geez_labialised():
    """The ʷä form of a non-velar consonant: ṭʷ in ጧት, bʷ in ቧንቧ.

    One syllable each rather than a five-order series, and derived from
    the Unicode names so the codepoints cannot be mistyped. Before these
    were listed they resolved to the plain consonant and read as it —
    ጧ as ṭä — because the base lookup rounds down to the nearest block
    of eight and lands on its neighbour.
    """
    import unicodedata
    # Unicode name prefix -> (romanisation, IPA) of the plain consonant
    series = [("L", "l", "l"), ("HH", "ḥ", "h"), ("M", "m", "m"),
              ("SZ", "ś", "s"), ("R", "r", "r"), ("S", "s", "s"),
              ("SH", "š", "ʃ"), ("B", "b", "b"), ("V", "v", "v"),
              ("T", "t", "t"), ("C", "č", "t͡ʃ"),
              ("N", "n", "n"), ("NY", "ñ", "ɲ"),
              ("Z", "z", "z"), ("ZH", "ž", "ʒ"),
              ("D", "d", "d"), ("DD", "ḍ", "d"),
              ("J", "ǵ", "d͡ʒ"), ("GG", "ġ", "ɡ"),
              ("TH", "ṭ", "tʼ"), ("CH", "čʼ", "t͡ʃʼ"),
              ("PH", "ṗ", "pʼ"), ("TS", "ṣ", "t͡sʼ"),
              ("F", "f", "f"), ("P", "p", "p")]
    words = {
     "ጧ": ("ጧት", "morning", "\U0001f305"),        # ጧት
     "ቧ": ("ቧንቧ", "pipe", "\U0001f6b0"),     # ቧንቧ
     "ፏ": ("ፏፏቴ", "waterfall", "\U0001f30a"),  # ፏፏቴ
    }
    word_ipa = {"ጧ": "tʼʷat", "ቧ": "bʷan.bʷa",
                "ፏ": "fʷa.fʷa.te"}
    out = []
    for i, (prefix, rom, ipa) in enumerate(series):
        try:
            g = unicodedata.lookup("ETHIOPIC SYLLABLE %sWA" % prefix)
        except KeyError:
            continue
        note = None
        if i == 0:
            note = ("A consonant said with rounded lips. Unlike the labiovelars "
                    "these have one form only, not a series of five.")
        out.append((g, "", rom + "ʷä", [ipa + "ʷə"],
                    words.get(g), word_ipa.get(g), note))
    return out
