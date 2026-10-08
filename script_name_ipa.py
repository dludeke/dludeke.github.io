# -*- coding: utf-8 -*-
"""How to say each letter's name.

A letter's name is often a word in the language, and knowing the letter's
sound does not tell you how to say its name: Korean ㄱ sounds /k/ but is
called 기역 /ki.jʌk̚/. This holds the reading of the name itself.

Only where it adds something. Where the name simply is the sound — a
Devanagari consonant called "ka", a kana called "shi", a Ge'ez syllable
whose name is its own romanisation — there is nothing to add and these
tables leave it out.

Keyed by the `name` the generator already gives each letter, not by
glyph, so a name shared by two letters is written once.
"""

# Modern Greek readings: the page gives the letters their modern values,
# so βῆτα is vita, not beta.
GREEK = {
 "alpha": "ˈalfa", "beta": "ˈvita", "gamma": "ˈɣama",
 "delta": "ˈðelta", "epsilon": "ˈepsilon", "zeta": "ˈzita",
 "eta": "ˈita", "theta": "ˈθita", "iota": "ˈjota",
 "kappa": "ˈkapa", "lambda": "ˈlamða", "mu": "mi", "nu": "ni",
 "xi": "ksi", "omicron": "ˈomikron", "pi": "pi", "rho": "ro",
 "sigma": "ˈsiɣma", "tau": "taf", "upsilon": "ˈipsilon",
 "phi": "fi", "chi": "çi", "psi": "psi", "omega": "oˈmeɣa",
}

LATIN = {
 "A": "eɪ", "B": "biː", "C": "siː", "D": "diː", "E": "iː",
 "F": "ɛf", "G": "d͡ʒiː", "H": "eɪt͡ʃ",
 "I": "aɪ", "J": "d͡ʒeɪ", "K": "keɪ", "L": "ɛl",
 "M": "ɛm", "N": "ɛn", "O": "oʊ", "P": "piː",
 "Q": "kjuː", "R": "ɑː", "S": "ɛs", "T": "tiː",
 "U": "juː", "V": "viː", "W": "ˈdʌbəljuː",
 "X": "ɛks", "Y": "waɪ", "Z": "zɛd",
}

CYRILLIC = {
 "A": "a", "Be": "b\u025b", "Ve": "v\u025b", "Ge": "\u0261\u025b", "De": "d\u025b",
 "Ye": "je", "Yo": "jo", "Zhe": "\u0292\u025b", "Ze": "z\u025b", "I": "i",
 "Short I": "i \u02c8krat\u0259k\u0259j\u0259", "Ka": "ka", "El": "\u025bl",
 "Em": "\u025bm", "En": "\u025bn", "O": "o", "Pe": "p\u025b", "Er": "\u025br",
 "Es": "\u025bs", "Te": "t\u025b", "U": "u", "Ef": "\u025bf", "Kha": "xa",
 "Tse": "t\u0361s\u025b", "Che": "t\u0361\u0255\u025b", "Sha": "\u0282a",
 "Shcha": "\u0255\u02d0a", "Hard sign": "\u02c8tv\u02b2\u0275rd\u0268j znak",
 "Yery": "\u0268\u02d0r\u0268", "Soft sign": "\u02c8m\u02b2\u00e6xk\u02b2\u026aj znak",
 "E": "\u025b", "Yu": "ju", "Ya": "ja",
}

HEBREW = {
 "Alef": "ˈalef", "Bet": "bet", "Gimel": "ˈɡimel", "Dalet": "ˈdalet",
 "He": "he", "Vav": "vav", "Zayin": "ˈzajin", "Het": "χet", "Tet": "tet",
 "Yod": "jod", "Kaf": "kaf", "Lamed": "ˈlamed", "Mem": "mem", "Nun": "nun",
 "Samekh": "ˈsameχ", "Ayin": "ˈajin", "Pe": "pe", "Tsadi": "ˈtsadi",
 "Qof": "kof", "Resh": "ʁeʃ", "Shin": "ʃin", "Tav": "tav",
 "patach": "paˈtaχ", "kamatz": "kaˈmats", "tzere": "tseˈʁe",
 "segol": "seˈɡol", "hiriq": "χiˈʁik", "holam": "χoˈlam",
 "kubutz": "kuˈbuts", "sheva": "ʃva", "hataf segol": "χaˈtaf seˈɡol",
 "hataf patach": "χaˈtaf paˈtaχ", "hataf kamatz": "χaˈtaf kaˈmats",
 "dagesh": "daˈɡeʃ", "shin dot": "ʃin", "sin dot": "sin",
}

# Korean names are two syllables built from the letter itself: the sound,
# then 이 and the sound again as a final.
HANGUL = {
 "giyeok": "ki.jʌk̚", "nieun": "ni.ɯn", "digeut": "ti.ɡɯt̚",
 "rieul": "ɾi.ɯl", "mieum": "mi.ɯm", "bieup": "pi.ɯp̚",
 "siot": "ɕi.ot̚", "ieung": "i.ɯŋ", "jieut": "t͡ɕi.ɯt̚",
 "chieut": "t͡ɕʰi.ɯt̚", "kieuk": "kʰi.ɯk̚",
 "tieut": "tʰi.ɯt̚", "pieup": "pʰi.ɯp̚", "hieut": "çi.ɯt̚",
 "a": "a", "ya": "ja", "eo": "ʌ", "yeo": "jʌ", "o": "o", "yo": "jo",
 "u": "u", "yu": "ju", "eu": "ɯ", "i": "i",
 "ssang-giyeok": "ˀsaŋ.ki.jʌk̚", "ssang-digeut": "ˀsaŋ.ti.ɡɯt̚",
 "ssang-bieup": "ˀsaŋ.pi.ɯp̚", "ssang-siot": "ˀsaŋ.ɕi.ot̚",
 "ssang-jieut": "ˀsaŋ.t͡ɕi.ɯt̚",
 "ae": "ɛ", "yae": "jɛ", "e": "e", "ye": "je", "wa": "wa", "wae": "wɛ",
 "oe": "we", "wo": "wʌ", "we": "we", "wi": "wi", "ui": "ɰi",
 "gieok-siot": "ki.jʌk̚.ɕi.ot̚", "nieun-jieut": "ni.ɯn.t͡ɕi.ɯt̚",
 "nieun-hieuh": "ni.ɯn.çi.ɯt̚", "rieul-gieok": "ɾi.ɯl.ki.jʌk̚",
 "rieul-mieum": "ɾi.ɯl.mi.ɯm", "rieul-bieup": "ɾi.ɯl.pi.ɯp̚",
 "rieul-siot": "ɾi.ɯl.ɕi.ot̚", "rieul-tieut": "ɾi.ɯl.tʰi.ɯt̚",
 "rieul-pieup": "ɾi.ɯl.pʰi.ɯp̚", "rieul-hieuh": "ɾi.ɯl.çi.ɯt̚",
 "bieup-siot": "pi.ɯp̚.ɕi.ot̚",
}

ARABIC = {
 "Alif": "ʔalif", "Ba": "baːʔ", "Ta": "taːʔ", "Tha": "θaːʔ",
 "Jim": "d͡ʒiːm", "Ha": "ħaːʔ", "Kha": "xaːʔ",
 "Dal": "daːl", "Dhal": "ðaːl", "Ra": "raːʔ", "Zay": "zaːj",
 "Sin": "siːn", "Shin": "ʃiːn", "Sad": "sˤaːd", "Dad": "dˤaːd",
 "Ta (emphatic)": "tˤaːʔ", "Za (emphatic)": "ðˤaːʔ", "Ayn": "ʕajn",
 "Ghayn": "ɣajn", "Fa": "faːʔ", "Qaf": "qaːf", "Kaf": "kaːf",
 "Lam": "laːm", "Mim": "miːm", "Nun": "nuːn",
 "Waw": "waːw", "Ya": "jaːʔ",
 "hamza": "ˈhamza", "alef madda": "ˈʔalif ˈmadːa",
 "alef with hamza above": "ˈʔalif", "alef with hamza below": "ˈʔalif",
 "waw with hamza": "ˈwaːw", "yeh with hamza": "ˈjaːʔ",
 "teh marbuta": "taːʔ marˈbuːtˤa", "alef maqsura": "ˈʔalif makˈsˤuːra",
 "fatha": "ˈfatħa", "damma": "ˈdˤamːa", "kasra": "ˈkasra",
 "tanwin fath": "tanˈwiːn fatħ", "tanwin damm": "tanˈwiːn dˤamː",
 "tanwin kasr": "tanˈwiːn kasr", "sukun": "suˈkuːn", "shadda": "ˈʃadːa",
 "dagger alef": "ˈʔalif", "alef wasla": "ˈʔalif ˈwasˤla",
}

FARSI = {
 "Alef": "ʔæˈlef", "Be": "be", "Pe": "pe", "Te": "te", "Se": "se",
 "Jim": "d͡ʒim", "Che": "t͡ʃe", "He": "he", "Khe": "xe",
 "Dal": "dɒːl", "Zal": "zɒːl", "Re": "re", "Ze": "ze", "Zhe": "ʒe",
 "Sin": "sin", "Shin": "ʃin", "Sad": "sɒːd", "Zad": "zɒːd",
 "Ta": "tɒː", "Za": "zɒː", "Eyn": "ʔejn", "Gheyn": "ɣejn",
 "Fe": "fe", "Ghaf": "ɣɒːf", "Kaf": "kɒːf", "Gaf": "ɡɒːf",
 "Lam": "lɒːm", "Mim": "mim", "Nun": "nun", "Vav": "vɒːv", "Ye": "je",
 "alef-e madd": "ʔæˈlefe mædː", "hamze": "hæmˈze",
 "alef with hamze above": "ʔæˈlef", "alef with hamze below": "ʔæˈlef",
 "vav-e hamze": "vɒːve hæmˈze", "ye-ye hamze": "jeje hæmˈze",
 "te-ye gerd": "teje ɡeˈved", "he-ye havvaz": "heje hævˈvæz",
 "zebar": "zeˈbær", "zir": "zir", "pish": "piʃ",
 "tashdid": "tæʃˈdid", "sokun": "soˈkun", "tanvin": "tænˈvin",
}

# Thai names are "letter, then the word it stands for": ko kai is k as in
# chicken. Tones are part of the name.
THAI = {
 "ko kai": "kɔː kàj", "kho khai": "kʰɔ̌ː kʰàj",
 "kho khuat": "kʰɔ̌ː kʰùat", "kho khwai": "kʰɔː kʰwaːj",
 "kho khon": "kʰɔː kʰon", "kho rakhang": "kʰɔː raːkʰaːŋ",
 "ngo ngu": "ŋɔː ŋuː", "cho chan": "t͡ɕɔː t͡ɕaːn",
 "cho ching": "t͡ɕʰɔ̌ː t͡ɕʰìŋ",
 "cho chang": "t͡ɕɔː t͡ɕʰáːŋ",
 "so so": "sɔː sôː", "cho choe": "t͡ɕɔː t͡ɕɤː",
 "yo ying": "jɔː jǐŋ", "do chada": "dɔː t͡ɕaːdaː",
 "to patak": "tɔː paːtàk", "tho than": "tʰɔ̌ː tʰǎːn",
 "tho nangmontho": "tʰɔː naːŋmontʰoː",
 "tho phuthao": "tʰɔː pʰúttʰāw",
 "no nen": "nɔː neːn", "do dek": "dɔː dèk", "to tao": "tɔː tàw",
 "tho thung": "tʰɔ̌ː tʰǔŋ", "tho thahan": "tʰːɔ tʰaːhǎːn",
 "tho thong": "tʰɔː tʰoːŋ", "no nu": "nɔː nǔː",
 "bo baimai": "bɔː baːjmáj", "po pla": "pɔː plaː",
 "pho phueng": "pʰɔ̌ː pʰɯ̂ŋ", "fo fa": "fɔ̌ː fǎː",
 "pho phan": "pʰɔː pʰaːn", "fo fan": "fɔː fan",
 "pho samphao": "pʰɔː sǎːmpʰaːw", "mo ma": "mɔː máː",
 "yo yak": "jɔː ják", "ro ruea": "rɔː rɨa",
 "lo ling": "lɔː liŋ", "wo waen": "wɔː wɛ̂ːn",
 "so sala": "sɔ̌ː sǎːlaː", "so ruesi": "sɔ̌ː rɯːsǐː",
 "so suea": "sɔ̌ː sɯ̌a", "ho hip": "hɔ̌ː hìːp",
 "lo chula": "lɔː t͡ɕuːlaː", "o ang": "ʔɔː ʔàːŋ",
 "ho nokhuk": "hɔː nóːkhúk",
 "sara a": "sàraː ʔà", "mai han akat": "máːj hǎn ʔaːkàːt",
 "sara aa": "sàraː ʔaː", "sara am": "sàraː ʔam",
 "sara i": "sàraː ʔì", "sara ii": "sàraː ʔiː",
 "sara ue": "sàraː ʔɨ", "sara uee": "sàraː ʔɨː",
 "sara u": "sàraː ʔù", "sara uu": "sàraː ʔuː",
 "sara e": "sàraː ʔeː", "sara ae": "sàraː ʔɛː",
 "sara o": "sàraː ʔoː", "sara ai maimuan": "sàraː ʔaj máːjmuːan",
 "sara ai maimalai": "sàraː ʔaj máːjmalaːj",
 "mai ek": "máːj ʔèːk", "mai tho": "máːj tʰoː",
 "mai tri": "máːj triː", "mai chattawa": "máːj t͡ɕàtːawaː",
 "mai taikhu": "máːj tàjkʰuː", "thanthakhat": "tʰantaːkʰâːt",
 "mai yamok": "máːj jaːmók", "paiyannoi": "pajjaːn nɔ́ːj",
 "nikhahit": "níkkʰaːhít", "phinthu": "pʰintʰú", "lu": "lɨ",
 "ru": "rɨ", "lakkhangyao": "lákkʰaːŋjaːw",
}

# Only the signs whose names are words; a kana called "shi" needs nothing.
KANA = {
 "small a": "ko.ɡakɯ.a", "small i": "ko.ɡakɯ.i",
 "small u": "ko.ɡakɯ.ɯ", "small e": "ko.ɡakɯ.e",
 "small o": "ko.ɡakɯ.o", "small ya": "ko.ɡakɯ.ja",
 "small yu": "ko.ɡakɯ.jɯ", "small yo": "ko.ɡakɯ.jo",
 "sokuon": "sokɯ.oɴ", "chouonpu": "t͡ɕoːoɴpɯ",
 "dakuten": "dakɯ.teɴ", "handakuten": "haɴ.dakɯ.teɴ",
}

DEVANAGARI = {
 "aa matra": "aː maːtɾaː", "i matra": "i maːtɾaː",
 "ii matra": "iː maːtɾaː", "u matra": "u maːtɾaː",
 "uu matra": "uː maːtɾaː", "ri matra": "ɾi maːtɾaː",
 "e matra": "eː maːtɾaː", "ai matra": "ɛː maːtɾaː",
 "o matra": "oː maːtɾaː", "au matra": "ɔː maːtɾaː",
 "anusvara": "ənusːʌaːɾ", "candrabindu": "t͡ɕəndɾəbindu",
 "visarga": "ʌisəɾɡə", "virama": "ʌiːɾaːm", "nukta": "nʌktaː",
 "candra o matra": "t͡ɕəndɾə oː", "candra O": "t͡ɕəndɾə oː",
 "candra e matra": "t͡ɕəndɾə eː", "candra E": "t͡ɕəndɾə eː",
 "avagraha": "əʌəɡɾəhə",
}

PINYIN = {
 "u umlaut": "juː ˈʊmlaʊt", "first tone": "fɜːst toʊn",
 "second tone": "ˈsɛkənd toʊn", "third tone": "θɜːd toʊn",
 "fourth tone": "fɔːθ toʊn",
}

BY_SCRIPT = {
 "greek": GREEK, "latin": LATIN, "cyrillic": CYRILLIC, "hebrew": HEBREW,
 "hangul": HANGUL, "arabic": ARABIC, "farsi": FARSI, "thai": THAI,
 "kana": KANA, "devanagari": DEVANAGARI, "pinyin": PINYIN,
}

# Arabic calls both ح and ه "Ha" but says them differently, so that one
# cannot be keyed by name. Anything here wins over the tables above.
BY_GLYPH = {
 "arabic": {"ه": "haːʔ"},
}


def name_ipa(slug, glyph, name):
    """How the letter's name is said, or None where the name is the sound."""
    by_glyph = BY_GLYPH.get(slug, {}).get(glyph)
    if by_glyph:
        return by_glyph
    table = BY_SCRIPT.get(slug)
    if not table or not name:
        return None
    return table.get(name) or table.get(name.lower()) or \
        {k.lower(): v for k, v in table.items()}.get(name.lower())
