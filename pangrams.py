# -*- coding: utf-8 -*-
"""One pangram per script, for the Pangrams tab in Study.

A pangram runs the whole alphabet past you in one sentence, which makes
it the natural thing to practise typing and to fingerspell: every letter
turns up, in ordinary words, in an order nobody chose for teaching.

The IPA is a broad transcription — phonemic, not a recording of any one
speaker — and it is the part of this file to trust least. It is offered
as a hint you turn on, not as the answer.

Fields: script slug, language, the sentence, what it means, the broad
reading, and a note where the sentence has a story.
"""
import io, json, os

from script_content import GEEZ_CONSONANTS as GEEZ_BASES

JAMO = [
 ("\u1100\u1101\u1102\u1103\u1104\u1105\u1106\u1107\u1108\u1109\u110a\u110b\u110c\u110d\u110e\u110f\u1110\u1111\u1112",
  "\u3131\u3132\u3134\u3137\u3138\u3139\u3141\u3142\u3143\u3145\u3146\u3147\u3148\u3149\u314a\u314b\u314c\u314d\u314e"),
 ("\u1161\u1162\u1163\u1164\u1165\u1166\u1167\u1168\u1169\u116a\u116b\u116c\u116d\u116e\u116f\u1170\u1171\u1172\u1173\u1174\u1175",
  "\u314f\u3150\u3151\u3152\u3153\u3154\u3155\u3156\u3157\u3158\u3159\u315a\u315b\u315c\u315d\u315e\u315f\u3160\u3161\u3162\u3163"),
 ("\u11a8\u11a9\u11aa\u11ab\u11ac\u11ad\u11ae\u11af\u11b0\u11b1\u11b2\u11b3\u11b4\u11b5\u11b6\u11b7\u11b8\u11b9\u11ba\u11bb\u11bc\u11bd\u11be\u11bf\u11c0\u11c1\u11c2",
  "\u3131\u3132\u3133\u3134\u3135\u3136\u3137\u3139\u313a\u313b\u313c\u313d\u313e\u313f\u3140\u3141\u3142\u3144\u3145\u3146\u3147\u3148\u314a\u314b\u314c\u314d\u314e"),
]
# a Devanagari vowel is reached when its dependent form turns up in a word
MATRA = {"\u0906": "\u093e", "\u0907": "\u093f", "\u0908": "\u0940",
         "\u0909": "\u0941", "\u090a": "\u0942", "\u090b": "\u0943",
         "\u090f": "\u0947", "\u0910": "\u0948", "\u0913": "\u094b",
         "\u0914": "\u094c"}

OUT = "assets/data/pangrams.json"

# slug, language, text, translation, ipa, note
PANGRAMS = [
("latin", "English",
 "Sphinx of black quartz, judge my vow",
 "Sphinx of black quartz, judge my vow",
 "sfɪŋks əv blæk kwɔːts d͡ʒʌd͡ʒ maɪ vaʊ",
 "Shorter than the quick brown fox and still all 26 letters."),

("greek", "Greek",
 "Τάχιστη αλχημεία, "
 "ψιθυριστός κώδωνας, "
 "ξένος φόβος",
 "Quick alchemy, whispered bell, foreign fear",
 "ˈtaxisti alçiˈmia psiθiriˈstos ˈkoðonas ˈksenos ˈfovos",
 "The better known Ξεσκεπάζω την "
 "ψυχοφθόρα βλακεία "
 "(“I uncover the soul-destroying stupidity”) is missing delta and gamma, "
 "so this one is used here instead."),

("cyrillic", "Russian",
 "Съешь же ещё этих "
 "мягких французских "
 "булок, да выпей чаю.",
 "Eat some more of these soft French buns, and drink some tea",
 "sjеʂ ʒɨ jɪˈɕːɵ ˈetʲɪx ˈmʲæxkʲɪx "
 "frɐnˈt͡suskʲɪx ˈbulək da ˈvɨpʲɪj ˈt͡ɕaju",
 "The standard Russian keyboard-test sentence, and the one that finally "
 "earns ъ and ь their place."),

("hebrew", "Hebrew",
 "דָּג מָהִיר וְקָפִיץ "
 "שִׁיר לְלֹא מִלִּים "
 "אָז גָּז וְנֶעֱלַם.",
 "A fast, springy fish, a song without words, then vanished and disappeared",
 "daɡ maˈhiʁ veˈkafit͡s ʃiʁ leˈlo miˈlim az ɡaz "
 "veneʔeˈlam",
 "Written with the vowel points, which ordinary Hebrew leaves out — so "
 "typing this one means typing the niqqud too."),

("arabic", "Arabic",
 "صِفْ خَلْقَ خَوْدٍ "
 "كَمِثْلَةِ الشَّمْسِ "
 "إِذْ بَزَغَتْ يَحْظَى "
 "الضَّجِيعُ بِهَا "
 "نَجْلًا وَخَرْدَلَا",
 "Describe the creation of a beautiful young woman, like the sun when it rises; "
 "whoever shares her bed obtains posterity and a grain of mustard seed",
 "sˤif xalqa xawdin kamiθlati ʃːamsi ʔið bazaɣat jaħˤˤaː "
 "dˤːad͡ʒiːʕu bihaː nad͡ʒlan waxardalaː",
 "Fully vocalised, so it exercises the harakat as well as the letters."),

("farsi", "Persian",
 "«بر اثر چنین تلقین و "
 "شستشوی مغزی جامعی، "
 "سطح و پایهٔ ذهن و فهم و "
 "نظر بعضی اشخاص واژگونه "
 "و معکوس می‌شود.»",
 "As a result of such thorough indoctrination and brainwashing, the level and "
 "foundation of the mind, understanding and viewpoint of some people becomes "
 "inverted and reversed",
 "bær ʔæsær t͡ʃeniːn tælɣiːn o ʃostoʃuːje "
 "mæɣziːje d͡ʒɒːmeːʔiː sætħ o pɒːjeje "
 "zehn o fæhm o næzˤære bæʔziː ʔæʃxɒːsː "
 "vɒːʒɡuːne o mæʔkuːs miːʃævæd",
 "It carries ژ and گ, the letters Persian adds to the Arabic alphabet."),

("devanagari", "Hindi",
 "ऋषियों को सताने वाले "
 "दुष्ट राक्षसों के "
 "राजा रावण का सर्वनाश "
 "करने वाले "
 "विष्णुवतार भगवान "
 "श्रीराम, अयोध्या के "
 "महाराज दशरथ के बड़े "
 "सपुत्र थे।",
 "Lord Rama, the avatar of Vishnu who destroyed Ravana — king of the wicked "
 "demons who tormented the sages — was the eldest son of King Dasharatha of Ayodhya",
 "ɾəʂijõ ko sətaːne ʌaːle dʊʂʈ "
 "raːkʂəsõ ke raːd͡ʒaː raːʌəɳ kaː "
 "sərʌənaːʃ kərne ʌaːle ʌiʂɳuʌtaːr "
 "bʱəɡʌaːn ʃriːraːm əjodʱjaː ke "
 "məhaːraːd͡ʒ dəʃərətʱ ke bəɾe "
 "səputɾ tʱe",
 "Covers the vowels and consonants of modern Hindi. The old Sanskrit pangram, "
 "कः खगौघाङ्चिच्छौजा, "
 "runs the consonants in alphabet order instead."),

("thai", "Thai",
 "นายสังฆภัณฑ์ "
 "เฮงพิทักษ์ฝั่ง "
 "ผู้เฒ่าซึ่งมีอาชีพ"
 "เป็นฅนขายฃวด "
 "ถูกตำรวจปฏิบัติการ"
 "จับฟ้องศาล "
 "ฐานลักนาฬิกาคุณหญิง"
 "ฉัตรชฎา ฌานสมาธิ",
 "Mr Sangkhaphan Hengphithakfang, an old man whose trade is selling bottles, was "
 "arrested by police and taken to court for stealing the watch of Khunying "
 "Chatchada Chansamathi",
 "naːj sǊŋ.kʰaː.pʰan heːŋ.pʰi.tʰak.faŋ "
 "pʰuː.tʰâw sɨ̂ŋ miː ʔaː.t͡ɕiːp pen "
 "kʰon kʰǎːj kʰùat tʰuːk tam.rúat pa­tì.bat.tiː.kan "
 "t͡ɕàp f◌́ːŋ sǎːn tʰǎːn lak naː.liː.kaː "
 "kʰun.jǐŋ t͡ɕàt.t͡ɕa.daː t͡ɕaːn.sa.maː.tʰí",
 "Famous for reaching ฅ and ฃ, the two consonants Thai has retired — "
 "which is why it is longer and odder than most pangrams."),

("kana", "Japanese",
 "とりなくこゑす ゆめさませ\n"
 "みよあけわたる ひんかしを\n"
 "そらいろはえて おきつへに\n"
 "ほふねむれゐぬ もやのうち",
 "Birds are calling — wake from your dream! Look, the east is growing light. "
 "The sky takes on colour, and out on the open sea the boats crowd together "
 "in the morning mist.",
 "to.ɾi na.kɯ ko.e sɯ jɯ.me sa.ma.se\n"
 "mi.jo a.ke wa.ta.ɾɯ çiŋ.ɡa.ɕi o\n"
 "so.ɾa i.ɾo ha.e.te o.ki tsɯ he ni\n"
 "ho.ɸɯ.ne mɯ.ɾe i.nɯ mo.ja no ɯ.tɕi",
 "The Toriuta of 1903: 48 kana, each used exactly once. It includes ゐ and ゑ, "
 "which modern Japanese has dropped, and unlike the older Iroha poem it reaches "
 "ん as well."),

("hangul", "Korean",
 "키스의 고유 조건은 입술끼리 "
 "만나야 하고 특별한 요령은 필요치 않다",
 "The essential condition for a kiss is that the lips must meet, and no special "
 "technique is required",
 "kʰi.sɯ.ɰi ko.ju t͡ɕo.ˀkʌ.nɯn ip̚.ˀsul.ˀki.ɾi "
 "man.na.ja ha.ɡo tʰɯk̚.ˀpjʌ.ɾan jo.ɾjʌŋ.ɯn "
 "pʰi.ɾjo.t͡ɕʰi an.tʰa",
 "The Korean typing-test sentence. The shorter 다람쥐 헌 "
 "쳃바퀴에 타고파 (“a squirrel wants to ride an old "
 "treadmill”) misses the harder clusters."),

("geez", "Amharic",
 "የቀድሞው የኢፌዲሪ ፕሬዝዳንት ዶክተር ነጋሶ ጊዳዳ በተወለዱ በ76 "
 "ዓመታቸው ከዚህ ዓለም በሞት ተለዩ።",
 "The former president of Ethiopia, Dr Negaso Gidada, passed away at the age of 76",
 "jə.qʼəd.mow jə.ʔi.fe.di.ri pɨ.rez.dant dok.tər nə.ɡa.so ɡi.da.da "
 "bə.tə.wə.lə.du bə.sə.ba sɨ.dɨst ʕa.mə.ta.t͡ʃəw kə.zih ʕa.ləm "
 "bə.mot tə.lə.ju",
 "A news sentence rather than one written to order, which is why it reads "
 "plainly. The 76 is said ሰባ ስድስት, not spelled out."),

("pinyin", "Mandarin",
 "zuótiān wǒ bāng nǚ'ér qù yī jiā chāoshì mǎi "
 "kělè, xīfàn, dòupí, hǎizhé.",
 "Yesterday I helped my daughter go to the supermarket to buy cola, congee, "
 "bean curd skin and jellyfish",
 "t͡swo˧˥.tʰjɛn˥˥ wo˨˩˧ paŋ˥˥ "
 "ny˨˩˧.ɕʱɚ˧˥ t͡ɕʰy˥˩ i˥˥ "
 "t͡ɕja˥˥ ʈ͡ʂʰau˥˥.ʂɹ̩˥˩ "
 "mai˨˩˧ kʰɤ˨˩˧.lɤ˥˩ "
 "ɕi˥˥.fan˥˩ tou˥˩.pʰi˧˥ "
 "xai˨˩˧.ʈ͡ʂɤ˧˥",
 "Written in pinyin with the tone marks, so it drills the diacritics as much as "
 "the letters."),
]


def coverage(slug, text):
    """How much of the alphabet the sentence actually reaches.

    Not every sentence here is a complete pangram, and a study page
    should say so rather than imply it. Counted per script's own rules:
    Hangul by jamo inside its syllable blocks, Ge'ez by consonant rather
    than by all 231 syllables, Devanagari counting a vowel as reached
    when its matra appears.
    """
    import unicodedata
    path = os.path.join("assets", "data", "scripts", slug + ".json")
    d = json.load(io.open(path, encoding="utf-8"))
    core = [L for L in d["letters"] if not L.get("section")]
    nfd, low = unicodedata.normalize("NFD", text), text.lower()
    if slug == "hangul":
        have = set()
        for ch in nfd:
            for a, b in JAMO:
                i = a.find(ch)
                if i >= 0:
                    have.add(b[i])
                    break
        miss = [L["glyph"] for L in core if L["glyph"] not in have]
    elif slug == "geez":
        core = [{"glyph": b} for b, _r, _i in GEEZ_BASES]
        miss = [c["glyph"] for c in core
                if not any(chr(ord(c["glyph"]) + i) in text for i in range(7))]
    else:
        miss = []
        for L in core:
            keys = [k for k in (L["glyph"], L.get("alt")) + tuple(L.get("aliases") or []) if k]
            if slug == "devanagari" and L["glyph"] in MATRA:
                keys.append(MATRA[L["glyph"]])
            if not any(k in text or k in nfd or k.lower() in low for k in keys):
                miss.append(L["glyph"])
    return len(core) - len(miss), len(core), miss


def main():
    rows = []
    for slug, lang, text, translation, ipa, note in PANGRAMS:
        got, total, miss = coverage(slug, text)
        rows.append({"script": slug, "language": lang, "text": text,
                     "translation": translation, "ipa": ipa, "note": note,
                     "characters": len(text),
                     "covers": got, "of": total, "missing": miss})
    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    io.open(OUT, "w", encoding="utf-8").write(
        json.dumps({"pangrams": rows}, ensure_ascii=False, indent=1) + "\n")
    for r in rows:
        print("%-11s %-9s %3d chars | reaches %3d/%-3d %s"
              % (r["script"], r["language"], r["characters"], r["covers"], r["of"],
                 " ".join(r["missing"][:14])))
    print("\n%d pangrams -> %s" % (len(rows), OUT))


if __name__ == "__main__":
    main()
