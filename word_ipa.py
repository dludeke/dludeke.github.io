#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Broad IPA for every word in the quiz and flashcard word lists.

One entry per word, keyed by the word exactly as the list spells it. These
are broad transcriptions of the citation form, written to each language's
usual conventions rather than to one scheme across all twelve: Thai and
Mandarin carry tone, Russian marks stress and vowel reduction, Japanese uses
ɯ and ɴ, Amharic writes the first order as ə and the ejectives with ʼ.

They are not checked by a native speaker. The ones that also appear as a
letter's example word in script_content.py are the same string in both
places, which generate_word_lists.py asserts rather than trusts.
"""

IPA = {}

IPA["hangul"] = {
 "밥": "pap̚", "물": "mul", "김치": "kimtɕʰi", "차": "tɕʰa", "우유": "uju",
 "빵": "ˀpaŋ", "고기": "kogi", "사과": "sagwa", "한국": "hanguk̚",
 "바다": "pada", "산": "san", "강": "kaŋ", "숲": "sup̚", "서울": "sʌul",
 "부산": "pusan", "인천": "intɕʰʌn", "제주": "tɕedʑu", "책": "tɕʰɛk̚",
 "학교": "hak̚ˀkʲo", "음악": "ɯmak̚", "친구": "tɕʰingu", "한글": "hangɯl",
 "해": "hɛ", "달": "tal", "나무": "namu", "꽃": "ˀkot̚", "개": "kɛ",
 "고양이": "kojaŋi",
}

IPA["kana"] = {
 "すし": "sɯɕi", "さけ": "sake", "おおさか": "oːsaka", "とうきょう": "toːkʲoː",
 "きょうと": "kʲoːto", "コーヒー": "koːhiː", "ケーキ": "keːki",
 "ラーメン": "ɾaːmeɴ", "てんぷら": "tempɯɾa", "みそ": "miso", "とうふ": "toːɸɯ",
 "そば": "soba", "うどん": "ɯdoɴ", "おちゃ": "ot͡ɕa", "みず": "mizɯ",
 "さかな": "sakana", "やさい": "jasai", "にく": "nikɯ", "たまご": "tamago",
 "パン": "paɴ", "ごはん": "gohaɴ", "アニメ": "anime", "マンガ": "maŋga",
 "カラオケ": "kaɾaoke", "すもう": "sɯmoː", "からて": "kaɾate",
 "ふじさん": "ɸɯd͡ʑisaɴ", "さくら": "sakɯɾa", "ひらがな": "hiɾagana",
 "カタカナ": "katakana",
}

IPA["greek"] = {
 "αλφα": "ˈalfa", "βητα": "ˈvita", "γαμμα": "ˈɣama", "δελτα": "ˈðelta",
 "επσιλον": "ˈepsilon", "ζητα": "ˈzita", "ητα": "ˈita", "θητα": "ˈθita",
 "ιωτα": "ˈjota", "καππα": "ˈkapa", "λαμβδα": "ˈlamvða", "μυ": "mi",
 "νυ": "ni", "ξι": "ksi", "ομικρον": "ˈomikron", "πι": "pi", "ρο": "ro",
 "σιγμα": "ˈsiɣma", "ταυ": "taf", "υψιλον": "ˈipsilon", "φι": "fi",
 "χι": "çi", "ψι": "psi", "ωμεγα": "oˈmeɣa",
 "Αθηνα": "aˈθina", "Σπαρτη": "ˈsparti", "Θεσσαλονικη": "θesaloˈnici",
 "Κορινθος": "ˈkorinθos", "Δελφοι": "ðelˈfi", "Ολυμπια": "oliˈbia",
 "Κρητη": "ˈkriti", "Ροδος": "ˈroðos", "Μυκηναι": "miˈcine",
 "Ολυμπος": "ˈolimbos", "Μαραθων": "maraˈθon", "Σωκρατης": "soˈkratis",
 "Πλατων": "ˈplaton", "Αριστοτελης": "aristoˈtelis",
 "Αλεξανδρος": "aˈleksanðros", "Περικλης": "periˈklis", "Ομηρος": "ˈomiros",
 "Ηροδοτος": "iˈroðotos", "Πυθαγορας": "piθaˈɣoras", "Ευκλειδης": "efˈkliðis",
 "Αρχιμηδης": "arçiˈmiðis", "Ιπποκρατης": "ipoˈkratis", "Λεωνιδας": "leoˈniðas",
 "μουσακας": "musaˈkas", "σουβλακι": "suˈvlaci", "τζατζικι": "d͡zaˈd͡zici",
 "ταραμοσαλατα": "taramosaˈlata", "φετα": "ˈfeta", "γυρος": "ˈʝiros",
 "σπανακοπιτα": "spanaˈkopita", "ντολμαδες": "dolˈmaðes",
 "μπακλαβας": "baklaˈvas", "ουζο": "ˈuzo", "ελια": "eˈʎa",
 "δημοκρατια": "ðimokraˈtia", "φιλοσοφια": "filosoˈfia",
}

IPA["cyrillic"] = {
 "хлеб": "xlʲep", "вода": "vɐˈda", "чай": "tɕaj", "молоко": "məlɐˈko",
 "суп": "sup", "яблоко": "ˈjabləkə", "сыр": "sɨr", "мясо": "ˈmʲasə",
 "Россия": "rɐˈsʲijə", "море": "ˈmorʲe", "гора": "gɐˈra", "река": "rʲɪˈka",
 "лес": "lʲes", "Москва": "mɐskˈva", "Киев": "ˈkʲijɪf", "Минск": "mʲinsk",
 "София": "sɐˈfʲijə", "книга": "ˈknʲigə", "школа": "ˈʂkolə",
 "музыка": "ˈmuzɨkə", "театр": "tʲɪˈatr", "друг": "druk", "солнце": "ˈsont͡sə",
 "луна": "lʊˈna", "дерево": "ˈdʲerʲɪvə", "цветок": "tsvʲɪˈtok",
 "собака": "sɐˈbakə", "кошка": "ˈkoʂkə",
}

IPA["geez"] = {
 "እንጀራ": "ɨndʒəra", "ውሃ": "wɨha", "ቡና": "buna", "ወተት": "wətət",
 "ዳቦ": "dabo", "ሥጋ": "sɨga", "እንቁላል": "ɨnkʼulal", "ጨው": "tʃʼəw",
 "ኢትዮጵያ": "itjopʼɨja", "ባሕር": "bahɨr", "ተራራ": "tərara", "ወንዝ": "wənz",
 "ጫካ": "tʃʼaka", "አዲስ አበባ": "addis abəba", "ጎንደር": "gondər",
 "አክሱም": "aksum", "ላሊበላ": "lalibəla", "መጽሐፍ": "mətsʼɨhaf",
 "ትምህርት": "tɨmhɨrt", "ሙዚቃ": "muzikʼa", "ጓደኛ": "gwadəɲa", "በዓል": "bəal",
 "ፀሐይ": "tsʼəhaj", "ጨረቃ": "tʃʼərəkʼa", "ዛፍ": "zaf", "አበባ": "abəba",
 "ውሻ": "wɨʃa", "ድመት": "dɨmət",
}

IPA["devanagari"] = {
 "रोटी": "roːʈiː", "पानी": "paːniː", "चाय": "tʃaːj", "दूध": "duːdʱ",
 "चावल": "tʃaːʋəl", "आम": "aːm", "दाल": "daːl", "नमक": "nəmək",
 "भारत": "bʱaːɾət", "समुद्र": "səmudrə", "पहाड़": "pəhaːɽ", "नदी": "nədiː",
 "जंगल": "dʒəŋgəl", "दिल्ली": "dɪlliː", "मुंबई": "mumbəiː",
 "जयपुर": "dʒəjpur", "कोलकाता": "koːlkaːtaː", "किताब": "kɪtaːb",
 "स्कूल": "skuːl", "संगीत": "səŋgiːt", "दोस्त": "doːst", "त्योहार": "tjoːhaːɾ",
 "सूरज": "suːɾədʒ", "चाँद": "tʃãːd", "पेड़": "peːɽ", "फूल": "pʰuːl",
 "कुत्ता": "kʊttaː", "बिल्ली": "bɪlliː",
}

IPA["thai"] = {
 "ข้าว": "kʰâːw", "น้ำ": "náːm", "ชา": "tɕʰaː", "นม": "nom", "ปลา": "plaː",
 "ไข่": "kʰàj", "ผลไม้": "pʰǒnlamáj", "กาแฟ": "kaːfɛː", "ไทย": "tʰaj",
 "ทะเล": "tʰáleː", "ภูเขา": "pʰuːkʰǎw", "แม่น้ำ": "mɛ̂ːnáːm", "ป่า": "pàː",
 "กรุงเทพ": "kruŋtʰêːp", "เชียงใหม่": "tɕʰiaŋmàj", "ภูเก็ต": "pʰuːkèt",
 "อยุธยา": "ʔajútʰajaː", "หนังสือ": "nǎŋsɯ̌ː", "โรงเรียน": "roːŋriːan",
 "ดนตรี": "dontriː", "เพื่อน": "pʰɯ̂an", "วัด": "wát",
 "ดวงอาทิตย์": "duaŋʔaːtʰít", "ดวงจันทร์": "duaŋtɕan", "ต้นไม้": "tônmáj",
 "ดอกไม้": "dɔ̀ːkmáj", "หมา": "mǎː", "แมว": "mɛːw",
}

IPA["arabic"] = {
 "خبز": "xubz", "ماء": "maːʔ", "قهوة": "ˈqahwa", "شاي": "ʃaːj",
 "تمر": "tamr", "لحم": "laħm", "سمك": "ˈsamak", "أرز": "ˈʔaruzz",
 "حليب": "ħaˈliːb", "مصر": "misˤr", "بحر": "baħr", "جبل": "ˈdʒabal",
 "صحراء": "sˤaħˈraːʔ", "نهر": "nahr", "القاهرة": "alˈqaːhira",
 "دمشق": "diˈmaʃq", "بغداد": "baɣˈdaːd", "الرباط": "arriˈbaːtˤ",
 "كتاب": "kiˈtaːb", "مدرسة": "ˈmadrasa", "موسيقى": "muːˈsiːqaː",
 "سلام": "saˈlaːm", "شمس": "ʃams", "قمر": "ˈqamar", "شجرة": "ˈʃadʒara",
 "وردة": "ˈwarda", "قط": "qitˤtˤ", "حصان": "ħiˈsˤaːn",
}

IPA["hebrew"] = {
 "לחם": "ˈleχem", "חלב": "χaˈlav", "גבינה": "gviˈna", "תפוח": "taˈpuaχ",
 "יין": "ˈjajin", "מים": "ˈmajim", "דג": "dag", "עוגה": "uˈga",
 "קפה": "kaˈfe", "ישראל": "jisraˈʔel", "ים": "jam", "הר": "haʁ",
 "מדבר": "midˈbaʁ", "נהר": "naˈhaʁ", "ירושלים": "jeʁuʃaˈlajim",
 "תל אביב": "tel aˈviv", "חיפה": "ˈχeifa", "אילת": "eiˈlat",
 "שבת": "ʃaˈbat", "ספר": "ˈsefeʁ", "שלום": "ʃaˈlom", "מוזיקה": "ˈmuzika",
 "תורה": "toˈʁa", "שמש": "ˈʃemeʃ", "ירח": "jaˈʁeaχ", "עץ": "ʕets",
 "פרח": "ˈpeʁaχ", "כלב": "ˈkelev", "חתול": "χaˈtul",
}

IPA["farsi"] = {
 "نان": "nɒːn", "آب": "ɒːb", "چای": "tʃɒːj", "برنج": "beˈrendʒ",
 "انار": "æˈnɒːɾ", "پنیر": "pæˈniːɾ", "شیر": "ʃiːɾ", "کباب": "kæˈbɒːb",
 "ایران": "iːˈɾɒːn", "دریا": "dæɾˈjɒː", "کوه": "kuːh",
 "بیابان": "bijɒːˈbɒːn", "رود": "ɾuːd", "تهران": "tehˈɾɒːn",
 "اصفهان": "esfæˈhɒːn", "شیراز": "ʃiːˈɾɒːz", "مشهد": "mæʃˈhæd",
 "کتاب": "keˈtɒːb", "شعر": "ʃeʔɾ", "نوروز": "nowˈɾuːz",
 "موسیقی": "muːsiːˈɢiː", "دوست": "duːst", "خورشید": "xoɾˈʃiːd",
 "ماه": "mɒːh", "درخت": "deˈɾæxt", "گل": "gol", "گربه": "ˈgoɾbe",
 "اسب": "æsb",
}

# Tone written with Chao letters, as the pinyin cards already do.
IPA["pinyin"] = {
 "米饭 (mǐfàn)": "mi˨˩˦ fan˥˩", "水 (shuǐ)": "ʂwei˨˩˦", "茶 (chá)": "ʈʂʰa˧˥",
 "牛奶 (niúnǎi)": "njoʊ̯˧˥ naɪ̯˨˩˦", "面条 (miàntiáo)": "mjɛn˥˩ tʰjaʊ̯˧˥",
 "鱼 (yú)": "y˧˥", "鸡蛋 (jīdàn)": "tɕi˥ tan˥˩", "苹果 (píngguǒ)": "pʰiŋ˧˥ kwo˨˩˦",
 "中国 (zhōngguó)": "ʈʂʊŋ˥ kwo˧˥", "海 (hǎi)": "xaɪ̯˨˩˦", "山 (shān)": "ʂan˥",
 "河 (hé)": "xɤ˧˥", "森林 (sēnlín)": "sən˥ lin˧˥", "北京 (běijīng)": "peɪ̯˨˩˦ tɕiŋ˥",
 "上海 (shànghǎi)": "ʂaŋ˥˩ xaɪ̯˨˩˦", "广州 (guǎngzhōu)": "kwaŋ˨˩˦ ʈʂoʊ̯˥",
 "成都 (chéngdū)": "ʈʂʰɤŋ˧˥ tu˥", "书 (shū)": "ʂu˥", "学校 (xuéxiào)": "ɕɥe˧˥ ɕjaʊ̯˥˩",
 "音乐 (yīnyuè)": "in˥ ɥe˥˩", "朋友 (péngyǒu)": "pʰɤŋ˧˥ joʊ̯˨˩˦",
 "春节 (chūnjié)": "ʈʂʰwən˥ tɕje˧˥", "太阳 (tàiyáng)": "tʰaɪ̯˥˩ jaŋ˧˥",
 "月亮 (yuèliàng)": "ɥe˥˩ ljaŋ˥˩", "树 (shù)": "ʂu˥˩", "花 (huā)": "xwa˥",
 "狗 (gǒu)": "koʊ̯˨˩˦", "猫 (māo)": "maʊ̯˥",
}

# Received Pronunciation, since the list spells theatre the British way.
IPA["latin"] = {
 "bread": "bred", "water": "ˈwɔːtə", "coffee": "ˈkɒfi", "apple": "ˈæpl̩",
 "cheese": "tʃiːz", "fish": "fɪʃ", "rice": "raɪs", "egg": "eg",
 "England": "ˈɪŋglənd", "sea": "siː", "mountain": "ˈmaʊntɪn",
 "river": "ˈrɪvə", "forest": "ˈfɒrɪst", "London": "ˈlʌndən",
 "Dublin": "ˈdʌblɪn", "Boston": "ˈbɒstən", "Sydney": "ˈsɪdni",
 "book": "bʊk", "school": "skuːl", "music": "ˈmjuːzɪk", "friend": "frend",
 "theatre": "ˈθɪətə", "sun": "sʌn", "moon": "muːn", "tree": "triː",
 "flower": "ˈflaʊə", "dog": "dɒg", "cat": "kæt",
}
