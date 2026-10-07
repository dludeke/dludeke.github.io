#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Example words per letter, keyed by script slug then letter glyph.

Each entry is (word, English gloss, emoji stand-in).

Where a language has a settled primer tradition the conventional word is used:
Hindi's varnamala (अ से अनार), the Arabic kids' alphabet (أ أرنب), the Russian
azbuka (А арбуз), and Thai, where the convention is built into the letter names
themselves (ก ไก่, "ko kai", chicken). Greek, Hebrew, Korean and Japanese have
no single canonical set, so these are ordinary picturable nouns of the kind
their primers use. Pinyin is a romanisation and has no such tradition at all;
its words are simply common nouns whose pinyin begins with that letter.
"""

WORDS = {}

WORDS["latin"] = {
 "A": ("Apple", "apple", "\U0001f34e"), "B": ("Ball", "ball", "⚽"),
 "C": ("Cat", "cat", "\U0001f408"), "D": ("Dog", "dog", "\U0001f415"),
 "E": ("Elephant", "elephant", "\U0001f418"), "F": ("Fish", "fish", "\U0001f41f"),
 "G": ("Giraffe", "giraffe", "\U0001f992"), "H": ("House", "house", "\U0001f3e0"),
 "I": ("Ice cream", "ice cream", "\U0001f366"), "J": ("Jug", "jug", "\U0001fad7"),
 "K": ("Kite", "kite", "\U0001fa81"), "L": ("Lion", "lion", "\U0001f981"),
 "M": ("Moon", "moon", "\U0001f319"), "N": ("Nest", "nest", "\U0001fab9"),
 "O": ("Orange", "orange", "\U0001f34a"), "P": ("Pencil", "pencil", "✏️"),
 "Q": ("Queen", "queen", "\U0001f478"), "R": ("Rainbow", "rainbow", "\U0001f308"),
 "S": ("Sun", "sun", "☀️"), "T": ("Tree", "tree", "\U0001f333"),
 "U": ("Umbrella", "umbrella", "☂️"), "V": ("Violin", "violin", "\U0001f3bb"),
 "W": ("Whale", "whale", "\U0001f40b"), "X": ("Xylophone", "xylophone", "\U0001f3b9"),
 "Y": ("Yacht", "yacht", "⛵"), "Z": ("Zebra", "zebra", "\U0001f993"),
}

WORDS["hebrew"] = {
 "א": ("אריה", "lion", "\U0001f981"),
 "ב": ("בית", "house", "\U0001f3e0"),
 "ג": ("גמל", "camel", "\U0001f42b"),
 "ד": ("דג", "fish", "\U0001f41f"),
 "ה": ("הר", "mountain", "⛰️"),
 "ו": ("ורד", "rose", "\U0001f339"),
 "ז": ("זברה", "zebra", "\U0001f993"),
 "ח": ("חתול", "cat", "\U0001f408"),
 "ט": ("טלפון", "telephone", "☎️"),
 "י": ("ילד", "child", "\U0001f9d2"),
 "כ": ("כלב", "dog", "\U0001f415"),
 "ל": ("לימון", "lemon", "\U0001f34b"),
 "מ": ("מים", "water", "\U0001f4a7"),
 "נ": ("נר", "candle", "\U0001f56f️"),
 "ס": ("סוס", "horse", "\U0001f40e"),
 "ע": ("עץ", "tree", "\U0001f333"),
 "פ": ("פרח", "flower", "\U0001f33a"),
 "צ": ("ציפור", "bird", "\U0001f426"),
 "ק": ("קוף", "monkey", "\U0001f412"),
 "ר": ("רכבת", "train", "\U0001f686"),
 "ש": ("שמש", "sun", "☀️"),
 "ת": ("תפוח", "apple", "\U0001f34e"),
}

WORDS["arabic"] = {
 "ا": ("أرنب", "rabbit", "\U0001f407"),
 "ب": ("بطة", "duck", "\U0001f986"),
 "ت": ("تفاحة", "apple", "\U0001f34e"),
 "ث": ("ثعلب", "fox", "\U0001f98a"),
 "ج": ("جمل", "camel", "\U0001f42b"),
 "ح": ("حصان", "horse", "\U0001f40e"),
 "خ": ("خبز", "bread", "\U0001f35e"),
 "د": ("دجاجة", "chicken", "\U0001f414"),
 "ذ": ("ذئب", "wolf", "\U0001f43a"),
 "ر": ("رمان", "pomegranate", None),
 "ز": ("زرافة", "giraffe", "\U0001f992"),
 "س": ("سمكة", "fish", "\U0001f41f"),
 "ش": ("شمس", "sun", "☀️"),
 "ص": ("صقر", "falcon", "\U0001f985"),
 "ض": ("ضفدع", "frog", "\U0001f438"),
 "ط": ("طائرة", "aeroplane", "✈️"),
 "ظ": ("ظرف", "envelope", "✉️"),
 "ع": ("عنب", "grapes", "\U0001f347"),
 "غ": ("غزال", "gazelle", "\U0001f98c"),
 "ف": ("فيل", "elephant", "\U0001f418"),
 "ق": ("قمر", "moon", "\U0001f319"),
 "ك": ("كتاب", "book", "\U0001f4d6"),
 "ل": ("لبن", "milk", "\U0001f95b"),
 "م": ("موز", "banana", "\U0001f34c"),
 "ن": ("نمر", "tiger", "\U0001f405"),
 "ه": ("هدهد", "hoopoe", "\U0001f426"),
 "و": ("وردة", "rose", "\U0001f339"),
 "ي": ("يد", "hand", "✋"),
}

WORDS["farsi"] = {
 "ا": ("آب", "water", "\U0001f4a7"),
 "ب": ("بادام", "almond", "\U0001f330"),
 "پ": ("پرنده", "bird", "\U0001f426"),
 "ت": ("توپ", "ball", "⚽"),
 "ث": ("ثانیه", "second (time)", "⏱️"),
 "ج": ("جوجه", "chick", "\U0001f425"),
 "چ": ("چتر", "umbrella", "☂️"),
 "ح": ("حلزون", "snail", "\U0001f40c"),
 "خ": ("خرس", "bear", "\U0001f43b"),
 "د": ("درخت", "tree", "\U0001f333"),
 "ذ": ("ذرت", "corn", "\U0001f33d"),
 "ر": ("روباه", "fox", "\U0001f98a"),
 "ز": ("زنبور", "bee", "\U0001f41d"),
 "ژ": ("ژاله", "hail", "\U0001f327️"),
 "س": ("سیب", "apple", "\U0001f34e"),
 "ش": ("شیر", "lion", "\U0001f981"),
 "ص": ("صدف", "seashell", "\U0001f41a"),
 "ض": ("ضبط صوت", "recorder", "\U0001f3a4"),
 "ط": ("طناب", "rope", "\U0001faa2"),
 "ظ": ("ظرف", "dish", "\U0001f37d️"),
 "ع": ("عسل", "honey", "\U0001f36f"),
 "غ": ("غاز", "goose", "\U0001fabf"),
 "ف": ("فیل", "elephant", "\U0001f418"),
 "ق": ("قاشق", "spoon", "\U0001f944"),
 "ک": ("کتاب", "book", "\U0001f4d6"),
 "گ": ("گل", "flower", "\U0001f33a"),
 "ل": ("لیمو", "lemon", "\U0001f34b"),
 "م": ("ماهی", "fish", "\U0001f41f"),
 "ن": ("نان", "bread", "\U0001f35e"),
 "و": ("وان", "van", "\U0001f690"),
 "ه": ("هویج", "carrot", "\U0001f955"),
 "ی": ("یخ", "ice", "\U0001f9ca"),
}

WORDS["devanagari"] = {
 "अ": ("अनार", "pomegranate", None),
 "आ": ("आम", "mango", "\U0001f96d"),
 "इ": ("इमली", "tamarind", "\U0001fad8"),
 "ई": ("ईख", "sugarcane", "\U0001f33e"),
 "उ": ("उल्लू", "owl", "\U0001f989"),
 "ऊ": ("ऊन", "wool", "\U0001f9f6"),
 "ऋ": ("ऋषि", "sage", "\U0001f9d8"),
 "ए": ("एडी", "heel", "\U0001f9b6"),
 "ऐ": ("ऐनक", "spectacles", "\U0001f453"),
 "ओ": ("ओखली", "mortar", "\U0001fad9"),
 "औ": ("औज़ार", "tool", "\U0001f528"),
 "क": ("कमल", "lotus", "\U0001fab7"),
 "ख": ("खरगोश", "rabbit", "\U0001f407"),
 "ग": ("गाय", "cow", "\U0001f404"),
 "घ": ("घड़ी", "clock", "⌚"),
 "ङ": ("ङ", "nasal (no initial word)", "\U0001f4ac"),
 "च": ("चम्मच", "spoon", "\U0001f944"),
 "छ": ("छतरी", "umbrella", "☂️"),
 "ज": ("जहाज", "ship", "\U0001f6a2"),
 "झ": ("झंडा", "flag", "\U0001f6a9"),
 "ञ": ("ञ", "nasal (no initial word)", "\U0001f4ac"),
 "ट": ("टमाटर", "tomato", "\U0001f345"),
 "ठ": ("ठठेरा", "tinsmith", "\U0001f528"),
 "ड": ("डमरू", "drum", "\U0001fa98"),
 "ढ": ("ढक्कन", "lid", "\U0001fad9"),
 "ण": ("ण", "retroflex nasal (no initial word)", "\U0001f4ac"),
 "त": ("तरबूज", "watermelon", "\U0001f349"),
 "थ": ("थैला", "bag", "\U0001f45c"),
 "द": ("दवाई", "medicine", "\U0001f48a"),
 "ध": ("धनुष", "bow", "\U0001f3f9"),
 "न": ("नल", "tap", "\U0001f6b0"),
 "प": ("पतंग", "kite", "\U0001fa81"),
 "फ": ("फल", "fruit", "\U0001f34f"),
 "ब": ("बकरी", "goat", "\U0001f410"),
 "भ": ("भालू", "bear", "\U0001f43b"),
 "म": ("मछली", "fish", "\U0001f41f"),
 "य": ("यज्ञ", "fire ritual", "\U0001f525"),
 "र": ("रथ", "chariot", "\U0001f6de"),
 "ल": ("लट्टू", "spinning top", "\U0001fa80"),
 "व": ("वन", "forest", "\U0001f332"),
 "श": ("शेर", "lion", "\U0001f981"),
 "ष": ("षटकोण", "hexagon", "⬢"),
 "स": ("सेब", "apple", "\U0001f34e"),
 "ह": ("हाथी", "elephant", "\U0001f418"),
}

WORDS["cyrillic"] = {
 "А": ("арбуз", "watermelon", "\U0001f349"),
 "Б": ("барабан", "drum", "\U0001fa98"),
 "В": ("волк", "wolf", "\U0001f43a"),
 "Г": ("груша", "pear", "\U0001f350"),
 "Д": ("дерево", "tree", "\U0001f333"),
 "Е": ("ель", "spruce", "\U0001f332"),
 "Ё": ("ёж", "hedgehog", "\U0001f994"),
 "Ж": ("жираф", "giraffe", "\U0001f992"),
 "З": ("зонт", "umbrella", "☂️"),
 "И": ("иголка", "needle", "\U0001faa1"),
 "Й": ("йогурт", "yoghurt", "\U0001f95b"),
 "К": ("карандаш", "pencil", "✏️"),
 "Л": ("лошадь", "horse", "\U0001f40e"),
 "М": ("мышь", "mouse", "\U0001f401"),
 "Н": ("ножницы", "scissors", "✂️"),
 "О": ("обезьяна", "monkey", "\U0001f412"),
 "П": ("помидор", "tomato", "\U0001f345"),
 "Р": ("рыба", "fish", "\U0001f41f"),
 "С": ("слон", "elephant", "\U0001f418"),
 "Т": ("телефон", "telephone", "☎️"),
 "У": ("улитка", "snail", "\U0001f40c"),
 "Ф": ("фонарь", "lantern", "\U0001f526"),
 "Х": ("хлеб", "bread", "\U0001f35e"),
 "Ц": ("цветок", "flower", "\U0001f33c"),
 "Ч": ("чайник", "kettle", "\U0001fad6"),
 "Ш": ("шарф", "scarf", "\U0001f9e3"),
 "Щ": ("щётка", "brush", "\U0001faa5"),
 "Ъ": ("подъезд", "entrance (letter never starts a word)", "\U0001f6aa"),
 "Ы": ("сыр", "cheese (letter never starts a word)", "\U0001f9c0"),
 "Ь": ("соль", "salt (letter never starts a word)", "\U0001f9c2"),
 "Э": ("экскаватор", "excavator", "\U0001f6a7"),
 "Ю": ("юла", "spinning top", "\U0001fa80"),
 "Я": ("яблоко", "apple", "\U0001f34e"),
}

# Thai is the strongest case: the convention is the letter name itself.
# "ก ไก่" is read "ko kai" and ไก่ means chicken.
WORDS["thai"] = {
 "ก": ("ไก่", "chicken", "🐔"), "ข": ("ไข่", "egg", "🥚"),
 "ฃ": ("ขวด", "bottle (obsolete letter)", "🍶"), "ค": ("ควาย", "buffalo", "🐃"),
 "ฅ": ("คน", "person (obsolete letter)", "🧍"), "ฆ": ("ระฆัง", "bell", "🔔"),
 "ง": ("งู", "snake", "🐍"), "จ": ("จาน", "plate", "🍽️"),
 "ฉ": ("ฉิ่ง", "cymbals", "🎵"), "ช": ("ช้าง", "elephant", "🐘"),
 "ซ": ("โซ่", "chain", "⛓️"), "ฌ": ("เฌอ", "tree", "🌳"),
 "ญ": ("หญิง", "woman", "👩"), "ฎ": ("ชฎา", "headdress", "👑"),
 "ฏ": ("ปฏัก", "goad", "🪝"), "ฐ": ("ฐาน", "base", "🧱"),
 "ฑ": ("มณโฑ", "Montho", "👸"), "ฒ": ("ผู้เฒ่า", "elder", "🧓"),
 "ณ": ("เณร", "novice monk", "🧘"), "ด": ("เด็ก", "child", "🧒"),
 "ต": ("เต่า", "turtle", "🐢"), "ถ": ("ถุง", "bag", "🛍️"),
 "ท": ("ทหาร", "soldier", "🪖"), "ธ": ("ธง", "flag", "🚩"),
 "น": ("หนู", "mouse", "🐁"), "บ": ("ใบไม้", "leaf", "🍃"),
 "ป": ("ปลา", "fish", "🐟"), "ผ": ("ผึ้ง", "bee", "🐝"),
 "ฝ": ("ฝา", "lid", "🫙"), "พ": ("พาน", "tray", "🏺"),
 "ฟ": ("ฟัน", "teeth", "🦷"), "ภ": ("สำเภา", "junk ship", "⛵"),
 "ม": ("ม้า", "horse", "🐎"), "ย": ("ยักษ์", "giant", "👹"),
 "ร": ("เรือ", "boat", "🚣"), "ล": ("ลิง", "monkey", "🐒"),
 "ว": ("แหวน", "ring", "💍"), "ศ": ("ศาลา", "pavilion", "⛩️"),
 "ษ": ("ฤๅษี", "hermit", "🧙"), "ส": ("เสือ", "tiger", "🐅"),
 "ห": ("หีบ", "chest", "🧰"), "ฬ": ("จุฬา", "kite", "🪁"),
 "อ": ("อ่าง", "basin", "🛁"), "ฮ": ("นกฮูก", "owl", "🦉"),
}

WORDS["kana"] = {
 "あ": ("あり", "ant", "🐜"), "い": ("いぬ", "dog", "🐕"), "う": ("うま", "horse", "🐎"),
 "え": ("えんぴつ", "pencil", "✏️"), "お": ("おに", "ogre", "👹"),
 "か": ("かさ", "umbrella", "☂️"), "き": ("きつね", "fox", "🦊"), "く": ("くま", "bear", "🐻"),
 "け": ("けむし", "caterpillar", "🐛"), "こ": ("こま", "spinning top", "🪀"),
 "さ": ("さかな", "fish", "🐟"), "し": ("しか", "deer", "🦌"), "す": ("すいか", "watermelon", "🍉"),
 "せ": ("せみ", "cicada", "🦗"), "そ": ("そら", "sky", "🌤️"),
 "た": ("たこ", "octopus", "🐙"), "ち": ("ちず", "map", "🗺️"), "つ": ("つき", "moon", "🌙"),
 "て": ("て", "hand", "✋"), "と": ("とり", "bird", "🐦"),
 "な": ("なす", "aubergine", "🍆"), "に": ("にわとり", "chicken", "🐔"), "ぬ": ("ぬいぐるみ", "stuffed toy", "🧸"),
 "ね": ("ねこ", "cat", "🐈"), "の": ("のり", "seaweed", "🍙"),
 "は": ("はな", "flower", "🌸"), "ひ": ("ひこうき", "aeroplane", "✈️"), "ふ": ("ふね", "boat", "⛵"),
 "へ": ("へび", "snake", "🐍"), "ほ": ("ほし", "star", "⭐"),
 "ま": ("まど", "window", "🪟"), "み": ("みず", "water", "💧"), "む": ("むし", "insect", "🐛"),
 "め": ("め", "eye", "👁️"), "も": ("もも", "peach", "🍑"),
 "や": ("やま", "mountain", "⛰️"), "ゆ": ("ゆき", "snow", "❄️"), "よ": ("よる", "night", "🌃"),
 "ら": ("らくだ", "camel", "🐫"), "り": ("りんご", "apple", "🍎"), "る": ("るすばん", "house-sitting", "🏠"),
 "れ": ("れいぞうこ", "refrigerator", "🧊"), "ろ": ("ろうそく", "candle", "🕯️"),
 "わ": ("わに", "crocodile", "🐊"), "を": ("を", "object particle, never starts a word", "💬"),
 "ん": ("みかん", "mandarin — ん never starts a word", "🍊"),
}

WORDS["hangul"] = {
 "ㄱ": ("가방", "bag", "🎒"), "ㄴ": ("나무", "tree", "🌳"), "ㄷ": ("달", "moon", "🌙"),
 "ㄹ": ("라면", "ramen", "🍜"), "ㅁ": ("물", "water", "💧"), "ㅂ": ("바다", "sea", "🌊"),
 "ㅅ": ("사과", "apple", "🍎"), "ㅇ": ("아기", "baby", "👶"), "ㅈ": ("자동차", "car", "🚗"),
 "ㅊ": ("책", "book", "📖"), "ㅋ": ("코", "nose", "👃"), "ㅌ": ("토끼", "rabbit", "🐰"),
 "ㅍ": ("포도", "grapes", "🍇"), "ㅎ": ("해", "sun", "☀️"),
 "ㅏ": ("사과", "apple", "🍎"), "ㅑ": ("야구", "baseball", "⚾"),
 "ㅓ": ("어머니", "mother", "👩"), "ㅕ": ("여우", "fox", "🦊"),
 "ㅗ": ("오리", "duck", "🦆"), "ㅛ": ("요리", "cooking", "🍳"),
 "ㅜ": ("우산", "umbrella", "☂️"), "ㅠ": ("유리", "glass", "🥛"),
 "ㅡ": ("그림", "picture", "🖼️"), "ㅣ": ("이", "tooth", "🦷"),
}

# Amharic does not teach letters this way at all: children learn the fidel
# grid of consonant x vowel order. These are ordinary words beginning with
# each consonant, not a traditional primer set.
WORDS["geez"] = {
 "ሀ": ("ሀገር", "country", "🏳️"), "ለ": ("ለውዝ", "nut", "🥜"), "ሐ": ("ሐይቅ", "lake", "🏞️"),
 "መ": ("መኪና", "car", "🚗"), "ሠ": ("ሠርግ", "wedding", "💒"), "ረ": ("ረሃብ", "hunger", "🍽️"),
 "ሰ": ("ሰው", "person", "🧍"), "ሸ": ("ሸማ", "cloth", "🧵"), "ቀ": ("ቀን", "day", "☀️"),
 "በ": ("በር", "door", "🚪"), "ተ": ("ተራራ", "mountain", "⛰️"), "ቸ": ("ቸኮሌት", "chocolate", "🍫"),
 "ኀ": ("ኀይል", "power", "⚡"), "ነ": ("ነብር", "leopard", "🐆"), "ኘ": ("ኘ", "rare letter", "💬"),
 "አ": ("አንበሳ", "lion", "🦁"), "ከ": ("ከተማ", "city", "🏙️"), "ኸ": ("ኸ", "rare letter", "💬"),
 "ወ": ("ወተት", "milk", "🥛"), "ዐ": ("ዐይን", "eye", "👁️"), "ዘ": ("ዘይት", "oil", "🫒"),
 "ዠ": ("ዠ", "rare letter", "💬"), "የ": ("የበግ", "of sheep", "🐑"),
 "ደ": ("ደብተር", "notebook", "📓"), "ጀ": ("ጀልባ", "boat", "⛵"), "ገ": ("ገበያ", "market", "🏪"),
 "ጠ": ("ጠረጴዛ", "table", "🪑"), "ጨ": ("ጨረቃ", "moon", "🌙"), "ጰ": ("ጰ", "rare letter", "💬"),
 "ጸ": ("ጸሐይ", "sun", "☀️"), "ፀ": ("ፀጉር", "hair", "💇"), "ፈ": ("ፈረስ", "horse", "🐎"),
 "ፐ": ("ፐርሙዝ", "thermos", "🫗"),
}

# Pinyin has no primer tradition — it is a romanisation, taught as syllables.
# These are common words whose pinyin begins with each letter. Note i, o and u
# never begin a pinyin syllable; those are written yi, wo, wu.
WORDS["pinyin"] = {
 "A": ("爱 (ài)", "love", "❤️"), "B": ("包 (bāo)", "bag", "🎒"), "C": ("草 (cǎo)", "grass", "🌿"),
 "D": ("灯 (dēng)", "lamp", "💡"), "E": ("鹅 (é)", "goose", "🪿"), "F": ("飞机 (fēijī)", "aeroplane", "✈️"),
 "G": ("狗 (gǒu)", "dog", "🐕"), "H": ("花 (huā)", "flower", "🌸"),
 "I": ("衣 (yī)", "clothes — i never begins a syllable", "👕"),
 "J": ("鸡 (jī)", "chicken", "🐔"), "K": ("咖啡 (kāfēi)", "coffee", "☕"),
 "L": ("龙 (lóng)", "dragon", "🐉"), "M": ("猫 (māo)", "cat", "🐈"), "N": ("牛 (niú)", "cow", "🐄"),
 "O": ("藕 (ǒu)", "lotus root", "🪷"), "P": ("苹果 (píngguǒ)", "apple", "🍎"),
 "Q": ("球 (qiú)", "ball", "⚽"), "R": ("人 (rén)", "person", "🧍"),
 "S": ("伞 (sǎn)", "umbrella", "☂️"), "T": ("兔子 (tùzi)", "rabbit", "🐰"),
 "U": ("屋 (wū)", "house — u never begins a syllable", "🏠"),
 "V": ("绿 (lǜ)", "green — v stands for ü, no native v sound", "💚"),
 "W": ("碗 (wǎn)", "bowl", "🥣"), "X": ("熊猫 (xióngmāo)", "panda", "🐼"),
 "Y": ("鱼 (yú)", "fish", "🐟"), "Z": ("字 (zì)", "character", "🔤"),
 "ZH": ("猪 (zhū)", "pig", "🐷"), "CH": ("车 (chē)", "car", "🚗"),
 "SH": ("书 (shū)", "book", "📖"),
 "NG": ("羊 (yáng)", "sheep — ng never begins a syllable", "🐑"),
}

# ---------------------------------------------------------------------------
# IPA values per letter, keyed by script slug then glyph.
#
# These are the values in the modern standard language named on each page, not
# historical ones: Modern Greek rather than Ancient, Modern Israeli Hebrew,
# Modern Standard Arabic, Standard Hindi, Standard Russian. Where a letter has
# more than one regular value, all are listed: Russian vowels reduce when
# unstressed, Greek and Russian consonants palatalise before front vowels,
# Thai consonants differ as initials and finals, and English letters are simply
# many-to-many.
# ---------------------------------------------------------------------------

IPA = {}

IPA["latin"] = {
 "A": ["æ", "eɪ", "ɑː"], "B": ["b"], "C": ["k", "s"], "D": ["d"],
 "E": ["ɛ", "iː", "ə"], "F": ["f"], "G": ["ɡ", "dʒ"], "H": ["h"],
 "I": ["ɪ", "aɪ"], "J": ["dʒ"], "K": ["k"], "L": ["l"], "M": ["m"],
 "N": ["n", "ŋ"], "O": ["ɒ", "oʊ"], "P": ["p"], "Q": ["k"], "R": ["ɹ"],
 "S": ["s", "z"], "T": ["t"], "U": ["ʌ", "juː", "ʊ"], "V": ["v"],
 "W": ["w"], "X": ["ks", "z"], "Y": ["j", "aɪ", "i"], "Z": ["z"],
}

IPA["hebrew"] = {
 "א": ["ʔ", "—"], "ב": ["b", "v"], "ג": ["ɡ"], "ד": ["d"], "ה": ["h", "—"],
 "ו": ["v", "o", "u"], "ז": ["z"], "ח": ["χ"], "ט": ["t"], "י": ["j", "i"],
 "כ": ["k", "χ"], "ל": ["l"], "מ": ["m"], "נ": ["n"], "ס": ["s"],
 "ע": ["ʔ", "—"], "פ": ["p", "f"], "צ": ["ts"], "ק": ["k"], "ר": ["ʁ"],
 "ש": ["ʃ", "s"], "ת": ["t"],
}

IPA["arabic"] = {
 "ا": ["aː", "ʔ"], "ب": ["b"], "ت": ["t"], "ث": ["θ"], "ج": ["d͡ʒ", "ʒ", "ɡ"],
 "ح": ["ħ"], "خ": ["x"], "د": ["d"], "ذ": ["ð"], "ر": ["r"], "ز": ["z"],
 "س": ["s"], "ش": ["ʃ"], "ص": ["sˤ"], "ض": ["dˤ"], "ط": ["tˤ"], "ظ": ["ðˤ"],
 "ع": ["ʕ"], "غ": ["ɣ"], "ف": ["f"], "ق": ["q"], "ك": ["k"], "ل": ["l"],
 "م": ["m"], "ن": ["n"], "ه": ["h"], "و": ["w", "uː"], "ي": ["j", "iː"],
}

IPA["farsi"] = {
 "ا": ["ɒː", "ʔ"], "ب": ["b"], "پ": ["p"], "ت": ["t"], "ث": ["s"],
 "ج": ["d͡ʒ"], "چ": ["t͡ʃ"], "ح": ["h"], "خ": ["x"], "د": ["d"], "ذ": ["z"],
 "ر": ["ɾ"], "ز": ["z"], "ژ": ["ʒ"], "س": ["s"], "ش": ["ʃ"], "ص": ["s"],
 "ض": ["z"], "ط": ["t"], "ظ": ["z"], "ع": ["ʔ"], "غ": ["ɣ", "ɢ"], "ف": ["f"],
 "ق": ["ɣ", "ɢ"], "ک": ["k"], "گ": ["ɡ"], "ل": ["l"], "م": ["m"], "ن": ["n"],
 "و": ["v", "uː", "o"], "ه": ["h", "e"], "ی": ["j", "iː"],
}

IPA["devanagari"] = {
 "अ": ["ə"], "आ": ["aː"], "इ": ["ɪ"], "ई": ["iː"], "उ": ["ʊ"], "ऊ": ["uː"],
 "ऋ": ["ɾɪ"], "ए": ["eː"], "ऐ": ["ɛː"], "ओ": ["oː"], "औ": ["ɔː"],
 "क": ["k"], "ख": ["kʰ"], "ग": ["ɡ"], "घ": ["ɡʱ"], "ङ": ["ŋ"],
 "च": ["t͡ʃ"], "छ": ["t͡ʃʰ"], "ज": ["d͡ʒ"], "झ": ["d͡ʒʱ"], "ञ": ["ɲ"],
 "ट": ["ʈ"], "ठ": ["ʈʰ"], "ड": ["ɖ"], "ढ": ["ɖʱ"], "ण": ["ɳ"],
 "त": ["t̪"], "थ": ["t̪ʰ"], "द": ["d̪"], "ध": ["d̪ʱ"], "न": ["n"],
 "प": ["p"], "फ": ["pʰ", "f"], "ब": ["b"], "भ": ["bʱ"], "म": ["m"],
 "य": ["j"], "र": ["ɾ"], "ल": ["l"], "व": ["ʋ"],
 "श": ["ʃ"], "ष": ["ʂ", "ʃ"], "स": ["s"], "ह": ["ɦ"],
}

IPA["cyrillic"] = {
 "А": ["a", "ɐ"], "Б": ["b", "bʲ"], "В": ["v", "vʲ"], "Г": ["ɡ", "ɡʲ"],
 "Д": ["d", "dʲ"], "Е": ["je", "ʲe"], "Ё": ["jo", "ʲo"], "Ж": ["ʐ"],
 "З": ["z", "zʲ"], "И": ["i"], "Й": ["j"], "К": ["k", "kʲ"], "Л": ["ɫ", "lʲ"],
 "М": ["m", "mʲ"], "Н": ["n", "nʲ"], "О": ["o", "ɐ"], "П": ["p", "pʲ"],
 "Р": ["r", "rʲ"], "С": ["s", "sʲ"], "Т": ["t", "tʲ"], "У": ["u"],
 "Ф": ["f", "fʲ"], "Х": ["x"], "Ц": ["t͡s"], "Ч": ["t͡ɕ"], "Ш": ["ʂ"],
 "Щ": ["ɕː"], "Ъ": ["—"], "Ы": ["ɯ"], "Ь": ["ʲ"], "Э": ["e"],
 "Ю": ["ju", "ʲu"], "Я": ["ja", "ʲa"],
}

# Thai consonants take one value as a syllable initial and often a different,
# unreleased one as a final; both are given where they differ.
IPA["thai"] = {
 "ก": ["k", "k̚"], "ข": ["kʰ", "k̚"], "ฃ": ["kʰ", "k̚"], "ค": ["kʰ", "k̚"],
 "ฅ": ["kʰ", "k̚"], "ฆ": ["kʰ", "k̚"], "ง": ["ŋ"], "จ": ["t͡ɕ", "t̚"],
 "ฉ": ["t͡ɕʰ"], "ช": ["t͡ɕʰ", "t̚"], "ซ": ["s", "t̚"], "ฌ": ["t͡ɕʰ"],
 "ญ": ["j", "n"], "ฎ": ["d", "t̚"], "ฏ": ["t", "t̚"], "ฐ": ["tʰ", "t̚"],
 "ฑ": ["tʰ", "d", "t̚"], "ฒ": ["tʰ", "t̚"], "ณ": ["n"], "ด": ["d", "t̚"],
 "ต": ["t", "t̚"], "ถ": ["tʰ", "t̚"], "ท": ["tʰ", "t̚"], "ธ": ["tʰ", "t̚"],
 "น": ["n"], "บ": ["b", "p̚"], "ป": ["p", "p̚"], "ผ": ["pʰ"],
 "ฝ": ["f"], "พ": ["pʰ", "p̚"], "ฟ": ["f", "p̚"], "ภ": ["pʰ", "p̚"],
 "ม": ["m"], "ย": ["j"], "ร": ["r", "n"], "ล": ["l", "n"], "ว": ["w"],
 "ศ": ["s", "t̚"], "ษ": ["s", "t̚"], "ส": ["s", "t̚"], "ห": ["h"],
 "ฬ": ["l", "n"], "อ": ["ʔ"], "ฮ": ["h"],
}

IPA["kana"] = {
 "あ": ["a"], "い": ["i"], "う": ["ɯ"], "え": ["e"], "お": ["o"],
 "か": ["ka"], "き": ["ki"], "く": ["kɯ"], "け": ["ke"], "こ": ["ko"],
 "さ": ["sa"], "し": ["ɕi"], "す": ["sɯ"], "せ": ["se"], "そ": ["so"],
 "た": ["ta"], "ち": ["t͡ɕi"], "つ": ["t͡sɯ"], "て": ["te"], "と": ["to"],
 "な": ["na"], "に": ["ɲi"], "ぬ": ["nɯ"], "ね": ["ne"], "の": ["no"],
 "は": ["ha", "wa"], "ひ": ["çi"], "ふ": ["ɸɯ"], "へ": ["he", "e"], "ほ": ["ho"],
 "ま": ["ma"], "み": ["mi"], "む": ["mɯ"], "め": ["me"], "も": ["mo"],
 "や": ["ja"], "ゆ": ["jɯ"], "よ": ["jo"],
 "ら": ["ɾa"], "り": ["ɾi"], "る": ["ɾɯ"], "れ": ["ɾe"], "ろ": ["ɾo"],
 "わ": ["wa"], "を": ["o"], "ん": ["n", "m", "ŋ", "ɴ"],
}

# Korean stops differ as syllable initials and finals; both are given.
IPA["hangul"] = {
 "ㄱ": ["k", "ɡ", "k̚"], "ㄴ": ["n"], "ㄷ": ["t", "d", "t̚"], "ㄹ": ["ɾ", "l"],
 "ㅁ": ["m"], "ㅂ": ["p", "b", "p̚"], "ㅅ": ["s", "ɕ", "t̚"], "ㅇ": ["—", "ŋ"],
 "ㅈ": ["t͡ɕ", "d͡ʑ", "t̚"], "ㅊ": ["t͡ɕʰ", "t̚"], "ㅋ": ["kʰ", "k̚"],
 "ㅌ": ["tʰ", "t̚"], "ㅍ": ["pʰ", "p̚"], "ㅎ": ["h", "t̚"],
 "ㅏ": ["a"], "ㅑ": ["ja"], "ㅓ": ["ʌ"], "ㅕ": ["jʌ"], "ㅗ": ["o"],
 "ㅛ": ["jo"], "ㅜ": ["u"], "ㅠ": ["ju"], "ㅡ": ["ɯ"], "ㅣ": ["i"],
}

# Ge'ez letters are listed in their first order, which carries the vowel ä.
# Values given are the Amharic consonant plus that vowel.
IPA["geez"] = {
 "ሀ": ["hä"], "ለ": ["lä"], "ሐ": ["hä"], "መ": ["mä"], "ሠ": ["sä"],
 "ረ": ["rä"], "ሰ": ["sä"], "ሸ": ["ʃä"], "ቀ": ["kʼä"], "በ": ["bä"],
 "ተ": ["tä"], "ቸ": ["t͡ʃä"], "ኀ": ["hä"], "ነ": ["nä"], "ኘ": ["ɲä"],
 "አ": ["ʔä"], "ከ": ["kä"], "ኸ": ["xä"], "ወ": ["wä"], "ዐ": ["ʔä"],
 "ዘ": ["zä"], "ዠ": ["ʒä"], "የ": ["jä"], "ደ": ["dä"], "ጀ": ["d͡ʒä"],
 "ገ": ["ɡä"], "ጠ": ["tʼä"], "ጨ": ["t͡ʃʼä"], "ጰ": ["pʼä"], "ጸ": ["t͡sʼä"],
 "ፀ": ["t͡sʼä"], "ፈ": ["fä"], "ፐ": ["pä"],
}

# Pinyin letters stand for different sounds as initials and as finals; the
# initial value is given first. The four digraphs are retroflex initials.
IPA["pinyin"] = {
 "A": ["a"], "B": ["p"], "C": ["t͡sʰ"], "D": ["t"], "E": ["ɤ", "ə"],
 "F": ["f"], "G": ["k"], "H": ["x"], "I": ["i", "ɹ̩"], "J": ["t͡ɕ"],
 "K": ["kʰ"], "L": ["l"], "M": ["m"], "N": ["n", "ŋ"], "O": ["o", "u̯o"],
 "P": ["pʰ"], "Q": ["t͡ɕʰ"], "R": ["ʐ", "ɻ"], "S": ["s"], "T": ["tʰ"],
 "U": ["u"], "V": ["y"], "W": ["w"], "X": ["ɕ"], "Y": ["j"], "Z": ["t͡s"],
 "ZH": ["ʈ͡ʂ"], "CH": ["ʈ͡ʂʰ"], "SH": ["ʂ"], "NG": ["ŋ"],
}

# ---------------------------------------------------------------------------
# Pronunciation of each example word, so the word can be read before its
# letters are known. Broad transcription in the modern standard language.
# ---------------------------------------------------------------------------

WORD_IPA = {}

WORD_IPA["latin"] = {
 "A": "ˈæpəl", "B": "bɔːl", "C": "kæt", "D": "dɒɡ", "E": "ˈɛlɪfənt",
 "F": "fɪʃ", "G": "dʒɪˈrɑːf", "H": "haʊs", "I": "ˈaɪs kriːm", "J": "dʒʌɡ",
 "K": "kaɪt", "L": "ˈlaɪən", "M": "muːn", "N": "nɛst", "O": "ˈɒrɪndʒ",
 "P": "ˈpɛnsəl", "Q": "kwiːn", "R": "ˈreɪnbəʊ", "S": "sʌn", "T": "triː",
 "U": "ʌmˈbrɛlə", "V": "ˌvaɪəˈlɪn", "W": "weɪl", "X": "ˈzaɪləfəʊn",
 "Y": "jɒt", "Z": "ˈzɛbrə",
}

WORD_IPA["greek"] = {
 "Α": "aˈsteri", "Β": "viˈvlio", "Γ": "ˈɣata", "Δ": "ˈðendro",
 "Ε": "eˈlefandas", "Ζ": "ˈzevra", "Η": "ˈilios", "Θ": "ˈθeatro",
 "Ι": "ipoˈpotamos", "Κ": "karˈðʝa", "Λ": "luˈluði", "Μ": "ˈmilo",
 "Ν": "neˈro", "Ξ": "ˈksilo", "Ο": "uraˈnos", "Π": "puˈli",
 "Ρ": "roˈloi", "Σ": "ˈspiti", "Τ": "ˈtreno", "Υ": "ipoloʝiˈstis",
 "Φ": "feˈɡari", "Χ": "ˈçeri", "Ψ": "ˈpsari", "Ω": "okeaˈnos",
}

WORD_IPA["hebrew"] = {
 "א": "aʁˈje", "ב": "ˈbajit", "ג": "ɡaˈmal", "ד": "daɡ", "ה": "haʁ",
 "ו": "ˈveʁed", "ז": "ˈzebʁa", "ח": "χaˈtul", "ט": "ˈtelefon", "י": "ˈjeled",
 "כ": "ˈkelev", "ל": "liˈmon", "מ": "ˈmajim", "נ": "neʁ", "ס": "sus",
 "ע": "ʕets", "פ": "ˈpeʁaχ", "צ": "tsiˈpoʁ", "ק": "kof", "ר": "raˈkevet",
 "ש": "ˈʃemeʃ", "ת": "taˈpuaχ",
}

WORD_IPA["arabic"] = {
 "ا": "ˈʔarnab", "ب": "ˈbatˤtˤa", "ت": "tufˈfaːħa", "ث": "ˈθaʕlab",
 "ج": "ˈdʒamal", "ح": "ħiˈsˤaːn", "خ": "xubz", "د": "daˈdʒaːdʒa",
 "ذ": "ðiʔb", "ر": "rumˈmaːn", "ز": "zaˈraːfa", "س": "ˈsamaka",
 "ش": "ʃams", "ص": "sˤaqr", "ض": "ˈdˤifdaʕ", "ط": "tˤaːʔiˈra",
 "ظ": "ðˤarf", "ع": "ˈʕinab", "غ": "ɣaˈzaːl", "ف": "fiːl", "ق": "ˈqamar",
 "ك": "kiˈtaːb", "ل": "ˈlaban", "م": "mawz", "ن": "ˈnamir", "ه": "ˈhudhud",
 "و": "ˈwarda", "ي": "jad",
}

WORD_IPA["farsi"] = {
 "ا": "ɒːb", "ب": "bɒːˈdɒːm", "پ": "parandeˈje", "ت": "tuːp", "ث": "sɒːniˈje",
 "ج": "dʒuːˈdʒe", "چ": "tʃætɾ", "ح": "hælæˈzuːn", "خ": "xeɾs", "د": "deˈɾæxt",
 "ذ": "zoˈɾæt", "ر": "ɾuːˈbɒːh", "ز": "zænˈbuːɾ", "ژ": "ʒɒːˈle", "س": "siːb",
 "ش": "ʃiːɾ", "ص": "sæˈdæf", "ض": "ˈzæbte sowt", "ط": "tæˈnɒːb", "ظ": "zæɾf",
 "ع": "æˈsæl", "غ": "ɣɒːz", "ف": "fiːl", "ق": "ɢɒːˈʃoɢ", "ک": "keˈtɒːb",
 "گ": "ɡol", "ل": "liːˈmuː", "م": "mɒːˈhiː", "ن": "nɒːn", "و": "vɒːn",
 "ه": "hæˈviːdʒ", "ی": "jæx",
}

WORD_IPA["cyrillic"] = {
 "А": "ɐrˈbus", "Б": "bərɐˈban", "В": "volk", "Г": "ˈɡruʂə", "Д": "ˈdʲerʲɪvə",
 "Е": "jelʲ", "Ё": "joʂ", "Ж": "ʐɯˈraf", "З": "zont", "И": "ɪˈɡolkə",
 "Й": "ˈjoɡurt", "К": "kərɐnˈdaʂ", "Л": "ˈloʂətʲ", "М": "mɯʂ",
 "Н": "ˈnoʐnʲɪtsɯ", "О": "ɐbʲɪˈzʲjanə", "П": "pəmʲɪˈdor", "Р": "ˈrɯbə",
 "С": "slon", "Т": "tʲɪlʲɪˈfon", "У": "ʊˈlʲitkə", "Ф": "fɐˈnarʲ", "Х": "xlʲep",
 "Ц": "tsvʲɪˈtok", "Ч": "ˈtɕajnʲɪk", "Ш": "ʂarf", "Щ": "ˈɕːɵtkə",
 "Ъ": "pɐdˈjest", "Ы": "sɯr", "Ь": "solʲ", "Э": "ɛkskɐˈvatər", "Ю": "jʊˈla",
 "Я": "ˈjablələkə",
}

WORD_IPA["devanagari"] = {
 "अ": "ənaːr", "आ": "aːm", "इ": "ɪmliː", "ई": "iːkʰ", "उ": "ʊlluː",
 "ऊ": "uːn", "ऋ": "ɾɪʃi", "ए": "eːɽiː", "ऐ": "ɛːnək", "ओ": "oːkʰliː",
 "औ": "ɔːzaːr", "क": "kəməl", "ख": "kʰəɾɡoːʃ", "ग": "ɡaːj", "घ": "ɡʱəɽiː",
 "ङ": "ŋə", "च": "tʃəmmətʃ", "छ": "tʃʰətɾiː", "ज": "dʒəhaːz", "झ": "dʒʱəɳɖaː",
 "ञ": "ɲə", "ट": "ʈəmaːʈəɾ", "ठ": "ʈʰəʈʰeːɾaː", "ड": "ɖəmɾuː", "ढ": "ɖʱəkkən",
 "ण": "ɳə", "त": "təɾbuːdʒ", "थ": "tʰɛːlaː", "द": "dəʋaːiː", "ध": "dʱənʊʃ",
 "न": "nəl", "प": "pətəŋɡ", "फ": "pʰəl", "ब": "bəkɾiː", "भ": "bʱaːluː",
 "म": "mətʃʰliː", "य": "jəɡjə", "र": "rətʰ", "ल": "ləʈʈuː", "व": "ʋən",
 "श": "ʃeːɾ", "ष": "ʂəʈkoːɳ", "स": "seːb", "ह": "haːtʰiː",
}

WORD_IPA["geez"] = {
 "ሀ": "haɡər", "ለ": "lewz", "ሐ": "hajk", "መ": "mekina", "ሠ": "sərɡ",
 "ረ": "rehab", "ሰ": "səw", "ሸ": "ʃema", "ቀ": "kʼen", "በ": "ber",
 "ተ": "terara", "ቸ": "tʃokolet", "ኀ": "hajl", "ነ": "nebr", "ኘ": "ɲä",
 "አ": "anbesa", "ከ": "ketema", "ኸ": "xä", "ወ": "wetet", "ዐ": "ajn",
 "ዘ": "zejt", "ዠ": "ʒä", "የ": "jebeɡ", "ደ": "debter", "ጀ": "dʒelba",
 "ገ": "ɡebeja", "ጠ": "tʼerepʼeza", "ጨ": "tʃʼereka", "ጰ": "pʼä",
 "ጸ": "tsʼehaj", "ፀ": "tsʼeɡur", "ፈ": "feres", "ፐ": "permuz",
}

WORD_IPA["thai"] = {
 "ก": "kàj", "ข": "kʰàj", "ฃ": "kʰùat", "ค": "kʰwaːj", "ฅ": "kʰon",
 "ฆ": "rákʰaŋ", "ง": "ŋuː", "จ": "tɕaːn", "ฉ": "tɕʰìŋ", "ช": "tɕʰáːŋ",
 "ซ": "soː", "ฌ": "tɕʰɤː", "ญ": "jǐŋ", "ฎ": "tɕʰadaː", "ฏ": "patàk",
 "ฐ": "tʰǎːn", "ฑ": "montʰoː", "ฒ": "pʰûːtʰâw", "ณ": "neːn", "ด": "dèk",
 "ต": "tàw", "ถ": "tʰǔŋ", "ท": "tʰahǎːn", "ธ": "tʰoŋ", "น": "nǔː",
 "บ": "bajmáj", "ป": "plaː", "ผ": "pʰɯ̂ŋ", "ฝ": "fǎː", "พ": "pʰaːn",
 "ฟ": "fan", "ภ": "sǎmpʰaw", "ม": "máː", "ย": "ják", "ร": "rɯa",
 "ล": "liŋ", "ว": "wɛ̌ːn", "ศ": "sǎːlaː", "ษ": "rɯːsǐː", "ส": "sɯ̌a",
 "ห": "hìːp", "ฬ": "tɕulaː", "อ": "àːŋ", "ฮ": "nók hûːk",
}

WORD_IPA["kana"] = {
 "あ": "aɾi", "い": "inɯ", "う": "ɯma", "え": "empitsɯ", "お": "oni",
 "か": "kasa", "き": "kitsɯne", "く": "kɯma", "け": "kemɯʃi", "こ": "koma",
 "さ": "sakana", "し": "ʃika", "す": "sɯika", "せ": "semi", "そ": "soɾa",
 "た": "tako", "ち": "tɕizɯ", "つ": "tsɯki", "て": "te", "と": "toɾi",
 "な": "nasɯ", "に": "niwatoɾi", "ぬ": "nɯiɡɯɾɯmi", "ね": "neko", "の": "noɾi",
 "は": "hana", "ひ": "çikoːki", "ふ": "ɸɯne", "へ": "hebi", "ほ": "hoʃi",
 "ま": "mado", "み": "mizɯ", "む": "mɯʃi", "め": "me", "も": "momo",
 "や": "jama", "ゆ": "jɯki", "よ": "joɾɯ", "ら": "ɾakɯda", "り": "ɾiŋɡo",
 "る": "ɾɯsɯbaɴ", "れ": "ɾeːzoːko", "ろ": "ɾoːsokɯ", "わ": "wani",
 "を": "o", "ん": "mikaɴ",
}

WORD_IPA["hangul"] = {
 "ㄱ": "kabaŋ", "ㄴ": "namu", "ㄷ": "tal", "ㄹ": "ɾamjʌn", "ㅁ": "mul",
 "ㅂ": "pada", "ㅅ": "sagwa", "ㅇ": "agi", "ㅈ": "tɕadoŋtɕʰa", "ㅊ": "tɕʰɛk",
 "ㅋ": "kʰo", "ㅌ": "tʰokki", "ㅍ": "pʰodo", "ㅎ": "hɛ",
 "ㅏ": "sagwa", "ㅑ": "jagu", "ㅓ": "ʌmʌni", "ㅕ": "jʌu", "ㅗ": "oɾi",
 "ㅛ": "joɾi", "ㅜ": "usan", "ㅠ": "juɾi", "ㅡ": "kɯɾim", "ㅣ": "i",
}

WORD_IPA["pinyin"] = {
 "A": "aɪ̯˥˩", "B": "paʊ̯˥", "C": "tsʰaʊ̯˨˩˦", "D": "tɤŋ˥", "E": "ɤ˧˥",
 "F": "feɪ̯˥ tɕi˥", "G": "koʊ̯˨˩˦", "H": "xwa˥", "I": "i˥", "J": "tɕi˥",
 "K": "kʰa˥ feɪ̯˥", "L": "lʊŋ˧˥", "M": "maʊ̯˥", "N": "njoʊ̯˧˥", "O": "oʊ̯˨˩˦",
 "P": "pʰiŋ˧˥ kwo˨˩˦", "Q": "tɕʰjoʊ̯˧˥", "R": "ʐən˧˥", "S": "san˨˩˦",
 "T": "tʰu˥˩ t͡sɯ", "U": "u˥", "V": "ly˥˩", "W": "wan˨˩˦",
 "X": "ɕjʊŋ˧˥ maʊ̯˥", "Y": "y˧˥", "Z": "t͡sɯ˥˩", "ZH": "ʈʂu˥",
 "CH": "ʈʂʰɤ˥", "SH": "ʂu˥", "NG": "jaŋ˧˥",
}

# ---------------------------------------------------------------------------
# Why a letter has more than one value. Only where a rule is worth knowing:
# these are the common cases a learner meets early, not a full phonology.
# ---------------------------------------------------------------------------

NOTES = {}

NOTES["greek"] = {
 "Γ": "Palatal /ʝ/ before the ε and ι sounds (ε αι ι η υ ει οι), velar /ɣ/ before α ο ω ου and consonants.",
 "Κ": "Palatal /c/ before the ε and ι sounds, plain /k/ elsewhere.",
 "Λ": "Palatal /ʎ/ before ι, plain /l/ elsewhere.",
 "Ν": "Palatal /ɲ/ before ι, plain /n/ elsewhere.",
 "Χ": "Palatal /ç/ before the ε and ι sounds — χέρι /ˈçeri/ — and velar /x/ before α ο ω ου or a consonant, as in χαρά /xaˈra/.",
 "Σ": "Voices to /z/ before a voiced consonant: κόσμος /ˈkozmos/. Written ς at the end of a word.",
}

NOTES["latin"] = {
 "C": "Soft /s/ before e, i or y (city); hard /k/ elsewhere (cat).",
 "G": "Often soft /dʒ/ before e, i or y (gem); hard /ɡ/ elsewhere (go). Less reliable than c.",
 "S": "/z/ between vowels and in most plurals (rose, dogs); /s/ otherwise.",
 "X": "/ks/ normally (box), /z/ word-initially (xylophone).",
 "A": "Long /eɪ/ before a silent e (cake), short /æ/ otherwise (cat).",
}

NOTES["cyrillic"] = {
 "А": "Reduces to /ɐ/ when unstressed.",
 "О": "Reduces to /ɐ/ when unstressed, so молоко is /məlɐˈko/ — only the stressed о is a clear /o/.",
 "Е": "Softens the consonant before it, and reduces towards /ɪ/ when unstressed.",
 "Б": "Devoices to /p/ at the end of a word; palatalised before е ё и ю я ь.",
 "В": "Devoices to /f/ at the end of a word.",
 "Г": "Devoices to /k/ at the end of a word.",
 "Д": "Devoices to /t/ at the end of a word.",
 "Ж": "Always hard, never palatalised, and devoices to /ʂ/ finally.",
 "З": "Devoices to /s/ at the end of a word.",
 "Ь": "No sound of its own: it palatalises the consonant before it.",
 "Ъ": "No sound of its own: it blocks palatalisation across a prefix boundary.",
}

NOTES["hebrew"] = {
 "ב": "Stop /b/ at the start of a word or with a dagesh; fricative /v/ otherwise.",
 "כ": "Stop /k/ with a dagesh, fricative /χ/ otherwise. Written ך at the end of a word.",
 "פ": "Stop /p/ with a dagesh, fricative /f/ otherwise. Written ף at the end of a word.",
 "ו": "Consonant /v/, or a vowel marker for /o/ and /u/.",
 "א": "Usually silent in modern speech; historically a glottal stop.",
 "ע": "Silent for most modern speakers; a pharyngeal in Mizrahi pronunciation.",
 "ש": "/ʃ/ with the dot on the right, /s/ with it on the left.",
}

NOTES["arabic"] = {
 "ا": "Carries a long /aː/, or supports a hamza for the glottal stop.",
 "و": "Consonant /w/, or a long /uː/.",
 "ي": "Consonant /j/, or a long /iː/.",
 "ل": "In the article الـ, the l assimilates to a following sun letter: الشمس is ash-shams, not al-shams.",
 "ج": "/dʒ/ in Modern Standard, but /ʒ/ in the Levant and /ɡ/ in Cairo.",
 "ق": "/q/ in Modern Standard; often /ʔ/ in city speech and /ɡ/ in Bedouin and Gulf speech.",
}

NOTES["farsi"] = {
 "ث": "One of three letters written differently but all said /s/: ث س ص.",
 "ذ": "One of four letters all said /z/: ذ ز ض ظ. The spellings are inherited from Arabic.",
 "ط": "Said /t/, the same as ت, despite the different letter.",
 "ق": "Merged with غ for most speakers, as /ɣ/ between vowels and /ɢ/ elsewhere.",
 "و": "Consonant /v/, or a long /uː/, or /o/ in a few common words.",
 "ه": "/h/ as a consonant; word-final it usually marks /e/ instead.",
}

NOTES["devanagari"] = {
 "अ": "Every consonant carries this vowel unless marked otherwise. In Hindi the final one is dropped: कमल is kamal, not kamala.",
 "क": "Unaspirated: hold the breath back. ख is the aspirated pair.",
 "फ": "/pʰ/ in native words, but /f/ in loans such as फोन.",
 "ड": "Retroflex — tongue curled back, not the dental द.",
 "त": "Dental: tongue on the teeth, not the ridge behind them as in English t.",
}

NOTES["kana"] = {
 "は": "Said /wa/ when it marks the topic of a sentence, /ha/ otherwise.",
 "へ": "Said /e/ when it marks direction, /he/ otherwise.",
 "を": "Only used to mark the object, and pronounced the same as お.",
 "ん": "Takes its place from what follows: /m/ before p b m, /ŋ/ before k g, /n/ elsewhere.",
 "し": "/ɕi/, not /si/ — the s row is irregular here.",
 "ち": "/tɕi/, not /ti/.",
 "つ": "/tsɯ/, not /tu/.",
 "ふ": "/ɸɯ/, closer to a soft blown f than an English h or f.",
}

NOTES["hangul"] = {
 "ㄱ": "/k/ at the start, /ɡ/ between vowels, and an unreleased /k̚/ at the end of a syllable.",
 "ㄷ": "/t/ initially, /d/ between vowels, unreleased /t̚/ finally.",
 "ㅂ": "/p/ initially, /b/ between vowels, unreleased /p̚/ finally.",
 "ㅈ": "/t͡ɕ/ initially, /d͡ʑ/ between vowels, unreleased finally.",
 "ㅇ": "Silent at the start of a syllable, where it is only a placeholder; /ŋ/ at the end.",
 "ㄹ": "A flap /ɾ/ between vowels, closer to /l/ at the end of a syllable.",
 "ㅅ": "/ɕ/ before i and y sounds, /s/ otherwise, and unreleased /t̚/ finally.",
}

NOTES["thai"] = {
 "ก": "Finals are unreleased: the sound is stopped without a release of breath.",
 "ร": "Becomes /n/ when it closes a syllable.",
 "ล": "Also becomes /n/ when it closes a syllable.",
 "ฃ": "Obsolete, replaced by ข. Kept in the alphabet for completeness.",
 "ฅ": "Obsolete, replaced by ค.",
 "อ": "A silent carrier at the start of a vowel-initial syllable, not a consonant of its own.",
 "ห": "Written before a low-class consonant to raise its tone class rather than to be pronounced.",
}

NOTES["pinyin"] = {
 "I": "After zh ch sh r z c s this is not /i/ but a buzzed continuation of the consonant, /ɹ̩/.",
 "U": "After j q x y it is /y/, the ü sound, since those consonants never take a plain u.",
 "V": "Not a Chinese sound. Typed for ü, and kept in the fingerspelling scheme.",
 "E": "/ɤ/ alone, but /e/ in the combinations ie and üe.",
 "NG": "Only ever closes a syllable, never begins one.",
}

NOTES["geez"] = {
 "ሀ": "Listed in first order, carrying the vowel ä. Each consonant has seven orders, one per vowel, written as changes to the same base shape.",
}

# ---------------------------------------------------------------------------
# Wikipedia pointers. Two kinds: a few per script, and a glossary of the
# technical terms used in the notes, linked on first mention so the notes can
# stay short without being opaque.
# ---------------------------------------------------------------------------

W = "https://en.wikipedia.org/wiki/"

LINKS = {
 "greek": [("Greek alphabet", W+"Greek_alphabet"),
           ("Greek Sign Language", W+"Greek_Sign_Language"),
           ("Modern Greek phonology", W+"Modern_Greek_phonology")],
 "latin": [("Latin alphabet", W+"Latin_alphabet"),
           ("American Sign Language", W+"American_Sign_Language"),
           ("British Sign Language", W+"British_Sign_Language"),
           ("Fingerspelling", W+"Fingerspelling")],
 "hebrew": [("Hebrew alphabet", W+"Hebrew_alphabet"),
            ("Israeli Sign Language", W+"Israeli_Sign_Language"),
            ("Abjad", W+"Abjad")],
 "arabic": [("Arabic alphabet", W+"Arabic_alphabet"),
            ("Arabic sign languages", W+"Arab_sign-language_family"),
            ("Arabic phonology", W+"Arabic_phonology")],
 "farsi": [("Persian alphabet", W+"Persian_alphabet"),
           ("Iranian Sign Language", W+"Iranian_Sign_Language"),
           ("Persian phonology", W+"Persian_phonology")],
 "devanagari": [("Devanagari", W+"Devanagari"),
                ("Nepalese Sign Language", W+"Nepalese_Sign_Language"),
                ("Abugida", W+"Abugida")],
 "cyrillic": [("Cyrillic script", W+"Cyrillic_script"),
              ("Russian Sign Language", W+"Russian_Sign_Language"),
              ("Russian phonology", W+"Russian_phonology")],
 "kana": [("Kana", W+"Kana"), ("Japanese Sign Language", W+"Japanese_Sign_Language"),
          ("Gojūon", W+"Goj%C5%ABon")],
 "hangul": [("Hangul", W+"Hangul"), ("Korean Sign Language", W+"Korean_Sign_Language"),
            ("Korean phonology", W+"Korean_phonology")],
 "thai": [("Thai script", W+"Thai_script"), ("Thai Sign Language", W+"Thai_Sign_Language"),
          ("Thai phonology", W+"Thai_phonology")],
 "geez": [("Ge'ez script", W+"Ge%CA%BDez_script"),
          ("Ethiopian Sign Language", W+"Ethiopian_Sign_Language"),
          ("Amharic", W+"Amharic")],
 "pinyin": [("Pinyin", W+"Pinyin"), ("Chinese Sign Language", W+"Chinese_Sign_Language"),
            ("Standard Chinese phonology", W+"Standard_Chinese_phonology")],
}

# Term -> article. Matched case-insensitively on whole words, first hit only.
GLOSSARY = {
 "palatal": W+"Palatalization_(phonetics)",
 "palatalises": W+"Palatalization_(phonetics)",
 "palatalised": W+"Palatalization_(phonetics)",
 "palatalisation": W+"Palatalization_(phonetics)",
 "devoices": W+"Final-obstruent_devoicing",
 "reduces": W+"Vowel_reduction",
 "dagesh": W+"Dagesh",
 "sun letter": W+"Sun_and_moon_letters",
 "glottal stop": W+"Glottal_stop",
 "pharyngeal": W+"Pharyngeal_consonant",
 "retroflex": W+"Retroflex_consonant",
 "dental": W+"Dental_consonant",
 "aspirated": W+"Aspirated_consonant",
 "unaspirated": W+"Aspirated_consonant",
 "unreleased": W+"Unreleased_stop",
 "flap": W+"Flap_consonant",
 "tone class": W+"Thai_script#Consonants",
 "hamza": W+"Hamza",
 "jamo": W+"Hangul#Letters",
 "schwa": W+"Schwa_deletion_in_Indo-Aryan_languages",
 "topic": W+"Topic_marker",
}

# ---------------------------------------------------------------------------
# Each IPA symbol to its Wikipedia article. Longest match wins, so affricates
# and the digraphs written with a tie bar resolve before their parts.
# ---------------------------------------------------------------------------

IPA_LINKS = {
 # affricates and clusters first
 "t͡ɕʰ": W+"Voiceless_alveolo-palatal_affricate", "t͡ɕ": W+"Voiceless_alveolo-palatal_affricate",
 "d͡ʑ": W+"Voiced_alveolo-palatal_affricate",
 "ʈ͡ʂʰ": W+"Voiceless_retroflex_affricate", "ʈ͡ʂ": W+"Voiceless_retroflex_affricate",
 "t͡ʃʰ": W+"Voiceless_postalveolar_affricate", "t͡ʃʼ": W+"Ejective_consonant",
 "t͡ʃ": W+"Voiceless_postalveolar_affricate", "d͡ʒʱ": W+"Breathy_voice",
 "d͡ʒ": W+"Voiced_postalveolar_affricate", "t͡sʼ": W+"Ejective_consonant",
 "t͡s": W+"Voiceless_alveolar_affricate", "ks": W+"Consonant_cluster",
 "ps": W+"Consonant_cluster",
 # aspirated and ejective stops
 "kʰ": W+"Aspirated_consonant", "pʰ": W+"Aspirated_consonant", "tʰ": W+"Aspirated_consonant",
 "ʈʰ": W+"Voiceless_retroflex_plosive", "t̪ʰ": W+"Voiceless_dental_and_alveolar_plosives",
 "kʼ": W+"Ejective_consonant", "pʼ": W+"Ejective_consonant", "tʼ": W+"Ejective_consonant",
 "ɡʱ": W+"Breathy_voice", "bʱ": W+"Breathy_voice", "d̪ʱ": W+"Breathy_voice",
 "ɖʱ": W+"Breathy_voice", "ɖ": W+"Voiced_retroflex_plosive", "ʈ": W+"Voiceless_retroflex_plosive",
 "t̪": W+"Voiceless_dental_and_alveolar_plosives", "d̪": W+"Voiced_dental_and_alveolar_plosives",
 # emphatics
 "sˤ": W+"Pharyngealization", "dˤ": W+"Pharyngealization",
 "tˤ": W+"Pharyngealization", "ðˤ": W+"Pharyngealization",
 # unreleased
 "k̚": W+"Unreleased_stop", "t̚": W+"Unreleased_stop", "p̚": W+"Unreleased_stop",
 # palatalised
 "bʲ": W+"Palatalization_(phonetics)", "vʲ": W+"Palatalization_(phonetics)",
 "ɡʲ": W+"Palatalization_(phonetics)", "dʲ": W+"Palatalization_(phonetics)",
 "zʲ": W+"Palatalization_(phonetics)", "kʲ": W+"Palatalization_(phonetics)",
 "lʲ": W+"Palatalization_(phonetics)", "mʲ": W+"Palatalization_(phonetics)",
 "nʲ": W+"Palatalization_(phonetics)", "pʲ": W+"Palatalization_(phonetics)",
 "rʲ": W+"Palatalization_(phonetics)", "sʲ": W+"Palatalization_(phonetics)",
 "tʲ": W+"Palatalization_(phonetics)", "fʲ": W+"Palatalization_(phonetics)",
 "ʲ": W+"Palatalization_(phonetics)",
 # long vowels and diphthongs
 "aː": W+"Open_front_unrounded_vowel", "iː": W+"Close_front_unrounded_vowel",
 "uː": W+"Close_back_rounded_vowel", "eː": W+"Close-mid_front_unrounded_vowel",
 "oː": W+"Close-mid_back_rounded_vowel", "ɔː": W+"Open-mid_back_rounded_vowel",
 "ɛː": W+"Open-mid_front_unrounded_vowel", "ɒː": W+"Open_back_rounded_vowel",
 "ɑː": W+"Open_back_unrounded_vowel", "juː": W+"Palatal_approximant",
 "eɪ": W+"Diphthong", "aɪ": W+"Diphthong", "oʊ": W+"Diphthong", "əʊ": W+"Diphthong",
 "aʊ": W+"Diphthong", "ja": W+"Palatal_approximant", "je": W+"Palatal_approximant",
 "jo": W+"Palatal_approximant", "ju": W+"Palatal_approximant",
 "ɹ̩": W+"Syllabic_consonant", "ɕː": W+"Gemination",
 # single consonants
 "p": W+"Voiceless_bilabial_plosive", "b": W+"Voiced_bilabial_plosive",
 "t": W+"Voiceless_dental_and_alveolar_plosives", "d": W+"Voiced_dental_and_alveolar_plosives",
 "k": W+"Voiceless_velar_plosive", "ɡ": W+"Voiced_velar_plosive",
 "q": W+"Voiceless_uvular_plosive", "ʔ": W+"Glottal_stop",
 "c": W+"Voiceless_palatal_plosive", "f": W+"Voiceless_labiodental_fricative",
 "v": W+"Voiced_labiodental_fricative", "θ": W+"Voiceless_dental_fricative",
 "ð": W+"Voiced_dental_fricative", "s": W+"Voiceless_alveolar_fricative",
 "z": W+"Voiced_alveolar_fricative", "ʃ": W+"Voiceless_postalveolar_fricative",
 "ʒ": W+"Voiced_postalveolar_fricative", "ʂ": W+"Voiceless_retroflex_fricative",
 "ʐ": W+"Voiced_retroflex_fricative", "ɕ": W+"Voiceless_alveolo-palatal_fricative",
 "ç": W+"Voiceless_palatal_fricative", "x": W+"Voiceless_velar_fricative",
 "ɣ": W+"Voiced_velar_fricative", "χ": W+"Voiceless_uvular_fricative",
 "ʁ": W+"Voiced_uvular_fricative", "ħ": W+"Voiceless_pharyngeal_fricative",
 "ʕ": W+"Voiced_pharyngeal_fricative", "h": W+"Voiceless_glottal_fricative",
 "ɦ": W+"Voiced_glottal_fricative", "ɸ": W+"Voiceless_bilabial_fricative",
 "m": W+"Bilabial_nasal", "n": W+"Alveolar_nasal", "ɲ": W+"Palatal_nasal",
 "ŋ": W+"Velar_nasal", "ɴ": W+"Uvular_nasal", "ɳ": W+"Retroflex_nasal",
 "l": W+"Voiced_alveolar_lateral_approximant", "ɫ": W+"Velarization",
 "ʎ": W+"Palatal_lateral_approximant", "r": W+"Alveolar_trill",
 "ɾ": W+"Alveolar_flap", "ɹ": W+"Alveolar_approximant", "ɻ": W+"Retroflex_approximant",
 "j": W+"Palatal_approximant", "w": W+"Voiced_labial–velar_approximant",
 "ʋ": W+"Labiodental_approximant", "ɢ": W+"Voiced_uvular_plosive",
 # single vowels
 "a": W+"Open_front_unrounded_vowel", "e": W+"Close-mid_front_unrounded_vowel",
 "i": W+"Close_front_unrounded_vowel", "o": W+"Close-mid_back_rounded_vowel",
 "u": W+"Close_back_rounded_vowel", "y": W+"Close_front_rounded_vowel",
 "ə": W+"Mid_central_vowel", "ɛ": W+"Open-mid_front_unrounded_vowel",
 "ɔ": W+"Open-mid_back_rounded_vowel", "æ": W+"Near-open_front_unrounded_vowel",
 "ʌ": W+"Open-mid_back_unrounded_vowel", "ɒ": W+"Open_back_rounded_vowel",
 "ɑ": W+"Open_back_unrounded_vowel", "ɪ": W+"Near-close_front_unrounded_vowel",
 "ʊ": W+"Near-close_back_rounded_vowel", "ɤ": W+"Close-mid_back_unrounded_vowel",
 "ɯ": W+"Close_back_unrounded_vowel", "ɯ": W+"Close_central_unrounded_vowel",
 "ɐ": W+"Near-open_central_vowel", "ä": W+"Open_central_unrounded_vowel",
 "—": None,
}

# ---------------------------------------------------------------------------
# Ge'ez full syllabary. Ethiopic is laid out in blocks of eight codepoints:
# seven vowel orders then a labialised form. Each row is one consonant, each
# column one vowel, so a syllable's glyph, romanisation and IPA all follow
# from its base consonant and its order.
# ---------------------------------------------------------------------------

GEEZ_ORDERS = [
    ("ä", "ə"),   # 1st  ä
    ("u", "u"),             # 2nd
    ("i", "i"),             # 3rd
    ("a", "a"),             # 4th
    ("e", "e"),             # 5th
    ("ə", "ɯ"),   # 6th  often silent
    ("o", "o"),             # 7th
]

# base consonant -> (romanisation, IPA). The order vowel is appended.
GEEZ_CONSONANTS = [
    ("ሀ", "h", "h"),   ("ለ", "l", "l"),   ("ሐ", "ḥ", "h"),
    ("መ", "m", "m"),   ("ሠ", "ś", "s"), ("ረ", "r", "r"),
    ("ሰ", "s", "s"),   ("ሸ", "š", "ʃ"), ("ቀ", "q", "kʼ"),
    ("በ", "b", "b"),   ("ተ", "t", "t"),   ("ቸ", "č", "t͡ʃ"),
    ("ኀ", "ḫ", "h"), ("ነ", "n", "n"), ("ኘ", "ñ", "ɲ"),
    ("አ", "ʾ", "ʔ"), ("ከ", "k", "k"), ("ኸ", "ḵ", "x"),
    ("ወ", "w", "w"),   ("ዐ", "ʿ", "ʔ"), ("ዘ", "z", "z"),
    ("ዠ", "ž", "ʒ"), ("የ", "y", "j"), ("ደ", "d", "d"),
    ("ጀ", "ǵ", "d͡ʒ"), ("ገ", "g", "ɡ"),
    ("ጠ", "ṭ", "tʼ"), ("ጨ", "čʼ", "t͡ʃʼ"),
    ("ጰ", "ṗ", "pʼ"), ("ጸ", "ṣ", "t͡sʼ"),
    ("ፀ", "ḍ", "t͡sʼ"), ("ፈ", "f", "f"), ("ፐ", "p", "p"),
]

# Words for syllables beyond the first order. Amharic does not teach the fidel
# with a word per cell the way an alphabet primer does, so these are ordinary
# common nouns chosen where one begins with that syllable, not a standard set.
GEEZ_EXTRA_WORDS = {
 "ሁ": ("ሁለት", "two", "✌️"),       "ሂ": ("ሂሳብ", "arithmetic", "🧮"),
 "ሃ": ("ሃሳብ", "idea", "💡"),       "ሆ": ("ሆድ", "stomach", "🫃"),
 "ላ": ("ላም", "cow", "🐄"),         "ሊ": ("ሊጥ", "dough", "🥟"),
 "ሙ": ("ሙዝ", "banana", "🍌"),      "ማ": ("ማር", "honey", "🍯"),
 "ሚ": ("ሚስት", "wife", "👰"),       "ሞ": ("ሞተር", "engine", "⚙️"),
 "ሩ": ("ሩዝ", "rice", "🍚"),        "ራ": ("ራስ", "head", "🗣️"),
 "ሱ": ("ሱሪ", "trousers", "👖"),     "ሳ": ("ሳር", "grass", "🌿"),
 "ሺ": ("ሺህ", "thousand", "🔢"),     "ቡ": ("ቡና", "coffee", "☕"),
 "ባ": ("ባቡር", "train", "🚆"),       "ቤ": ("ቤት", "house", "🏠"),
 "ቢ": ("ቢላ", "knife", "🔪"),        "ቶ": ("ቶሎ", "quickly", "🏃"),
 "ታ": ("ታሪክ", "history", "📜"),     "ቲ": ("ቲማቲም", "tomato", "🍅"),
 "ኑ": ("ኑሮ", "living", "🏡"),       "ና": ("ናት", "she is", "💬"),
 "ኢ": ("ኢትዮጵያ", "Ethiopia", "🇪🇹"), "ኡ": ("ኡደት", "cycle", "🔄"),
 "ኩ": ("ኩባያ", "cup", "🍵"),         "ካ": ("ካርታ", "map", "🗺️"),
 "ኮ": ("ኮከብ", "star", "⭐"),        "ኪ": ("ኪስ", "pocket", "👖"),
 "ዋ": ("ዋና", "swimming", "🏊"),     "ዉ": ("ዉሃ", "water", "💧"),
 "ዛ": ("ዛፍ", "tree", "🌳"),         "ዙ": ("ዙሪያ", "surroundings", "🔄"),
 "ያ": ("ያዝ", "hold", "✊"),          "ዩ": ("ዩኒቨርሲቲ", "university", "🎓"),
 "ዳ": ("ዳቦ", "bread", "🍞"),        "ዶ": ("ዶሮ", "chicken", "🐔"),
 "ዲ": ("ዲሽ", "dish", "🍽️"),        "ጋ": ("ጋዜጣ", "newspaper", "📰"),
 "ጉ": ("ጉዞ", "journey", "🧳"),      "ጎ": ("ጎመን", "cabbage", "🥬"),
 "ጤ": ("ጤና", "health", "💚"),       "ጣ": ("ጣፋጭ", "sweet", "🍬"),
 "ጮ": ("ጮማ", "fatty meat", "🥩"),   "ፊ": ("ፊት", "face", "😊"),
 "ፋ": ("ፋብሪካ", "factory", "🏭"),    "ፖ": ("ፖሊስ", "police", "👮"),
 "ሰው": ("ሰው", "person", "🧍"),
}

# Second pass on the fidel. Amharic has no word-per-cell tradition, so these
# are ordinary nouns. Where a syllable does not begin words, a word that
# contains it is used instead and the card bolds it in place.
GEEZ_EXTRA_WORDS.update({
 # ሀ h
 "ሄ": ("ሄሊኮፕተር", "helicopter", "🚁"), "ህ": ("ህይወት", "life", "🌱"),
 # ለ l
 "ሉ": ("ሉል", "pearl", "🦪"), "ሌ": ("ሌሊት", "night", "🌙"),
 "ል": ("ልብስ", "clothes", "👕"), "ሎ": ("ሎሚ", "lemon", "🍋"),
 # ሐ ḥ — merged with ሀ in speech, so mostly historical spellings
 "ሕ": ("ሕግ", "law", "⚖️"), "ሓ": ("ሓምሌ", "July", "📅"),
 # መ m
 "ሜ": ("ሜዳ", "field", "🌾"), "ም": ("ምግብ", "food", "🍲"),
 # ረ r
 "ሪ": ("ሪፖርት", "report", "📄"), "ሬ": ("ሬድዮ", "radio", "📻"),
 "ር": ("ርግብ", "dove", "🕊️"), "ሮ": ("ሮማን", "pomegranate", "🫒"),
 # ሰ s
 "ሲ": ("ሲኒ", "coffee cup", "☕"), "ሴ": ("ሴት", "woman", "👩"),
 "ስ": ("ስም", "name", "🏷️"), "ሶ": ("ሶፋ", "sofa", "🛋️"),
 # ሸ š
 "ሹ": ("ሹካ", "fork", "🍴"), "ሻ": ("ሻይ", "tea", "🍵"),
 "ሼ": ("ሼፍ", "chef", "👨‍🍳"), "ሽ": ("ሽንኩርት", "onion", "🧅"),
 "ሾ": ("ሾርባ", "soup", "🍜"),
 # ቀ q
 "ቁ": ("ቁልፍ", "key", "🔑"), "ቂ": ("ቂጣ", "flatbread", "🫓"),
 "ቃ": ("ቃል", "word", "💬"), "ቄ": ("ቄስ", "priest", "⛪"),
 "ቅ": ("ቅቤ", "butter", "🧈"), "ቆ": ("ቆዳ", "hide", "🧳"),
 # በ b
 "ብ": ("ብርሃን", "light", "💡"), "ቦ": ("ቦርሳ", "bag", "👜"),
 # ተ t
 "ቱ": ("ቱታ", "tracksuit", "🩳"), "ቴ": ("ቴሌቪዥን", "television", "📺"),
 "ት": ("ትምህርት", "education", "📚"),
 # ቸ č
 "ቻ": ("ቻርጀር", "charger", "🔌"), "ች": ("ችግር", "problem", "⚠️"),
 # ነ n
 "ኒ": ("ኒሻን", "medal", "🏅"), "ኔ": ("ኔትወርክ", "network", "🌐"),
 "ን": ("ንብ", "bee", "🐝"), "ኖ": ("ኖራ", "chalk", "🧱"),
 # አ ʾ
 "ኤ": ("ኤሌክትሪክ", "electricity", "⚡"), "እ": ("እንቁላል", "egg", "🥚"),
 "ኦ": ("ኦክስጅን", "oxygen", "🫁"),
 # ከ k
 "ኬ": ("ኬክ", "cake", "🍰"), "ክ": ("ክንድ", "arm", "💪"),
 # ወ w
 "ዊ": ("ዊስኪ", "whisky", "🥃"), "ዌ": ("ዌብሳይት", "website", "🌐"),
 "ው": ("ውሻ", "dog", "🐕"), "ዎ": ("ዎርክሾፕ", "workshop", "🔧"),
 # ዐ ʿ — merged with አ in speech
 "ዓ": ("ዓመት", "year", "📅"), "ዕ": ("ዕቃ", "goods", "📦"),
 # ዘ z
 "ዚ": ("ዚሮ", "zero", "0️⃣"), "ዜ": ("ዜና", "news", "📰"),
 "ዝ": ("ዝናብ", "rain", "🌧️"), "ዞ": ("ዞን", "zone", "🗺️"),
 # የ y
 "ይ": ("ይቅርታ", "apology", "🙏"), "ዮ": ("ዮጋ", "yoga", "🧘"),
 # ደ d
 "ዱ": ("ዱቄት", "flour", "🌾"), "ዴ": ("ዴሞክራሲ", "democracy", "🗳️"),
 "ድ": ("ድመት", "cat", "🐈"),
 # ጀ ǵ
 "ጁ": ("ጁስ", "juice", "🧃"), "ጂ": ("ጂፕ", "jeep", "🚙"),
 "ጃ": ("ጃንጥላ", "umbrella", "☂️"), "ጄ": ("ጄኔራል", "general", "🎖️"),
 "ጅ": ("ጅብ", "hyena", "🐺"), "ጆ": ("ጆሮ", "ear", "👂"),
 # ገ g
 "ጊ": ("ጊዜ", "time", "⏰"), "ጌ": ("ጌጥ", "ornament", "💍"),
 "ግ": ("ግድግዳ", "wall", "🧱"),
 # ጠ ṭ
 "ጡ": ("ጡብ", "brick", "🧱"), "ጢ": ("ጢስ", "smoke", "💨"),
 "ጥ": ("ጥርስ", "tooth", "🦷"), "ጦ": ("ጦር", "spear", "🗡️"),
 # ጨ čʼ
 "ጩ": ("ጩኸት", "shout", "📢"), "ጫ": ("ጫማ", "shoe", "👟"),
 "ጭ": ("ጭራ", "tail", "🐈"),
 # ጸ ṣ
 "ጻ": ("ጻፊ", "writer", "✍️"), "ጽ": ("ጽዋ", "goblet", "🏆"),
 # ፈ f
 "ፉ": ("ፉጨት", "whistle", "📯"), "ፌ": ("ፌስታል", "plastic bag", "🛍️"),
 "ፍ": ("ፍቅር", "love", "❤️"), "ፎ": ("ፎቶ", "photo", "📷"),
 # ፐ p — loanwords almost entirely
 "ፒ": ("ፒያሳ", "piazza", "🏙️"), "ፓ": ("ፓርክ", "park", "🏞️"),
 "ፕ": ("ፕሮግራም", "programme", "💻"),
})

# Rows where Amharic simply does not supply words for most cells. These are
# consonants that merged with another in pronunciation and survive only in
# historical spellings, or that appear almost entirely in loanwords.
GEEZ_RARE_ROWS = {
 "ሐ": "Pronounced the same as ሀ in modern Amharic. It survives in historical spellings, so few cells begin words.",
 "ሠ": "Pronounced the same as ሰ. Kept for etymological spelling, so most cells do not begin words.",
 "ኀ": "Pronounced the same as ሀ. Historical spelling only.",
 "ኘ": "A genuine Amharic sound, but one that rarely begins a word.",
 "ኸ": "Mostly an allophone of ከ; rare word-initially.",
 "ዐ": "Pronounced the same as አ. Historical spelling only.",
 "ዠ": "Rare, and mostly in loanwords.",
 "ጰ": "Almost entirely in words borrowed through Greek, such as ጳውሎስ.",
 "ፀ": "Pronounced the same as ጸ. Historical spelling only.",
}

# Third pass: cells in the merged rows that do have well-established spellings,
# plus a few mid-word syllables. ሆድ/🫃 replaced — no stomach emoji exists.
GEEZ_EXTRA_WORDS.update({
 "ሆ": ("ሆቴል", "hotel", "🏨"),
 "ሣ": ("ሣር", "grass", "🌿"), "ሥ": ("ሥራ", "work", "💼"),
 "ቺ": ("ቺፕስ", "chips", "🍟"), "ቼ": ("ቼክ", "cheque", "🧾"),
 "ኃ": ("ኃይል", "power", "⚡"),
 "ኛ": ("አማርኛ", "Amharic", "🇪🇹"),
 "ዑ": ("ዑደት", "cycle", "🔄"),
 "ጳ": ("ጳጳስ", "bishop", "⛪"),
 "ጺ": ("ጺም", "beard", "🧔"), "ጾ": ("ጾም", "fast", "🍽️"),
})

# Thai writes its vowels as marks around the consonant rather than as letters
# in the alphabet, so the 44 consonants alone cannot spell a word. These are
# the vowel signs, the four tone marks and the two modifiers that complete it.
# (glyph, name, [ipa], (word, gloss, emoji), word_ipa, note)
THAI_VOWELS = [
 ("ะ", "sara a", ["a"], ("กะทิ", "coconut milk", "\U0001f965"), "kàː.tí", None),
 ("ั", "mai han akat", ["a"], ("วัน", "day", "\U0001f4c5"), "wan", None),
 ("า", "sara aa", ["aː"], ("ปลา", "fish", "\U0001f41f"), "plaː", None),
 ("ำ", "sara am", ["am"], ("น้ำ", "water", "\U0001f4a7"), "náːm", "Stands for a vowel plus a final m, which is why it takes a space of its own rather than sitting above the consonant."),
 ("ิ", "sara i", ["i"], ("ดิน", "soil", "\U0001faa8"), "din", None),
 ("ี", "sara ii", ["iː"], ("สี", "colour", "\U0001f3a8"), "sǐː", None),
 ("ึ", "sara ue", ["ɯ"], ("หนึ่ง", "one", "1️⃣"), "nɯ̀ŋ", None),
 ("ื", "sara uee", ["ɯː"], ("มือ", "hand", "✋"), "mɯː", None),
 ("ุ", "sara u", ["u"], ("ลุง", "uncle", "\U0001f468"), "luŋ", None),
 ("ู", "sara uu", ["uː"], ("หมู", "pig", "\U0001f437"), "mǔː", None),
 ("เ", "sara e", ["eː"], ("เสือ", "tiger", "\U0001f405"), "sɯːa", "Written before the consonant it follows in speech."),
 ("แ", "sara ae", ["ɛː"], ("แมว", "cat", "\U0001f408"), "mɛːw", "Written before the consonant it follows in speech."),
 ("โ", "sara o", ["oː"], ("โต๊ะ", "table", "\U0001fa91"), "toːʔ", "Written before the consonant it follows in speech."),
 ("ใ", "sara ai maimuan", ["aj"], ("ใจ", "heart", "❤️"), "tɕaj", "One of only twenty words use this form; everything else takes ไ."),
 ("ไ", "sara ai maimalai", ["aj"], ("ไฟ", "fire", "\U0001f525"), "faj", "Written before the consonant it follows in speech."),
]

THAI_TONES = [
 ("่", "mai ek", ["˨˩"], ("พ่อ", "father", "\U0001f468"), "phôː", "Low tone on a mid or high class consonant, falling on a low class one."),
 ("้", "mai tho", ["˥˩"], ("บ้าน", "house", "\U0001f3e0"), "bâːn", "Falling on a mid or high class consonant, high on a low class one."),
 ("๊", "mai tri", ["˦˥"], ("ตุ๊กตา", "doll", "\U0001f9f8"), "túk.ka.taː", "Used almost only with mid class consonants."),
 ("๋", "mai chattawa", ["˩˩˦"], ("ก๋วยเตี๋ยว", "noodle soup", "\U0001f35c"), "kǔaj.tǐaw", "Used almost only with mid class consonants."),
]

THAI_MODIFIERS = [
 ("็", "mai taikhu", [], ("เป็ด", "duck", "\U0001f986"), "pèt", "Shortens the vowel it sits over."),
 ("์", "thanthakhat", [], ("จันทร์", "moon", "\U0001f319"), "tɕan", "Silences the letter beneath it, usually in a word borrowed from Sanskrit or English."),
]

THAI_SECTIONS = ([("Consonants", None)] +
                 [("Vowel signs", g) for g, *_ in THAI_VOWELS] +
                 [("Tone marks", g) for g, *_ in THAI_TONES] +
                 [("Modifiers", g) for g, *_ in THAI_MODIFIERS])

# Where each mark sits relative to its consonant. The reference shows it on a
# dotted circle so the position is visible rather than described: เ◌ is written
# to the left of the consonant, ◌ั above it, ◌ุ below.
THAI_PREFIX = "\u0e40\u0e41\u0e42\u0e43\u0e44"


def thai_display(glyph):
    import unicodedata
    if glyph in THAI_PREFIX:
        return glyph + "\u25cc"
    if unicodedata.combining(glyph) or unicodedata.category(glyph) == "Mn":
        return "\u25cc" + glyph
    return "\u25cc" + glyph

# The tone letters the Thai tone marks use. They are a scale, not segments,
# so they all point at the one article that explains the staff.
IPA_LINKS.update({
 "˩": W + "Tone_letter", "˨": W + "Tone_letter",
 "˧": W + "Tone_letter", "˦": W + "Tone_letter",
 "˥": W + "Tone_letter",
})

# --- Signs the four alphabets leave out --------------------------------
# Same problem as Thai: the letter list alone cannot spell a word. Each
# entry is (glyph, alt, name, [ipa], (word, gloss, emoji), word_ipa, note).

DEVANAGARI_MATRAS = [
 ("ा","","aa matra",["aː"],("काम","work","\U0001f4bc"),"kaːm",None),
 ("ि","","i matra",["i"],("दिन","day","\U0001f4c5"),"dɪn",
  "Written to the left of its consonant although it is pronounced after it."),
 ("ी","","ii matra",["iː"],("नदी","river","\U0001f3de️"),"nədiː",None),
 ("ु","","u matra",["u"],("सुबह","morning","\U0001f305"),"sʊbah",None),
 ("ू","","uu matra",["uː"],("फूल","flower","\U0001f338"),"pʰuːl",None),
 ("ृ","","ri matra",["r̩i"],("कृषि","agriculture","\U0001f33e"),"kɹ̩ʂi",
  "A Sanskrit vowel, now said as a consonant plus i."),
 ("े","","e matra",["eː"],("मेज़","table","\U0001fa91"),"meːz",None),
 ("ै","","ai matra",["ɛː"],("पैसा","money","\U0001f4b0"),"pɛːsaː",None),
 ("ो","","o matra",["oː"],("मोर","peacock","\U0001f99a"),"moːɾ",None),
 ("ौ","","au matra",["ɔː"],("मौसम","weather","\U0001f326️"),"mɔːsəm",None),
]

DEVANAGARI_MARKS = [
 ("ं","","anusvara",["ⁿ"],("हिंदी","Hindi","\U0001f1ee\U0001f1f3"),"hɪndːiː",
  "Nasalises the vowel, or stands for the nasal that matches the following consonant."),
 ("ँ","","candrabindu",["̃"],("चाँद","moon","\U0001f319"),"tɕãːd",
  "Nasalises the vowel without adding a consonant."),
 ("ः","","visarga",["h"],("दुःख","sorrow","\U0001f622"),"dʊkʰ",
  "A breath after the vowel, almost only in words taken from Sanskrit."),
 ("्","","virama",[],("नमस्ते","greeting","\U0001f64f"),"nəməsteː",
  "Cancels the a that every consonant otherwise carries, so two consonants can meet."),
 ("़","","nukta",[],("ज़मीन","land","\U0001f30d"),"zəmiːn",
  "Adapts a letter to a sound borrowed from Persian, Arabic or English."),
]

KANA_SMALL = [
 ("ぁ","ァ","small a",["a"],("ファイル","file","\U0001f4c1"),"ɟairu",None),
 ("ぃ","ィ","small i",["i"],("ティー","tea","\U0001f375"),"tiː",None),
 ("ぅ","ゥ","small u",["u"],("タトゥー","tattoo","✒️"),"tatuː",None),
 ("ぇ","ェ","small e",["e"],("シェフ","chef","\U0001f468‍\U0001f373"),"ɕeɟu",None),
 ("ぉ","ォ","small o",["o"],("フォーク","fork","\U0001f374"),"ɟoːku",None),
 ("ゃ","ャ","small ya",["ja"],("シャツ","shirt","\U0001f455"),"ɕat͡su",None),
 ("ゅ","ュ","small yu",["ju"],("ジュース","juice","\U0001f9c3"),"dʑuːsu",None),
 ("ょ","ョ","small yo",["jo"],("きょうと","Kyoto","\U0001f3ef"),"kʲoːto",None),
 ("っ","ッ","sokuon",[],("きって","stamp","\U0001f4ee"),"kitte",
  "Doubles the consonant that follows; it is never read on its own."),
]

KANA_MARKS = [
 ("ー","","chouonpu",["ː"],("ラーメン","ramen","\U0001f35c"),"ɾaːmeɴ",
  "Lengthens the vowel before it, in katakana."),
 ("゙","","dakuten",[],("ぞう","elephant","\U0001f418"),"zoː",
  "Voices the kana it sits on: ka becomes ga, sa becomes za."),
 ("゚","","handakuten",[],("パン","bread","\U0001f35e"),"paɴ",
  "Turns the ha row into pa."),
]

ARABIC_FORMS = [
 ("ء","","hamza",["ʔ"],("ماء","water","\U0001f4a7"),"maːʔ",
  "The glottal stop, written on its own or carried by alef, waw or yeh."),
 ("آ","","alef madda",["ʔaː"],("القرآن","the Quran","\U0001f4d6"),"al.qur.ʔaːn",None),
 ("أ","","alef with hamza above",["ʔa"],("أم","mother","\U0001f469"),"ʔumm",None),
 ("إ","","alef with hamza below",["ʔi"],("إسلام","Islam","☪️"),"ʔis.laːm",None),
 ("ؤ","","waw with hamza",["ʔ"],("سؤال","question","❓"),"su.ʔaːl",None),
 ("ئ","","yeh with hamza",["ʔ"],("رئيس","president","\U0001f454"),"ra.ʔiːs",None),
 ("ة","","teh marbuta",["a","at"],("مدرسة","school","\U0001f3eb"),"mad.ra.sa",
  "Ends most feminine nouns. Said as a at the end of a phrase, as t before a following word."),
 ("ى","","alef maqsura",["aː"],("مستشفى","hospital","\U0001f3e5"),"mus.taʃ.faː",
  "A long a written with the shape of yeh."),
]

ARABIC_HARAKAT = [
 ("َ","","fatha",["a"],("كَتَب","he wrote","✍️"),"ka.ta.ba",None),
 ("ُ","","damma",["u"],("كُتُب","books","\U0001f4da"),"ku.tub",None),
 ("ِ","","kasra",["i"],("بِنت","girl","\U0001f467"),"bint",None),
 ("ً","","tanwin fath",["an"],("شكراً","thank you","\U0001f64f"),"ʃuk.ran",
  "The three tanwin marks add a final n, which marks a noun as indefinite."),
 ("ٌ","","tanwin damm",["un"],("كتابٌ","a book","\U0001f4d5"),"ki.taː.bun",None),
 ("ٍ","","tanwin kasr",["in"],("بيتٍ","of a house","\U0001f3e0"),"baj.tin",None),
 ("ْ","","sukun",[],("مِنْ","from","➡️"),"min",
  "Marks a consonant with no vowel after it."),
 ("ّ","","shadda",[],("مدرّس","teacher","\U0001f468‍\U0001f3eb"),"mu.dar.ris",
  "Doubles the consonant it sits on."),
]

HANGUL_DOUBLE = [
 ("ㄲ","","ssang-giyeok",["ˀk"],("꽃","flower","\U0001f338"),"ˀkot","The tense series, said with a tightened glottis: neither voiced like the plain letters nor breathy like the aspirated ones."),
 ("ㄸ","","ssang-digeut",["ˀt"],("딸","daughter","\U0001f467"),"ˀtal",None),
 ("ㅃ","","ssang-bieup",["ˀp"],("빵","bread","\U0001f35e"),"ˀpaŋ",None),
 ("ㅆ","","ssang-siot",["ˀs"],("쌀","uncooked rice","\U0001f33e"),"ˀsal",None),
 ("ㅉ","","ssang-jieut",["ˀt͡ɕ"],("찌개","stew","\U0001f372"),"ˀt͡ɕi.ɡɛ",None),
]

HANGUL_COMPOUND = [
 ("ㅐ","","ae",["ɛ"],("개","dog","\U0001f415"),"kɛ",None),
 ("ㅒ","","yae",["jɛ"],("얘기","talk","\U0001f4ac"),"jɛ.ɡi",None),
 ("ㅔ","","e",["e"],("네","yes","✅"),"ne",
  "Merged with ㅐ in most speech today, which is why spelling them apart has to be learnt."),
 ("ㅖ","","ye",["je"],("예술","art","\U0001f3a8"),"je.sul",None),
 ("ㅘ","","wa",["wa"],("과일","fruit","\U0001f34e"),"kwa.il",None),
 ("ㅙ","","wae",["wɛ"],("왜","why","❓"),"wɛ",None),
 ("ㅚ","","oe",["ø","we"],("외국","foreign country","\U0001f30d"),"we.ɡuk",None),
 ("ㅝ","","wo",["wʌ"],("원","won","\U0001f4b1"),"wʌn",None),
 ("ㅞ","","we",["we"],("\uada4도","orbit","\U0001f6f0️"),"kwe.do",None),
 ("ㅟ","","wi",["wi"],("귀","ear","\U0001f442"),"kwi",None),
 ("ㅢ","","ui",["ɰi"],("의사","doctor","\U0001f468‍⚕️"),"ɰi.sa",None),
]


def display_for(slug, glyph):
    """How a sign is shown when it is standing on its own.

    A mark that normally sits on a letter has nothing to sit on in a list,
    so it is given a base and its position becomes visible: ◌ि to the left,
    ◌ा after, ◌ु below. Letters that stand alone are left as they are.
    """
    import unicodedata
    # kana has real spacing forms of its two marks, so no base is needed
    if slug == "kana":
        return {"\u3099": "\u309b", "\u309a": "\u309c"}.get(glyph)
    # Thai's pre-posed vowels are spacing characters, not combining marks,
    # and showing them to the left of the circle is the whole point
    if slug == "thai" and glyph in THAI_PREFIX:
        return glyph + "\u25cc"
    if slug == "thai" and glyph in "\u0e30\u0e32\u0e33":
        return "\u25cc" + glyph
    if unicodedata.category(glyph) not in ("Mn", "Mc"):
        return None
    # Arabic shows a mark on a tatweel, the connecting stroke. Its fonts
    # carry no dotted circle, so a circle would come from elsewhere and the
    # mark, unable to attach to it, would render as tofu beside it.
    if slug in ("arabic", "farsi"):
        return "\u0640" + glyph
    return "\u25cc" + glyph

IPA_LINKS.update({"ˀ": W + "Glottalization"})
