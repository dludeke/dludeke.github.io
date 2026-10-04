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
 "थ": ("थरमस", "thermos", "\U0001f376"),
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
 "Щ": ["ɕː"], "Ъ": ["—"], "Ы": ["ɨ"], "Ь": ["ʲ"], "Э": ["e"],
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
