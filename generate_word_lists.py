# -*- coding: utf-8 -*-
"""Build the practice word lists, one per script.

Each list is ordinary vocabulary rather than the one-word-per-letter
examples on the reference page, so the same five categories run across
every script: food, geography, cities, culture, nature. They are meant
to be the common words a beginner meets first, not a full lexicon.

Greek and Japanese were written by hand before this script existed and
are carried over unchanged, only reshaped and moved alongside the rest.
"""
import io, json, os

OUT = "assets/data/words"

F, G, C, K, N = "Food & Drinks", "Geography", "Cities", "Culture", "Nature"

# Proper nouns and loanwords whose English "translation" is just the name
# again. Echoing it teaches nothing, so these carry a short gloss instead.
# the 24 letter names, in alphabet order, paired with the letter each names
GREEK_LETTER_NAMES = list(zip(
  ["αλφα", "βητα", "γαμμα", "δελτα", "επσιλον", "ζητα", "ητα", "θητα", "ιωτα", "καππα", "λαμβδα", "μυ", "νυ", "ξι", "ομικρον", "πι", "ρο", "σιγμα", "ταυ", "υψιλον", "φι", "χι", "ψι", "ωμεγα"],
  "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"))

MEANINGS = {
 "greek": dict([(w, "the letter " + c) for w, c in GREEK_LETTER_NAMES] + [
  ("σουβλακι", "grilled meat skewer"),
  ("τζατζικι", "yoghurt and cucumber dip"),
  ("ταραμοσαλατα", "fish roe dip"),
  ("γυρος", "meat roasted on a spit"),
  ("σπανακοπιτα", "spinach pie"),
  ("ντολμαδες", "stuffed vine leaves"),
  ("ουζο", "anise spirit"),
  ("Σπαρτη", "Sparta, the warrior city-state"),
  ("Ολυμπια", "Olympia, home of the games"),
  ("Μαραθων", "Marathon, site of the battle"),
  ("Πυθαγορας", "Pythagoras, mathematician"),
  ("Αρχιμηδης", "Archimedes, inventor"),
  ("Λεωνιδας", "Leonidas, king of Sparta")]),
 "kana": {"すし": "sushi, vinegared rice", "さけ": "rice wine",
  "みそ": "fermented soybean paste", "そば": "buckwheat noodles",
  "うどん": "thick wheat noodles", "アニメ": "animation",
  "マンガ": "comics", "カラオケ": "karaoke, “empty orchestra”",
  "からて": "karate, “empty hand”",
  "ひらがな": "hiragana, the cursive syllabary",
  "カタカナ": "katakana, the angular syllabary"},
 "farsi": {"ایران": "Iran, the country", "تهران": "Tehran, the capital",
  "شیراز": "Shiraz, city of poets", "مشهد": "Mashhad, pilgrimage city",
  "نوروز": "Nowruz, the new year"},
 "hangul": {"서울": "Seoul, the capital", "부산": "Busan, the southern port",
  "인천": "Incheon, the harbour city", "제주": "Jeju, the volcanic island"},
 "pinyin": {"北京 (běijīng)": "Beijing, the capital",
  "上海 (shànghǎi)": "Shanghai, the port city",
  "广州 (guǎngzhōu)": "Guangzhou, the southern hub",
  "成都 (chéngdū)": "Chengdu, the Sichuan capital"},
 "hebrew": {"תל אביב": "Tel Aviv, the coastal city",
  "חיפה": "Haifa, the northern port", "אילת": "Eilat, the Red Sea resort"},
 "devanagari": {"मुंबई": "Mumbai, the film capital",
  "कोलकाता": "Kolkata, the eastern city"},
 "geez": {"እንጀራ": "injera, the sourdough flatbread",
  "ላሊበላ": "Lalibela, the rock-hewn churches"},
 "thai": {"ภูเก็ต": "Phuket, the island province",
  "อยุธยา": "Ayutthaya, the old capital"},
 "arabic": {"بغداد": "Baghdad, the capital of Iraq"},
 "cyrillic": {"Минск": "Minsk, the capital of Belarus"},
}

# slug -> (language, [(word, romanisation, english, category), ...])
LISTS = {
"hebrew": ("Hebrew", [
 ("לחם","lehem","bread",F), ("חלב","halav","milk",F), ("גבינה","gvina","cheese",F),
 ("תפוח","tapuah","apple",F), ("יין","yayin","wine",F), ("מים","mayim","water",F),
 ("דג","dag","fish",F), ("עוגה","uga","cake",F), ("קפה","kafe","coffee",F),
 ("ישראל","yisrael","Israel",G), ("ים","yam","sea",G), ("הר","har","mountain",G),
 ("מדבר","midbar","desert",G), ("נהר","nahar","river",G),
 ("ירושלים","yerushalayim","Jerusalem",C), ("תל אביב","tel aviv","Tel Aviv",C),
 ("חיפה","haifa","Haifa",C), ("אילת","eilat","Eilat",C),
 ("שבת","shabat","Sabbath",K), ("ספר","sefer","book",K), ("שלום","shalom","peace",K),
 ("מוזיקה","muzika","music",K), ("תורה","tora","Torah",K),
 ("שמש","shemesh","sun",N), ("ירח","yareah","moon",N), ("עץ","etz","tree",N),
 ("פרח","perah","flower",N), ("כלב","kelev","dog",N), ("חתול","hatul","cat",N)]),

"arabic": ("Arabic", [
 ("خبز","khubz","bread",F), ("ماء","maa","water",F), ("قهوة","qahwa","coffee",F),
 ("شاي","shay","tea",F), ("تمر","tamr","dates",F), ("لحم","lahm","meat",F),
 ("سمك","samak","fish",F), ("أرز","aruzz","rice",F), ("حليب","halib","milk",F),
 ("مصر","misr","Egypt",G), ("بحر","bahr","sea",G), ("جبل","jabal","mountain",G),
 ("صحراء","sahraa","desert",G), ("نهر","nahr","river",G),
 ("القاهرة","al-qahira","Cairo",C), ("دمشق","dimashq","Damascus",C),
 ("بغداد","baghdad","Baghdad",C), ("الرباط","ar-ribat","Rabat",C),
 ("كتاب","kitab","book",K), ("مدرسة","madrasa","school",K),
 ("موسيقى","musiqa","music",K), ("سلام","salam","peace",K),
 ("شمس","shams","sun",N), ("قمر","qamar","moon",N), ("شجرة","shajara","tree",N),
 ("وردة","warda","rose",N), ("قط","qitt","cat",N), ("حصان","hisan","horse",N)]),

"farsi": ("Persian", [
 ("نان","nan","bread",F), ("آب","ab","water",F), ("چای","chay","tea",F),
 ("برنج","berenj","rice",F), ("انار","anar","pomegranate",F), ("پنیر","panir","cheese",F),
 ("شیر","shir","milk",F), ("کباب","kabab","kebab",F),
 ("ایران","iran","Iran",G), ("دریا","darya","sea",G), ("کوه","kuh","mountain",G),
 ("بیابان","biyaban","desert",G), ("رود","rud","river",G),
 ("تهران","tehran","Tehran",C), ("اصفهان","esfahan","Isfahan",C),
 ("شیراز","shiraz","Shiraz",C), ("مشهد","mashhad","Mashhad",C),
 ("کتاب","ketab","book",K), ("شعر","sher","poetry",K), ("نوروز","nowruz","Nowruz",K),
 ("موسیقی","musiqi","music",K), ("دوست","dust","friend",K),
 ("خورشید","khorshid","sun",N), ("ماه","mah","moon",N), ("درخت","derakht","tree",N),
 ("گل","gol","flower",N), ("گربه","gorbeh","cat",N), ("اسب","asb","horse",N)]),

"cyrillic": ("Russian", [
 ("хлеб","khleb","bread",F), ("вода","voda","water",F), ("чай","chay","tea",F),
 ("молоко","moloko","milk",F), ("суп","sup","soup",F), ("яблоко","yabloko","apple",F),
 ("сыр","syr","cheese",F), ("мясо","myaso","meat",F),
 ("Россия","rossiya","Russia",G), ("море","more","sea",G), ("гора","gora","mountain",G),
 ("река","reka","river",G), ("лес","les","forest",G),
 ("Москва","moskva","Moscow",C), ("Киев","kiev","Kyiv",C), ("Минск","minsk","Minsk",C),
 ("София","sofiya","Sofia",C),
 ("книга","kniga","book",K), ("школа","shkola","school",K), ("музыка","muzyka","music",K),
 ("театр","teatr","theatre",K), ("друг","drug","friend",K),
 ("солнце","solntse","sun",N), ("луна","luna","moon",N), ("дерево","derevo","tree",N),
 ("цветок","tsvetok","flower",N), ("собака","sobaka","dog",N), ("кошка","koshka","cat",N)]),

"devanagari": ("Hindi", [
 ("रोटी","roti","bread",F), ("पानी","pani","water",F), ("चाय","chai","tea",F),
 ("दूध","dudh","milk",F), ("चावल","chaval","rice",F), ("आम","aam","mango",F),
 ("दाल","dal","lentils",F), ("नमक","namak","salt",F),
 ("भारत","bharat","India",G), ("समुद्र","samudra","sea",G), ("पहाड़","pahar","mountain",G),
 ("नदी","nadi","river",G), ("जंगल","jangal","forest",G),
 ("दिल्ली","dilli","Delhi",C), ("मुंबई","mumbai","Mumbai",C), ("जयपुर","jaypur","Jaipur",C),
 ("कोलकाता","kolkata","Kolkata",C),
 ("किताब","kitab","book",K), ("स्कूल","skul","school",K), ("संगीत","sangit","music",K),
 ("दोस्त","dost","friend",K), ("त्योहार","tyohar","festival",K),
 ("सूरज","suraj","sun",N), ("चाँद","chand","moon",N), ("पेड़","ped","tree",N),
 ("फूल","phul","flower",N), ("कुत्ता","kutta","dog",N), ("बिल्ली","billi","cat",N)]),

"hangul": ("Korean", [
 ("밥","bap","cooked rice",F), ("물","mul","water",F), ("김치","gimchi","kimchi",F),
 ("차","cha","tea",F), ("우유","uyu","milk",F), ("빵","ppang","bread",F),
 ("고기","gogi","meat",F), ("사과","sagwa","apple",F),
 ("한국","hanguk","Korea",G), ("바다","bada","sea",G), ("산","san","mountain",G),
 ("강","gang","river",G), ("숲","sup","forest",G),
 ("서울","seoul","Seoul",C), ("부산","busan","Busan",C), ("인천","incheon","Incheon",C),
 ("제주","jeju","Jeju",C),
 ("책","chaek","book",K), ("학교","hakgyo","school",K), ("음악","eumak","music",K),
 ("친구","chingu","friend",K), ("한글","hangeul","Hangul",K),
 ("해","hae","sun",N), ("달","dal","moon",N), ("나무","namu","tree",N),
 ("꽃","kkot","flower",N), ("개","gae","dog",N), ("고양이","goyangi","cat",N)]),

"thai": ("Thai", [
 ("ข้าว","khao","rice",F), ("น้ำ","nam","water",F), ("ชา","cha","tea",F),
 ("นม","nom","milk",F), ("ปลา","pla","fish",F), ("ไข่","khai","egg",F),
 ("ผลไม้","phonlamai","fruit",F), ("กาแฟ","kafae","coffee",F),
 ("ไทย","thai","Thailand",G), ("ทะเล","thale","sea",G), ("ภูเขา","phukhao","mountain",G),
 ("แม่น้ำ","maenam","river",G), ("ป่า","pa","forest",G),
 ("กรุงเทพ","krungthep","Bangkok",C), ("เชียงใหม่","chiangmai","Chiang Mai",C),
 ("ภูเก็ต","phuket","Phuket",C), ("อยุธยา","ayutthaya","Ayutthaya",C),
 ("หนังสือ","nangsue","book",K), ("โรงเรียน","rongrian","school",K),
 ("ดนตรี","dontri","music",K), ("เพื่อน","phuean","friend",K), ("วัด","wat","temple",K),
 ("ดวงอาทิตย์","duang athit","sun",N), ("ดวงจันทร์","duang chan","moon",N),
 ("ต้นไม้","tonmai","tree",N), ("ดอกไม้","dokmai","flower",N),
 ("หมา","ma","dog",N), ("แมว","maeo","cat",N)]),

"geez": ("Amharic", [
 ("እንጀራ","injera","injera",F), ("ውሃ","wiha","water",F), ("ቡና","buna","coffee",F),
 ("ወተት","wetet","milk",F), ("ዳቦ","dabo","bread",F), ("ሥጋ","sga","meat",F),
 ("እንቁላል","inkulal","egg",F), ("ጨው","chew","salt",F),
 ("ኢትዮጵያ","ityopya","Ethiopia",G), ("ባሕር","bahr","sea",G), ("ተራራ","terara","mountain",G),
 ("ወንዝ","wenz","river",G), ("ጫካ","chaka","forest",G),
 ("አዲስ አበባ","addis abeba","Addis Ababa",C), ("ጎንደር","gonder","Gondar",C),
 ("አክሱም","aksum","Axum",C), ("ላሊበላ","lalibela","Lalibela",C),
 ("መጽሐፍ","metshaf","book",K), ("ትምህርት","timhrt","education",K),
 ("ሙዚቃ","muziqa","music",K), ("ጓደኛ","gwadegna","friend",K), ("በዓል","beal","holiday",K),
 ("ፀሐይ","tsehay","sun",N), ("ጨረቃ","chereqa","moon",N), ("ዛፍ","zaf","tree",N),
 ("አበባ","abeba","flower",N), ("ውሻ","wisha","dog",N), ("ድመት","dmet","cat",N)]),

# The spellable form is the pinyin, so it is written in brackets after the
# characters, the same shape the reference page's example words use.
"pinyin": ("Mandarin", [
 ("米饭 (mǐfàn)","mifan","cooked rice",F), ("水 (shuǐ)","shui","water",F),
 ("茶 (chá)","cha","tea",F), ("牛奶 (niúnǎi)","niunai","milk",F),
 ("面条 (miàntiáo)","miantiao","noodles",F), ("鱼 (yú)","yu","fish",F),
 ("鸡蛋 (jīdàn)","jidan","egg",F), ("苹果 (píngguǒ)","pingguo","apple",F),
 ("中国 (zhōngguó)","zhongguo","China",G), ("海 (hǎi)","hai","sea",G),
 ("山 (shān)","shan","mountain",G), ("河 (hé)","he","river",G),
 ("森林 (sēnlín)","senlin","forest",G),
 ("北京 (běijīng)","beijing","Beijing",C), ("上海 (shànghǎi)","shanghai","Shanghai",C),
 ("广州 (guǎngzhōu)","guangzhou","Guangzhou",C), ("成都 (chéngdū)","chengdu","Chengdu",C),
 ("书 (shū)","shu","book",K), ("学校 (xuéxiào)","xuexiao","school",K),
 ("音乐 (yīnyuè)","yinyue","music",K), ("朋友 (péngyǒu)","pengyou","friend",K),
 ("春节 (chūnjié)","chunjie","Spring Festival",K),
 ("太阳 (tàiyáng)","taiyang","sun",N), ("月亮 (yuèliàng)","yueliang","moon",N),
 ("树 (shù)","shu","tree",N), ("花 (huā)","hua","flower",N),
 ("狗 (gǒu)","gou","dog",N), ("猫 (māo)","mao","cat",N)]),

# The Latin script is paired with BSL and ASL fingerspelling here, so the
# drill is spelling the word out rather than transliterating it.
"latin": ("English", [
 ("bread","","bread",F), ("water","","water",F), ("coffee","","coffee",F),
 ("apple","","apple",F), ("cheese","","cheese",F), ("fish","","fish",F),
 ("rice","","rice",F), ("egg","","egg",F),
 ("England","","England",G), ("sea","","sea",G), ("mountain","","mountain",G),
 ("river","","river",G), ("forest","","forest",G),
 ("London","","London",C), ("Dublin","","Dublin",C),
 ("Boston","","Boston",C), ("Sydney","","Sydney",C),
 ("book","","book",K), ("school","","school",K), ("music","","music",K),
 ("friend","","friend",K), ("theatre","","theatre",K),
 ("sun","","sun",N), ("moon","","moon",N), ("tree","","tree",N),
 ("flower","","flower",N), ("dog","","dog",N), ("cat","","cat",N)]),
}

# Greek and Japanese predate this file; carry them over rather than retype them.
LEGACY = {"greek": ("Greek", "greek-word-list.json", "greek"),
          "kana":  ("Japanese", "japanese-word-list.json", "kana")}


def write(slug, language, words):
    path = os.path.join(OUT, slug + ".json")
    io.open(path, "w", encoding="utf-8").write(json.dumps(
        {"script": slug, "language": language, "words": words},
        ensure_ascii=False, indent=1) + "\n")
    return path


def main():
    os.makedirs(OUT, exist_ok=True)
    total = 0
    for slug, (language, rows) in sorted(LISTS.items()):
        m = MEANINGS.get(slug, {})
        words = [{"word": w, "romaji": r, "english": m.get(w, e), "categories": [c]}
                 for w, r, e, c in rows]
        write(slug, language, words)
        total += len(words)
        print("%-11s %-9s %3d words" % (slug, language, len(words)))
    for slug, (language, src, key) in sorted(LEGACY.items()):
        old = json.load(io.open(src, encoding="utf-8"))["words"]
        m = MEANINGS.get(slug, {})
        words = [{"word": w[key], "romaji": w.get("romaji", ""),
                  "english": m.get(w[key], w.get("english", "")),
                  "categories": w.get("categories", [])} for w in old]
        write(slug, language, words)
        total += len(words)
        print("%-11s %-9s %3d words (carried over from %s)" % (slug, language, len(words), src))
    print("\n%d scripts, %d words total" % (len(LISTS) + len(LEGACY), total))


if __name__ == "__main__":
    main()
