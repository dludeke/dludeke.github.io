#!/usr/bin/env python3
"""Generate per-continent country-sign data files for signs.html.

Country metadata (M49 code, ISO2, native-script name, official languages) comes
from the mledoze/countries dataset so it is not hand-typed. Sign language names
are curated below for the countries where a national sign language is
well documented; the rest are left empty rather than guessed.
"""
import json
import os
import sys
import urllib.request

SOURCE_URL = ("https://raw.githubusercontent.com/mledoze/countries/"
              "master/dist/countries.json")

HERE = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(HERE, "assets", "data")
CACHE = os.path.join(HERE, "countries-source.json")

CONTINENTS = {
    "north-america": ("North America", {"North America", "Central America", "Caribbean"}),
    "south-america": ("South America", {"South America"}),
    "europe": ("Europe", {"Central Europe", "Eastern Europe", "Northern Europe",
                          "Southeast Europe", "Southern Europe", "Western Europe"}),
    "asia": ("Asia", {"Central Asia", "Eastern Asia", "South-Eastern Asia",
                      "Southern Asia", "Western Asia"}),
    "africa": ("Africa", {"Eastern Africa", "Middle Africa", "Northern Africa",
                          "Southern Africa", "Western Africa"}),
    "oceania": ("Oceania", {"Australia and New Zealand", "Melanesia",
                            "Micronesia", "Polynesia"}),
}

REGION_OF = {
    "north-america": "Americas", "south-america": "Americas",
    "europe": "Europe", "asia": "Asia", "africa": "Africa", "oceania": "Oceania",
}

# National sign languages, keyed by ISO2. Only entries I can state with
# reasonable confidence; anything unknown is deliberately absent so the page
# shows "unknown" instead of an invented name.
SIGN_LANGUAGES = {
    # North America
    "US": ["American Sign Language (ASL)"],
    "CA": ["American Sign Language (ASL)", "Langue des signes québécoise (LSQ)"],
    "MX": ["Lengua de Señas Mexicana (LSM)"],
    "GT": ["Lengua de Señas Guatemalteca (LENSEGUA)"],
    "BZ": ["Belizean Sign Language"],
    "SV": ["Lengua de Señas Salvadoreña (LESSA)"],
    "HN": ["Lengua de Señas Hondureña (LESHO)"],
    "NI": ["Idioma de Señas Nicaragüense (ISN)"],
    "CR": ["Lengua de Señas Costarricense (LESCO)"],
    "PA": ["Lengua de Señas Panameñas (LSP)"],
    "CU": ["Lengua de Señas Cubana (LSC)"],
    "DO": ["Lengua de Señas Dominicana (LSRD)"],
    "HT": ["Haitian Sign Language (LSH)"],
    "JM": ["Jamaican Sign Language (JSL)", "Konchri Sain"],
    "TT": ["Trinidad and Tobago Sign Language (TTSL)"],
    "BB": ["Barbadian Sign Language"],
    "BS": ["Bahamian Sign Language"],
    "GD": ["Grenadian Sign Language"],
    # South America
    "BR": ["Língua Brasileira de Sinais (Libras)"],
    "AR": ["Lengua de Señas Argentina (LSA)"],
    "CL": ["Lengua de Señas Chilena (LSCh)"],
    "CO": ["Lengua de Señas Colombiana (LSC)"],
    "PE": ["Lengua de Señas Peruana (LSP)"],
    "VE": ["Lengua de Señas Venezolana (LSV)"],
    "EC": ["Lengua de Señas Ecuatoriana (LSEC)"],
    "BO": ["Lengua de Señas Boliviana (LSB)"],
    "PY": ["Lengua de Señas Paraguaya (LSPy)"],
    "UY": ["Lengua de Señas Uruguaya (LSU)"],
    "GY": ["Guyanese Sign Language"],
    "SR": ["Surinamese Sign Language"],
    # Europe
    "GB": ["British Sign Language (BSL)"],
    "IE": ["Irish Sign Language (ISL)"],
    "FR": ["Langue des signes française (LSF)"],
    "DE": ["Deutsche Gebärdensprache (DGS)"],
    "ES": ["Lengua de signos española (LSE)", "Llengua de signes catalana (LSC)"],
    "PT": ["Língua Gestual Portuguesa (LGP)"],
    "IT": ["Lingua dei segni italiana (LIS)"],
    "NL": ["Nederlandse Gebarentaal (NGT)"],
    "BE": ["Vlaamse Gebarentaal (VGT)", "Langue des signes de Belgique francophone (LSFB)"],
    "CH": ["Deutschschweizer Gebärdensprache (DSGS)", "Langue des signes suisse romande (LSF-SR)"],
    "AT": ["Österreichische Gebärdensprache (ÖGS)"],
    "SE": ["Svenskt teckenspråk (STS)"],
    "NO": ["Norsk tegnspråk (NTS)"],
    "DK": ["Dansk tegnsprog (DTS)"],
    "FI": ["Suomalainen viittomakieli (FinSL)"],
    "IS": ["Íslenskt táknmál (ÍTM)"],
    "PL": ["Polski Język Migowy (PJM)"],
    "CZ": ["Český znakový jazyk (ČZJ)"],
    "SK": ["Slovenský posunkový jazyk (SPJ)"],
    "HU": ["Magyar jelnyelv (MJNY)"],
    "RO": ["Limbaj Mimico-Gestual Român (LMG)"],
    "BG": ["Български жестомимичен език (БЖЕ)"],
    "GR": ["Ελληνική Νοηματική Γλώσσα (ΕΝΓ)"],
    "HR": ["Hrvatski znakovni jezik (HZJ)"],
    "RS": ["Српски знаковни језик"],
    "SI": ["Slovenski znakovni jezik (SZJ)"],
    "UA": ["Українська жестова мова (УЖМ)"],
    "RU": ["Русский жестовый язык (РЖЯ)"],
    "BY": ["Беларуская жэставая мова"],
    "LT": ["Lietuvių gestų kalba (LGK)"],
    "LV": ["Latviešu zīmju valoda (LZV)"],
    "EE": ["Eesti viipekeel (EVK)"],
    "TR": ["Türk İşaret Dili (TİD)"],
    "MT": ["Lingwa tas-Sinjali Maltija (LSM)"],
    "CY": ["Cypriot Sign Language"],
    "AL": ["Gjuha e Shenjave Shqipe"],
    "MK": ["Македонски знаковен јазик"],
    "BA": ["Bosanski znakovni jezik"],
    # Asia
    "JP": ["日本手話 (Japanese Sign Language, JSL)"],
    "CN": ["中国手语 (Chinese Sign Language, CSL)"],
    "KR": ["한국 수어 (Korean Sign Language, KSL)"],
    "IN": ["Indian Sign Language (ISL)"],
    "PK": ["Pakistan Sign Language (PSL)"],
    "BD": ["Bangladeshi Sign Language (BdSL)"],
    "LK": ["Sri Lankan Sign Language"],
    "NP": ["Nepali Sign Language (NSL)"],
    "TH": ["ภาษามือไทย (Thai Sign Language, TSL)"],
    "VN": ["Ngôn ngữ ký hiệu Việt Nam"],
    "ID": ["Bahasa Isyarat Indonesia (BISINDO)"],
    "MY": ["Bahasa Isyarat Malaysia (BIM)"],
    "PH": ["Filipino Sign Language (FSL)"],
    "SG": ["Singapore Sign Language (SgSL)"],
    "MM": ["Myanmar Sign Language"],
    "KH": ["Cambodian Sign Language"],
    "MN": ["Mongolian Sign Language"],
    "IL": ["שפת הסימנים הישראלית (Israeli Sign Language, ISL)"],
    "SA": ["لغة الإشارة السعودية (Saudi Sign Language)"],
    "JO": ["لغة الإشارة الأردنية (Jordanian Sign Language)"],
    "IR": ["زبان اشاره ایرانی (Persian Sign Language)"],
    "IQ": ["لغة الإشارة العراقية (Iraqi Sign Language)"],
    "KW": ["لغة الإشارة الكويتية (Kuwaiti Sign Language)"],
    "AE": ["لغة الإشارة الإماراتية (Emirati Sign Language)"],
    # Africa
    "ZA": ["South African Sign Language (SASL)"],
    "KE": ["Kenyan Sign Language (KSL)"],
    "TZ": ["Lugha ya Alama ya Tanzania (LAT)"],
    "UG": ["Ugandan Sign Language (USL)"],
    "NG": ["Nigerian Sign Language (NSL)"],
    "GH": ["Ghanaian Sign Language (GhSL)"],
    "ET": ["Ethiopian Sign Language (EthSL)"],
    "EG": ["لغة الإشارة المصرية (Egyptian Sign Language)"],
    "MA": ["لغة الإشارة المغربية (Moroccan Sign Language)"],
    "TN": ["لغة الإشارة التونسية (Tunisian Sign Language)"],
    "DZ": ["لغة الإشارة الجزائرية (Algerian Sign Language)"],
    "ZW": ["Zimbabwe Sign Language"],
    "ZM": ["Zambian Sign Language"],
    "MW": ["Malawian Sign Language"],
    "NA": ["Namibian Sign Language"],
    "BW": ["Botswana Sign Language"],
    "MZ": ["Língua de Sinais Moçambicana"],
    "AO": ["Língua Angolana de Sinais"],
    "SN": ["Langue des signes sénégalaise"],
    "ML": ["Langue des signes malienne"],
    "CI": ["Langue des signes ivoirienne"],
    "CM": ["Cameroonian Sign Language"],
    "RW": ["Rwandan Sign Language"],
    "BI": ["Burundian Sign Language"],
    "MG": ["Tenin'ny tanana malagasy"],
    # Oceania
    "AU": ["Auslan (Australian Sign Language)"],
    "NZ": ["New Zealand Sign Language (NZSL)"],
    "PG": ["Papua New Guinean Sign Language (PNGSL)"],
    "FJ": ["Fiji Sign Language"],
}


# Country name sign videos from The Sign Polyglot's "210 Countries: Native /
# Indigenous Name Signs" playlist, keyed by ISO2 -> (YouTube id, sign language).
# Scraped from the public playlist; videos are embedded, never re-hosted, so
# the creator keeps attribution and view counts. A handful fall back to that
# channel's International Sign playlist where no native-language video exists.
SIGN_VIDEOS = {
    "AD": ("q7_KYuT2_YE", "Andorran Sign Language"),
    "AE": ("FV3ya4NdDWM", "Emirati Sign Language"),
    "AF": ("VyDLUAMbZ3c", "Afghan Sign Language"),
    "AG": ("dGJrZRC_uL8", "Antiguan Sign Language"),
    "AL": ("wMVRpCL6cNg", "Albanian Sign Language"),
    "AM": ("xcAlfehzUkA", "Armenian Sign Language"),
    "AO": ("zBFpuJ7zuTo", "Angolan Sign Language"),
    "AR": ("MX74p75wedA", "Argentine Sign Language"),
    "AT": ("ZiYsaoaqUHI", "Austrian Sign Language"),
    "AU": ("Cg8HOKvnqbU", "Australian Sign Language"),
    "AW": ("VW7YAP_eTew", "Aruban Sign Language"),
    "AZ": ("4kJZf-G4PMc", "Azerbaijani Sign Language"),
    "BA": ("5Vo1Myemw40", "Bosnian Sign Language"),
    "BB": ("_DYhXgDn-74", "Barbados Sign Language"),
    "BD": ("zRLX0x-Qwoo", "Bangladeshi Sign Language"),
    "BE": ("_OupvK7LzNc", "Belgian Sign Language"),
    "BF": ("MLOig_BurRA", "Burkina Faso Sign Language"),
    "BG": ("377MQpdBOpc", "Bulgarian Sign Language"),
    "BH": ("UYnitSznLN0", "Bahraini Sign Language"),
    "BI": ("Xs0JePMs-ts", "Burundian Sign Language"),
    "BJ": ("IlHrCXU5kDo", "Beninese Sign Language"),
    "BN": ("-Ece1EjnsD0", "Bruneian Sign Language"),
    "BO": ("ypxMIT77-So", "Bolivian Sign Language"),
    "BR": ("Ulr6DEDTTzI", "Brazilian Sign Language"),
    "BS": ("Zf-OZU0ZcgA", "Bahamian Sign Language"),
    "BT": ("m9ssiKQG-wk", "Bhutanese Sign Language"),
    "BW": ("uKBt_8x4a5Y", "Botswanan Sign Language"),
    "BY": ("_06DVYGR4kQ", "Belarusian Sign Language"),
    "BZ": ("eMdl4A91nKM", "Belizean Sign Language"),
    "CA": ("DUI5Po99yOg", "Canadian Sign Language"),
    "CD": ("rxjrSkVIALY", "Congolese Sign Language"),
    "CG": ("0Bwt9TodTbg", "International Sign"),
    "CH": ("WqLmLJouqzI", "Swiss Sign Language"),
    "CI": ("sqylQ8bTnLI", "Ivoirian Sign Language"),
    "CL": ("rLcBeVILzfI", "Chilean Sign Language"),
    "CM": ("xoA7aBJBLL4", "Cameroon Sign Language"),
    "CN": ("5USYePX6ZMI", "Chinese Sign Language"),
    "CO": ("58OVGclsMzk", "Colombian Sign Language"),
    "CR": ("wzufJk1x3Ic", "Costa Rican Sign Language"),
    "CU": ("B7ub3b6eAzI", "Cuban Sign Language"),
    "CV": ("T0LDLbsNJcs", "Cabo Verdean Sign Language"),
    "CY": ("uybxUdGx9RQ", "Cypriot Sign Language"),
    "CZ": ("2HFoKFZeNsw", "Czech Sign Language"),
    "DE": ("AYBmd47gaKw", "German Sign Language"),
    "DJ": ("yGzZV4yIE1Q", "Djiboutian Sign Language"),
    "DK": ("tUnnZfgqHgg", "Danish Sign Language"),
    "DM": ("0OxxqAsLCZM", "Dominica Sign Language"),
    "DO": ("mwOMIAzbnr4", "Dominican Sign Language"),
    "DZ": ("TF2Si3Ur7ck", "Algerian Sign Language"),
    "EC": ("eByZIOzkRuc", "Ecuadorian Sign Language"),
    "EE": ("eQC2hdu1D-Y", "Estonian Sign Language"),
    "EG": ("dgY5eBsIe3E", "Egyptian Sign Language"),
    "ER": ("jfa0pevSJH4", "Eritrean Sign Language"),
    "ES": ("o8iUSyxfXgI", "Spanish Sign Language"),
    "ET": ("WH8FvJUbfQo", "Ethiopian Sign Language"),
    "FI": ("dvNFRCO9T9U", "Finnish Sign Language"),
    "FJ": ("G2zR7u3wF_g", "Fijian Sign Language"),
    "FM": ("xQvS-HUG43c", "Micronesian Sign Language"),
    "FR": ("Zrr-2lN_F80", "French Sign Language"),
    "GA": ("OLN5MqcTeyM", "Gabonese Sign Language"),
    "GB": ("c9ZgiV2-OL4", "British Sign Language"),
    "GD": ("If76RC6iPOY", "Grenadian Sign Language"),
    "GE": ("nXLIk-xHPWc", "Georgian Sign Language"),
    "GH": ("uP-4MG0dx1Y", "Ghanaian Sign Language"),
    "GM": ("hwf2iyH6ONQ", "Gambian Sign Language"),
    "GN": ("hcHyPylIPqI", "Guinean Sign Language"),
    "GQ": ("-knTvVwGpzA", "Equatoguinean Sign Language"),
    "GR": ("bVtsZc6dFUs", "Greek Sign Language"),
    "GT": ("Ymc1c3San_E", "Guatemalan Sign Language"),
    "GW": ("OPWp0txUIH0", "Bissau-Guinean Sign Language"),
    "GY": ("x1SgaNJcXA8", "Guyanese Sign Language"),
    "HN": ("xweB2Q1xGng", "Honduran Sign Language"),
    "HR": ("OVNzrjb5i14", "Croatian Sign Language"),
    "HT": ("NdVqj1o1hbQ", "Haitian Sign Language"),
    "HU": ("3wsG0PlgQUc", "Hungarian Sign Language"),
    "ID": ("KzKjHPqQGKM", "Indonesian Sign Language"),
    "IE": ("KNw5oBIGlCU", "Irish Sign Language"),
    "IL": ("XNabx57X53E", "Israeli Sign Language"),
    "IN": ("Gq6OKFugRqY", "Indian Sign Language"),
    "IQ": ("KCZrBVek4dA", "Iraqi Sign Language"),
    "IR": ("-c4GXYQtuPQ", "Iranian Sign Language"),
    "IS": ("MPrNL0Q63-g", "Icelandic Sign Language"),
    "IT": ("3_vFwesfW9g", "Italian Sign Language"),
    "JM": ("qWNIYMmYSnI", "Jamaican Sign Language"),
    "JO": ("RKTS7JaW-AY", "Jordanian Sign Language"),
    "JP": ("QRXZMU9Ha54", "Japanese Sign Language"),
    "KE": ("E3h2nzmOkwg", "Kenyan Sign Language"),
    "KG": ("r7OqwK0dI8A", "Kyrgyz Sign Language"),
    "KH": ("xNXa0AP8fYw", "Cambodian Sign Language"),
    "KI": ("KFSuN4PfF6c", "Kiribati Sign Language"),
    "KM": ("MU_CH13E-6E", "Comorian Sign Language"),
    "KN": ("d5kS9a4Ewrs", "Kittitian Sign Language"),
    "KP": ("32EgrUF-6gg", "North Korean Sign Language"),
    "KR": ("ZVaj84OJR_c", "Korean Sign Language"),
    "KW": ("p34CxFuT8es", "Kuwaiti Sign Language"),
    "KZ": ("UXIrV12XXxA", "Kazakh Sign Language"),
    "LA": ("Uc6Su1xf_oY", "Laotian Sign Language"),
    "LB": ("xyNtcl4w6U4", "Lebanese Sign Language"),
    "LC": ("wXlWw4DPKTU", "Saint Lucian Sign Language"),
    "LI": ("Yar0foCzjZk", "Liechtenstein Sign Language"),
    "LK": ("hyvZ4mjm5dk", "Sri Lankan Sign Language"),
    "LR": ("gstcvZquC3Y", "Liberian Sign Language"),
    "LS": ("m1r9dtOePyk", "Lesotho Sign Language"),
    "LT": ("GWnKAuZUX5M", "Lithuanian Sign Language"),
    "LU": ("MPamSswKPh8", "Luxembourgish Sign Language"),
    "LV": ("IKEtWifXipk", "Latvian Sign Language"),
    "LY": ("gR0RUNqTwtI", "Libyan Sign Language"),
    "MA": ("TdvERnzmrCg", "Moroccan Sign Language"),
    "MC": ("E_7i1U1lpv4", "Monégasque Sign Language"),
    "MD": ("NhMZ5TF5j8s", "Moldovan Sign Language"),
    "ME": ("1k72hfxWr1I", "Montenegrin Sign Language"),
    "MG": ("W8s85hQkBlQ", "Malagasy Sign Language"),
    "MH": ("hDTpV5xtpHA", "Marshallese Sign Language"),
    "MK": ("_1H46S68KHQ", "Macedonian Sign Language"),
    "ML": ("shhwAf0T7W0", "International Sign"),
    "MM": ("TFOXbO9mnWk", "Burmese Sign Language"),
    "MN": ("sHg17BWcDag", "Mongolian Sign Language"),
    "MR": ("hm45uzcbqFs", "Mauritanian Sign Language"),
    "MT": ("CyPGW7FgVxA", "Maltese Sign Language"),
    "MU": ("jS1b9Ybv154", "Mauritian Sign Language"),
    "MV": ("9vLK4y1p43U", "Maldivian Sign Language"),
    "MW": ("1Fu8MXlxICQ", "International Sign"),
    "MX": ("FZtkNASsfM8", "Mexican Sign Language"),
    "MY": ("Asyk-iudxHY", "Malaysian Sign Language"),
    "MZ": ("P1BaMv1ud5Q", "Mozambican Sign Language"),
    "NA": ("TWAbLNaJcR0", "Namibian Sign Language"),
    "NE": ("S_4a0lNehDA", "Nigerien Sign Language"),
    "NG": ("J6v6GnHDcKg", "Nigerian Sign Language"),
    "NI": ("Ts_qX7o0nNQ", "Nicaraguan Sign Language"),
    "NL": ("-iU8sevq8QQ", "Dutch Sign Language"),
    "NO": ("OrcSc9k40bM", "Norwegian Sign Language"),
    "NP": ("MPLUWWr04b4", "Nepali Sign Language"),
    "NR": ("XOtlqeSegdM", "Nauruan Sign Language"),
    "NZ": ("IrruOXMLBic", "New Zealand Sign Language"),
    "OM": ("tqZY7gMfdxw", "Omani Sign Language"),
    "PA": ("GB5_Pa2fEOo", "Panamanian Sign Language"),
    "PE": ("BmSr6z0FMDg", "Peruvian Sign Language"),
    "PG": ("p5YwUH1fUHk", "Papua New Guinean Sign Language"),
    "PH": ("Vg3KUIpVUh0", "Filipino Sign Language"),
    "PK": ("xbXbwUEuu54", "Pakistani Sign Language"),
    "PL": ("nCXrDo-8VHU", "Polish Sign Language"),
    "PT": ("Es2-BKdS82g", "Portuguese Sign Language"),
    "PW": ("6TvRzoCoemY", "Palauan Sign Language"),
    "PY": ("m2el1vfOhwc", "Paraguayan Sign Language"),
    "QA": ("BhOJiY8gytM", "Qatari Sign Language"),
    "RO": ("gg4NC2xedV8", "Romanian Sign Language"),
    "RS": ("DlePxQVMzSk", "Serbian Sign Language"),
    "RU": ("wT3-m84EeCM", "Russian Sign Language"),
    "RW": ("RBtKLi4YyIk", "Rwandan Sign Language"),
    "SA": ("tcpjh45aYWU", "Saudi Arabian Sign Language"),
    "SB": ("kyTZCAyxExY", "Solomon Islands Sign Language"),
    "SD": ("X2A3K8JXvqA", "Sudanese Sign Language"),
    "SE": ("doQWLjx6ohQ", "Swedish Sign Language"),
    "SG": ("WDGzviVDFl0", "Singapore Sign Language"),
    "SI": ("_81Hk4gzrZ4", "Slovenian Sign Language"),
    "SK": ("wivex68zKoI", "Slovakian Sign Language"),
    "SL": ("oijCyd8nBdE", "Sierra Leonean Sign Language"),
    "SN": ("-OLv0iGGQHs", "Senegalese Sign Language"),
    "SO": ("fCasiPYTRI0", "Somalian Sign Language"),
    "SR": ("qLW2Wdzbtm0", "Surinamese Sign Language"),
    "SS": ("hC1qtD54FOg", "South Sudanese Sign Language"),
    "SV": ("n2vwaxMFKXk", "Salvadorian Sign Language"),
    "SY": ("4Pfw7ocgKP8", "Syrian Sign Language"),
    "SZ": ("teW0FaooS_M", "Swazi Sign Language"),
    "TD": ("ev1Fqto0TBk", "Chadian Sign Language"),
    "TG": ("TOQRRq2z5TQ", "Togolese Sign Language"),
    "TH": ("iKmnd1pDSb8", "Thai Sign Language"),
    "TJ": ("_Ibtr04ww30", "Russian-Tajik Sign Language"),
    "TL": ("Iw79-krF_yY", "Timorese Sign Language"),
    "TM": ("rdycmMyJ3Go", "Turkmen Sign Language"),
    "TN": ("MYIVYr09zxQ", "Tunisian Sign Language"),
    "TO": ("nhYDtDVUQSo", "Tongan Sign Language"),
    "TR": ("iQ9kEKs2n7I", "Turkish Sign Language"),
    "TT": ("FUPolNSpUCE", "Trinbagonian Sign Language"),
    "TV": ("yUW3c0HLaCE", "Tuvaluan Sign Language"),
    "TW": ("oZwkSSun1GA", "Taiwanese Sign Language"),
    "TZ": ("CqAOGTMD59s", "Tanzanian Sign Language"),
    "UA": ("qo_OTdTqPO4", "Ukrainian Sign Language"),
    "UG": ("8vsQXGQppJo", "Ugandan Sign Language"),
    "US": ("Vy2bmKpLi8g", "American Sign Language"),
    "UY": ("zFXXoPWbCt4", "Uruguayan Sign Language"),
    "UZ": ("PWFA23CFOZ8", "Uzbek Sign Language"),
    "VA": ("dpaw1AW_VyU", "International Sign"),
    "VC": ("PxkEhLC4TWw", "Vincentian Sign Language"),
    "VI": ("lfi0GWWY6aQ", "Virgin Island Sign Language"),
    "VN": ("kjje7pr2_14", "Vietnamese Sign Language"),
    "VU": ("K6qI6vUbkoE", "Vanuatu Sign Language"),
    "WS": ("PzW0t6dmyGk", "Samoan Sign Language"),
    "YE": ("l7sE59G5rgM", "Yemeni Sign Language"),
    "ZA": ("qKZBl7ycsTU", "South African Sign Language"),
    "ZM": ("CbXNX00KRZs", "Zambian Sign Language"),
    "ZW": ("-aRfAvpwZj8", "Zimbabwean Sign Language"),
}

# Titles naming International Sign, or ASL for a country that is not the USA,
# are exonyms rather than the country's own sign for itself.
def endonym(iso2, sign_language):
    sl = sign_language.lower()
    if "international sign" in sl:
        return False
    if "american sign language" in sl and iso2 != "US":
        return False
    return True

RESEARCH_NOTES = (
    "Sign videos come from The Sign Polyglot's public YouTube playlist '210 "
    "Countries: Native / Indigenous Name Signs' (plus that channel's "
    "International Sign playlist for a few gaps). They are embedded by video "
    "id, never downloaded or re-hosted, so attribution and view counts stay "
    "with the creator. Every entry is marked needs_review: the video id and "
    "the sign language named in its title are recorded automatically, but "
    "whether the clip shows the correct current sign needs a signer's eye. "
    "is_endonym is derived from the video title -- false where the title names "
    "International Sign, or ASL for a country other than the USA. Videos run "
    "about a minute and are embedded whole; no per-video timestamp for the "
    "exact moment of the sign was available without watching all of them."
)

SCHEMA_NOTES = (
    "sign.status is one of: verified | needs_review | no_source_found. "
    "sign.is_endonym is true when the video shows the country's own sign for "
    "itself, false when it shows an ASL/IS exonym, null when unknown. Populate "
    "video_url with an embeddable URL (YouTube embed, mp4, or gif) and set "
    "video_type to youtube | mp4 | gif; leave null until a source is confirmed."
)


def native_name(c):
    """Native-script common name, preferring the first official language."""
    nat = c.get("name", {}).get("native") or {}
    langs = c.get("languages") or {}
    for code in list(langs.keys()) + list(nat.keys()):
        if code in nat:
            entry = nat[code]
            return entry.get("common") or entry.get("official"), langs.get(code, code)
    return c["name"]["common"], ", ".join(langs.values()) or "unknown"


def main():
    if not os.path.exists(CACHE):
        print(f"fetching {SOURCE_URL}")
        urllib.request.urlretrieve(SOURCE_URL, CACHE)
    countries = json.load(open(CACHE, encoding="utf-8"))
    topo = json.load(open(os.path.join(DATA_DIR, "countries-50m.json")))
    shape_ids = {g.get("id") for g in topo["objects"]["countries"]["geometries"]}

    by_key = {}
    for c in countries:
        if not c.get("unMember"):
            continue
        for key, (_, subs) in CONTINENTS.items():
            if c.get("region") == REGION_OF[key] and c.get("subregion") in subs:
                by_key.setdefault(key, []).append(c)
                break

    summary, no_shape = [], []
    for key, (label, _) in CONTINENTS.items():
        group = sorted(by_key.get(key, []), key=lambda c: c["name"]["common"])
        out = []
        for c in group:
            nm, lang = native_name(c)
            m49 = c["ccn3"]
            if m49 not in shape_ids:
                no_shape.append((label, c["name"]["common"], m49))
            latlng = c.get("latlng") or []
            vid = SIGN_VIDEOS.get(c["cca2"])
            if vid:
                ytid, slname = vid
                sign = {
                    "video_url": f"https://www.youtube.com/embed/{ytid}",
                    "video_type": "youtube",
                    "source_name": "The Sign Polyglot",
                    "source_url": f"https://www.youtube.com/watch?v={ytid}",
                    "sign_language": slname,
                    "is_endonym": endonym(c["cca2"], slname),
                    "status": "needs_review",
                    "notes": "",
                }
            else:
                sign = {
                    "video_url": None, "video_type": None, "source_name": None,
                    "source_url": None, "sign_language": None, "is_endonym": None,
                    "status": "no_source_found", "notes": "",
                }
            out.append({
                "m49": m49,
                "latlng": latlng,
                "iso2": c["cca2"].lower(),
                "name_en": c["name"]["common"],
                "name_native": nm,
                "native_language": lang,
                "sign_languages": SIGN_LANGUAGES.get(c["cca2"], []),
                "sign": sign,
            })
        doc = {
            "continent": label,
            "schema_notes": SCHEMA_NOTES,
            "research_notes": RESEARCH_NOTES,
            "countries": out,
        }
        path = os.path.join(DATA_DIR, f"country-signs-{key}.json")
        with open(path, "w", encoding="utf-8") as fh:
            json.dump(doc, fh, ensure_ascii=False, indent=2)
            fh.write("\n")
        withsl = sum(1 for c in out if c["sign_languages"])
        summary.append((label, len(out), withsl, os.path.basename(path)))

    for label, n, withsl, fn in summary:
        print(f"{label:<16} {n:>3} countries  {withsl:>3} with named sign language  -> {fn}")
    print(f"TOTAL {sum(s[1] for s in summary)} countries")
    if no_shape:
        print("\nNO MAP SHAPE (map hint will fail for these):")
        for label, nm, m49 in no_shape:
            print(f"  {label}: {nm} ({m49})")
    else:
        print("\nEvery country matched a map shape.")


if __name__ == "__main__":
    sys.exit(main())
