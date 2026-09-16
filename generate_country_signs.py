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

RESEARCH_NOTES = (
    "Video sourcing status: no embeddable, license-clean video source covering "
    "national country name signs was found during the initial research pass. "
    "Checked: Spreadthesign (comprehensive but copyrighted, link-out only); "
    "signasl.org (self-hosted video, copyright Daniel Mitchell); Handspeak and "
    "Signing Savvy (copyrighted); Global Signbank / Radboud (research-access, "
    "CC BY-NC-SA, covers NGT/BSL/FinSL/ASL rather than country name signs). "
    "The practical paths are YouTube embeds vetted individually, or self-recorded "
    "clips. Every country below therefore has video_url null and status "
    "no_source_found; the page links out to Spreadthesign and YouTube per country."
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
            out.append({
                "m49": m49,
                "latlng": latlng,
                "iso2": c["cca2"].lower(),
                "name_en": c["name"]["common"],
                "name_native": nm,
                "native_language": lang,
                "sign_languages": SIGN_LANGUAGES.get(c["cca2"], []),
                "sign": {
                    "video_url": None, "video_type": None, "source_name": None,
                    "source_url": None, "sign_language": None, "is_endonym": None,
                    "status": "no_source_found", "notes": "",
                },
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
