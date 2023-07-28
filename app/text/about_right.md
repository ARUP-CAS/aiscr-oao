## Data

Informace o organizacích jsou čerpány přímo ze smluv uzavřených mezi **Akademií věd České Republiky** ([AV ČR](https://www.avcr.cz/)) a konkrétními organizacemi, veřejných rejstříků, webových stránek daných organizací a 
[seznamu oprávnění](https://www.mkcr.cz/seznam-organizaci-opravnenych-k-provadeni-archeologickych-vyzkumu-278.html) 
udělených **Ministerstvem kultury ČR** ([MK ČR](https://www.mkcr.cz/)).  
Data o provedených výzkumech a jejich poloze jsou čerpána z 
**Archeologické mapy ČR** ([AMČR](https://amcr-info.aiscr.cz/)).

Data pro jednotlivé organizace lze stáhnout ve formátu [GeoPackage (.gpkg)](https://www.geopackage.org/). 
Data jsou zveřejněna pod licencí *Creative Commons Uveďte původ -- Neužívejte komerčně 4.0 Mezinárodní* 
([<i class="fab fa-creative-commons" role="presentation" aria-label="creative-commons icon"></i><i class="fab fa-creative-commons-by" role="presentation" aria-label="creative-commons-by icon"></i><i class="fab fa-creative-commons-nc" role="presentation" aria-label="creative-commons-nc icon"></i> CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/)).

### Struktura dat

*GeoPackage* obsahuje následující vrstvy (převedené do S-JTSK, [EPSG:5514](https://epsg.io/5514)):

- `OAO Metadata` -- Adresní bod vybrané organizace.

    - `ico` -- IČO organizace.
    - `nazev_zkraceny` -- Název používaný v heslářích AMČR.
    - `nazev` -- Oficiální název organizace.
    - `adresa` -- Adresa organizace.
    - `web0` -- Webové stránky organizace.
    - `mail0` -- Emailová adresa organizace.
    - `mk_id` -- Spisová značka oprávnění uděleného MK ČR.
    - `mk_from` / `mk_to` / `mk_neomezena` -- Platnost oprávnění MK ČR.
    - `av_from` / `av_to` / `av_neomezena` -- Platnost dohody s AV ČR.
    - `note` -- Poznámka.
    
- `OAO Polygon` -- Polygon působnosti vybrané oragnizace.
    
    - `ico` -- IČO organizace.
    - `area` -- Plocha polygonu působnosti organizace (km<sup>2</sup>).
    
- `OAO Grid` -- Čtvercová síť s počtem výzkumů organizace odvozená z čtvrtinových kvadrantů *Kartierung der Flora Mitteleuropas* (KFME) 
 o původní velikosti čtverců 5’ z. š. × 3’ z. d. (zhruba 6 × 5,5 km).
 
    - `ico` -- IČO organizace.
    - `ctverec` -- Unikátní identifikáotor daného čtverce.
    - `value` -- Počet výzkumů organizace v daném čtverci.
    - `scaled` -- Logarytmicky škálovaný počet výzkumů v daném čtverci.

