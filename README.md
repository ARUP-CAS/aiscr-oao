# Mapa OAO — Mapa oprávněných archeologických organizací

[![Website](https://img.shields.io/website?down_message=down&label=https%3A%2F%2Foao.aiscr.cz%2F&up_message=up&url=https%3A%2F%2Foao.aiscr.cz%2F)](https://oao.aiscr.cz)
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.8178384.svg)](https://doi.org/10.5281/zenodo.8178384)
[![GitHub license](https://img.shields.io/github/license/ARUP-CAS/aiscr-oao)](LICENSE)

[ENGLISH VERSION HERE](README_en.md)

Repozitář obsahuje zdrojový kód aplikace [R Shiny](https://shiny.posit.co/)
zobrazující data o organizacích oprávněných provádět archeologické výzkumy v ČR.

Aplikace je dostupná na: **<https://oao.aiscr.cz/>**

Repozitář je forkem projektu [petrpajdla/map_oao](https://github.com/petrpajdla/map_oao).
ARUP-CAS repozitář rozšiřuje původní aplikaci o emailer komponentu, grafy (`plots/`),
GitHub Actions workflows a další funkcionality.

Aplikace je součástí infrastruktury
[**Archeologického informačního systému ČR** (AIS CR)](https://www.aiscr.cz/).

---

## Obsah repozitáře

```
app/          — zdrojový kód Shiny aplikace
  data/       — zpracovaná geoprostorová data
  R/          — funkce využívané serverovou částí
  text/       — stránka O aplikaci a popisy organizací
  www/        — statické soubory (obrázky, CSS, JS)
code/         — R skripty pro přípravu dat
emailer/      — komponenta pro odesílání oznámení krajům
plots/        — grafy a vizualizace dat
.github/      — GitHub Actions workflows
```

---

## Funkcionalita

Aplikace umožňuje:

- hledání organizací podle polohy nebo katastrálního území
- zobrazení mapy působnosti organizací
- přehled organizací v jednotlivých krajích
- stažení dat ve formátu GeoPackage (CC BY 4.0)
- plnění oznamovací povinnosti OAO vůči krajům (emailer)

---

## Datové zdroje

- smlouvy uzavřené mezi AV ČR a oprávněnými organizacemi
- seznam oprávnění vydaných Ministerstvem kultury ČR
- data o provedených výzkumech z AMČR

---

## Citace

```
Pajdla, P. 2023: Mapa oprávněných archeologických organizací
[Map of Authorized Archaeological Organizations].
DOI: 10.5281/zenodo.8178384.
Zdrojový kód: https://github.com/ARUP-CAS/aiscr-oao,
aplikace: https://oao.aiscr.cz/.
```

---

## Licence

Kód je uvolněn pod licencí [MIT](LICENSE).  
Data, texty a obrázky jsou uvolněny pod licencí [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

---

## Přispívání

Viz [CONTRIBUTING.md](CONTRIBUTING.md).

Chyby hlaste jako [GitHub Issues](https://github.com/ARUP-CAS/aiscr-oao/issues).
