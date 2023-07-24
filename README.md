# Licensed archaeological organizations app

![Website](https://img.shields.io/website?down_message=down&label=https%3A%2F%2Foao.aiscr.cz%2F&up_message=up&url=https%3A%2F%2Foao.aiscr.cz%2F)
![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.8178384.svg)
![GitHub](https://img.shields.io/github/license/ARUP-CAS/map_oao)

## Mapa archeologických organizací (Mapa OAO)

A repo containing code for a Shiny app publishing data on organizations 
with a licence to conduct archaeological excavations in the Czech Republic.
Currently, the app is in Czech language only, sorry.

The app is deployed here: <https://oao.aiscr.cz/>. Originally, it was developed here: <https://github.com/petrpajdla/map_oao/>.

The app is part of the **Archaeological information system of the Czech Republic**
(<a href="https://www.aiscr.cz">AIS CR</a>).

## How to cite

Please cite this sofware as:

> Pajdla, P. 2023: *Mapa archeologických organizací* *[Map of Archaeological Organizations]*.
> Source code: <https://github.com/ARUP-CAS/aiscr-oao>, application: <https://oao.aiscr.cz/>
  DOI: [10.5281/zenodo.8178384](https://doi.org/10.5281/zenodo.8178384).

A poster presenting the app:

> Pajdla, P. 2022: *Organizace s oprávněním provádět archeologický výzkum: Mapová aplikace*
> *[Organizations with permission to conduct archaeological research: Map application]*.
> Počítačová podpora v archeologii 21, 1. 6. 2022 – 3. 6. 2022, Kostelec nad Černými lesy, Czech Republic.
> DOI: <https://doi.org/10.5281/zenodo.7515094>.

## Contents

The **repo** contains:

- [app/](/app/) Code for the [Shiny](https://shiny.rstudio.com/) app:
  - [app/data/](/app/data/) Processed geospatial data;
  - [app/R/](/app/R/) Functions used by the server;
  - [app/text/](/app/text/) About page and descriptions for some of the OAOs;
  - [app/www/](/app/www/) Images, .CSS etc.
- [code/](/code/): R scripts preparing data to desired shape and format.

## Licenses

Code is released under [MIT License](LICENSE), data, texts, figures etc. under [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/) license.
