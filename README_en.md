# OAO Map — Map of Licensed Archaeological Organizations

[![Website](https://img.shields.io/website?down_message=down&label=https%3A%2F%2Foao.aiscr.cz%2F&up_message=up&url=https%3A%2F%2Foao.aiscr.cz%2F)](https://oao.aiscr.cz)
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.8178384.svg)](https://doi.org/10.5281/zenodo.8178384)
[![GitHub license](https://img.shields.io/github/license/ARUP-CAS/aiscr-oao)](LICENSE)

[ČESKÁ VERZE ZDE](README.md)

This repository contains the source code for an [R Shiny](https://shiny.posit.co/)
application presenting data on organizations licensed to conduct archaeological
excavations in the Czech Republic.

The application is deployed at: **<https://oao.aiscr.cz/>**

This repository is a fork of [petrpajdla/map_oao](https://github.com/petrpajdla/map_oao).
The ARUP-CAS fork extends the original application with an emailer component,
plots (`plots/`), GitHub Actions workflows and additional functionality.

The application is part of the
[**Archaeological Information System of the Czech Republic** (AIS CR)](https://www.aiscr.cz/)
infrastructure.

---

## Repository Contents

```
app/          — Shiny application source code
  data/       — Processed geospatial data
  R/          — Functions used by the server
  text/       — About page and organization descriptions
  www/        — Static assets (images, CSS, JS)
code/         — R scripts for data preparation
emailer/      — Component for sending notifications to regional authorities
plots/        — Charts and data visualisations
.github/      — GitHub Actions workflows
```

---

## Features

The application allows users to:

- search for organizations by location or cadastral territory
- display the sphere of activity of individual organizations on a map
- view an overview of organizations by region (kraj)
- download data in GeoPackage format (CC BY 4.0)
- fulfil the statutory obligation of OAOs to notify regional authorities (emailer)

---

## Data Sources

- contracts between the Czech Academy of Sciences and licensed organizations
- list of licences issued by the Ministry of Culture of the Czech Republic
- archaeological fieldwork data from AMCR

---

## How to Cite

```
Pajdla, P. 2023: Mapa oprávněných archeologických organizací
[Map of Authorized Archaeological Organizations].
DOI: 10.5281/zenodo.8178384.
Source code: https://github.com/ARUP-CAS/aiscr-oao,
application: https://oao.aiscr.cz/.
```

---

## Licences

Code is released under the [MIT License](LICENSE).  
Data, texts and figures are released under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

---

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

Report bugs as [GitHub Issues](https://github.com/ARUP-CAS/aiscr-oao/issues).
