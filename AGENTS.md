# AGENTS.md — Instructions for AI Agents

This file defines the rules, scope and workflows for AI agents (Claude Code,
GitHub Copilot, Cursor, etc.) working in this repository.

Rules in this file apply to the entire repository.
A nested `AGENTS.md` in a subdirectory takes precedence for that subtree.

---

## Repository Overview

**Repository:** `ARUP-CAS/aiscr-oao`  
**Type:** R Shiny application  
**Published at:** https://oao.aiscr.cz/  
**Version:** v3.1.0 (January 2026)  
**Licence:** MIT (code), CC BY 4.0 (data, texts, figures)

This repository contains the **Mapa OAO** application — an interactive map
of organizations licensed to conduct archaeological excavations in the Czech Republic.

The application is part of the **AIS CR** (Archaeological Information System of the
Czech Republic) infrastructure maintained by ARUP-CAS.

Key components:

- Shiny application (`app/`) — interactive map and data views
- Data preparation scripts (`code/`) — R scripts producing processed geospatial data
- Emailer component (`emailer/`) — purpose and mechanism to be confirmed by reading the
  source code; see T05 in `docs_agents/PROMPT.md`
- Plots and visualisations (`plots/`)
- GitHub Actions CI/CD (`.github/`)

---

## Repository Orientation (Mandatory First Step)

Before starting any work, agents must gather repository context.

Always read the following files first:

- `docs_agents/repository_map.json`
- `docs_agents/review_cache.json`
- `docs_agents/bugs.md`
- `docs_agents/refactoring_backlog.md`
- `docs_agents/PROMPT.md`

These files contain context accumulated across previous review sessions.
Reading them prevents duplicated work and ensures continuity.

### Resolving Inconsistencies

If content in `docs_agents/` contradicts high-level repository rules or governance
defined in this document (`AGENTS.md`), `CONTRIBUTING.md`, or other authoritative
project documentation, agents must treat those higher-level documents as the
**source of truth**.

In such cases agents should:

1. Prefer the high-level governance rules defined in:
   - `AGENTS.md`
   - `CONTRIBUTING.md`
   - repository coding standards
2. Adapt or update affected files in `docs_agents/` to align with those rules.
3. Record the adjustment in the review history (for example `review_cache.json`
   or `refactoring_backlog.md`) when relevant.

This ensures long-running AI review artefacts remain consistent with
current repository governance.

---

## AI-Generated Content

All artefacts produced by AI agents belong in the `docs_agents/` directory.

Examples include:

- audit reports
- analysis JSON files
- review state updates
- prompt evolution notes

Agent work must be committed to branches named:

```markdown
agents/<agent_name>/<topic>
```

Examples:

```markdown
agents/claude/review-shiny-structure
agents/claude/emailer-analysis
```

Rules:

- agent branches must always be created from `main`
- agent branches must never push directly to protected branches
- all agent work requires human review before merge

---

## Goal

Agents should aim for **small, safe, reviewable improvements** aligned with:

- repository coding conventions
- CI requirements
- long-term maintainability of the Shiny application
- data accuracy and update procedures

Agents must avoid large refactors unless explicitly requested.

The framework is designed to support **long-running incremental technical review**
of the repository.

---

## Agent Behaviour

Agents must:

- gather repository context before starting work
- avoid repeating previously recorded work
- prefer incremental improvements
- record findings in `docs_agents/`
- keep changes minimal and well scoped
- follow existing R coding conventions
- suggest improvements to `AGENTS.md` when appropriate

Agents must not modify data files in `app/data/` unless explicitly instructed.
Data updates follow a defined process via `code/` scripts.

### Recommended Skills

Specialised capabilities that may improve maintenance efficiency.

Skills are **optional helpers**, not mandatory tools.

- `doc` — reviewing and editing documentation artefacts
- `gh-fix-ci` — diagnosing and fixing GitHub Actions CI failures
- `gh-address-comments` — incorporating pull request review comments

---

## Verification Sources

When verifying behaviour or documentation, use the following sources in order of authority:

1. Live application at https://oao.aiscr.cz/
2. Source code in this repository
3. Repository documentation

Related live systems:

| Source | URL | Purpose |
| --- | --- | --- |
| Mapa OAO | https://oao.aiscr.cz/ | Live application |
| AMCR info site | https://amcr-info.aiscr.cz/ | Terminology, OAO data context |
| AIS CR main site | https://www.aiscr.cz/ | System context |
| AMCR | https://amcr.aiscr.cz/ | Source of fieldwork data used by the app |

---

## Scope

### In Scope

Agents may review and modify all source code in this repository:

- `app/` — Shiny application code and static assets
- `code/` — R scripts for data preparation
- `emailer/` — emailer component (read code first before drawing any conclusions
  about its purpose; do not assume its function without inspecting the source)
- `plots/` — plots and visualisations
- `.github/` — CI/CD workflows
- `docs_agents/` — AI review artefacts
- `CITATION.cff`, `README.md`, `README_en.md`, `CONTRIBUTING.md`

### Out of Scope

Agents must not modify:

- `app/data/` — processed geospatial data (updated via `code/` scripts, not manually)

Generated artefacts must not be modified:

- any output files produced by `code/` scripts

---

## Tech Stack

Technologies detected in this repository:

### Application

- R (primary language, 65.5%)
- Shiny — interactive web application framework
- HTML (27.1%) — templates and static content
- JavaScript (4.4%) — custom interactivity
- CSS (1.9%) — custom styling
- Shell (1.1%) — deployment scripts

### Key R Packages

- `shiny` — application framework
- `leaflet` — interactive maps
- `sf` — geospatial data processing
- `dplyr` / `tidyverse` — data manipulation
- `ggplot2` — static visualisations (`plots/`)

### Infrastructure

- GitHub Actions — CI/CD
- Shiny Server or shinyapps.io — deployment (verify in `.github/`)
- R environment management: `renv` (verify if `renv.lock` exists)

### Coding Standards

R code must follow conventions already established in the repository:

- use `styler` for formatting where applicable
- prefer tidyverse-style data manipulation
- comments may be in Czech or English (match surrounding code)
- avoid hardcoded paths — use relative paths from project root

---

## Branch and PR Rules

This repository uses a **single-branch workflow**.

Development workflow:

```markdown
feature / bugfix / agents branches → main
```

Rules:

- all work targets `main`
- never push directly to `main`
- always open a Pull Request
- changes must pass CI before merge

Branch naming:

```markdown
feature/<topic>
bugfix/<topic>
docs/<topic>
data/<topic>
chore/<topic>
agents/<agent>/<topic>
```

Changes to `docs_agents/` always require **human review before merge**.

---

## docs_agents Structure

The `docs_agents/` directory stores persistent AI review context.

`PROMPT.md` — instructions for long-running AI review sessions;
contains the initialization sequence, task registry and execution procedure.

`review_config.yaml` — configuration for review modules based on the repository stack.

`repository_map.json` — high-level structural overview of the repository.

`dependency_graph.json` — R package dependencies and external data dependencies.

`review_cache.json` — persistent storage of results from previous AI review sessions.

`bugs.md` — structured log of discovered issues.

`refactoring_backlog.md` — long-term improvement backlog.

`shiny_analysis.json` — Shiny application structure and reactivity analysis.

`emailer_analysis.json` — emailer component analysis (purpose determined during T05).

`frontend_analysis.json` — custom HTML, JS and CSS analysis.

`cicd_analysis.json` — GitHub Actions workflow analysis.

For execution procedures, refer to `docs_agents/PROMPT.md`.

---

## Repository Context

This repository is part of the **AIS CR infrastructure** maintained by **ARUP-CAS**.

Related repositories:

- `aiscr-webamcr` — AMCR main application (source of fieldwork data displayed in Mapa OAO)
- `aiscr-digiarchiv-2` — Digital Archive AMCR
- `aiscr-webamcr-help` — AMCR user documentation
- `aiscr-api-home` — AMCR API documentation
