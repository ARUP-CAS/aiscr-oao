# Mapa OAO — CLAUDE.md

Project memory for Claude Code. Keep concise; details live in **AGENTS.md** and **CONTRIBUTING.md**.

---

## Canonical agent context

- **Governance:** `AGENTS.md` — scope, behaviour, branch rules, `.agents/` orientation.
- **Automation:** `.agents/config/automation_recommendations.md` — MCP servers, skills, hooks, subagents (shared; do not rely on `.cursor/` or `.claude/` for team rules).
- **Before starting work:** Read `.agents/analysis/repository_map.json`, `.agents/config/review_cache.json`, `.agents/reports/bugs.md`, `.agents/reports/refactoring_backlog.md`, `.agents/prompts/review_codebase.md`.

---

## Commands

```bash
# Run Shiny app locally (from repo root; R required)
R -e "shiny::runApp('app')"
```

- **Data pipeline:** R scripts in `code/` (e.g. `00_ku_centroids.R` → `05_kraje.R`). Run in order when refreshing geospatial data; do **not** edit `app/data/` by hand.
- **Format R:** `styler` (see CONTRIBUTING.md). No dedicated test suite; run the app or relevant `code/` scripts after changes.

---

## Gotchas (not in AGENTS.md)

- **Do not modify** `app/data/`, `emailer/secret.txt`, or generated outputs of `code/` unless explicitly asked.
- Remote Shiny server restarts only when `app/app.R` changes (see comment in `app/app.R`).

For architecture, workflow, and branch rules see **AGENTS.md** and **CONTRIBUTING.md**.
