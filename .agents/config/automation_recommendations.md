# Automation recommendations (shared rules)

This file defines **shared** automation recommendations for AI agents and editors (Cursor, Claude Code, etc.). Keep these in the repo (AGENTS.md + `.agents/`) so they are versioned and not gitignored. Do **not** rely on `.cursor/` or `.claude/` for shared rules — those directories are in `.gitignore`.

**Canonical governance:** `AGENTS.md` (and `CONTRIBUTING.md`) remain the source of truth. This document is a companion for automation setup only.

---

## Codebase profile

- **Type:** R Shiny application (Mapa OAO)
- **Framework:** Shiny, leaflet, sf, tidyverse
- **CI/CD:** GitHub Actions
- **Existing shared context:** `AGENTS.md`, `.agents/analysis/`, `.agents/config/review_config.yaml`, `.agents/prompts/review_codebase.md`

---

## MCP servers (recommended for team)

Document here so everyone can install the same set. Actual config (e.g. `.mcp.json`) may be local; this list is the shared recommendation.

| Priority | Server | Why |
|----------|--------|-----|
| 1 | **plugin-github-github** (GitHub) | Issues, PRs, Actions — repo uses GitHub for CI and collaboration. |
| 2 | **context7** | Up-to-date docs for R, Shiny, leaflet, sf, tidyverse when agents need API reference. |

*Optional:* If the team uses Figma for UI, add **plugin-figma-figma**. No database/Supabase/Docker in this repo, so skip those MCPs.

---

## Skills (where to document)

- **Canonical list:** `AGENTS.md` → “Recommended Skills”. Keep that section as the single place for repo-recommended skills.
- **Implementation:** Skills live in each user’s environment (Cursor/Claude). This file only summarises; AGENTS.md is the source of truth.

**Recommended (see AGENTS.md for full list):**

| Skill | When to use |
|-------|--------------|
| `gh-fix-ci` | Diagnosing or fixing GitHub Actions CI failures. |
| `gh-address-comments` | Addressing PR review comments. |
| `security-best-practices` | *(optional)* Reviewing `emailer/`, secrets, or on explicit security request. |

Do **not** add skills that assume a different stack (e.g. frontend-design, database, Figma) unless the repo gains that stack.

---

## Hooks (shared guidance only)

Hooks are editor-specific and often live in gitignored config (e.g. `.cursor/`). So **do not** store hook config in the repo. Instead, document desired behaviour here so humans can configure locally:

| Hook type | Recommendation | Reason |
|-----------|----------------|--------|
| PreToolUse | Block edits to `app/data/`, `emailer/secret.txt`, `renv.lock` (or require confirmation) | Aligns with AGENTS.md out-of-scope and data-update rules. |
| PostToolUse | Optional: run `styler` on edited R files, or run relevant R script if `code/` changed | Matches repo coding standards (styler, R conventions). |

Agents and users set these up in their own editor (e.g. Cursor rules or Claude hooks); no shared `.cursor/` files.

---

## Subagents / specialized reviewers

Document profiles here; actual subagent config stays in each editor. Agents reading this file can suggest or run equivalent workflows.

| Role | When to use | Focus |
|------|-------------|--------|
| **code-reviewer** | Larger refactors or multi-file changes | Consistency with repo conventions, AGENTS.md scope, no `app/data/` edits. |
| **security-reviewer** | Changes in `emailer/`, any new secrets or config | Secrets, config exposure, and `review_config.yaml` security_analysis. |

Point subagent prompts at reading `AGENTS.md`, `.agents/analysis/repository_map.json`, and `.agents/reports/refactoring_backlog.md` before reviewing.

---

## Plugins (optional)

- **General:** Plugin that provides `doc`, `gh-fix-ci`, `gh-address-comments` aligns with AGENTS.md “Recommended Skills”.
- No need for frontend-design or database plugins for this R/Shiny repo.

---

## Where to update

- **Governance and scope:** `AGENTS.md`, `CONTRIBUTING.md`
- **Review workflow and task list:** `.agents/prompts/review_codebase.md`, `.agents/config/review_config.yaml`
- **This automation list:** `.agents/config/automation_recommendations.md`
- **Do not** put shared rules or automation lists under `.cursor/` or `.claude/` — they are gitignored and not shared.
