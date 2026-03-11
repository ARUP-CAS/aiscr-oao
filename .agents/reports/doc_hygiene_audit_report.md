# Documentation Hygiene Audit Report

**Repository:** aiscr-oao  
**Date:** 2026-03-11  
**Files audited:** 17  
**Fixes applied:** 2026-03-11

## Summary

| Check | Status | Findings |
|-------|--------|----------|
| C1 File Discovery | OK | 17 instruction-bearing files found |
| C2 Audience Mapping | OK | No redundant files; distinct audiences |
| C3 Duplication | OK | Reduced by trimming CLAUDE.md and repo-rules; 0 contradictory |
| C4 Drift | OK | Fixed: embedded config removed, task ID T09→T08 |
| C5 Cross-References | OK | Fixed: review_reports/README T09→T08 |
| C6 Token Efficiency | OK | CLAUDE.md and repo-rules.mdc trimmed; cross-refs to AGENTS.md |
| C7 Governance | OK | “Dokumentace a konfigurace (správa)” added to CONTRIBUTING.md |

---

## C1 — File Discovery

| File | Lines | Language | Apparent audience |
|------|-------|----------|-------------------|
| `README.md` | 81 | Czech | GitHub visitors, contributors |
| `README_en.md` | 82 | English | Same (EN) |
| `CONTRIBUTING.md` | 131 | Czech | Human contributors, AI agents |
| `AGENTS.md` | 326 | English | AI agents (all platforms) |
| `CLAUDE.md` | 51 | English | Claude Code (auto-injected) |
| `.cursor/rules/repo-rules.mdc` | 37 | English | Cursor AI (always-applied) |
| `.cursor/rules/arena-model-sync.mdc` | 26 | English | Cursor AI (always-applied) |
| `.agents/README.md` | 22 | Czech | Developers, agents exploring .agents/ |
| `.agents/config/review_config.yaml` | 182 | YAML | Review system, agents |
| `.agents/config/review_cache.json` | 21 | JSON | Review system state |
| `.agents/config/automation_recommendations.md` | 87 | English | Team automation setup |
| `.agents/prompts/audit_doc_hygiene.md` | 239 | English | Agent running the audit |
| `.agents/prompts/review_codebase.md` | 691 | Czech/English | Review agent (long-running) |
| `.agents/prompts/prompt_evolution/README.md` | 19 | Czech | Agents, human reviewers |
| `.agents/reports/bugs.md` | 13 | Markdown | Agents, maintainers |
| `.agents/reports/refactoring_backlog.md` | 21 | Markdown | Agents, maintainers |
| `.agents/reports/review_reports/README.md` | 28 | Czech | Agents, reviewers |

**Note:** `.github/` contains only `dependabot.yml`; no `CODEOWNERS` or `PULL_REQUEST_TEMPLATE.md`. No `.github/workflows/` directory present. `.cursor/` is in `.gitignore` (local-only rules).

---

## C2 — Audience & Responsibility

| File | Primary audience | Responsibility |
|------|------------------|----------------|
| `README.md` / `README_en.md` | GitHub visitors | Project overview, repo contents, link to CONTRIBUTING |
| `CONTRIBUTING.md` | Contributors, agents | Branch naming, PRs, code style, AI branch conventions |
| `AGENTS.md` | AI agents | Governance, scope, orientation, “do not” rules, .agents/ |
| `CLAUDE.md` | Claude Code | Short project memory; points to AGENTS.md and CONTRIBUTING.md |
| `.cursor/rules/*.mdc` | Cursor | Repo gotchas; model sync (local) — no duplication of AGENTS.md |
| `.agents/README.md` | Humans/agents in .agents/ | Structure of .agents/ and pointer to AGENTS.md |
| `review_config.yaml` | Review system | Task config, limits, paths, domain analyses |
| `review_cache.json` | Review system | Task status, file hashes |
| `automation_recommendations.md` | Team | MCP, skills, hooks, subagents (shared; not .cursor/) |
| `review_codebase.md` | Review agent | Task registry, procedures, init sequence |
| `audit_doc_hygiene.md` | Any agent | Portable doc-hygiene audit prompt |
| `bugs.md` / `refactoring_backlog.md` | Maintainers, agents | Log of issues; backlog |
| `review_reports/README.md` | Report readers | Report naming and structure |

**Verdict:** Each file has a distinct audience and responsibility. No redundant file pair.

---

## C3 — Content Duplication Detection

### Topics duplicated across files

| Topic | Files | Classification |
|-------|--------|----------------|
| Branch workflow / base branch `main` | CONTRIBUTING.md, AGENTS.md, CLAUDE.md, repo-rules.mdc | **Acceptable** — CONTRIBUTING is human-facing; AGENTS/CLAUDE/rules reinforce for agents. |
| Branch naming `feature/`, `bugfix/`, `agents/<name>/<topic>` | CONTRIBUTING.md, AGENTS.md, CLAUDE.md, repo-rules.mdc | **Redundant** — Same audience (agents) in AGENTS.md, CLAUDE.md, and repo-rules. Could keep in AGENTS.md only and cross-refer from CLAUDE and rules. |
| “Do not edit `app/data/`” | AGENTS.md, CLAUDE.md, repo-rules.mdc, review_codebase.md, automation_recommendations | **Redundant** — Stated in many places; AGENTS.md is canonical. |
| Code style / styler | CONTRIBUTING.md, AGENTS.md, CLAUDE.md | **Acceptable** — CONTRIBUTING owns detail; AGENTS/CLAUDE reference it. |
| “Read these files before starting” (repository_map, review_cache, bugs, refactoring_backlog, review_codebase) | AGENTS.md, CLAUDE.md, repo-rules.mdc | **Redundant** — Same list in three auto-injected / always-read files. |
| Directory table (app/, code/, emailer/, etc.) | README, README_en, AGENTS.md, CLAUDE.md, repo-rules | **Acceptable** — README for visitors; AGENTS/CLAUDE for agents. |
| Emailer: “read source, see T05” | AGENTS.md, CLAUDE.md, repo-rules.mdc, review_codebase.md | **Acceptable** — Single consistent rule. |
| PRs target main, CI must pass | CONTRIBUTING.md, AGENTS.md | **Acceptable** — CONTRIBUTING detail; AGENTS summary. |

**Contradictory duplication:** None.

**Redundant duplication (token waste):** Branch naming and “must read” list repeated in AGENTS.md, CLAUDE.md, and repo-rules. Replacing with “see AGENTS.md” in CLAUDE and repo-rules would reduce tokens.

---

## C4 — Drift Detection

### 1. Embedded config vs live config — **FAIL**

- **Files:** `.agents/prompts/review_codebase.md` (embedded YAML, ca. lines 91–156) vs `.agents/config/review_config.yaml` (live file).
- **Difference:** The prompt contains an **inline YAML block** that does not match the live config:
  - Embedded: flat `repository: aiscr-oao`, `branch: main`, `github_url`, `open_issues`; no `stack`, no `ci_cd`, no `review`; `domain_analyses` is a subset; no `repository_map`/T01 or `scripts_analysis`/T04 in embedded snippet; no `.gitignore` in `important_files`.
  - Live: full `repository` object (name, url, type, organisation, version, etc.), `stack`, `ci_cd`, `review`, full `domain_analyses` with task_id and scripts_analysis, security_analysis, `.gitignore` in important_files.
- **Which is correct:** The **live** `review_config.yaml` is the source of truth. The embedded block is outdated and can mislead agents who only read the prompt.
- **Action:** Remove the embedded YAML from `review_codebase.md` and replace with a short pointer: “Configuration is in `.agents/config/review_config.yaml`. Do not duplicate it here.”

### 2. Task ID for final audit — **FAIL**

- **Files:** `.agents/reports/review_reports/README.md` vs `.agents/prompts/review_codebase.md`.
- **Difference:** review_reports/README says `final_audit.md — finální souhrnný audit (T09)`; review_codebase.md defines T08 as the final audit and “T08 requires T01–T07”.
- **Which is correct:** **T08** is correct (task registry in review_codebase.md defines T01–T08; T08 is final).
- **Action:** In `.agents/reports/review_reports/README.md`, change “T09” to “T08”.

---

## C5 — Cross-Reference Integrity

| Reference | Target | Status |
|-----------|--------|--------|
| README.md “Viz [CONTRIBUTING.md](CONTRIBUTING.md)” | CONTRIBUTING.md | OK — file exists; no § section. |
| README_en.md “See [CONTRIBUTING.md](CONTRIBUTING.md)” | CONTRIBUTING.md | OK. |
| CLAUDE.md “see CONTRIBUTING.md” (styler) | CONTRIBUTING.md | OK. |
| CLAUDE.md “see T05 in `.agents/prompts/review_codebase.md`” | review_codebase.md, § T05 | OK — section exists. |
| .agents/README.md “Viz [AGENTS.md](../AGENTS.md)” | AGENTS.md | OK. |
| AGENTS.md “see T05 in `.agents/prompts/review_codebase.md`” | review_codebase.md, T05 | OK. |
| .agents/reports/review_reports/README.md “final_audit.md (T09)” | Task ID | **Was stale** — fixed to T08 (2026-03-11). |

**Broken/stale:** 0 (was 1; fixed T09→T08 in review_reports/README.md).

---

## Applied fixes (2026-03-11)

| Recommendation | File(s) | Action taken |
|-----------------|---------|--------------|
| Remove embedded config | `.agents/prompts/review_codebase.md` | Replaced inline YAML block with pointer to `.agents/config/review_config.yaml`. |
| Fix final audit task ID | `.agents/reports/review_reports/README.md` | Changed “T09” to “T08”. |
| Trim CLAUDE.md | `CLAUDE.md` | Removed full architecture table and workflow paragraph; kept canonical context, commands, and 2 gotchas; added “See AGENTS.md and CONTRIBUTING.md” for the rest. |
| Trim repo-rules.mdc | `.cursor/rules/repo-rules.mdc` | Shortened “Branch workflow” to one line + cross-ref; added “See AGENTS.md for full orientation” to AI artefacts bullet. |
| Add documentation governance | `CONTRIBUTING.md` | Added section “Dokumentace a konfigurace (správa)” defining ownership of README, CONTRIBUTING, AGENTS.md, .agents/ and prohibiting duplication of branch/workflow/code-style rules outside these files. |

---

## C6 — Token Efficiency (AI-specific)

Files likely auto-injected or always applied: `CLAUDE.md`, `.cursor/rules/repo-rules.mdc` (and possibly `AGENTS.md` depending on platform).

- **CLAUDE.md (51 lines):** Explicitly defers to AGENTS.md and CONTRIBUTING.md. Some overlap remains: architecture table, gotchas, workflow one-liner. Estimated **~150–250 tokens** could be saved by shortening to “See AGENTS.md and CONTRIBUTING.md; only list here repo-root commands and 2–3 gotchas not in AGENTS.md.”
- **repo-rules.mdc (37 lines):** Repeats canonical sources, branch workflow, and “read repository_map, review_cache, bugs, refactoring_backlog, review_codebase.” If agents already get AGENTS.md, this is **~100–150 tokens** of redundancy. Recommendation: keep only repo-specific gotchas (app/, code/, emailer/, tests, .agents/) and “see AGENTS.md for full workflow”.
- **AGENTS.md:** Long but is the single source of truth; reducing duplication *into* AGENTS.md from CLAUDE and rules is the main lever.

**Estimated token savings:** ~400–600 if CLAUDE.md and repo-rules are trimmed to cross-references and non-duplicated gotchas.  
**Status:** Applied 2026-03-11 (CLAUDE.md and repo-rules.mdc trimmed).

---

## C7 — Governance Rules Presence

- **Explicit “which file owns which information” / “do not duplicate” section:** Not present in CONTRIBUTING.md or AGENTS.md.
- **Implicit governance:** AGENTS.md states that it and CONTRIBUTING.md are sources of truth and that `.agents/` should align with them; repo-rules say “Do **not** duplicate rules here—keep details in the canonical documents.” So the *intent* is clear but there is no dedicated “Documentation governance” subsection.
- **Verdict:** **WARN.** Suggest adding a short “Documentation and config ownership” subsection to CONTRIBUTING.md or AGENTS.md (e.g. “README = project overview; CONTRIBUTING = workflow and style; AGENTS.md = agent governance; .agents/ = review and automation; do not duplicate branch/workflow rules outside CONTRIBUTING and AGENTS.md”).

---

## Recommended Fixes

### Critical (FAIL) — applied

1. ~~**Remove embedded config from review prompt**~~ — **File:** `.agents/prompts/review_codebase.md` — **Done:** Replaced with pointer to live `review_config.yaml`.
2. ~~**Fix final audit task ID**~~ — **File:** `.agents/reports/review_reports/README.md` — **Done:** T09 → T08.

### Important (WARN) — applied

1. ~~**Trim CLAUDE.md for token efficiency**~~ — **File:** `CLAUDE.md` — **Done:** Architecture/workflow replaced with cross-ref; kept commands and 2 unique gotchas.
2. ~~**Trim repo-rules.mdc**~~ — **File:** `.cursor/rules/repo-rules.mdc` — **Done:** Branch workflow shortened; added “See AGENTS.md” for full rules.
3. ~~**Add documentation governance**~~ — **File:** `CONTRIBUTING.md` — **Done:** New subsection “Dokumentace a konfigurace (správa)” with ownership and no-duplication rule.

### Optional improvements (unchanged)

1. Create `.github/CODEOWNERS` and optionally `.github/PULL_REQUEST_TEMPLATE.md` if the project wants them (audit does not require them).
2. If `.github/workflows/` is intentionally empty, add a one-line note in `review_config.yaml` or review_codebase.md that “workflows directory may be empty; Dependabot only in .github at present.”
