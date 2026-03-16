# Prompt Evolution — aiscr-oao

Tento adresář obsahuje návrhy na zlepšení `.agents/prompts/review_codebase.md`
shromážděné v průběhu AI review sessions.

## Formát názvu souboru

```
<task_id>_prompt_update.md     — návrhy z konkrétního tasku (např. T01_prompt_update.md)
```

## Workflow

1. Agent na konci každého tasku zapíše návrhy do `<task_id>_prompt_update.md`.
2. Návrhy se hromadí napříč sekcemi.
3. Lidský reviewer přijme nebo zamítne návrhy.
4. Přijané návrhy se aplikují na `.agents/prompts/review_codebase.md` před zahájením nového audit cyklu.
5. Agent nesmí sám modifikovat `review_codebase.md`.
