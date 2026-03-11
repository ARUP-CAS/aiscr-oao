# Review Reports — aiscr-oao

Tento adresář obsahuje výstupy AI review sessions.

## Formát názvu souboru

```
<task_id>.md             — výstup konkrétního tasku (např. T01.md)
final_audit.md           — finální souhrnný audit (T08)
```

## Struktura každého reportu

```markdown
# <Task ID> — <Název úlohy>

**Datum:** <iso8601>
**Dotčené soubory:** <seznam>

## 1. Shrnutí zjištění
## 2. Identifikované problémy
## 3. Návrhy zlepšení
## 4. Odhad náročnosti
## 5. Návrhy na zlepšení promptu
```

Reporty jsou generovány AI agenty a vyžadují lidský review před mergem.
