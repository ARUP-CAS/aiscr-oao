# Přispívání do repozitáře aiscr-oao

Děkujeme za zájem o přispění do projektu Mapa OAO.

---

## Nahlašování chyb

Chyby a návrhy hlaste jako [GitHub Issues](https://github.com/ARUP-CAS/aiscr-oao/issues).

Před vytvořením issue prosím zkontrolujte, zda podobný issue již neexistuje.

---

## Vývojový workflow

Repozitář používá **jednobranching workflow** — veškerý vývoj cílí na větev `main`.

### Vytvoření větve

Větve vytvářejte z aktuální větve `main`:

```bash
git checkout main
git pull
git checkout -b <typ>/<popis>
```

### Pojmenování větví

Aplikační změny:

```markdown
feature/<popis>
bugfix/<popis>
docs/<popis>
data/<popis>
chore/<popis>
```

Větve agentů:

```markdown
agents/<jmeno-agenta>/<popis>
```

Příklady:

```markdown
feature/add-kraj-filter
bugfix/fix-map-popup
docs/update-readme
data/refresh-oao-data
agents/claude/review-shiny-structure
```

---

## Pull requesty

Každý pull request musí:

- cílit na větev `main`
- obsahovat stručný popis změny
- uvádět motivaci pro změnu
- odkazovat na příslušný issue (je-li k dispozici)
- projít CI kontrolami před mergem

### Šablona popisu PR

```markdown
## Popis změny
<stručný popis>

## Motivace
<proč je změna potřebná>

## Testování
<co bylo ověřeno>

## Issue
Closes #<číslo>
```

Používejte **Draft PR** pro rozpracované změny.

---

## Konvence commit messages

Formát:

```markdown
<typ>: <stručný popis v češtině nebo angličtině>
```

Typy:

- `feat` — nová funkce
- `fix` — oprava chyby
- `docs` — změna dokumentace
- `data` — aktualizace dat
- `style` — formátování bez změny logiky
- `refactor` — refaktoring bez změny chování
- `chore` — údržba, závislosti

Příklady:

```markdown
feat: přidat filtr podle kraje
fix: opravit zobrazení popup okna na mobilních zařízeních
data: aktualizovat data OAO k 2026-02
chore: aktualizovat R závislosti
```

---

## AI-generované příspěvky

Větve generované AI agenty musí dodržovat konvenci:

```markdown
agents/<jmeno-agenta>/<popis>
```

Příklady:

```markdown
agents/claude/review-shiny-structure
agents/claude/dependency-analysis
```

AI agenti nesmí:

- pushovat přímo do větve `main`
- slučovat větve bez lidské kontroly
- měnit data v `app/data/` bez explicitního pokynu

Veškerá práce AI agentů musí být zkontrolována člověkem před mergem.

Výstupy AI review se ukládají do:

```markdown
docs_agents/
```

---

## Code style

- R kód musí být formátován pomocí [`styler`](https://styler.r-lib.org/)
- dodržujte stávající styl kódu v repozitáři
- komentáře a dokumentace mohou být psány v češtině nebo angličtině
