# FC5 main.adl — onafhankelijke verificatie van de diagnose-spreadsheet

Doel: controleren of de waarden in `main-diagnosis.xlsx` voor het script
`FC5/project/main.adl` overeenkomen met onafhankelijk uit de `FSpec`
afgeleide waarden, zónder gebruik te maken van `Ampersand.Diagnosis.Extract`.

## Aanpak

1. **Independent walker** in `DiagnoseCheck.hs` (in de project-root). Deze
   module importeert uitsluitend `Ampersand.{ADL1,Basics,Classes,
   FSpec.FSpec,Core.AbstractSyntaxTree}` en herimplementeert:
   - `termComplexity :: Expression -> Int` (telt EDcD/EDcI/EDcV/EBin/EMp1
     bladen),
   - `internalRefs :: Set Relation -> Expression -> Int` (alleen EDcD
     bladen die in de gegeven set zitten),
   - `walkIfc :: Interface -> IWalk` (telt `Box` nodes, max nesting,
     verzamelt alle `ObjectDef` velden).
2. **Sandbox-loader** (`Sandbox.loadTestFSpec`) bouwt een complete
   `FSpec` voor `FC5/project/main.adl`. De resulterende `fSpec` is
   doorgegeven aan `DiagnoseCheck.dumpAll`, die per categorie een TSV
   schrijft naar `/tmp/fc5_independent/`.
3. **Ampersand documentation** opnieuw gedraaid met de huidige
   `stack build` binary; de productie-xlsx is in `/tmp/fc5_fresh/main-diagnosis.xlsx`.
   `openpyxl` heeft de sheets gedumpt naar `/tmp/fc5_fresh_dump/*.tsv`.
4. **Vergelijking** met `compare_diagnose.py`. Per pattern / rule /
   interface worden de overlappende metrics gemeten. Mismatches komen
   als checklist-items in `/tmp/fc5_comparison.txt`.

## Totalen (FSpec → independent)

| Categorie | FSpec count |
|---|---|
| Patronen (`vpatterns`) | 30 |
| Relaties (`vrels`) | 312 |
| Regels (`vrules`) | 55 |
| Concepten (via `concs (vrels)`) | 96 |
| Interfaces (`interfaceS`) | 17 |
| Views (`vviews`) | 19 |
| IDENT-regels globaal (`vIndices`) | 1 |

Vergelijking met de versgegenereerde xlsx (kolom = aantal datarijen,
zonder header):

| Sheet | independent | xlsx | match? |
|---|---|---|---|
| Patterns | 30 | 30 | ✅ |
| Concepts | 96 | 96 | ✅ |
| Relations | 312 | 312 | ✅ |
| Rules | 55 | 55 | ✅ |
| Interfaces | 17 | 17 | ✅ |

## Per-rij vergelijking

`compare_diagnose.py` vergelijkt onderstaande kolommen:

- **Patterns** — `#concepts`, `#relations`, `#invariant rules`,
  `#process rules`, `#IDENT rules`, `#ENFORCE rules`, `#views`,
  `#ExecEngine rules`, `Largest rule term complexity`,
  `#violations in initial population`, `Cohesion ratio`.
- **Rules** — `Term complexity`, `#relations referenced`,
  `#concepts referenced`, `#violations`, `Sample violations`.
- **Interfaces** — `#boxes`, `Max nesting depth`, `#fields`,
  `CRUD coverage`, `#relations with C`, `#relations with U`,
  `#relations with D`.

### Vinklijst — afwijkingen

- [x] **Patterns** — 30 rijen, **0** afwijkingen op de 11 vergeleken
      metrics. _(Side-note: er bestaan twéé patronen met de naam
      `EisenEnDekkingen` — één in `Kernmodel.adl:14` en één in
      `Eisen.adl:34`. Beide instances staan in zowel het independent
      bestand als de xlsx, met identieke waarden. Het script dat per
      naam dedupliceert ziet er daarom 29; dat is dus een artefact van
      de vergelijking, niet van de diagnose-output.)_
- [x] **Rules** — 55 rijen, **0** afwijkingen op de 5 vergeleken metrics
      (inclusief `Sample violations`).
- [x] **Interfaces** — 17 rijen, **0** afwijkingen op de 7 vergeleken
      metrics (`#boxes`, `Max nesting depth`, `#fields`,
      `CRUD coverage`, `#relations with C/U/D`).
- [ ] **Concepts D1–D4 + D10–D12** — niet vergeleken: deze kolommen
      (generalisation depth, suspect synonyms, populatie-aantallen) zijn
      in `Extract.hs` nog stubs (0/`""`/`False`). Pas op het moment dat
      de stubs ingevuld worden, kan deze vergelijking uitgebreid worden.
- [ ] **Relations E1–E7** — niet vergeleken om dezelfde reden: stubs.
- [ ] **Patterns C3, C4, C7–C9** — niet vergeleken: stubs.
- [ ] **Interfaces G12, G13** — niet vergeleken: stubs.
- [ ] **Rules F5** — niet vergeleken: stub.

### Bijzondere observatie

In `Patterns.tsv` zien we dat de fresh-xlsx 96 concepts, 312 relations
en 30 patterns geeft. Een **eerder** geleverde
`main-diagnosis.xlsx` (in de Google Drive folder) had 99 concepts,
320 relations en 30 patterns. Dat verschil zit dus in een eerdere
build/codepath: de huidige `extractDiagnostics` levert dezelfde aantallen
op als de onafhankelijke walker.

## Reproduceerbaarheid

```bash
# 1. Onafhankelijke dump (GHCi)
stack ghci Sandbox.hs DiagnoseCheck.hs
# In de GHCi-prompt:
:m + Sandbox DiagnoseCheck
fSpec <- Sandbox.loadTestFSpec "/pad/naar/FC5/project/main.adl"
DiagnoseCheck.dumpAll "/tmp/fc5_independent" fSpec
:q

# 2. Verse xlsx
mkdir -p /tmp/fc5_fresh
ampersand documentation "/pad/naar/FC5/project/main.adl" \
  --no-Intro --no-SharedLang --no-ConceptualAnalysis --no-DataAnalysis \
  --no-graphics --output-dir /tmp/fc5_fresh --format markdown

# 3. xlsx → tsv (eenmalig)
python3 -c "import openpyxl, os; wb=openpyxl.load_workbook('/tmp/fc5_fresh/main-diagnosis.xlsx', data_only=True); os.makedirs('/tmp/fc5_fresh_dump', exist_ok=True); [open(f'/tmp/fc5_fresh_dump/{s}.tsv','w').write('\n'.join('\t'.join('' if v is None else str(v) for v in r) for r in wb[s].iter_rows(values_only=True))) for s in wb.sheetnames]"

# 4. Vergelijken
python3 compare_diagnose.py
```

## Conclusie

Voor de elf invulvelden die we recentelijk uit het stub-niveau hebben
gehaald (F1 term complexity, C2 largest rule term complexity, C6
cohesion ratio, F4 sample violations, G2–G9 + G14 interface-walker
afgeleide kolommen, plus eerder ingevulde basis-counts) levert
`extractDiagnostics` exact dezelfde waarden als een schone
herimplementatie tegen dezelfde FSpec. Er zijn dus geen afwijkingen
gevonden tussen `main-diagnosis.xlsx` (huidige build) en een
volledig onafhankelijke berekening voor `FC5/project/main.adl`.
