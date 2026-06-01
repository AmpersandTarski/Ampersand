# Diagnose-stubs invul-checklist (issue #1625)

Status: alle records, kolommen, headers en de Pandoc/xlsx-renderers zijn
op hun plek; deze checklist beschrijft per stub-veld wát er nog moet
gebeuren in `src/Ampersand/Diagnosis/Extract.hs`. Iedere wijziging
blijft beperkt tot dat bestand (en eventueel een nieuwe helper-module);
de spreadsheet- en Pandoc-renderers hoeven niet meer aangepast te
worden.

Volgorde van werken is van **laag-risico, hoog-rendement** (graafje
opbouwen, daarna populatie-statistieken, daarna similarity).

## C — Patterns sheet

- [x] **C3 `pdNrOrphanRelations`** — geleverd door
  `Ampersand.Diagnosis.PatternGraph.nrOrphanRelations`.
- [x] **C4 `pdNrUnreachableProcessRules`** — geleverd door
  `PatternGraph.unreachableProcessRules` (rol-tot-interface mapping in
  dezelfde module).
- [x] **C7 `pdSelfContained`** — geleverd door
  `PatternGraph.isSelfContained` (alle rules-leaves moeten in `ptdcs p`
  zitten).
- [x] **C8 `pdDependsOnPatterns`** — geleverd door
  `PatternGraph.dependsOn`.
- [x] **C9 `pdDependedOnByPatterns`** — geleverd door
  `PatternGraph.dependedOnBy` (transpose van C8).

> **Helper-module-suggestie**: `Ampersand.Diagnosis.PatternGraph` met
> twee `Map Text (Set Text)` (`dependsOn`, `dependedBy`). Bouw die
> één keer in `extractDiagnostics` en hang hem aan een `let`.

## D — Concepts sheet

- [x] **D1 `cdGeneralisationDepth`** — geleverd door
  `Ampersand.Diagnosis.ClassifyGraph.classifyDepth`.
- [x] **D2 `cdNrSpecialisations`** — geleverd door
  `ClassifyGraph.childrenOf`.
- [x] **D3 `cdNrGeneralisations`** — geleverd door
  `ClassifyGraph.parentsOf`.
- [x] **D4 `cdInClassifyCycle`** — geleverd door
  `ClassifyGraph.inCycle`.
- [x] **D10 `cdNrAtomsInitial`** — `Set.size (atomsInCptIncludingSmaller fSpec c)`.
- [ ] **D11 `cdPopulationSource`** — herkomst van populatie:
  POPULATION-statement, INCLUDE \*.csv/xlsx of computed. Hergebruik
  `Origin` van `ARelPopu`/`ACptPopu` records uit
  `Ampersand.FSpec.ToFSpec.Populated`.
- [ ] **D12 `cdSuspectSynonyms`** — string-similarity tegen alle
  concepts. Eenvoudig: Levenshtein <= 2 én case-insensitive verschil
  alleen in interpunctie of suffix. Implementeer in
  **`Ampersand.Diagnosis.Similarity`**, gebruik in Extract.

> **Helper-module-suggesties**:
> - `Ampersand.Diagnosis.ClassifyGraph` voor D1..D4 (depth, parents,
>   children, SCC).
> - `Ampersand.Diagnosis.Similarity` voor D12 / E7 (gedeelde code).

## E — Relations sheet

- [x] **E1 `rdNrInterfacesReferencing`** —
  `length [ifc | ifc <- interfaceS fSpec, rel ∈ bindedRelationsIn ifc]`.
- [x] **E2 `rdNrViewsReferencing`** — analog over `vviews fSpec`.
- [x] **E3 `rdNrIdentRulesReferencing`** — analog over `vIndices fSpec`.
- [ ] **E4 `rdComputedByEnforce`** — bouw map
  `Map Relation AEnforce` uit `[(ptenfs p, p) | p <- pats]`. Voor elke
  relatie: toon de `Origin` (file:line) en `enforceOperator` als hij in
  de map zit, anders "".
- [x] **E5 `rdNrPairsInitial`** — `Set.size (pairsInExpr fSpec (EDcD rel))`.
- [ ] **E6 `rdPopulationSource`** — zelfde idee als D11; `Origin` van
  de `ARelPopu` die populaties levert.
- [ ] **E7 `rdSuspectDuplicates`** — similarity-buren binnen dezelfde
  signature. Hergebruik `Ampersand.Diagnosis.Similarity` (D12).

> **Volgorde-tip**: E1–E3 zijn drie regels code per stuk en zijn
> eerlijk: doe ze samen in één klap.

## F — Rules sheet

- [ ] **F5 `rldUnreachable`** — process-regel is unreachable wanneer
  geen rol die de regel maintains een interface in `interfaceS fSpec`
  heeft. (Voor invariants: laat `False`, of label "n.v.t." in een
  toekomstige iteratie.) Hergebruikt de mapping uit C4.

## G — Interfaces sheet

- [ ] **G12 `ifdUnreachableRole`** — `True` als alle `ifcRoles ifc` in
  `vroles fSpec` voorkomen maar nooit als handhavende rol (`fRoleRuls`)
  én geen sub-interface ernaar verwijst. Reuse van de C4-mapping
  (rol → interfaces).
- [ ] **G13 `ifdOrphanConceptFields`** — velden waarvan de
  `target (objExpression o)` géén relatie / regel / interface gebruikt
  *buiten* dit veld. Praktisch: walk de fields, kijk welke concepts
  alleen daar voorkomen.

## Globaal: kleine houdbaarheidstaken

- [ ] `dsScriptName` — momenteel `fullName fSpec`. Controleer of we de
  bestandsnaam (zonder `.adl`) willen tonen — dat past beter bij de
  rest van de spreadsheet die het `<scriptname>-diagnosis.xlsx` heet.
- [ ] `DiagSummary` aanvullen met de zojuist ingevulde aggregaten zodra
  C3/C4/D1..D4 leven (bv. _total orphan relations_,
  _patterns with cycle_).
- [ ] `docs/diagnosis-refactor-plan.md` Sectie A..G aanvinken zodra een
  metric uit de stub-status komt.

## Hoe te toetsen

Iedere ingevulde metric kan automatisch worden gevalideerd via het
bestaande verificatie-platform:

1. Voeg het veld toe aan `DiagnoseCheck.hs` (onafhankelijke walker, zonder
   import uit `Ampersand.Diagnosis.*`).
2. Voeg het paartje `(headerInExtract, headerInXlsx)` toe aan de juiste
   kolomtabel in `compare_diagnose.py`.
3. Run:

   ```bash
   stack build --fast
   ampersand documentation /pad/.../FC5/project/main.adl \
     --no-Intro --no-SharedLang --no-ConceptualAnalysis --no-DataAnalysis \
     --no-graphics --output-dir /tmp/fc5_fresh --format markdown
   python3 -c "import openpyxl, os; wb=openpyxl.load_workbook('/tmp/fc5_fresh/main-diagnosis.xlsx', data_only=True); os.makedirs('/tmp/fc5_fresh_dump', exist_ok=True); [open(f'/tmp/fc5_fresh_dump/{s}.tsv','w').write('\n'.join('\t'.join('' if v is None else str(v) for v in r) for r in wb[s].iter_rows(values_only=True))) for s in wb.sheetnames]"
   python3 compare_diagnose.py
   ```

   Verwacht resultaat: `0 ... rows with metric diffs` voor élke
   categorie. Mismatches komen meteen als checklist-items uit
   `compare_diagnose.py` rollen.

4. Hang de naam van het ingevulde stub-veld in
   `docs/diagnosis-refactor-plan.md` om naar `[x]` en verwijs zo nodig
   naar de nieuwe helper-module.

## Aanbevolen werkvolgorde

1. **CLASSIFY-graaf** (D1..D4 + de cycle-flag) — één helper-module
   maakt vier kolommen ineens.
2. **Pattern-graaf** (C3/C4/C7..C9) — gebruikt `bindedRelationsIn`
   die we al hadden, plus de C4/F5/G12-rol-tot-interface map.
3. **Populatie-aantallen** (D10, D11, E5, E6) — één pass over
   `udefpops fSpec` / `vrelpopu fSpec`.
4. **Cross-referenties** (E1, E2, E3) — drie kleine zoekacties.
5. **ENFORCE-koppeling** (E4) — map opbouwen uit `ptenfs`.
6. **Similarity** (D12, E7) — nieuwe helper-module
   `Ampersand.Diagnosis.Similarity`.
7. **Rule/Interface "unreachable"** (F5, G12) en orphan-fields (G13)
   — kunnen aansluiten op de rol-tot-interface map uit stap 2.

Na alle zeven stappen is de spreadsheet volledig "groen" tegenover de
onafhankelijke verifier en kan issue #1625 worden afgesloten.
