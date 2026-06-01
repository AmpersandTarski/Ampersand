# Diagnosis Refactoring Plan (issue #1625)

## Doel

De `ChapterDiagnosis.hs` (767 regels) splitsen in drie helder gescheiden lagen,
zodat:
1. de diagnose-data één pure extractie heeft (geen Pandoc, geen IO);
2. een nieuwe xlsx-output mogelijk is naast de bestaande Pandoc-output;
3. het beknopte diagnose-hoofdstuk verwijst naar het xlsx-bestand voor detail;
4. de FSpec-data-structuur opgeruimd kan worden van wat na de scheiding dead
   code blijkt te zijn.

## Architectuur

```
                       ┌──────────────────────────────────┐
                       │  Ampersand.Diagnosis.Types       │
                       │  • DiagnosticData                 │
                       │  • DiagSummary                    │
                       │  • PatternDiag                    │
                       │  • ConceptDiag                    │
                       │  • RelationDiag                   │
                       │  • RuleDiag                       │
                       │  • InterfaceDiag                  │
                       └─────────────┬────────────────────┘
                                     │
                       ┌─────────────▼────────────────────┐
                       │  Ampersand.Diagnosis.Extract     │
                       │  extractDiagnostics ::            │
                       │    Lang -> FSpec -> DiagnosticData│
                       │  (pure, geen Pandoc/xlsx/IO)      │
                       └──────┬───────────────┬───────────┘
                              │               │
            ┌─────────────────▼─────┐  ┌──────▼──────────────────┐
            │ ChapterDiagnosis.hs   │  │ Diagnosis2Xlsx.hs       │
            │ (beknopte samenvat-   │  │ (sheets: Patterns,       │
            │  ting + xlsx-verwij-  │  │  Concepts, Relations,    │
            │  zing)                │  │  Rules, Interfaces)      │
            └─────────────┬─────────┘  └──────────┬──────────────┘
                          │                       │
                  ┌───────▼───────────────────────▼─────────┐
                  │ Commands/Documentation.hs                │
                  │ • roept extractDiagnostics 1× aan         │
                  │ • voert beide renderers uit               │
                  │ • schrijft <scriptname>-diagnosis.xlsx    │
                  └───────────────────────────────────────────┘
```

## Fasen

### Fase 1: data-laag
- `src/Ampersand/Diagnosis/Types.hs` (pure datatypes)
- `src/Ampersand/Diagnosis/Extract.hs` (`extractDiagnostics`)
- toegevoegd aan `exposed-modules` in `package.yaml`
- `stack build` → groen

### Fase 2: xlsx-renderer
- `src/Ampersand/Output/Diagnosis2Xlsx.hs`
- `diagnostics2Xlsx :: POSIXTime -> DiagnosticData -> BL.ByteString`
- volgt `Population2Xlsx.hs` patroon
- `stack build` → groen

### Fase 3: Pandoc summary refactor
- `ChapterDiagnosis.hs` neemt `DiagnosticData` als parameter
- producert beknopte statistieken + verwijzingen
- detail-prose verdwijnt; lange opsommingen worden verwijsteksten

### Fase 4: orchestratie
- `Commands/Documentation.hs` schrijft xlsx wanneer Diagnosis-chapter actief is
- `FSpec2Pandoc.hs` geeft `DiagnosticData` door aan `chpDiagnosis`

### Fase 5: FSpec opruimen (dead-code oogsten)
- `initialConjunctSignals` verwijderen (al dead code)
- `stack build` met `-Wall` → unused imports/bindings oplossen
- `weeder --config weeder.dhall` → cross-module dead code

### Fase 6: testen
- compileert warning-free
- xlsx opent in Excel/LibreOffice
- Pandoc-chapter is beknopt en leesbaar

## Iteratieve strategie binnen fase 1

Begin met de **essentiële** kolommen die direct uit FSpec te halen zijn.
Uitbreiding naar de volledige kolommen-lijst uit het issue gebeurt in
vervolgiteraties; de records kunnen velden bijkrijgen zonder dat de structuur
breekt.

### Essentiële velden eerste iteratie

| Record         | Velden (eerste iteratie) |
|----------------|--------------------------|
| PatternDiag    | name, sourceFileLine, hasPurpose, nrConcepts, nrRelations, nrRules, nrIdentRules, nrEnforce, nrViews |
| ConceptDiag    | name, definedInPattern, sourceFileLine, hasConceptDef, hasPurpose, representation |
| RelationDiag   | name, signature, definedInPattern, sourceFileLine, hasPurpose, hasMeaning, properties (UNI,INJ,TOT,SUR,PROP,IRF,RFX,SYM,ASY,TRN), nrRulesReferencing |
| RuleDiag       | name, definedInPattern, sourceFileLine, kind (Invariant/Process/Enforce/Ident), maintainedByRoles, hasPurpose, hasMeaning, hasMessage, hasViolation, nrViolations |
| InterfaceDiag  | name, forRoles, sourceFileLine, hasPurpose, topLevelConcept |
| DiagSummary    | nrPatterns, nrConcepts, nrConceptsWithPurpose, nrRelations, nrRelationsWithPurpose, nrRelationsWithMeaning, nrRules, nrRulesWithPurpose, nrRulesWithMeaning, nrInterfaces, nrInvariantViolations, nrProcessViolations |

Uitbreidingskandidaten (latere iteraties): cohesion ratio, suspect synonyms,
sample violations, CRUD coverage, complexity scores, unreachable rules/roles.

## Niet-doelen

- We refactoren `fDeriveProofs :: Blocks` (Pandoc-data in FSpec) niet — buiten
  scope van dit issue. Wordt apart genoteerd als design smell.
- We veranderen het FSpec-type niet meer dan strikt nodig voor dead-code
  eliminatie.

----

## Voortgang — sessie 27 mei 2026

De architectuur (Types / Extract / Diagnosis2Xlsx / ChapterDiagnosis) is uitgerold en compileert.
De volledige checklist uit issue #1625 zit echter nog niet aan een complete invulling toe.
Onderstaande tabel houdt de stand bij per checklist-item; "stub" betekent dat de
kolom in de spreadsheet zichtbaar is maar de berekening nog placeholder-waarde (0 / "" / False)
heeft, gemarkeerd in de broncode met `TODO #1625`.

### Sectie A — Pandoc-fixes (klaar)

- [x] **A1** "Pattern" als header en eerste kolom-cel.
- [x] **A2** Pandoc-tabel produceert echt een `simpleTable` (niet langer `rawBlock`).
- [x] **A3** Sheetnamen worden inline-code (backticks) i.p.v. lopende tekst.
- [x] **A4** Split de "violations"-zin in twee zinnen ("0 invariants" + "0 open signals").
- [x] **A5** xlsx: booleans als ✓ / leeg, en gegroepeerd rechts van iedere sheet.
- [x] **A1/A2 vervolg** `[TABLE]`-placeholder definitief verholpen door
       `writerExtensions = getDefaultExtensions writerName` te zetten in
       `PandocAux.writerOptions`. Standaard `def :: WriterOptions` gebruikt
       `emptyExtensions`, waardoor Markdown-writer pipe/simple/grid tables uitschakelt.
- [x] **A6** Header-capitalisatie Patterns-sheet uniform Sentence-case
       (`Concepts with PURPOSE`, `Largest rule term complexity`, `Cohesion ratio`).

### Sectie B — Datavullingsfixes (klaar)

- [x] **B1** Concepten zonder PATTERN worden gegroepeerd onder het synthetische
       pattern `"No Pattern"` (zie ook **B7** hieronder). Sinds 28 mei 2026
       vervangt deze label de eerdere CONTEXT-naam, zodat alle "buiten-pattern"
       definities op één regel in de Patterns-sheet samenkomen.
- [x] **B2** Concepten zonder CONCEPT-definitie tonen file/line van de eerste
       relatie die het concept noemt (geen "None" meer).
- [x] **B3** Relaties zonder PATTERN krijgen óók de naam `"No Pattern"` (zie B1).
- [x] **B4** Numerieke kolommen tonen 0 i.p.v. leeg / None.
- [x] **B7** Default-pattern `"No Pattern"`. `extractDiagnostics` voegt nu een
       synthetische `PatternDiag`-rij toe achter de gewone patroon-rijen.
       Deze rij aggregeert élke definitie die direct in de CONTEXT staat:
       concepten zonder PATTERN, relaties zonder PATTERN, regels zonder
       PATTERN, IDENT- en VIEW-definities, en ENFORCE-statements met
       `enfPatName == Nothing`. Daarmee telt iedere definitie in de
       Patterns-sheet in precies één rij mee.
       Implementatie in `Extract.hs`: `mkNoPatternDiag`, met de helpers
       `relsNoPattern`, `rulesNoPattern`, `cdefsNoPattern`, `identRulesNoPattern`,
       `viewsNoPattern` en `enforcesNoPattern`.

### Sectie C — Patterns-sheet kolommen

- [x] **C0** Bestaande percentages (% concepts/relations/rules met PURPOSE/MEANING) toegevoegd.
- [x] **C1** `#ExecEngine rules` (gebaseerd op `nameOfExecEngineRole`).
- [x] **C2** Largest rule term complexity — via `exprRelOccurrences` walker over `formalExpression`.
- [x] **C3** `#orphan relations` — via `PG.nrOrphanRelations` (`Ampersand.Diagnosis.PatternGraph`).
- [x] **C4** `#unreachable process rules` — via `PG.unreachableProcessRules` (rol-tot-interface mapping).
- [x] **C5** `#violations in initial population` (som over `allViolations`).
- [x] **C6** Cohesion ratio — internal/total references via `exprInternalRelOccurrences`.
- [x] **C7** Self-contained — via `PG.isSelfContained`.
- [x] **C8** Patterns this one depends on — via `PG.dependsOn`.
- [x] **C9** Patterns that depend on this one — via `PG.dependedOnBy`.
- [x] **C10** Concepts declared (komma-gescheiden).

### Sectie D — Concepts-sheet kolommen

- [x] **D1** Generalisation depth — via `CG.classifyDepth` (`Ampersand.Diagnosis.ClassifyGraph`).
- [x] **D2** `#specialisations` — via `CG.childrenOf`.
- [x] **D3** `#generalisations` — via `CG.parentsOf`.
- [x] **D4** In CLASSIFY cycle — via `CG.inCycle`.
- [x] **D5** Has IDENT rule (via `vIndices`).
- [x] **D6** Has VIEW (via `vviews`).
- [x] **D7** `#rules referencing` (via `concs r`).
- [x] **D8** `#interfaces referencing` (via `concs ifc`).
- [x] **D9** Patterns using (via `concs p`).
- [x] **D10** `#atoms in initial population` — via `atomsInCptIncludingSmaller`.
- [ ] **D11** Population source — *stub*.
- [ ] **D12** Suspect synonyms — *stub* (Levenshtein o.i.d.).

### Sectie E — Relations-sheet kolommen

- [x] **E0** Bestaande "Used in any rule" verwijderd, vervangen door `#rules referencing` (al aanwezig).
- [x] **E1** `#interfaces referencing` — `length [ifc | rel ∈ bindedRelationsIn ifc]`.
- [x] **E2** `#views referencing` — analog over `vviews fSpec`.
- [x] **E3** `#IDENT rules referencing` — analog over `vIndices fSpec`.
- [ ] **E4** Computed by ENFORCE — *stub* (lookup via `ptenfs`).
- [x] **E5** `#pairs in initial population` — via `pairsInExpr fSpec (EDcD rel)`.
- [ ] **E6** Population source — *stub*.
- [ ] **E7** Suspect duplicates — *stub*.

### Sectie F — Rules-sheet kolommen

- [x] **F1** Term complexity — via `exprRelOccurrences` walker.
- [x] **F2** `#relations referenced` (`Set.size . bindedRelationsIn`).
- [x] **F3** `#concepts referenced` (`Set.size . concs`).
- [x] **F4** Sample violations — eerste drie paren via `showValADL`/`apLeft`/`apRight`.
- [ ] **F5** Unreachable — *stub*.

### Sectie G — Interfaces-sheet kolommen

- [x] **G1** Top-level expression (`tshow (objExpression ...)` ).
- [x] **G2** `#boxes` — via `walkInterface` (telt elke `Box`-`SubInterface`).
- [x] **G3** Max nesting depth — diepste `Box`-nesting (root = 1).
- [x] **G4** `#fields` — alle `BxExpr`-`ObjectDef`s onder de root.
- [x] **G5** CRUD coverage — OR-aggregatie van `crudC/R/U/D` over alle velden.
- [x] **G6/G7/G8** `#relations with C/U/D` — `bindedRelationsIn` per veld
       gefilterd op `crudC/U/D`.
- [x] **G9** Read-only — true als geen enkel veld C/U/D heeft.
- [x] **G10** Concepts covered (uit `concs ifc`).
- [x] **G11** Relations covered (uit `bindedRelationsIn ifc`).
- [x] **G14** Uses VIEW for top concept — `isJust (objmView (ifcObj ifc))`.
- [ ] **G12, G13** Unreachable role en Orphan concept fields — *stub*.

### Volgende stappen (aanbevolen volgorde)

1. ~~**Term-complexity walker** (F1, C2, C6).~~
   **Klaar (sessie 27 mei 2026):** `exprRelOccurrences` en `exprInternalRelOccurrences`
   staan onderaan `Extract.hs`; F1, C2 en C6 zijn nu echte waarden.
2. ~~**CLASSIFY-helpers** (D1..D4).~~
   **Klaar (sessie 27 mei 2026):** `Ampersand.Diagnosis.ClassifyGraph` bouwt de
   graaf één keer; `classifyDepth`, `childrenOf`, `parentsOf` en `inCycle`
   leveren D1..D4.
3. ~~**Interface-walker** (G2..G14).~~ **Klaar (sessie 27 mei 2026):** `walkInterface`
   geeft `iwNrBoxes`, `iwMaxDepth` en alle `ObjectDef`s onder de root; daarmee zijn
   G2..G9 + G14 ingevuld. Restant: G12 (unreachable role) en G13 (orphan concept
   fields).
4. ~~**Pattern-graaf** (C3, C4, C7, C8, C9).~~
   **Klaar (sessie 27 mei 2026):** `Ampersand.Diagnosis.PatternGraph` levert
   `nrOrphanRelations`, `unreachableProcessRules`, `isSelfContained`, `dependsOn`
   en `dependedOnBy`.
5. ~~**Cross-referenties** (E1, E2, E3, E5, D10).~~ **Klaar (sessie 27 mei 2026):**
   directe queries via `bindedRelationsIn` resp. `pairsInExpr` /
   `atomsInCptIncludingSmaller`.
6. **Population-source** (D11, E6): aansluiten op
   `Ampersand.FSpec.ToFSpec.Populated` voor `Origin` van `ARelPopu`/`ACptPopu`.
7. **ENFORCE-koppeling** (E4): map opbouwen `Relation -> AEnforce` uit `ptenfs`.
8. **Reachability** (F5, G12, G13): gedeelde rol→interface-map; G13 herbruikt
   bovendien de concept-usage scan uit het pattern-graaf-werk.
9. **Suspect synonyms/duplicates** (D12, E7): nieuwe helper-module
   `Ampersand.Diagnosis.Similarity` (Levenshtein / shared prefix).

Iedere stap blijft beperkt tot `Ampersand.Diagnosis.Extract` en `Ampersand.Diagnosis.Types`
(plus eventueel een nieuwe helper-module); de xlsx-renderer hoeft niet meer
gewijzigd te worden.
