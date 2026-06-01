# Ampersand - Project Specifieke Instructies

Telkens als je iets nieuws leert, specifiek over het FC5 project, wat nut heeft voor toekomstige ingrepen in deze werkdirectory (FC5), dan mag je deze instructies aanpassen.

## 1. Tech Stack & Architectuur
- **Taal:** Haskell (GHC 9.6.6)
- **Build Tool:** Stack (LTS 22.39) met Cabal
- **Extensies:** `NoImplicitPrelude`, `OverloadedStrings`
- **Kritieke Libraries:** `parsec`, `megaparsec`, `aeson`, `yaml`, `pandoc`, `xlsx`
- **Ecosysteem:** Ampersand compileert ADL (Ampersand Definition Language) naar een PHP/Angular framework met een MariaDB 10.4 database.

## 2. Build & Test Workflow
**ALTIJD `stack build` uitvoeren na codewijzigingen en VOORDAT je test met `stack exec ampersand`.**
Dit is een harde eis. Zonder build test je oude code.

**Standaard cyclus:**
1. Wijzig code
2. `stack build` (of `stack build --fast`)
3. `stack exec ampersand -- check <file.adl>`
4. Itereren

## 3. Veelgebruikte Commando's
- **Snelle build:** `stack build --fast`
- **Regressie test suite draaien:** `stack test` (of `nice -n 10 time stack test 2>&1 | tee test.log`)
- **Specifiek ADL bestand checken:** `stack exec ampersand -- check <pad/naar/bestand.adl>`
- **CLI help:** `stack exec ampersand -- --help`
- **Code quality:** `hlint src/`
- **Documentatie genereren:** `stack haddock`

## 4. Test Infrastructuur
- De originele testset bevindt zich in `testing/`.
- Een verbeterde testset met populaties bevindt zich in `testing_with_populations/`.
- Om de nieuwe set te testen, hernoem je tijdelijk de mappen zodat `testing_with_populations` de naam `testing` krijgt, waarna je `stack test` draait.
- Let op: Er zijn ~14 bekende bestanden die pre-existing compiler bugs triggeren (zie `COMPILATION_STATUS_REPORT.md`).

## 5. Debugging & Ontwikkeling
- **Type Holes:** Gebruik `_` in de code om GHC de verwachte types te laten infereren.
- **Debug.Trace:** Gebruik dit voor runtime debugging output.
- **HLS:** Maak gebruik van de Haskell Language Server in VS Code voor real-time foutcontrole.