# Ampersand Compiler Pipeline: van .adl-bestand naar SQL

Dit document beschrijft de interne pipeline van de Ampersand-compiler, van een
ADL-bronbestand tot de gegenereerde SQL-queries. Het is geschreven voor
software-engineers die de compiler intern willen begrijpen, bijvoorbeeld om
bugs te lokaliseren of nieuwe optimalisaties toe te voegen.

## Wanneer heb je deze kennis nodig?

### Use case: een trage of foute SQL-query debuggen

Stel: een RULE of ENFORCE-statement genereert een SQL-query die te traag is of
verkeerde resultaten geeft. Voorbeeld uit de praktijk
(`testing/Travis/testcases/prototype/shouldSucceed/CartesianTest.adl`):

```adl
ENFORCE product[Uitspraak*Product] :=
  vorm;vorm~ /\ productnaam;voorkeursNaam~
```

Deze ENFORCE genereert intern een *deletion check* van de vorm
`product /\ -(vorm;vorm~ /\ productnaam;voorkeursNaam~)`, wat door De Morgan
herschreven wordt naar `product /\ (-vorm;vorm~ \/ -productnaam;voorkeursNaam~)`.
Elke `-` (complement) genereert dan een cartesisch product `V[Uitspraak*Product]`,
wat in MariaDB voor `O(n²)`-gedrag zorgt.

Om dit probleem te begrijpen moet je weten:
1. Waar in de pipeline de ENFORCE wordt opgesplitst in conjuncts
2. Welke normalisatiestap de De Morgan-herschrijving veroorzaakt
3. Waar `selectExpr` de cartesische SQL produceert
4. Hoe je deze stappen interactief kunt reproduceren in GHCi

Zonder kennis van de pipeline weet je niet welke module je moet aanpassen.
Met die kennis isoleer je het probleem in minuten.

---

## De pipeline in zes stappen

```
.adl bestand
    │  parseFilesTransitive   (Ampersand.Input.Parsing)
    ▼
P_Context                     (parse tree, ongetypeerd)
    │  pCtx2aCtx              (Ampersand.ADL1.P2A_Converters)  -- type checking
    ▼
A_Context                     (typed AST)
    │  makeFSpec              (Ampersand.FSpec.ToFSpec.ADL2FSpec)
    ▼
FSpec                         (functional specification, hét centrale datatype)
    │  conjNF / normStep      (Ampersand.FSpec.ToFSpec.NormalForms)
    ▼
genormaliseerde Expressions   (conjuncts in vconjs fSpec)
    │  selectExpr             (Ampersand.FSpec.SQL)
    ▼
SQL QueryExpr                 (Language.SQL.SimpleSQL.Syntax)
```

### Stap 1 — Parsing: `.adl` → `P_Context`

- **Module**: `Ampersand.Input.Parsing`
- **Hoofdfunctie**:
  ```haskell
  parseFilesTransitive
    :: (HasDirOutput env, HasFSpecGenOpts env, HasTrimXLSXOpts env, HasRunner env)
    => Roots
    -> RIO env (NonEmpty ParseCandidate, Guarded P_Context)
  ```
- Parst het rootbestand en alle transitieve `INCLUDE`s.
- Het preprocessor-systeem (`Ampersand.Input.PreProcessor`) verwerkt `IF`/`ENDIF`-directives.
- Resultaat is een `Guarded P_Context`: kan errors of warnings bevatten.
- `P_Context` is "Parsed Context" — geen type-info, alleen syntactische structuur.

### Stap 2 — Type checking: `P_Context` → `A_Context`

- **Module**: `Ampersand.ADL1.P2A_Converters`
- **Hoofdfunctie**:
  ```haskell
  pCtx2aCtx :: env -> P_Context -> Guarded A_Context
  ```
- Resolveert namen, checkt types, leidt signaturen af.
- Een `Term TermPrim` wordt een `Expression` met expliciete `Signature` (`Sign src tgt`).
- Disambiguation van relaties met dezelfde naam maar verschillende signaturen gebeurt hier.

### Stap 3 — FSpec constructie: `A_Context` → `FSpec`

- **Module**: `Ampersand.FSpec.ToFSpec.ADL2FSpec`
- **Hoofdfunctie**:
  ```haskell
  makeFSpec :: HasFSpecGenOpts env => env -> A_Context -> FSpec
  ```
- Bouwt:
  - `plugInfos` — de SQL-tabel-layout (zie `Ampersand.FSpec.ToFSpec.ADL2Plug`)
  - `interfaceS` / `interfaceG` — gegenereerde en gespecificeerde interfaces
  - `vconjs` — alle conjuncts (genormaliseerde subterm-stukken van rules)
  - `vrules` — alle rules (zowel user-defined als afgeleid)
  - `fSpecAllInstances` — alle populaties
- `FSpec` is het centrale datatype voor *alle* downstream-generatoren
  (SQL, JSON, HTML, Pandoc, Graphviz, etc.).

### Stap 4 — Normalisatie

- **Module**: `Ampersand.FSpec.ToFSpec.NormalForms`
- **Hoofdfuncties**:
  - `conjNF :: env -> Expression -> Expression` — conjunctieve normaalvorm
  - `cfProof :: Expression -> [(Expression, [Text], Text)]` — stapsgewijs proof
  - `normStep` — één stap van het normalisatie-algoritme
  - `deMorganEUni` — De Morgan-regel voor unie
- Wordt aangeroepen tijdens `makeFSpec` om conjuncts te bouwen.
- Belangrijke herschrijvingen:
  - `-(p ∨ q)` → `(-p) ∧ (-q)` (De Morgan)
  - `p ∧ (q ∨ r)` → `(p ∧ q) ∨ (p ∧ r)` (distributie)
  - `--p` → `p` (dubbele negatie)
  - `p - q` ↔ `p ∧ -q` (verschil als doorsnede met complement)

### Stap 5 — SQL-generatie: `Expression` → `QueryExpr`

- **Module**: `Ampersand.FSpec.SQL`
- **Hoofdfunctie**:
  ```haskell
  selectExpr :: FSpec -> Expression -> BinQueryExpr
  ```
- Patroongericht op `Expression`-constructors:
  - `EDcD rel` — directe relatietabel-lookup
  - `EDcI cpt` — identiteit (`SELECT c, c FROM cpt`)
  - `EDcV sgn` — universele relatie (cartesisch product van bron- en doelconcept)
  - `EUni (l, r)` — `UNION` van twee subqueries
  - `EIsc (l, r)` — `INNER JOIN` / specialcase met `maybeSpecialCase`
  - `ECps (l, r)` — composition, `JOIN ON l.tgt = r.src`
  - `ECpl e` — complement: closed-world via `WHERE NOT EXISTS`
  - `EDif (l, r)` — verschil: `LEFT JOIN ... WHERE IS NULL`
- Convenience-wrappers: `sqlQuery`, `prettySQLQuery`, `broadQuery`.

### Stap 6 — Pretty-printing: `QueryExpr` → `Text`

- Gebruikt `Language.SQL.SimpleSQL.Pretty.prettyQueryExpr` met `theDialect`.
- Het dialect is MariaDB-compatibel (zie `Ampersand.FSpec.SQL`).

---

## De gecombineerde functie `pCtx2Fspec`

Voor de combinatie van stap 2 + stap 3 + invariant-check:

```haskell
-- Ampersand.FSpec.ToFSpec.CreateFspec
pCtx2Fspec
  :: (HasFSpecGenOpts env, HasRunner env)
  => env -> P_Context -> Guarded FSpec
pCtx2Fspec env c = do
  fSpec <- makeFSpec env <$> pCtx2aCtx env c
  checkInvariants fSpec
```

Dit is wat de meeste consumers gebruiken — `parseFilesTransitive` gevolgd door
`pCtx2Fspec` levert een complete `FSpec` op.

---

## De RIO-environment

De hele compiler draait in `RIO env`, met een gelaagd environment:

```
Runner
  ├── GlobalOpts          (logLevel, outputDir, terminalWidth, ...)
  ├── LogFunc
  ├── ProcessContext
  └── UseColor / TermWidth

ExtendedRunner FSpecGenOpts  -- wat parseFilesTransitive en pCtx2Fspec nodig hebben
  ├── eRunner = Runner
  └── eCmdOpts = FSpecGenOpts
```

`ExtendedRunner FSpecGenOpts` bevredigt automatisch alle vier de constraints
(`HasRunner`, `HasFSpecGenOpts`, `HasDirOutput`, `HasTrimXLSXOpts`) via de
instances in `Ampersand.Types.Config`.

### Bootstrappen buiten de CLI

Voor scripts, tests en sandboxen kun je de environment zelf opzetten:

```haskell
import Ampersand.Misc.HasClasses          (Roots(..), FSpecGenOpts)
import Ampersand.Options.FSpecGenOptsParser (defFSpecGenOpts)
import Ampersand.Runners                  (withRunnerGlobal)
import Ampersand.Types.Config             (extendWith, GlobalOpts(..))
import Ampersand.Input.Parsing            (parseFilesTransitive)
import Ampersand.FSpec.ToFSpec.CreateFspec (pCtx2Fspec)

loadTestFSpec :: FilePath -> IO FSpec
loadTestFSpec path = do
  let globalOpts = GlobalOpts
        { globalLogLevel  = LevelWarn
        , globalTimeInLog = False
        , globalTerminal  = False
        , globalTermWidth = Nothing
        , globalOutputDir = "."
        }
      fSpecOpts = defFSpecGenOpts (path NE.:| [])
  withRunnerGlobal globalOpts $ extendWith fSpecOpts $ do
    (_, gPctx) <- parseFilesTransitive (Roots (path NE.:| []))
    env <- ask
    case pCtx2Fspec env =<< gPctx of
      Checked fSpec _ -> pure fSpec
      Errors errs     -> error ("parse errors:\n" ++ show errs)
```

### Belangrijke ingangen

| Functie | Module | Doel |
|---|---|---|
| `withRunnerGlobal` | `Ampersand.Runners` | Bouwt een `Runner` met logging, output, etc. |
| `extendWith` | `Ampersand.Types.Config` | Voegt command-specifieke opties (zoals `FSpecGenOpts`) toe |
| `defFSpecGenOpts` | `Ampersand.Options.FSpecGenOptsParser` | Default `FSpecGenOpts` op basis van root-paden |
| `parseFilesTransitive` | `Ampersand.Input.Parsing` | Parsen van `.adl` + transitieve includes |
| `pCtx2Fspec` | `Ampersand.FSpec.ToFSpec.CreateFspec` | Type-check + FSpec-constructie + invariant-check |

---

## Foutafhandeling: het `Guarded`-type

Door de hele pipeline wordt foutpropagatie gedaan via `Guarded`:

```haskell
data Guarded a
  = Errors  (NonEmpty CtxError)   -- één of meer geaccumuleerde fouten
  | Checked a [Warning]            -- resultaat plus eventuele warnings
```

Belangrijke eigenschappen:
- `Functor`, `Applicative` en `Monad` instance.
- `<*>` accumuleert errors uit beide kanten (parallelle foutmelding).
- `>>=` propageert de eerste error (sequential).
- **Best practice**: gebruik `ApplicativeDo`-notatie zodat onafhankelijke
  controles parallel fouten kunnen rapporteren.

---

## Concreet voorbeeld: hoe de Daemon dit gebruikt

`Ampersand.Daemon.Parser.parseProject` doet exact dit patroon — een goed
referentievoorbeeld als je iets soortgelijks bouwt:

```haskell
parseProject rootAdl = local (set rootFileL (Roots (rootAdl NE.:| []))) $ do
  (pc, gPctx) <- parseFilesTransitive (Roots (rootAdl NE.:| []))
  env <- ask
  let gActx = pCtx2Fspec env =<< gPctx
  ...
```

---

## Sandbox-patroon: `testSpec :: FSpec` voor GHCi-experimenten

Voor interactief experimenteren in GHCi (zie `Sandbox.hs` in de project-root):

```haskell
import System.IO.Unsafe (unsafePerformIO)

testSpec :: FSpec
testSpec = unsafePerformIO $ loadTestFSpec "testscript.adl"
{-# NOINLINE testSpec #-}
```

Werking:
- Bij `:load Sandbox` wordt `testSpec` lazy geëvalueerd zodra je het aanraakt.
- Bij `:reload` (na wijzigen van `testscript.adl` of `Sandbox.hs`) wordt de CAF
  opnieuw aangemaakt en `testscript.adl` opnieuw geparseerd.
- Je kunt direct dingen doen als:
  ```
  ghci> :t testSpec
  testSpec :: FSpec
  ghci> map (showA . rcConjunct) (vconjs testSpec)
  ghci> prettySQLQuery (sqlQuery testSpec someExpr)
  ```

Hiermee heb je een complete typegecheckte `FSpec` zonder steeds `stack exec
ampersand -- ...` te hoeven draaien.

---

## Pipeline-vragen die deze kennis beantwoordt

| Vraag | Antwoord |
|---|---|
| "Waar wordt mijn ADL-syntaxisfout gevangen?" | Stap 1, `parseFilesTransitive`. |
| "Waar wordt mijn type-fout (signature mismatch) gevangen?" | Stap 2, `pCtx2aCtx`. |
| "Waar worden mijn rules opgesplitst in conjuncts?" | Stap 3+4, `makeFSpec` roept `conjNF` aan tijdens het bouwen van `vconjs`. |
| "Waarom genereert deze RULE een rare SQL?" | Stap 5, `selectExpr` (en eventueel de normalisatie in stap 4). |
| "Waar staat de De Morgan-herschrijving?" | `Ampersand.FSpec.ToFSpec.NormalForms.deMorganEUni`. |
| "Hoe test ik mijn fix snel?" | Sandbox-patroon hierboven: `testSpec` + `:reload` in GHCi. |

---

## Gerelateerde bronnen in de codebase

- `Sandbox.hs` (project-root) — werkende GHCi-sandbox met `testSpec :: FSpec`
- `run_sandbox.ghci` (project-root) — minimale GHCi-launcher
- `app/Ampersand/Main.hs` — de "echte" CLI-entrypoint
- `src/Ampersand/Daemon/Parser.hs` — referentievoorbeeld van het pipeline-patroon
- `src/Ampersand/Test/Parser/ParserTest.hs` — kleinste werkende voorbeeld van
  `defFSpecGenOpts` + `extendWith` + `parseFilesTransitive`
