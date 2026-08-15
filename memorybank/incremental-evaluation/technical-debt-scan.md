# Technical-debt scan of the incremental-evaluation feature path

Scan date: 2026-08-13, branch `incremental-evaluation` (HEAD 77d4dca7f).
Scope: only the ten modules the feature plan touches or builds beside.
Every number below was measured on this working tree; commands are named per section.

## 1. Module metrics

Line counts (`wc -l`) and top-level type signatures (count of `^name ::` lines;
data/class/instance declarations not included):

| Module | Lines | Top-level sigs | TODO/FIXME lines |
|---|---:|---:|---:|
| `src/Ampersand/Core/AbstractSyntaxTree.hs` | 2467 | 41 | 7 |
| `src/Ampersand/FSpec/ToFSpec/NormalForms.hs` | 1971 | 39 | 7 |
| `src/Ampersand/FSpec/SQL.hs` | 1822 | 41 | 4 |
| `src/Ampersand/FSpec/ToFSpec/ADL2FSpec.hs` | 612 | 3 | 5 |
| `src/Ampersand/FSpec/FSpec.hs` | 609 | 16 | 2 |
| `src/Ampersand/Prototype/TableSpec.hs` | 230 | 14 | 0 |
| `src/Ampersand/FSpec/ToFSpec/ConceptTables.hs` | 195 | 2 | 0 |
| `src/Ampersand/FSpec/ToFSpec/Populated.hs` | 189 | 4 | 0 |
| `src/Ampersand/Output/FSpec2SQL.hs` | 99 | 5 | 0 |
| `src/Ampersand/Output/ToJSON/Conjuncts.hs` | 38 | 0 | 0 |

TODO comments that bear on the feature (verbatim):

- `NormalForms.hs:55` — `TODO: Function f inside dfproofs does not terminate! Must be fixed before fDeriveProofs can be made strict.`
- `NormalForms.hs:1232-1233` — `TODO: the use of posCpl is erroneous` (twice, inside the normalizer core `nM`).
- `NormalForms.hs:1389-1390` — two commented-out Peirce rewrite rules, each marked `SJ 20131124 TODO: check this rule. It is wrong!`
- `NormalForms.hs:1937` — `TODO: Get rid of head', tail', init' and last' in this module.`
- `SQL.hs:148` — on the case decomposition of `selectExpr`: `each of which is supposed to generate correct code in 100% of the cases. (TODO: how do we establish that properly?)`
- `SQL.hs:584` — `TODO: Check these assumptions:` (block of unverified assumptions inside `selectExpr`).
- `SQL.hs:902,937` — `TODO enhance to full signature` (EBin handling; same TODO at `NormalForms.hs:588`).
- `ADL2FSpec.hs:290` — `TODO: needs refactoring due to overcomplication caused by bitrot.`
- `AbstractSyntaxTree.hs:1126` — `FIXME: this in incorrect! (AAtomValue should probably not be in Unique at all...)`
- `AbstractSyntaxTree.hs:2261-2316` — four `TODO: handle these cases?` in `geq`/`join`/`meet`/`meetIsect` for `DISJT, UNION, ISECT`.
- `FSpec.hs:355` — `FIXME: change type of attributes to attributes :: NE.NonEmpty SqlAttribute`.

Imports coupling within the set (grep on import lines):

- `ADL2FSpec.hs` imports `AbstractSyntaxTree`, `FSpec`, `ConceptTables`, `NormalForms`, `Populated` — it is the hub that assembles the `FSpec` record.
- `ConceptTables.hs` and `Output/ToJSON/Conjuncts.hs` each import `NormalForms (conjNF)`.
- `SQL.hs` imports `FSpec.FSpec` (plus `FSpecAux` for `getConceptTableInfo`).
- `FSpec2SQL.hs` imports `SQL` and `TableSpec`; `TableSpec.hs` imports `SQL`.
- `NormalForms.hs` and `Populated.hs` import only `AbstractSyntaxTree` from the set.
- `AbstractSyntaxTree.hs` and `FSpec.hs` import none of the set (they are the base).

A relevant structural fact for the evaluator work: `pairsInExpr` is a
function-valued field of the `FSpec` record (`FSpec.hs:147`), filled in
`ADL2FSpec.hs:101` with a closure over `fullContents` (`ADL2FSpec.hs:157-158`),
which lives in `Populated.hs`.

## 2. hlint

One run over all ten files: **2 hints total**, both in
`AbstractSyntaxTree.hs:1113-1114` ("Use record patterns" for `AAVString _ _ _`).
No hints at all on the other nine files. `.hlint.yaml` ignores only
`Use camelCase` and `Reduce duplication`.

## 3. Warnings policy and current warning state

`package.yaml` library ghc-options: `-Wall -Wcompat -Widentities
-Wredundant-constraints` (plus `-fwrite-ide-info`). No `-Werror`.
`-Wincomplete-patterns` is active as part of `-Wall`.
`-Wincomplete-record-updates` and `-Wincomplete-uni-patterns` are present but
**commented out** (package.yaml lines 30-31).

Verified empirically: after `touch`ing all ten modules, `stack build --fast`
recompiled them (and dependents) to completion with **zero warnings** emitted
(exit 0). The feature-path modules are warning-clean under the current flag set.

## 4. Test safety net

`stack test` builds `app/Test/Main.hs`, which calls `mainTest`
(`src/MainApps.hs:77`) = the CLI command `ampersand test testing`. That command
(`src/Ampersand/Commands/Test.hs`) runs, in order:

1. **QuickCheck parser roundtrip** (`Test/Parser/QuickChecks.hs`,
   `ArbitraryTree.hs`) — parser/pretty-printer only; nothing about semantics.
2. **STEP / EXPRESS / IFC reader tests** — unrelated to this feature path.
3. **`regressionTest`** (`src/Ampersand/Test/Regression.hs`) — a black-box walk
   of `testing/`: 35 `testinfo.yaml` files, each naming commands run against
   the `.adl` files in that directory. The assertion is **exit code only**
   (`testAdlfile`, Regression.hs:~245); stdout/stderr are logged, never diffed.

Command census across the 35 `testinfo.yaml` files: 20× `ampersand validate`,
5× `ampersand check`, 3× `export`, 2× `check --fail-on-oscillation`,
2× `proto`, 6× `population`, 1× `documentation`, 1× `data-analysis`.

The one semantic guard over SQL vs. in-memory evaluation is `ampersand
validate` (`src/Ampersand/Prototype/ValidateSQL.hs`): it creates a temporary
MariaDB database, runs the generated SQL for every rule/interface/view/ident
term, and compares the row set against the in-memory evaluator —
`ValidateSQL.hs:127` literally compares against `pairsInExpr fSpec expr`. So a
refactor of `SQL.hs` **and** of `pairsInExpr` is cross-checked — but only
end-to-end, only on the populations in `testing/`, and only when a MariaDB is
reachable (locally it exits 60 without one; see memory note
`local-validate-needs-db-root`). If SQL.hs and Populated.hs were changed to be
wrong *in the same way*, validate cannot see it.

There is **no unit test** at function granularity for `selectExpr` output, for
`pairsInExpr`/`fullContents` semantics, or for `conjNF` (the `dfProofs` export
is commented "these are for confluence testing" but its only consumer is
`ToFSpec/Calc.hs`, the proof-text generator — no test invokes it).

## 5. Known fragility

**ConceptTables mirror.** The header of `ConceptTables.hs` (lines 1-22) states
the contract explicitly: `conceptsReadBy` *mirrors* `selectExpr`, a change to
`selectExpr` that reads a concept table in a new place must be reflected there,
and the failure mode is `getConceptTableInfo` (`FSpecAux.hs:35`) calling
`fatal`. The sync is guarded by **comment + runtime fatal only**: no test
references `conceptsReadBy`, and no `testing/` case targets issue #1672. The
end-to-end suite would catch a de-sync only if one of the 35 test directories
happens to exercise the newly-reading `selectExpr` case. The module is one
commit old (8dcca55f3, 2026-07-18, PR #1672).

**Change frequency** (`git log --oneline`): `SQL.hs` — 8 commits in the last
9 months (Nov 2025 – Jul 2026), most recently two semantic fixes on 2026-07-03
(Kleene star must include identity; comment escaping). `NormalForms.hs` — 15
commits over the same window, including the 2026-07-02 replacement of four
unsound Kleene laws and the 2026-07-03 `%` operator. Both files are actively
edited, roughly monthly, and the recent edits were correctness fixes, not
cosmetics.

## 6. Dead weight on the path

Commented-out code blocks > 10 lines (awk scan for comment runs):

- `ADL2FSpec.hs:317-330` — commented-out `qlfname` function (namespace
  qualification of plug names).
- `ADL2FSpec.hs:598-612` — commented-out `mkUniqueNames` function.
- `ADL2FSpec.hs:359-369` — leftover prose from the plug-making era including
  `TODO151210 -> Plug A is overbodig...` (dated 2010).
- `AbstractSyntaxTree.hs:1466-1479` — commented-out `geqSig` ("just for the fun
  of it").
- `NormalForms.hs:1848-1858` — commented-out `diagnostic`/`sh` debug helpers;
  plus the two commented-out Peirce rules at 1389-1390.
- The long comment runs in `SQL.hs:1648-1662` and `ConceptTables.hs` are
  documentation, not dead code.

Export spot-check (grep for external users):

- `TableSpec.showColumsSql` — exported, **no caller anywhere** in src/ or app/.
- `TableSpec.doubleQuote` — exported, used only inside TableSpec.hs itself.
- `SQL.commentBlockSQL` — one external caller (`TableSpec.hs:82`).
- `NormalForms.dfProofs` — one external caller (`ToFSpec/Calc.hs`).
- `NormalForms.cfProof` — used by `Calc.hs` and `ToJSON/Interfaces.hs`.
- `Populated.genericAndSpecifics`, `safePSingleton2AAtomVal` — both used.
