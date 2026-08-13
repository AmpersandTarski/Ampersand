# Data-structure readiness for the delta transformation

Assessment of the compiler's data structures for carrying a DBSP-style delta
transformation at the `Expression` level (plan.md, Phase 1-3). All file:line
references verified against branch `incremental-evaluation` on 2026-08-13.

## 1. The Expression AST and how to represent Δr

`data Expression` lives at `src/Ampersand/Core/AbstractSyntaxTree.hs:1200-1243`.
It has 21 constructors: binary operators `EEqu` (=), `EInc` (|-), `EIsc` (/\),
`EUni` (\/), `EDif` (-), `ELrs` (/), `ERrs` (\), `EDia` (<>), `ECps` (;),
`ERad` (!), `EPrd` (*); unary `EKl0` (*), `EKl1` (+), `EFlp` (~), `ECpl` (-),
`EBrk` (parentheses); leaves `EDcD !Relation`, `EDcI !A_Concept`,
`EBin !PBinOp !Signature` (comparison operators), `EDcV !Signature` (full
relation), `EMp1 !PAtomValue !A_Concept` (atom literal). Everything a delta
term needs is expressible except the delta leaf Δr itself.

### Option (i): a new constructor (e.g. `EDelta !Relation`)

Measured ripple. `package.yaml:24-34` sets `-Wall -Wcompat` but no `-Werror`
(`-Wincomplete-record-updates`/`-Wincomplete-uni-patterns` are commented out,
lines 30-31), and CI adds no `-Werror`/pedantic flag. So a new constructor
produces *warnings only* — and therefore a real risk of runtime
"Non-exhaustive patterns" crashes at any missed site. Exhaustive match sites
(counted via the rarely-special-cased `EDia` in pattern position): about 26
sites in 18 modules:

- `Core/AbstractSyntaxTree.hs`: `Hashable` (1245), `flp` (1342), `sign` (1366) — 3
- `ADL1/Expression.hs`: `subst` (40), `primitives` (79), `subExpressions`
  (104), `insParentheses` (236) — 4
- `ADL1/P2A_Converters.hs`: `refineANY` (1479); `getBetweenConcept` (1416) has
  an explicit fatal catch-all — 1
- `Core/A2P_Converters.hs`: `aExpression2pTermPrim` (305, feeds showA) — 1
- `Classes/Relational.hs`: two exhaustive property cases (176, 209); the
  `isUni/isTot/...` family (101-148) uses `_ -> False` catch-alls — 2
- `Classes/ConceptStructure.hs`: `affectedByInsOrDel` (156-177, all 21 listed) — 1
- `Output/PredLogic.hs` (213), `Output/PandocAux.hs` `showExpr` (292) — 2
- `FSpec/SQL.hs`: `selectExpr` (1266 region) — 1
- `FSpec/Transformers.hs` (1251), `FSpec/ShowHS.hs` (745) — 2
- `FSpec/Oscillation.hs` (386, 408), `Diagnosis/Extract.hs` (661, 693),
  `Diagnosis/PatternGraph.hs` (170) — 5
- `FSpec/ToFSpec/CreateFspec.hs` (312), `ConceptTables.hs` (116),
  `Populated.hs` (106), `NormalForms.hs` `expr2RTerm` (555) — 4

Beyond warnings, catch-all sites would *silently* mis-handle the new
constructor: `normStep`'s final clause `nM _ x _ = (x, [], "<=>")`
(`NormalForms.hs:1658`) treats it as inert (actually correct for a delta
leaf), but `Relational.hs`'s `_ -> False` and `CtxError.hs:855` defaults give
no warning at all. Since delta terms are built after type checking, only ~6
sites are load-bearing (SQL.hs, Populated.hs, ConceptTables.hs, ShowHS,
A2P/showA, the three AST instances); the other ~20 need mechanical clauses.

### Option (ii): a synthetic Relation value (no AST change)

`Relation` (`AbstractSyntaxTree.hs:597-620`) is a plain record, fully exported
(`Relation (..)` at `AbstractSyntaxTree.hs:29` and `ADL1.hs:79`). Fields:
`decnm`, `decsgn`, `declabel`, `decprps`, `decDefaults`, `decpr`, `decMean`,
`decfpos`, `decusr`, `decpat`, `dechash`. Invariants are light: `Eq`/`Ord`
compare `(name, SignOrd sign)` only (622-629); `Hashable` reads the
precomputed `dechash` (637-638), which the fabricator must fill (e.g.
`hash (name, sign)`). Today only one place constructs a `Relation`
(`P2A_Converters.hs:2165`), and `mkName` is public (`Basics/Name.hs:159`;
precedent: `nameOfExecEngineRole`, line 166). Historical precedent: the
commented-out `delta` placeholder (`NormalForms.hs:1148-1168`) is *exactly*
this option — `EDcD Relation {decnm = "Delta", decsgn = sgn, decusr = False,
...}`. (Its field list is stale: `decprL/decprM/decprR` are now
`decpr :: Maybe Pragma`, and `declabel`/`decDefaults` were added since.)

The one hard obstacle: SQL generation. `getRelationTableInfo`
(`FSpecAux.hs:9-14`) looks the relation up in `dLkpTbl` of the plugs and
calls `fatal "Relation not found"` on a miss; `selectRelation` calls it at
`SQL.hs:1327`. So a fabricated Δr compiles to SQL only if it *has a plug* —
which the design wants anyway: the transaction's changed pairs live in a
delta table, so registering one `BinSQL` plug (with a `RelStore`) per delta
relation in `plugInfos` makes `sqlQuery` work unchanged. Precedent for a
plugless leaf exists too: `EBin` compiles to SQL without any table
(`SQL.hs:902-940`). Remaining care: pick names outside the user namespace and
keep `decusr = False`, so `bindedRelationsIn`-based maps (`ADL2FSpec.hs:228`)
can filter delta relations where needed.

### Option (iii): a wrapper datatype over Expression

A separate `data DeltaTerm` cannot be fed to `sqlQuery`/`fullContents`, so it
must lower to `Expression` anyway — at which point its leaves need (i) or
(ii). It is still useful as the *working type of the rewrite phase* (keeping
the calculus honest about which sub-terms are deltas), lowered via option (ii)
leaves before SQL generation.

**Verdict Q1:** option (ii) is the least invasive and is the historical
approach; combine with (iii) internally if the calculus wants it. Option (i)
costs ~26 match sites across 18 modules with only warnings (no `-Werror`) to
find them, plus silent catch-all hazards. Bonus: `subst`
(`ADL1/Expression.hs:40`) already substitutes a relation by an arbitrary
expression — `r := r ∪ Δr` comes for free.

## 2. Weighted (Z-set) evaluation

`fullContents :: ContextInfo -> [Population] -> Expression -> AAtomPairs`
(`Populated.hs:61`) returns a `Set AAtomPair`; internally it evaluates via an
adjacency map `Map AAtomValue (Set AAtomValue)` (local `contents`,
`Populated.hs:76-189`). Dispatch on all 21 constructors happens in **one
place** — the single `case expr of` at lines 81-189. The FSpec field
`pairsInExpr` (`FSpec.hs:147`) is bound to this function in
`ADL2FSpec.hs:101`; `allViolations` (FSpec.hs:150) is derived from it.

A parallel weighted evaluator (`Map AAtomPair Int` or
`Map AAtomValue (Map AAtomValue Int)`) is a one-module job mirroring that
case block (~110 lines). Points of attention: the non-monotone cases —
`ELrs/ERrs/EDia/ERad/EPrd/EDcV` (lines 95-127, 164-177) enumerate active
domains via `atomValuesOf` (line 30), so concept-population deltas must feed
in as unary Z-sets; `ECpl` is defined as `EDcV (sign x) .-. x` (line 150),
the same complement discipline issue the plan already names; closures use
`transClosureMap` (lines 141-148). **Verdict Q2:** ready; single dispatch
site, contained effort.

## 3. Dead or vestigial structures in the path

- **(a) `rc_dnfClauses`** (`AbstractSyntaxTree.hs:519`): built once at
  `NormalForms.hs:1912` (via `allShifts`), consumed *only* by the Haskell dump
  (`ShowHS.hs:204`). Vestigial for the running system. Removing it would
  delete the `allShifts`/`shiftL`/`shiftR` call chain's only consumer; leaving
  it is harmless (lazy field). Recommendation: leave, but do not build on it.
- **(b) `vquads`** (`FSpec.hs:122`, type at 263, built `ADL2FSpec.hs:76` via
  `Calc.hs:105-121`): consumed only by `ShowHS.hs` (247, 306, 361-367).
  Documentation-only; same recommendation as (a).
- **(c) The commented-out `delta`** (`NormalForms.hs:1148-1168`): a synthetic
  `Relation` named "Delta" over a given signature, "a placeholder for
  inserting or removing links from terms" — the leaf the old ECA/repair
  machinery substituted into terms to reason about which insertions/deletions
  restore a rule. It validates option (ii) as the house style; the code
  itself is stale against today's `Relation` record.
- **(d) Other FSpec fields:** `fDeriveProofs` (`FSpec.hs:80`) is consumed only
  by the `proof` command (`Commands/Proof.hs:37`) and is deliberately lazy
  ("non-termination issue in normalforms", comment at FSpec.hs:80 and
  `NormalForms.hs:55`) — keep clear of it. `crudInfo`, `fsisa`, `allExprs`
  all have real consumers. Nothing here blocks the delta work; nothing needs
  removal first.

## 4. NormalForms.hs structure

1971 lines, four parts: (1) the **RTerm system** — `RTerm` datatype (76-98,
n-ary, set-based for associative/commutative operators), `expr2RTerm`/
`rTerm2expr` (≈540-660), the matching/substitution engine `dSteps` (169-460),
and text-parsed derivation rules `tceDerivRules` (≈1050-1118, incl. the
machine-checked Kleene lemmas); used **only** by `dfProofs` (line 48, "for
confluence testing") and the proof output — not by the compile path.
(2) The **hand-written normalizer** `normStep` (1207-1660) working directly
on `Expression`, with a final inert catch-all (1658), driven by `nfPr`
(1662-1674). (3) **`conjNF`/`disjNF`** (1676-1682) and `conjuncts` (1713),
`allShifts` (1721-1791), `makeAllConjs` (1906-1920). (4) The `Proof` type and
commented-out rule sets (1120-1168).

`conjNF, disjNF :: env -> Expression -> Expression` — the `env` parameter is
**ignored** (`pr dnf _ expr`, line 1679): conjNF is a pure
`Expression -> Expression` function any delta module can call. The RTerm
rule engine is normalization-specific (rules must be equalities/inclusions;
`dSteps` is a fatal on terms containing variables, line 231) and the delta
transformation is a single structural recursion, not a fixpoint rewrite — so
write Δ as a plain recursive function; the derivation-rule parser is not
needed. **Verdict Q4:** conjNF is directly reusable and pure; the rewrite
engine is not needed and not in the way.

## 5. Typing of delta terms

The infix combinators (`.\/.`, `./\.`, `.-.`, `.:.` etc.,
`AbstractSyntaxTree.hs:1286-1331`) are raw constructor applications — no
smart-constructor checks at build time. Type correctness is only asserted
lazily, when `sign` is demanded: `EIsc/EUni/EDif` compute `joinSig` of the
operand signatures and call `fatal` if incompatible
(`AbstractSyntaxTree.hs:1372-1383`); the other operators just combine
source/target. Since Δe has by construction the same signature as e (option
(ii) gives the delta relation `decsgn = decsgn r`), every combination the
delta calculus builds (`e .-. Δe`, `Δr .:. s`, …) is signature-compatible and
no check fires. `HasSignature Relation` is just `decsgn` (line 659-660).
**Verdict Q5:** no obstacles; the type system stays out of the way because
checking is post-hoc and delta terms are born well-typed.

## Overall verdict

Represent Δr as a **fabricated Relation behind `EDcD`** (option ii), exactly
as the historical placeholder did: zero AST ripple, `Eq/Ord/Hashable` work
with `dechash` filled, `sqlQuery`, `fullContents`, `conjNF` and `subst` all
apply unchanged. The two structural obstacles to clear first: give each delta
relation a **plug** (`getRelationTableInfo` is fatal without one,
`FSpecAux.hs:13`) so the existing SQL path compiles it against the
transaction's delta table; and add the **weighted evaluator** next to
`Populated.hs` (set semantics is baked into `AAtomPairs` everywhere; weights
exist nowhere yet). The vestigial `rc_dnfClauses`/`vquads` can stay as they
are — they neither help nor hinder.
