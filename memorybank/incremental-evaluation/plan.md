# Incremental evaluation — plan of approach

Status: proposed (2026-08-13). Tracking issue: AmpersandTarski/Ampersand — "Incremental evaluation".
Branch: `incremental-evaluation`.

## Goal

Per transaction, compute each rule's violation *delta* in time proportional to the size
of the change, instead of re-running full violation queries over the whole database.
The DBSP paper (arXiv 2203.16684, VLDB 2023 best paper) supplies the theory: a
mechanical, semantics-preserving, compile-time transformation that turns any
relational-algebra circuit into its incremental form.

## What we know (study results)

Three study notes in this folder carry the ground truth:

- [dbsp-paper-study.md](dbsp-paper-study.md) — the theory: Z-sets, the stream
  calculus, Algorithm 4.8, and an operator-by-operator mapping to Ampersand's
  relation algebra.
- [ampersand-architecture-map.md](ampersand-architecture-map.md) — the compiler
  pipeline with verified file:line references, the existing partial incrementality
  (`affectedConjuncts`), and four candidate intervention points.
- [ecosystem-and-video.md](ecosystem-and-video.md) — the Developer Voices episode
  with Lalith Suresh, Feldera and the alternative engines, and the annotated IVM
  literature from Gupta & Mumick to the Lean formalization of DBSP.

The load-bearing facts:

1. Ampersand's `Expression` AST is relational algebra over binary relations with set
   semantics — exactly the language DBSP incrementalizes. Composition `r;s` is the
   bilinear (join) case; converse, union, difference are linear; set semantics is the
   `distinct` case the paper treats explicitly.
2. The compiler already knows *which* conjuncts a change touches
   (`allConjsPerDecl`/`allConjsPerConcept`); what is missing is incrementality *inside*
   the conjunct query. The commented-out `delta` placeholder in `NormalForms.hs`
   shows the idea has been on the table before.
3. The state an incremental circuit needs (the "integrals") is, for base relations,
   exactly the tables MariaDB already stores. Extra state is needed only per join
   (indexes that MariaDB also already has) and per `distinct`/violation cache.
4. MariaDB offers no incremental view maintenance of its own, and no existing engine
   (Feldera, differential dataflow) embeds into a Haskell compiler or a PHP runtime.
   Whatever we do lives in *generated artifacts*: delta queries and, where useful,
   materialized violation tables maintained by them.
5. Complement is the one construct demanding discipline: it must be rewritten into
   antijoin shapes (`r - s`, "no witness" patterns) before incrementalization, never
   materialized against `V[A×B]`. Ampersand's violation queries (`notCpl` of a
   conjunct) already push in that direction.

## Chosen direction

Implement the DBSP delta transformation **inside the Ampersand compiler, at the
`Expression` level, before SQL generation** (intervention point (a) of the
architecture map), and keep all state in MariaDB (materialized violation tables per
conjunct, maintained by the generated delta queries — the workable half of
intervention point (c)). The existing full violation queries remain as fallback and
as test oracle.

Why this direction: it is the only one that keeps the current MariaDB+PHP stack
(no sidecar engine, no new runtime dependency), it reuses `FSpec/SQL.hs` unchanged
(delta terms are ordinary `Expression`s), and it puts the intelligence where
Ampersand's intelligence already lives — in the compiler, as a semantics-preserving
rewrite, which is also the form in which it can be tested against the existing
evaluator and, eventually, proved.

## Readiness (added 2026-08-13)

Two readiness reports examined whether the compiler can carry this plan:
[data-structure-readiness.md](data-structure-readiness.md) and
[technical-debt-scan.md](technical-debt-scan.md). Conclusions folded into the plan:

- **Δr representation:** a fabricated `Relation` behind `EDcD` (with `dechash`
  filled, `decusr = False`, a name outside the user namespace) — zero AST ripple,
  and the historical `delta` placeholder in `NormalForms.hs:1148-1168` is this exact
  approach. A new `Expression` constructor would touch ~26 match sites in 18 modules
  with only warnings to find them. Each delta relation gets its own `BinSQL` plug
  (the transaction's delta table), which `getRelationTableInfo` requires anyway.
- **Reusable as-is:** `conjNF` is pure; `subst` already substitutes a relation by an
  arbitrary term (`r := r ∪ Δr` for free); the combinators accept well-typed delta
  terms without smart-constructor friction; evaluation dispatches in one `case`
  block in `Populated.hs`, so the weighted evaluator is a contained one-module job.
- **Vestigial but harmless:** `rc_dnfClauses` and `vquads` feed only the Haskell
  dump; leave them, build nothing on them.

### Phase 0 — Preparatory interventions

Targeted work before Phase 1, chosen because the feature leans on it — not a broad
simplification pass (the ten path modules build with zero warnings under `-Wall`
and carry two hlint hints in total; there is little to simplify there):

1. **Pin down `pairsInExpr`/`fullContents` semantics with unit and property tests.**
   Today the only semantic guard is `ampersand validate` — end-to-end, database
   required, and blind to an error that hits SQL and Haskell alike. Phase 2 promotes
   `pairsInExpr` to oracle; an oracle must first be trusted on its own.
2. **Resolve the marked uncertainties in the normalizer core** — the two
   "use of posCpl is erroneous" TODOs (`NormalForms.hs:1232-1233`) and the two
   disabled Peirce rules — by fixing or by documenting why they are safe. `conjNF`
   is load-bearing for today's violation queries and for tomorrow's delta terms.
3. **Guard the `ConceptTables`↔`selectExpr` mirror with an automated check.** The
   sync contract is comment-plus-runtime-`fatal` today, and delta queries will read
   concept tables in new places (population deltas).

*Exit:* the tests of (1) and (3) run in `stack test`; the TODOs of (2) are fixed or
carry a documented verdict.

## Phases

Each phase has a deliverable and an exit criterion; no phase starts before the
previous one's exit criterion is met and its insights are folded back into this plan.

### Phase 1 — Delta calculus for Ampersand terms (design) — DONE 2026-08-13

Write the delta rules for every `Expression` constructor: for a change
`Δr` to relation `r`, the term `Δ(e)` that computes the change of `e`, following
Table 4.2 and Theorem 3.4 of the paper (weights become witness counts; concept
populations join in as unary relations, so atom creation/deletion is a delta too).
Decide the complement/residual discipline: the normal form every conjunct must reach
(antijoins only) before incrementalization, and what to do when a term resists it
(fall back to full evaluation for that conjunct).
*Deliverable:* a design document in this folder with the rule table and worked
examples from the regression suite. *Exit:* the rules cover every constructor or
name its explicit fallback.

*Status:* [delta-calculus.md](delta-calculus.md) carries the desugaring table,
the delta rules (D1-D7) and the proof obligations (S/Z/B series); every
constructor has a rule or the explicit D7 fallback.

### Phase 2 — Oracle validation in pure Haskell — DONE 2026-08-13

Build a small in-memory incremental evaluator next to `pairsInExpr`/`allViolations`
(the compiler's existing full evaluator) and test the delta rules against it:
apply random and regression-derived change sequences, check after every step that
integrated deltas equal full re-evaluation.
This validates the theory-to-Ampersand mapping before any SQL or PHP exists, catches
weight/`distinct` bookkeeping errors early, and doubles as executable documentation.
*Deliverable:* the evaluator plus a property-test suite. *Exit:* green on the full
regression population set, including flipped/UNI/INJ storage variants.

*Status:* implemented as `Ampersand.FSpec.Incremental`(+`.ZSet`) with the CLI
command `ampersand incremental-bench` (OK-6). The `--verify` oracle holds on
ten regression models (Kleene, cartesian products, subtyping, complements,
script populations) with 60+ verified transactions each; the oracle caught and
killed two real bugs on the way (OK-7, consideration 2). Phase 0.1 is closed:
`Ampersand.Test.Incremental.Properties` runs in `stack test` — one QuickCheck
property per proven lemma over the real `ZSet` functions, plus an engine
oracle property on a miniature context (ISA hierarchy, ONE-typed terms, one
circuit per node kind) driven by random set-disciplined transaction streams.
First scaling measurements stand in [bench/RESULTS.md](bench/RESULTS.md):
across a ×40 database growth the incremental step grows ×6 (10→65 µs) while
full re-evaluation grows ×1200 (1.65 ms→1.98 s), speedup ×166 → ×30 626.

### Phase 3 — Delta SQL generation — FIRST INCREMENT DONE 2026-08-13

A new compiler module derives, per (conjunct, affected relation), the delta term and
compiles it with the existing `sqlQuery` machinery, parameterized by the transaction's
changed pairs; `conjuncts.json` grows an optional per-relation delta-query field, and
the schema gains one materialized violation table per conjunct.
`ConceptTables.hs` must keep seeing every concept the delta queries read.
*Deliverable:* generated delta SQL behind a feature flag. *Exit:* an
`ampersand validate`-style harness shows delta-maintained violation tables equal to
full-query results on the test suite.

*Status:* issue #1684, branch `delta-sql`. The design shifted from weighted
caches to **delta-scoped re-evaluation** (OK-8): candidate queries name the
pairs to recheck, the recheck runs the existing violation predicate, and the
cache schema stays as it is. The candidate calculus (W/N envelopes + D-rules,
[delta-calculus.md](delta-calculus.md) §7) lives in
`Ampersand.FSpec.Incremental.DeltaTerms`; `conjuncts.json` carries the
candidate queries, `relations.json` the delta-table names, `database.sql` the
delta tables — all additive. The referee harness (`incremental-bench --sql`,
`Ampersand.Prototype.DeltaSQLHarness`) runs green on five models against a
real MariaDB, subtyping and complements included; Kleene models correctly
report zero delta support. The K-obligation proofs (candidate completeness,
§7; named C-obligations until 2026-08-14) are machine-checked in
`proofs/incremental/Candidates.thy` on branch `incremental-evaluation`
(register claim PRF-7), and the QuickCheck bridge
`Ampersand.Test.Incremental.CandidateProperties` re-checks the real
`widen`/`narrow`/`candidateTerms` against those lemmas on every
`stack test`. Open within this phase: a candidate-cost measurement.

### Phase 4 — Runtime adoption and measurement

With the prototype-framework repo (AmpersandTarski/prototype): consume the delta
queries, maintain the violation tables per transaction, keep the full queries as
fallback and as periodic self-check. Benchmark on a realistic model and population;
the claim to verify is per-transaction cost proportional to change size.
Production confidence is built operationally on top of the proofs: a **shadow
run** first (a real application maintains violations both ways for a period,
logs every divergence, users see only the old route), then a permanent
**sampled self-check** in production (periodically recompute one conjunct in
full and compare; on divergence: alarm plus automatic cache rebuild from the
full queries), and a **feature switch per application**. Edge cases the runtime
map already names must be covered explicitly: bulk mutations
(`deleteAllLinks`, `removeAtom`), ExecEngine iterations, transaction
boundaries, crash recovery.
*Deliverable:* an end-to-end prototype and a measurement report. *Exit:* measured
order-of-magnitude improvement on transaction-heavy scenarios, with identical
violation sets — including a divergence-free shadow-run period on a real
application.

### Phase 5 — The harder constructs

Kleene closures via the nested-stream construction (semi-naïve evaluation that also
handles deletions — better than today's `WITH RECURSIVE` per query); the ExecEngine
repair loop as an outer feedback cycle; interface (`_SRCATOM`) queries if profiling
says they matter.
*Deliverable/exit:* per construct, decided when Phase 4's numbers show where the
remaining cost sits.

## To investigate before Phase 1

- **Runtime ground truth** — done, see [prototype-runtime-map.md](prototype-runtime-map.md).
  Key findings: the framework already materializes violations per conjunct in
  `__conj_violation_cache__`, refreshed wholesale at each commit; signal rules read
  entirely from that cache. Delta maintenance therefore changes the refresh strategy
  of an existing store (OK-3). The natural delta hook is `MysqlDB::addLink/deleteLink`
  (bulk operations like `deleteAllLinks` need care); `conjuncts.json` tolerates added
  optional fields; reinstall executes `database.sql` verbatim, so extra tables ride
  along; RAP uses this same framework, so it inherits the improvement.
- **Baseline measurement.** Profile a realistic prototype (e.g. a `testing/` model
  with population, or an RVB-class model) to verify the premise that conjunct
  violation queries dominate transaction cost, and to fix the yardstick Phase 4
  must beat. If interface (`_SRCATOM`) read queries dominate instead, the plan's
  priorities shift.
- **The backfill story.** Initial population of materialized violation tables at
  install/reinstall time (the model-hash flow): one full-query run per conjunct at
  install is the obvious route; confirm it fits the framework's reinstall mechanism.
  The Feldera episode names backfill as the engineering Achilles heel — for us it is
  bounded because the full queries already exist.
- **Scope the Isabelle/HOL proof base** (decision OK-4: we redo the correctness
  proofs ourselves, with the Lean formalization as inspiration): determine how
  `proofs/spike/Ampersand_RA.thy` extends to a Z-set (weighted) semantics of the
  heterogeneous operators, which DBSP theorems we re-prove versus specialize
  (Prop 3.2, Thm 3.4, Prop 4.7), and how the Lean development
  (tchajed/database-stream-processing-theory) maps onto that plan lemma by lemma.

## Proof track — Isabelle/HOL (parallel to Phases 1–2)

Decision OK-4: every delta rule of Phase 1 carries its own machine-checked proof in
Isabelle/HOL, or an explicit flag that it does not yet — and an unproved rule falls
back to full evaluation. The proofs extend the existing shallow embedding of
Ampersand's heterogeneous relation algebra (`proofs/spike/Ampersand_RA.thy`) with a
Z-set semantics (weighted pairs over typed domains); the Kleene theories in
`proofs/kleene/` — including `IncrementalDelete.thy`, which already proves
incremental-deletion facts for transitive closure — feed Phase 5 directly. The Lean
development by Chajed serves as a lemma-by-lemma roadmap, not as trust base.
*Deliverable:* one theory file per Phase-1 rule group, running headless via the
`proofs/` toolchain. *Exit:* the Phase-1 rule table cites a checked lemma per rule.

*Status (2026-08-13, updated after issue
[#1683](https://github.com/AmpersandTarski/Ampersand/issues/1683)):* session
`Incremental_Delta` in `proofs/incremental/` builds green from clean
(verified first-hand: `isabelle build -c -D proofs/incremental`), zero
`sorry`, six theories. Covered: Z1-Z5, B1-B6, S1-S5 as before, and now also
the whole-circuit induction (`Circuit.thy`, obligations C1-C5: a step
preserves the invariant, specification nodes recover unconditionally,
well-formed outputs are the set semantics, the all-zero base is well-formed,
and every state reachable from the base by at least one transaction —
backfill included — is correct) and the population mirror (`Population.thy`,
P1-P5: the occurrence integral is linear, carries the `atomValuesOf` set,
its distinct-delta is the zero-crossing H, and the relation front-end sums
feeders with `pairsOf` contents). The obligation→lemma table stands in
`proofs/incremental/README.md`, together with what deliberately remains
outside the proofs (dirty-flag shortcuts, the Kleene nodes' reading of their
child, and the code itself — covered by the QuickCheck bridge and the
oracle).

Working out the base case surfaced two real engine defects, both fixed on
branch `prove-incremental-core` and pinned by the new engine property:
ONE's population was pre-seeded instead of travelling through the backfill
transaction (leaving `I[ONE]`/`V[..*ONE]` circuits permanently empty), and
the feeder/cone wiring was derived from the initially populated relations,
so transactions on initially empty relations or concepts fell through
silently.

## Paper track (continuous)

Decision OK-5: the work is documented for publication. Decisions go into
[DesignChoices.md](DesignChoices.md) at decision time, with the rejected
alternatives; every phase ends with a written record in this folder; measurements
ship with the scripts and data that produced them; proofs are versioned in
`proofs/`. The article is assembled from this material — candidate storyline:
DBSP-style incrementalization of a relation-algebra rule engine, with a
machine-checked delta calculus and measured order-of-magnitude gains.

## When the referee can go

`fullContents` carries three roles; each has its own dismantling criterion.
As a runtime component (the fallback nodes) it disappears construct by
construct as proven delta rules replace fallbacks — residuals first (S1-S4
proven, implementation work remains), then Kleene closures (Phase 5,
IncrementalDelete.thy), then EBin and composite concepts; criterion:
`circuitFallbacks` reports 0 on the target models. As load-bearing evidence it
has retired: Circuit.thy (the whole-circuit induction) and Phase 0.1 (the
lemmas as QuickCheck properties over the Haskell functions) have landed, so
`--verify` is now a diagnostic switch, the status `ampersand validate` has
today. As executable specification it stays: the compiler needs it for
compile-time checks, and Phase 3 gives it a new referee role — holding the
generated delta SQL against the proven engine.

## Risks and open questions

- **Weights vs. sets in SQL.** Z-set weights (witness counts) must live in the
  materialized tables as a count column; getting `distinct`'s zero-crossing logic
  right in SQL is the trickiest translation step. Phase 2 exists to pin the semantics
  before this translation.
- **Concurrent writes.** The violation tables are caches; the design must state their
  consistency story relative to MariaDB transactions (single-writer per prototype
  today, but say so explicitly).
- **Schema migration.** Extra tables and a count column change `database.sql`; the
  model-hash reinstall mechanism covers prototypes, but document it.
- **Scope of proof.** The delta rules earn trust twice: through the Phase 2 oracle
  tests and through the Isabelle/HOL proof track (OK-4). The open modelling question
  is the Z-set semantics of the heterogeneous operators in Isabelle; until a rule's
  proof lands, the compiler treats it as unproved (full-evaluation fallback).

## Working agreements

Results land in this folder; the GitHub issue tracks progress and decisions at the
feature level. Before starting any sub-investigation, check this folder to avoid
duplicate work; after finishing one, update this plan.
