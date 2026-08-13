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

## Phases

Each phase has a deliverable and an exit criterion; no phase starts before the
previous one's exit criterion is met and its insights are folded back into this plan.

### Phase 1 — Delta calculus for Ampersand terms (design)

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

### Phase 2 — Oracle validation in pure Haskell

Build a small in-memory incremental evaluator next to `pairsInExpr`/`allViolations`
(the compiler's existing full evaluator) and test the delta rules against it:
apply random and regression-derived change sequences, check after every step that
integrated deltas equal full re-evaluation.
This validates the theory-to-Ampersand mapping before any SQL or PHP exists, catches
weight/`distinct` bookkeeping errors early, and doubles as executable documentation.
*Deliverable:* the evaluator plus a property-test suite. *Exit:* green on the full
regression population set, including flipped/UNI/INJ storage variants.

### Phase 3 — Delta SQL generation

A new compiler module derives, per (conjunct, affected relation), the delta term and
compiles it with the existing `sqlQuery` machinery, parameterized by the transaction's
changed pairs; `conjuncts.json` grows an optional per-relation delta-query field, and
the schema gains one materialized violation table per conjunct.
`ConceptTables.hs` must keep seeing every concept the delta queries read.
*Deliverable:* generated delta SQL behind a feature flag. *Exit:* an
`ampersand validate`-style harness shows delta-maintained violation tables equal to
full-query results on the test suite.

### Phase 4 — Runtime adoption and measurement

With the prototype-framework repo (AmpersandTarski/prototype): consume the delta
queries, maintain the violation tables per transaction, keep the full queries as
fallback and as periodic self-check. Benchmark on a realistic model and population;
the claim to verify is per-transaction cost proportional to change size.
*Deliverable:* an end-to-end prototype and a measurement report. *Exit:* measured
order-of-magnitude improvement on transaction-heavy scenarios, with identical
violation sets.

### Phase 5 — The harder constructs

Kleene closures via the nested-stream construction (semi-naïve evaluation that also
handles deletions — better than today's `WITH RECURSIVE` per query); the ExecEngine
repair loop as an outer feedback cycle; interface (`_SRCATOM`) queries if profiling
says they matter.
*Deliverable/exit:* per construct, decided when Phase 4's numbers show where the
remaining cost sits.

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
- **Scope of proof.** The DBSP theorems are machine-checked in Lean
  (tchajed/database-stream-processing-theory — inspect before leaning on it); our own
  rewrite must earn trust through the Phase 2 oracle tests, and possibly later through
  the proofs/ toolchain in this repo.

## Working agreements

Results land in this folder; the GitHub issue tracks progress and decisions at the
feature level. Before starting any sub-investigation, check this folder to avoid
duplicate work; after finishing one, update this plan.
