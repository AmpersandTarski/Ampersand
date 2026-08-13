# Design choices — incremental evaluation

Register of design choices for the incremental-evaluation research line
(issue [#1682](https://github.com/AmpersandTarski/Ampersand/issues/1682)).
Numbers are stable and never reused; the current state stands here, the history
lives in git. Open questions sit at the bottom under "Still to decide".

## The transformation

**The incremental transformation operates on relation-algebra terms inside the compiler; SQL is the target language the delta terms compile to.**
*OK-1 · valid · 2026-08-13 · origin: issue #1682, [dbsp-paper-study.md](dbsp-paper-study.md), [ampersand-architecture-map.md](ampersand-architecture-map.md)*

The delta calculus lives at the `Expression` level, between `conjNF` and SQL
generation. A delta term is an ordinary `Expression`, and the existing
`sqlQuery` machinery compiles it unchanged.

*Considerations:*

1. The goal is per-transaction evaluation cost proportional to the size of the
   change, obtained through DBSP's compile-time incrementalization
   (arXiv 2203.16684, Algorithm 4.8).
2. DBSP's rewrite rules are keyed to operator properties — linear, bilinear,
   `distinct` — that are visible constructor by constructor in the AST, while
   the generated SQL has fused whole subterms into single SELECT statements.
3. Terms at this level are testable against the in-memory evaluator without a
   database, and provable in Isabelle/HOL (OK-4).
4. Transforming the generated SQL instead was considered and rejected: it would
   require reconstructing the operator circuit out of SQL, a second delta-aware
   SQL generator beside the existing one, and it ties the transformation to the
   MariaDB dialect.

*Impact on the specification:* none — ADL syntax and rule semantics are
untouched; the choice concerns compiler internals.

*Impact in production:* the generated artifacts gain per-(conjunct, relation)
delta queries next to the existing full violation queries; the prototype
database and runtime contract grow accordingly (OK-3).

**A delta relation Δr is a fabricated `Relation` value behind `EDcD`, with its own `BinSQL` plug.**
*OK-2 · valid · 2026-08-13 · origin: [data-structure-readiness.md](data-structure-readiness.md)*

Each Δr carries the signature of r, a name outside the user namespace,
`decusr = False`, and a filled `dechash`; its plug is the transaction's delta
table, registered in `plugInfos` so that `getRelationTableInfo` resolves it.

*Considerations:*

1. The goal is a delta leaf that every existing consumer — `sqlQuery`,
   `fullContents`, `conjNF`, `subst` — accepts without modification.
2. The commented-out `delta` placeholder in `NormalForms.hs:1148-1168`, a
   remnant of the historical ECA machinery, is this same construction; the
   choice restores a house pattern rather than inventing one.
3. A new `Expression` constructor was considered and rejected: it touches about
   26 exhaustive match sites in 18 modules, and with `-Wall` but no `-Werror` a
   missed site surfaces as a runtime crash instead of a build failure.
4. A wrapper datatype over `Expression` remains available as the internal
   working type of the rewrite phase; its leaves lower to this representation
   before SQL generation.

*Impact on the specification:* none; delta relations never appear in user
models and are filtered by `decusr`.

*Impact in production:* per relation one delta table (two columns plus weight)
exists in the generated schema; the runtime fills it with the transaction's
changed pairs (OK-3).

## State and runtime

**Violation state lives in MariaDB, in the `__conj_violation_cache__` table the runtime already maintains; the generated delta queries keep that table up to date incrementally, and the full violation queries remain as fallback and self-check.**
*OK-3 · valid · 2026-08-13 · origin: [prototype-runtime-map.md](prototype-runtime-map.md), [ecosystem-and-video.md](ecosystem-and-video.md)*

The prototype framework persists a materialized violation set per conjunct in
`__conj_violation_cache__` and refreshes it wholesale (DELETE+INSERT) at each
commit. Under this choice, the same table is maintained by delta queries, so
incrementality changes the refresh strategy of an existing store rather than
the architecture.

*Considerations:*

1. The goal is incrementality without a new runtime dependency: the stack stays
   MariaDB + PHP/Angular, and MariaDB itself offers no incremental view
   maintenance (verified — no materialized views at all).
2. The runtime already routes signal-rule reads entirely through this cache
   table, and cache writes already precede COMMIT on the same connection, so
   atomicity is inherited.
3. A Feldera/`dbsp`-crate sidecar engine was considered and rejected for now:
   it duplicates all state next to MariaDB with a synchronization obligation at
   every transaction boundary, and offers no Haskell or PHP embedding. It
   remains the fallback if generated-SQL incrementality hits a wall.
4. Switching to PostgreSQL for `pg_ivm` was rejected: it trades the feature for
   a DBMS migration and covers a smaller query class than DBSP.
5. Hand-written caching logic in the PHP runtime was rejected: it reimplements
   per-operator delta rules by hand — the bespoke approach DBSP replaces.

*Impact on the specification:* none.

*Impact in production:* the cache table gains a weight column; `database.sql`
gains delta tables; the reinstall flow (which executes `database.sql` verbatim)
carries both along. The framework's commit path maintains the cache from delta
queries instead of replacing it, also inside each ExecEngine iteration.

## Assurance and publication

**Correctness of the delta calculus rests on our own Isabelle/HOL proofs in `proofs/`, with the Lean formalization of DBSP as inspiration.**
*OK-4 · valid · 2026-08-13 · origin: [dbsp-paper-study.md](dbsp-paper-study.md), proofs/spike/, tchajed/database-stream-processing-theory*

Every delta rule of Phase 1 carries a machine-checked proof in Isabelle/HOL, or
an explicit flag that it does not yet. The proofs build on the existing shallow
embedding of Ampersand's heterogeneous relation algebra
(`proofs/spike/Ampersand_RA.thy`), extended with a Z-set (weighted) semantics.
A claim counts as machine-checked only after we have run the proof ourselves.

*Considerations:*

1. The goal is paper-grade assurance (OK-5): the delta calculus is the
   theoretical core of the intended article, and its correctness argument must
   be ours to state and to check.
2. The Lean formalization by Chajed proves the DBSP theorems we depend on
   (Prop 3.2, Thm 3.4, Prop 4.7) and serves as a roadmap of lemmas and proof
   structure; adopting it as trust base was rejected because it lives in a
   different system (Lean-3-era mathlib), formalizes generic Z-sets rather than
   Ampersand's typed heterogeneous algebra, and we accept "machine-checked"
   only on own observation.
3. The `proofs/` layer already carries this house style: the Kleene theories
   (`proofs/kleene/`) back the `r+`/`r*`/`r%` implementation, and
   `IncrementalDelete.thy` already proves incremental-deletion facts for
   transitive closure — directly reusable for Phase 5.
4. Informal proofs only were rejected: the normalizer's history (unsound
   Kleene laws, disabled Peirce rules) shows hand-derived rule sets in this
   codebase deserve mechanical checking.

*Impact on the specification:* none.

*Impact in production:* none directly; the proofs gate which delta rules the
compiler may apply (an unproved rule falls back to full evaluation).

**The research line is documented for publication: every phase leaves a written, reproducible record from which the article can be assembled.**
*OK-5 · valid · 2026-08-13 · origin: user decision 2026-08-13*

Decisions stand in this register; study results, measurements and phase
write-ups stand in `memorybank/incremental-evaluation/`; proofs stand in
`proofs/`; benchmark scripts and data accompany their reports. The article
draws on this material without a separate reconstruction effort.

*Considerations:*

1. The goal is an article about the incremental-evaluation work; material
   gathered along the way is the cheapest and most faithful source for it.
2. Reproducibility discipline (scripts with every measurement, versioned
   proofs, decision rationale at decision time) is what distinguishes paper
   material from work notes.
3. Writing the paper afterwards from memory was rejected: reconstruction loses
   the rejected alternatives and the measurement conditions, which are half the
   scientific value.

*Impact on the specification:* none.

*Impact in production:* none; this choice governs the working process.

## Still to decide

- The exact Z-set semantics of the heterogeneous operators in Isabelle
  (weighted pairs over typed domains) and which DBSP theorems are re-proved
  versus specialized — input for Phase 1 and OK-4's first theory file.
- The venue and scope of the article (OK-5): compiler-engineering story,
  formalization story, or both.
- Whether delta maintenance runs per ExecEngine iteration from the start or
  first lands for user transactions only (prototype-runtime-map.md, §8).
