# Design choices — incremental evaluation

Register of design choices for the incremental-evaluation research line
(issue [#1682](https://github.com/AmpersandTarski/Ampersand/issues/1682)).
Numbers are stable and never reused; the current state stands here, the history
lives in git. Open questions sit at the bottom under "Still to decide".

## The transformation

**The incremental transformation operates on relation-algebra terms inside the compiler; SQL is the target language the delta terms compile to.**
*DC-1 · valid · 2026-08-13 · origin: issue #1682, [dbsp-paper-study.md](dbsp-paper-study.md), [ampersand-architecture-map.md](ampersand-architecture-map.md)*

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
   database, and provable in Isabelle/HOL (DC-4).
4. Transforming the generated SQL instead was considered and rejected: it would
   require reconstructing the operator circuit out of SQL, a second delta-aware
   SQL generator beside the existing one, and it ties the transformation to the
   MariaDB dialect.

*Impact on the specification:* none — ADL syntax and rule semantics are
untouched; the choice concerns compiler internals.

*Impact in production:* the generated artifacts gain per-(conjunct, relation)
delta queries next to the existing full violation queries; the prototype
database and runtime contract grow accordingly (DC-3).

**A delta relation Δr is a fabricated `Relation` value behind `EDcD`, with its own `BinSQL` plug.**
*DC-2 · valid · 2026-08-13 · origin: [data-structure-readiness.md](data-structure-readiness.md)*

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
changed pairs (DC-3).

## State and runtime

**Violation state lives in MariaDB, in the `__conj_violation_cache__` table the runtime already maintains; the generated delta queries keep that table up to date incrementally, and the full violation queries remain as fallback and self-check.**
*DC-3 · valid · 2026-08-13 · origin: [prototype-runtime-map.md](prototype-runtime-map.md), [ecosystem-and-video.md](ecosystem-and-video.md)*

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

**The compiler carries an in-memory incremental evaluator, exposed as the command `ampersand incremental-bench`, with per-transaction oracle verification.**
*DC-6 · valid · 2026-08-13 · origin: user decision 2026-08-13, issue #1682*

The modules `Ampersand.FSpec.Incremental` and `Ampersand.FSpec.Incremental.ZSet`
implement the delta calculus of [delta-calculus.md](delta-calculus.md) as a
circuit interpreter; the command benchmarks it against full re-evaluation of
the affected conjuncts on synthetic populations of chosen scales, and
`--verify` holds every transaction's maintained violation sets against
`fullContents`.

*Considerations:*

1. The goal is measurable evidence — for issue #1682 and the article — that
   per-transaction cost stays flat where full re-evaluation grows with the
   database, before any SQL generation or runtime work builds on the calculus.
2. The evaluator doubles as Phase 2's oracle validation: the `--verify` runs on
   the regression models are the observational-equality test the plan demanded.
3. A benchmark inside the prototype stack (PHP + MariaDB) was considered and
   rejected for this phase: it measures the framework and the DBMS as much as
   the calculus, and it needs the Phase 3/4 artifacts that this phase must
   justify first.

*Impact on the specification:* none; models are unchanged.

*Impact in production:* none yet — the command is measurement-only and the
generated SQL is untouched.

**A circuit node carries a set as output; weighted state stays local to the nodes that need it, and every construct without a proven delta rule runs as a recompute node.**
*DC-7 · valid · 2026-08-13 · origin: [delta-calculus.md](delta-calculus.md), oracle runs of 2026-08-13*

Each node's output is a Z-set with all weights 1; pre-`distinct` integrals,
composition's flipped index, and the product projections live inside the node
kinds that use them. Residuals, diamond, relative addition, `EBin`,
non-collapsible complements, and Kleene closures evaluate as recompute
(fallback) nodes over the current population.

*Considerations:*

1. The goal is an engine that is correct on every model from day one and
   incremental on the common violation shapes; coverage grows per proven rule
   (DC-4 gates this), and `circuitFallbacks` reports the coverage per conjunct.
2. The oracle caught two real errors during construction, which the register
   records as the argument for keeping the oracle in every phase: a
   left/right-occurrence merge that silently dropped concept-population
   weights, and a De Morgan push that assumed untyped complements — in
   Ampersand's typed algebra, double-negation elimination is valid only when
   the signatures coincide, so the push carries the target signature and the
   difference absorption carries a signature guard (`signLeq`).
3. A single global distinct-consolidation pass (the paper's step 2) was
   considered and postponed: per-node distinct state is simpler to prove and
   to test, and the measurements show the target behaviour already.

*Impact on the specification:* none.

*Impact in production:* none yet; the node inventory is the blueprint for the
delta-SQL generation of Phase 3.

**The engine's transaction domain is the full set of declared relations and concepts; all wiring is fixed at construction.**
*DC-10 · valid · 2026-08-13 · origin: issue #1683, engine oracle property*

`mkEngine` receives the declared relations and fixes the feeder lists per
term relation, the concept cones per relation, and the ISA-upward map for
explicit atom populations, all over the declaration domain. `engineInit`
only runs the backfill transaction; ONE's singleton population is part of
that transaction.

*Considerations:*

1. The goal is an engine that is correct for every transaction the runtime
   can send, not only for relations that happen to hold initial population.
2. The earlier wiring derived these maps from the initially populated
   relations and concepts, and the pre-seeded ONE population never crossed
   the circuits. Both defects stayed invisible to `incremental-bench`
   (which populates every relation at init and never sends ONE deltas);
   the base case of the circuit invariant (Circuit.thy, C4) predicted the
   first, and the engine oracle property demonstrated both.
3. Wiring lazily inside `applyTx` was considered and rejected: it moves a
   per-construction cost into every transaction and leaves the transaction
   domain implicit.

*Impact on the specification:* none.

*Impact in production:* none yet; Phase 3's generated artifacts inherit the
rule that delta plumbing exists per declared relation, populated or not.

**The correctness of the incremental core rests on two machine-checked layers: Isabelle proves the model (whole-circuit induction included), and per-build QuickCheck properties bind the Haskell code to that model.**
*DC-11 · valid · 2026-08-13 · origin: issue #1683, [correctness-argument.md](correctness-argument.md)*

The session `Incremental_Delta` proves the delta rules, the whole-circuit
induction (C1-C5) and the population mirror (P1-P5); the test-suite module
`Ampersand.Test.Incremental.Properties` holds one QuickCheck property per
proven lemma against the real `ZSet` functions, plus an engine oracle
property over random set-disciplined transaction streams. `--verify` in
`incremental-bench` is thereby a diagnostic, no longer load-bearing
evidence.

*Considerations:*

1. The goal is to retire the runtime referee from the trust chain (the plan's
   "When the referee can go") with evidence that every build re-checks.
2. Code generation from Isabelle (verified extraction) was considered and
   rejected: it replaces the performance-critical `Map`-based code path and
   ties the build to the proof toolchain, for a gap the per-build properties
   cover; it returns to the table if a property ever finds a divergence.
3. Proving against the literal Haskell (hs-to-coq-style translation) was
   rejected: no maintained toolchain for this GHC/stack setup.
4. The property suite caught a real defect on its second random stream
   (DC-10, consideration 2), which is the empirical argument for keeping it
   in `stack test` permanently.

*Impact on the specification:* none.

*Impact in production:* none directly; the proofs and properties gate which
constructs may leave the fallback route, and Phase 3 inherits a proven core.

**The phase-4 shadow environment is an API-level copy of FC5: the delta-branch framework worktree mounted into a stock framework image, with host-generated generics and a replay endpoint that drives transactions through the full request pipeline.**
*DC-12 · valid · 2026-08-14 · origin: [fase4-fc5-schaduwdraai.md](fase4-fc5-schaduwdraai.md), first shadow runs of 2026-08-14*

The shadow run executes against a dedicated stack (own containers, ports and
database) in which `/var/www` is the `feat-delta-conjunct-maintenance`
worktree, the base image supplies only PHP and Apache, and the generics come
from the delta-sql compiler on the host; an Angular frontend is absent.
Replay transactions enter through `POST /admin/replay/txn`, an uncommitted
route file that mutates relations via `Relation::addLink`/`deleteLink` and
closes with `runExecEngine()->close()` — the same path interface edits
follow. Replay pairs come from the exporter's view of the current
population. The configuration deviates from FC5 production on two points:
`session.loginEnabled` is false and `deltaConjunctMaintenance` stands on
`shadow`.

*Considerations:*

1. The goal is to exercise exactly the framework code under test (the delta
   recording in `MysqlDB`, the partition in `Transaction::close`,
   `Conjunct::deltaMaintain`) on real FC5 generics, while FC5's own
   containers, database and working copy stay untouched.
2. A self-contained FC5 image (the road the phase-4 plan first named) was
   considered and set aside: it requires a linux/amd64 build of the
   unreleased delta compiler plus an Angular build, while the shadow run
   drives the API only — the frontend takes no part in transaction
   processing. The mounted worktree gives the same backend code byte for
   byte.
3. Sampling replay pairs from `populations.json` was tried and rejected: the
   ExecEngine rewrites part of the script population at install, so a
   replayed delete can hit a pair that no longer exists (observed as an
   HTTP 500 on the first probe). The exporter reflects the actual state;
   pairs that still drift mid-run are dropped from the stream.
4. `loginEnabled` false lets the replay driver work without SIAM accounts.
   Conjunct maintenance is role-independent, so the shadow comparison keeps
   its meaning; the known artefact is a standing violation of the
   PrototypeContext rule 'Active roles MUST be a subset of allowed roles'.

*Impact on the specification:* none; FC5's model files are read, never
changed.

*Impact in production:* none; every published image keeps the switch on
`off`, and the shadow stack is disposable.

**The RAP validation of issue #1687 runs on two local deployments side by side: RAP as-is from `origin/main` on the v1 toolchain, and RAP from the modernized `feature/interactive-editor` sources on the published compiler v5.9.7 and framework v2.6.0.**
*DC-13 · valid · 2026-08-15 · origin: issue #1687 step 1, [interface-queries.md](interface-queries.md)*

The baseline (`:8081`) is the image the production pipeline would build:
`origin/main` sources, prototype-framework v1.18.1, the framework's bundled
compiler. The current-stack deployment (`:8089`) builds from RAP branch
`incremental-evaluation` — based on `feature/interactive-editor`, whose
sources already satisfy the current names-and-labels syntax — with the
published images `ampersandtarski/ampersand:v5.9.7` and
`prototype-framework:v2.6.0`. Each deployment owns its MariaDB and volumes;
`docker-compose.1687.yml` in the RAP repo holds both.

*Considerations:*

1. The goal is a production-faithful baseline next to a current-stack RAP,
   so that step 2 profiles real behaviour and step 4 can add the
   incremental build as a third, comparable deployment.
2. Modernizing `origin/main` afresh was considered and set aside: `main`
   stops the current compiler at `src/RAP4.adl:196` (`UnexpectedChar '_'`,
   names such as `pf_ifcRoles`), and the interactive-editor branch has this
   conversion already behind it, verified by a clean
   `ampersand proto` run and a working deployment.
3. Building the baseline with the current compiler was rejected for the
   same reason in reverse: the as-is baseline derives its value from being
   the image production runs, old toolchain included.
4. The RAP work lives on its own branch and worktree
   (`~/git/RAP-incremental-evaluation`), so the interactive-editor line and
   its running deployment on `:8088` stay undisturbed.

*Impact on the specification:* none; both deployments compile the RAP4
model as their branches carry it.

*Impact in production:* none; both stacks are local and disposable. For
step 4, the sources of RAP branch `incremental-evaluation` are the ones
the incremental compiler must accept.

## Assurance and publication

**Correctness of the delta calculus rests on our own Isabelle/HOL proofs in `proofs/`, with the Lean formalization of DBSP as inspiration.**
*DC-4 · valid · 2026-08-13 · origin: [dbsp-paper-study.md](dbsp-paper-study.md), proofs/spike/, tchajed/database-stream-processing-theory*

Every delta rule of Phase 1 carries a machine-checked proof in Isabelle/HOL, or
an explicit flag that it does not yet. The proofs build on the existing shallow
embedding of Ampersand's heterogeneous relation algebra
(`proofs/spike/Ampersand_RA.thy`), extended with a Z-set (weighted) semantics.
A claim counts as machine-checked only after we have run the proof ourselves.

*Considerations:*

1. The goal is paper-grade assurance (DC-5): the delta calculus is the
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
*DC-5 · valid · 2026-08-13 · origin: user decision 2026-08-13*

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

- The venue and scope of the article (DC-5): compiler-engineering story,
  formalization story, or both.
- Whether delta maintenance runs per ExecEngine iteration from the start or
  first lands for user transactions only (prototype-runtime-map.md, §8).
