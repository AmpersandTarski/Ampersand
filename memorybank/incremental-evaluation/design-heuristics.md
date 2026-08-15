# Design heuristics and research hypotheses from the RAP validation

Status: proposed 2026-08-15, from the measurements of issue
[#1687](https://github.com/AmpersandTarski/Ampersand/issues/1687)
([rap-bench/RESULTS.md](rap-bench/RESULTS.md)) and the engine-level
benchmark ([bench/RESULTS.md](bench/RESULTS.md)). The two together nuance
the reasoning "incremental beats integral": the asymptotics of DBSP are
real, but constants decide where they bite. This note generalizes the
findings into design heuristics for information systems and phrases each
as a testable hypothesis. Article material (DC-5): this is the discussion
section that keeps the speedup story honest.

## The economics in one line

Incremental maintenance of a derived result pays exactly when

> (cost of one full re-evaluation − cost of one incremental update) ×
> (number of re-evaluations avoided) > fixed protocol cost per transaction.

The measured constants on RAP at 12 000 scripts: an index-answerable full
violation query costs 1–8 ms; the candidate protocol's fixed machinery
costs ~5–15 ms per transaction; an expensive global term costs 26–29 ms
and grows linearly. The formula then sorts every query into "never
incrementalize" (the cheap majority), "always" (the expensive growing
minority), and "depends on write rate" (the middle, empty on RAP).

## Heuristics

**H1 — Profile before you incrementalize; the profitable set is small and
identifiable.** On RAP, 78% of conjuncts are index-cheap multiplicity
checks and can never repay a per-transaction protocol. The win
concentrates in the few terms whose cost grows with the database: global
quantification (`V`, `#`), compositions spanning whole relations,
closures. Grounding: rap-bench follow-up findings A and B.

**H2 — The language shape predicts the cost shape.** In a constraint
language, locally-shaped constraints (UNI, INJ, SYM, ASY — the bulk of
any model) compile to index lookups; the expensive terms are syntactically
recognizable. A compiler can therefore classify at generation time which
queries are candidates for incremental maintenance, without measuring.
Grounding: every one of RAP's 351 non-EE conjuncts timed cheap, and both
expensive ones contain a cartesian term.

**H3 — Deduplicate before you incrementalize.** RAP evaluates its
expensive rule queries twice per transaction (repair engine + close).
Removing a redundant evaluation saves a full query cost with zero new
machinery and zero proof burden; incrementalizing a duplicated evaluation
optimizes the wrong factor. In any pipeline: first make evaluation happen
once, then make it incremental. Grounding: follow-up finding C.

**H4 — Incrementality belongs inside loops, not behind them.** A repair
loop (ExecEngine, triggers, sagas) re-evaluates by construction, and each
iteration's own repairs are small deltas — recurrence and small change
are structural there. A once-per-transaction end check re-evaluates only
once, so there is at most one evaluation to save. Apply incremental
evaluation at the point of structural recurrence. Grounding: the harvest
map — the only linearly-growing recurring cost on RAP sits in the EE
fixpoint loop.

**H5 — Cover entity churn or cover little.** Real workloads create and
destroy entities constantly (every RAP script submission). An incremental
path that falls back on population change serves only the rare
mutation-in-place. The delta representation must carry atom/entity
deltas as first-class citizens — the theory does (the P-obligations);
the protocol must follow. Grounding: the seed stream ran at off/on parity
because every batch created atoms.

**H6 — Materialize on read economics, not on principle.** A view earns
its maintenance when read-frequency × saved-read-cost exceeds
write-frequency × maintenance-cost. Ampersand's violation cache passes
(read at every commit decision, kept by an already-scoped refresh);
interface materialization fails on both factors (rare reads, cheap
queries). Grounding: DC-15 and the flat 22 ms page opens.

**H7 — Delta streams are a capability, speed is a bonus.** Even where
incremental evaluation loses on latency, the delta stream enables what
recomputation cannot: push to open pages, subscriptions, audit,
replication. Justify delta infrastructure by the capability it unlocks;
harvest latency only where H1's profile says so. Grounding: the push
track of interface-queries.md, still open, unaffected by the latency
results.

## The hypotheses, phrased for further research

Each hypothesis is falsifiable with instruments that already exist
(rap-bench, incremental-bench, the model corpus in `testing/`).

- **O1 (cheap majority).** In production-scale Ampersand models, at least
  three quarters of conjunct queries are index-answerable, and their full
  evaluation is cheaper at realistic volumes than any per-transaction
  incremental bookkeeping. *Test:* run the analyze-coverage classifier
  plus calibrated timing over a corpus (RAP, FC5, RVB-class models,
  `testing/` with populations).
- **O2 (static predictability).** A classifier on term shape alone
  (cartesian/`V`/closure/composition-length versus local multiplicity)
  predicts which violation queries grow with database size, with
  precision and recall above 90%. *Test:* classifier output against
  measured scaling curves per conjunct on the same corpus.
- **O3 (deduplication dividend).** Skipping the close's re-evaluation
  when the repair engine made no repairs after its last evaluation
  reduces median transaction latency on RAP-class workloads by 30–50% at
  production volume, with no change in commit decisions. *Test:* the
  one-line pipeline change behind a switch, replayed shadow-style like
  the FC5 run, then rap-bench off/off+skip.
- **O4 (loop incrementality).** Feeding the repair engine's fixpoint from
  delta-maintained state makes transaction cost independent of database
  size for transactions with bounded repair sets. *Test:* Phase-5
  implementation, then the unchanged rap-bench harness; the hypothesis
  predicts the seed curve flattens.
- **O5 (churn dominance).** In production transaction logs, the majority
  of transactions touch at least one concept population, so incremental
  coverage without population deltas reaches a minority of transactions.
  *Test:* count `affectedConcepts > 0` over an FC5 or RAP production
  replay stream.
- **O6 (interface economics).** Pages whose read-frequency × query-cost
  clears the maintenance threshold are rare in deployed applications;
  concretely, on production RAP no interface query except computed
  overview pages would repay materialization. *Test:* access-log
  frequencies × measured query costs on production RAP.
- **O7 (gated engine).** A per-conjunct cost gate — candidate maintenance
  only for queries the H2-classifier marks expensive, wholesale refresh
  otherwise — is never slower than either pure mode on any model in the
  corpus. *Test:* implement the gate, run rap-bench and incremental-bench
  across the corpus; the hypothesis fails if any model shows a regression
  against its best pure mode.

## Interfaces as materialized views (added 2026-08-15)

Interfaces are not merely *like* views — every interface field expression
is a relation-algebra term, hence a view definition. What distinguishes
them from the conjunct views is parameterization: the compiler bakes a
placeholder for the source atom into every interface query
(`broadQueryWithPlaceholder`). That splits the repertoire into three
classes with three different answers:

1. **Parameterized point queries** (`MyScripts`, detail forms) — a view
   indexed by one atom, answered by an index lookup. Measured flat at
   ~22 ms; materialization can only lose (DC-15 stands).
2. **Global overviews** (`StudentScripts`-class) — the head
   (`"_SESSION" # …`) only gates access; the body
   (`I[Account] /\ submittor~;submittor` with its subtree) is
   session-free and global. This is an unparameterized view in disguise,
   and the class where H6's gate can flip: the read costs 310 ms at
   12 000 scripts and grows, while maintaining the body under a script
   submission is a few scoped rows. Materialization turns a growing
   user-visible latency into a flat read plus a bounded per-transaction
   write. The H2 classifier recognizes the class at compile time — the
   same global-term shapes that mark expensive conjuncts mark expensive
   interface bodies.
3. **Parameterized expensive subtrees** (per-atom closures, atlas-style
   context views) — a family of views, one per atom, too many to
   materialize eagerly. The fitting shape is partial materialization
   (Noria's "partial state"): materialize per atom on first open,
   maintain while open, evict later. Real machinery: on-demand backfill
   and eviction. Defer until class 2 has proven itself.

The change-stream side is already unified, which is what makes class 2
cheap to build: every edit — interactive field edits and the batched
commits of transactional interfaces alike — flows through
`Relation::addLink/deleteLink`, exactly where the delta tables record;
ExecEngine repairs travel the same road. One recorded change stream can
therefore feed two consumers with the same candidate machinery: the rule
caches (built) and materialized interface bodies (class 2, proposed). The
same materialized body plus its delta is also precisely the payload the
push track needs: the delta of an open overview, filtered per subscriber,
is the patch to push. Materialized overviews are the stepping stone from
the performance track to the push track, not a detour.

Two hypotheses extend the research program:

- **O8 (overview materialization).** For interface bodies that the
  H2-classifier marks expensive and session-free, incremental
  materialization beats recomputation already at modest read rates:
  maintenance stays within a few ms per touching transaction while the
  saved read grows with volume. *Test:* materialize the `StudentScripts`
  body on the rap-bench stack as a shadow table maintained by candidate
  queries; measure page-open latency and per-transaction overhead across
  the three sizes; find the break-even read/write ratio.
- **O9 (partial materialization).** For parameterized expensive subtrees,
  per-atom partial materialization with on-demand backfill bounds both
  storage and maintenance to the working set of open atoms, at eviction
  complexity that a prototype framework can carry. *Test:* only after O8;
  prototype on an atlas-style view over compiled scripts.

## Relation to the literature

The nuance is not new, but it is rarely quantified at the language level:
Gupta & Mumick already catalogue when view maintenance loses to
recomputation; Noria's partial state and SQL Server's indexed-view
guidance both encode read-economics gates; the DBSP paper proves the
asymptotic transformation but leaves the constants to the engine. What
the Ampersand setting adds is H2: a *constraint language* whose term
shapes classify the profitable set at compile time — the gate can be
static where databases need runtime statistics. That is the claim worth
carrying into the article.
