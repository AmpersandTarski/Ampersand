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
*Refined by the #1690 corpus study:* the shape predicts the growth
**exponent** (anchored probe, linear scan, product, recursion), not the
threshold crossing — population size decides when a linear scan outgrows
the protocol fee, and size is runtime knowledge. The workable form is a
static scan profile judged against live table sizes (DC-17;
[cost-gate/RESULTS.md](cost-gate/RESULTS.md), finding 3).

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
  *Outcome (2026-08-15): refuted as stated* — shape alone reached 21.7%
  precision at 62.5% recall on the eight-model corpus. The refined,
  confirmed form: shape fixes the growth exponent, and a static scan
  profile combined with live table sizes reaches 100% recall at 98.6%
  specificity, with every false positive within the bounded-damage band
  ([cost-gate/RESULTS.md](cost-gate/RESULTS.md)).
- **O3 (deduplication dividend).** Skipping the close's re-evaluation
  when the repair engine made no repairs after its last evaluation
  reduces median transaction latency on RAP-class workloads by 30–50% at
  production volume, with no change in commit decisions. *Test:* the
  one-line pipeline change behind a switch, replayed shadow-style like
  the FC5 run, then rap-bench off/off+skip. **Confirmed 2026-08-15:**
  −37/−40/−41% median single-edit close at 1 000/4 000/12 000 scripts,
  identical commit decisions and violation cache on a replayed stream
  (rap-bench `data/o3-*`, [prototype#443](https://github.com/AmpersandTarski/prototype/issues/443),
  framework branch `feat-skip-clean-conjuncts`).
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
  against its best pure mode. *Research line:* O2 and O7 together, plus
  the backfill/maintenance split, are issue
  [#1690](https://github.com/AmpersandTarski/Ampersand/issues/1690)
  (compile-time cost gate). *Status (2026-08-15):* the research is
  closed. The gate's form is DC-16 (case table) and DC-17 (cost-profile
  contract); dominance holds pointwise on the corpus measurements, and
  the end-to-end gated run moves to the implementation issue.

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
cheap to build: every edit — interactive field edits, the dry-run replays
and the final commit of a transactional interface alike — flows through
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

## Transactional interfaces are a recurrence site (added 2026-08-17)

A `TRANSACTIONAL INTERFACE` (Ampersand issue #1658) reads as a series of
edits that reach the database as one change. That is the commit
semantics, and it holds. The evaluation cadence is the opposite, and that
is what decides the cost.

The mechanics, verified in the framework at v2.7.0 (`eb6906fa`). The
frontend buffers each edit client-side and, on **every** buffered edit,
re-sends the **whole buffer so far** as a PATCH with `dryRun=true`
(`frontend/src/app/shared/interfacing/ampersand-interface.class.ts:408`
→ `:454` → `:449`); there is no debounce. The backend applies those ops
for real inside a database transaction, skips the ExecEngine because a
dry run must not fire side effects on uncommitted data
(`backend/src/Ampersand/Controller/ResourceController.php:125`),
evaluates every affected conjunct with its **full** violation query
(`Transaction.php:327-328` → `Conjunct.php:191`), checks the invariants
(`Transaction.php:332`) and rolls back (`Transaction.php:337`). SAVE is
enabled from the answer, and the violation messages behind a disabled
SAVE come from the same response
(`ampersand-interface.class.ts:397,404`). SAVE then replays the same
buffer once more without `dryRun` (`:488`).

So a transactional interface of *n* edits costs *n* dry runs plus one
commit: *n+1* rounds of full conjunct evaluation, over a monotonically
growing affected-conjunct set, with *n(n+1)/2* pair writes replayed and
rolled back. Batching moved the cost from *n* commits to *n+1*
evaluations; it did not remove it. By H4 this is a structural recurrence
— a loop, like the ExecEngine fixpoint — and therefore the second place
in the stack where incremental maintenance has something to earn.

Three things follow for the delta design:

1. **The dry-run round is exactly the delta case.** Each round asks
   "does the base state plus this buffer violate any invariant?" — a
   question the maintained violation table answers from the base state
   plus the buffer's Z-set, without touching database-sized relations.
   Where the affected conjuncts are index-cheap (O1's majority) this
   changes nothing worth measuring; where one of them is a global term,
   today's per-edit feedback grows with the population and the
   maintained form stays flat.
2. **The net delta is smaller than the sum of the edits.** Retyping a
   field, or adding and then removing a link, cancels in a Z-set. Full
   re-evaluation cannot profit from that, since its cost never depended
   on the change; delta evaluation profits from it for free. This is the
   one respect in which "a series of changes as one change" is literally
   true of the cost.
3. **The SAVE gate needs violation *state*, not a violation *delta*.**
   The ExecEngine can be fed new violations only (§4 of
   prototype-runtime-map.md), but the hover text behind a disabled SAVE
   lists every invariant violation that currently blocks the commit. The
   maintained table must therefore carry the complete set. It does — the
   correctness obligation is that the maintenance rides *inside* the
   database transaction, so the rollback at `Transaction.php:337`
   discards the dry run's maintenance along with its writes. Cache rows
   already commit on the same connection just before COMMIT
   (`Transaction.php:364-365`), so the delta tables inherit that
   atomicity as long as they stay on that connection.

- **O10 (transactional feedback).** For a transactional interface whose
  affected conjuncts include an H2-expensive term, delta-maintained
  violation state makes the per-edit SAVE-enabling latency independent
  of database size, where full re-evaluation grows with it; and the
  advantage compounds with buffer length, because the maintained form
  processes the net Z-set of the buffer while re-evaluation repeats the
  whole query. *Test:* a transactional interface over the rap-bench
  model with one global invariant, driven with buffers of 1, 5 and 20
  edits at the three database sizes, off versus on.

### What the user must stop noticing

The felt quantity is the gap between leaving a field and seeing the SAVE
state and the violation text settle. Fields patch on blur
(`BaseAtomicComponent.class.ts:33,117`), so that gap competes with the
user's own glance to the next field: under ~100 ms it never registers as
waiting, and beyond ~1 s it breaks the working rhythm.

That gap has not been measured. rap-bench times GET page opens and commit
closes, never the dry-run cadence, so the figure below is a
reconstruction from the measured decomposition of finding 3, not an
observation. At 12 000 scripts on RAP a round plausibly costs ~20 ms of
HTTP-plus-PHP floor (the flat 22 ms page opens), ~12 ms of transaction
machinery (the measured non-growing share: writes, bookkeeping,
rollback), and one evaluation of the expensive term at ~28 ms plus ~9 ms
for the second — the ExecEngine's duplicate is absent because a dry run
skips it. Of that ~70 ms, roughly half grows linearly with the
population. Measuring it is O10's test and the precondition for
everything below.

Four interventions, in order of yield per unit of work. They are
independent, and only the last one belongs to this research line.

1. **Take the round off the critical path.** The verdict is only
   *needed* at SAVE, where the server decides anyway and reports "not
   saved" on rollback. Keeping SAVE clickable and letting the advisory
   catch up turns *n+1* rounds into one and removes the felt wait
   entirely, at the price of later feedback. Whether that price is
   acceptable is a design question about the interface, not about
   evaluation cost.
2. **Coalesce and cancel the rounds that remain.** `runValidation`
   (`ampersand-interface.class.ts:454`) starts a fresh `forkJoin` per
   edit and cancels nothing, so the verdict that *arrives* last wins
   rather than the one that was *sent* last. A slow server — that is,
   a large database — makes it likelier that a stale "holds" overwrites
   a fresh violation. Debounce plus `switchMap` semantics fixes the
   ordering and cuts the load in the same change.
3. **Evaluate only what the newest edit affects.** A round re-evaluates
   every conjunct affected by the whole replayed buffer, while only the
   newest op can change a verdict; carrying per-conjunct verdicts across
   rounds bounds the work to that op. This is H3 applied to the
   dry-run cadence, and it needs no delta calculus.
4. **Make the remaining round independent of database size.** Only
   delta-maintained violation state removes the growing half. It cannot
   touch the other half: the ~32 ms of request floor and transaction
   machinery survives any query optimisation. So incremental evaluation
   buys back the headroom under a 100 ms budget and keeps it as the
   population grows; reaching well below that floor needs intervention 1,
   or checks the browser can decide alone — which covers field format
   and mandatory-field constraints, but not multiplicity over a whole
   relation, since the browser does not hold the relation.

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
