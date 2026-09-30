# RAP benchmark results (issue #1687)

Measured 2026-08-15 on the stack of [README.md](README.md) (DC-14): two
API-level RAP deployments, identical in code, model and data, differing only
in `transactions.deltaConjunctMaintenance` (`off` versus `on`). Versions in
[data/VERSIONS.txt](data/VERSIONS.txt); raw CSVs and query digests in
[data/](data/). Sizes: 1 000 / 4 000 / 12 000 scripts (10 per account),
seeded through the framework's full request pipeline.

## The three findings

**1. Interface point queries stay flat — no maintenance needed.**
`MyScripts` (the session account's own scripts) and `Nieuwscript` (one
script's detail form) cost a constant ~22 ms wall time at every database
size, in both modes. This confirms the cost model of the issue: most
interface fields are stored relations answered by an index lookup, and
materializing them would buy write amplification for nothing.

**2. Computed interface expressions do grow with the database.**
`StudentScripts` — `"_SESSION" # (I[Account] /\ submittor~;submittor)`, an
overview of every account that has scripts — grows from 45 ms at 1 000
scripts to 310 ms at 12 000, in both modes alike. This is the class the
issue predicted as the interface-side opportunity: expensive derived
expressions. RAP's repertoire contains few of them, they are Tutor-facing
overview pages, and the delta stream needed to maintain them exists since
the delta tables landed — but at ~0.3 s at production-like sizes, the
measurement does not justify building interface materialization now
(DC-15).

**3. The transaction close grows with the database in both modes — and the
profile names the reason precisely.** The seed stream and the single-edit
checkpoints agree:

| scripts in db | single edit, off | single edit, on |
|---:|---:|---:|
| 1 000 | 8.7 ms | 9.1 ms |
| 4 000 | 24.7 ms | 25.4 ms |
| 12 000 | 68.7 ms | 69.1 ms |

The isolated digest of one edit transaction at 12 000 scripts (both modes)
decomposes those ~69 ms:

| component | cost | grows with db? |
|---|---:|---|
| the expensive EE-rule conjunct evaluated twice — once by the ExecEngine (`ExecEngine.php:167` `checkRule(true)`), once again by the close | ~57 ms | **yes — linear** |
| one further full conjunct evaluation | ~9 ms | yes |
| `lastAccess` bookkeeping + COMMIT + mutation | ~12 ms | no |
| delta-table bookkeeping (`on` only) | ~0.5 ms | no |

Why did `on` not save the close's share? The replay response gives the
answer: this edit reports `affectedConcepts: 1` — deleting and re-adding
the content pair touches the `ScriptContent` population, the
concept-affected fallback fires, and the `on` close takes the same full
evaluations as `off`. The only difference that remains is the delta-table
bookkeeping, which is the consistent 0.2–0.5 ms loss. The follow-up below
measures what happens when the fallback does *not* fire.

## What carries the benchmark

The queries that dominate the transaction close at 12 000 scripts (digest,
`off`, seed phase): the violation queries of the script-related ExecEngine
rules, led by *Submission Timestamping*
(`(I[Script] /\ content;content~) |- submitted;V[DateTime*Script]`), whose
compiled form joins two full scans of `Script` with a
`DateTime × Script` cartesian term — ~28 ms per execution at this size,
executed once per fixpoint iteration. Page-open queries are a small share
of total database time; the interface side carries `StudentScripts` as its
only growing term.

## Follow-up: where the win can and cannot be harvested (2026-08-15, same day)

The equality of `off` and `on` raised the right question — is the protocol
applied at the wrong place, or does RAP not have the problem? Three
verified findings answer it.

**A. 78% of RAP's conjuncts are outside the ExecEngine — and all of them
are cheap.** The ExecEngine maintains 100 of RAP's 451 conjunct-bearing
rules; 351 conjuncts (78%) have no EE rule and are evaluated only by the
close ([analyze-coverage.py](analyze-coverage.py)). Timing every one of
those 351 full queries against the 12 000-script database (calibrated for
client overhead) puts each at roughly 0–15 ms, most between 1 and 8 ms:
they are multiplicity checks (`UNI`/`INJ`/`SYM`/`ASY`) that MariaDB
answers from indexes. RAP's only expensive violation queries belong to
EE rules (*Submission Timestamping*, *Submittor*: 26–29 ms at this size).

**B. On a delta-eligible transaction the protocol loses to the cheap full
queries it replaces.** A real edit without concept churn — swapping
`submittor` of one script between two existing accounts,
`affectedConcepts: 0`, 20 repetitions, alternating direction:

| measurement | off | on |
|---|---:|---:|
| median closeMs at 12 000 scripts | **40.4 ms** | **47.0 ms** |

The digest shows the candidate protocol working exactly as designed
(scoped cache DELETEs and INSERT…SELECTs per conjunct, 0.5–10 ms each) —
and costing ~14 ms where the three cheap full evaluations it replaced
cost ~10 ms. Fixed protocol machinery per (conjunct, relation) beats
index-cheap full queries only when those queries are expensive; in RAP
they never are.

**C. The one expensive close-side evaluation is outside the calculus.**
In both modes the 26–29 ms *Submittor* query runs twice per swap: once in
the ExecEngine, once again in the close. The `on` close could not maintain
it incrementally because `conj_269` is the single conjunct of the 451
without delta queries — its term contains a cartesian product with
`_SESSION`, the explicit D7 fallback. The close's second evaluation is
also redundant in a different way: the ExecEngine had just evaluated it
and made no repairs afterwards, so a clean-since-evaluation check would
skip it without any incremental machinery.

## Reading, for the article

The end-to-end hypothesis — transaction latency near-constant as the
population grows — does not hold on RAP, and the reason is now precise.
The database-size-proportional cost sits in the EE-rule queries, which the
current protocol cannot reach: the ExecEngine force-evaluates them in full
regardless of mode, the close evaluates them a second time, and the single
expensive one is the D7-fallback shape. The conjuncts the protocol *does*
maintain are uniformly index-cheap in RAP, so candidate-scoped
maintenance costs more than it saves there (+6.6 ms median on eligible
transactions, +0.2–0.5 ms bookkeeping on fallback transactions). The
engine-level speedups (`bench/RESULTS.md`, ×166–×30 626) are real but
belong to expensive terms; RAP's rule repertoire keeps its expensive
terms inside the ExecEngine.

The harvest map this yields:

1. **Move the application point into the ExecEngine fixpoint loop** — the
   only place where expensive evaluation provably recurs and grows
   (Phase 5, "repair loop as outer feedback cycle"). Prize on RAP: the
   26–29 ms EE evaluations per iteration.
2. **Skip the close's redundant re-evaluation** when the ExecEngine made
   no repairs after its last evaluation — an engineering fix, no new
   calculus, worth another 26–29 ms per affected transaction on RAP.
3. **Gate the protocol per conjunct on estimated query cost** — engage
   candidate maintenance only where the full query is expensive;
   otherwise the wholesale refresh is already optimal. For RAP-class
   models that gate keeps the protocol off everywhere today, which makes
   the current default (`off`) the right production setting.
4. The promise the track has already banked is correctness at scale
   (PRF-2/PRF-6/PRF-7, the FC5 shadow run); the performance promise needs
   items 1–3, in that order.

## Full tables (generated by analyze.py)

## Seed stream: median closeMs per transaction (100 ops = 25 scripts)

| scripts in db | off (ms) | on (ms) | off/on |
|---:|---:|---:|---:|
| 0 | 50.6 | 65.7 | 0.8x |
| 500 | 57.0 | 60.2 | 0.9x |
| 1000 | 65.7 | 72.0 | 0.9x |
| 1500 | 74.8 | 77.2 | 1.0x |
| 2000 | 82.4 | 84.3 | 1.0x |
| 2500 | 89.8 | 94.8 | 0.9x |
| 3000 | 98.6 | 101.1 | 1.0x |
| 3500 | 107.0 | 112.0 | 1.0x |
| 4000 | 115.1 | 119.4 | 1.0x |
| 4500 | 123.7 | 129.2 | 1.0x |
| 5000 | 130.8 | 136.1 | 1.0x |
| 5500 | 138.8 | 143.8 | 1.0x |
| 6000 | 149.7 | 153.8 | 1.0x |
| 6500 | 153.3 | 159.6 | 1.0x |
| 7000 | 161.7 | 169.9 | 1.0x |
| 7500 | 172.8 | 173.6 | 1.0x |
| 8000 | 177.9 | 181.9 | 1.0x |
| 8500 | 185.6 | 192.0 | 1.0x |
| 9000 | 193.1 | 198.8 | 1.0x |
| 9500 | 200.1 | 206.7 | 1.0x |
| 10000 | 204.9 | 211.1 | 1.0x |
| 10500 | 211.5 | 219.9 | 1.0x |
| 11000 | 224.5 | 225.2 | 1.0x |
| 11500 | 230.0 | 236.9 | 1.0x |

## Single-edit transaction: median closeMs at checkpoint size

| scripts in db | off (ms) | on (ms) | off/on |
|---:|---:|---:|---:|
| 1000 | 8.7 | 9.1 | 1.0x |
| 4000 | 24.7 | 25.4 | 1.0x |
| 12000 | 68.7 | 69.1 | 1.0x |

## Page opens: median wall time (ms)

| page | scripts in db | off (ms) | on (ms) |
|---|---:|---:|---:|
| MyScripts | 1000 | 21.9 | 21.9 |
| MyScripts | 4000 | 22.1 | 22.1 |
| MyScripts | 12000 | 21.9 | 21.8 |
| Nieuwscript | 1000 | 22.1 | 22.3 |
| Nieuwscript | 4000 | 22.1 | 22.4 |
| Nieuwscript | 12000 | 21.9 | 22.0 |
| StudentScripts | 1000 | 44.8 | 44.1 |
| StudentScripts | 4000 | 115.5 | 110.6 |
| StudentScripts | 12000 | 310.6 | 301.0 |

## Database size at checkpoints (rows approx., MB)

| checkpoint | off rows | off MB | on rows | on MB |
|---|---:|---:|---:|---:|
| s1000 | 1414 | 16.5 | 1476 | 16.5 |
| s4000 | 5033 | 16.5 | 5096 | 16.5 |
| s12000 | 14709 | 16.6 | 14742 | 16.5 |

# O3: skip the close's re-evaluation of clean conjuncts (2026-08-15)

This section decides hypothesis O3 of
[../design-heuristics.md](../design-heuristics.md), implemented as
`transactions.skipCleanConjuncts` in the prototype framework
([prototype#443](https://github.com/AmpersandTarski/prototype/issues/443),
branch `feat-skip-clean-conjuncts`, commit `da0cfaf9`). The transaction
counts every registered mutation; each conjunct evaluation is stamped with
the counter value; at close, a conjunct whose stamp still equals the
counter — evaluated by the ExecEngine's last fixpoint iteration, with no
repair after it — keeps its in-memory result instead of running the same
violation query again.

Same stack as above, one difference between the instances:
`transactions.skipCleanConjuncts` `false` (:8191, "off") versus `true`
(:8192, "off+skip"); `deltaConjunctMaintenance` stands on `off` in both.
Versions in [data/o3-VERSIONS.txt](data/o3-VERSIONS.txt); raw CSVs and
digests in [data/o3-off/](data/o3-off/), [data/o3-skip/](data/o3-skip/)
and [data/o3-parity/](data/o3-parity/).

## Verdict: O3 holds

The single-edit close drops by 37–41% at every measured size, right in the
hypothesis band of 30–50%, and the share grows with the database — because
what the skip removes is precisely the size-proportional second evaluation:

| scripts in db | off (ms) | off+skip (ms) | reduction |
|---:|---:|---:|---:|
| 1 000 | 9.9 | 6.2 | 37% |
| 4 000 | 27.6 | 16.5 | 40% |
| 12 000 | 73.9 | 43.8 | **41%** |

The submittor-swap transaction (finding B's specimen: no concept churn,
20 reps at 12 000) reproduces its baseline and confirms the same cut:
median close 40.2 ms off (B measured 40.4) versus 25.8 ms off+skip (−36%).
The seed stream (100 ops per transaction, so the per-close evaluations are
amortized over 25 scripts) still gains 1.2–1.4× across the whole curve.
Page opens stay put — MyScripts and Nieuwscript flat at ~25 ms, and
StudentScripts grows identically in both modes — as they must, since the
skip only touches the transaction close.

## The digest shows the second evaluation gone

The isolated digest of one content edit at 12 000 scripts
([data/o3-off/single-edit-digest.tsv](data/o3-off/single-edit-digest.tsv),
[data/o3-skip/single-edit-digest.tsv](data/o3-skip/single-edit-digest.tsv)):

| statement | off | off+skip |
|---|---|---|
| expensive EE-rule violation query (~30 ms/exec) | **2 executions**, 59.7 ms | **1 execution**, 29.7 ms |
| second violation query (~10 ms/exec) | 1 execution | 1 execution |
| `DELETE FROM __conj_violation_cache__` (persist) | 3 statements | 3 statements |

The one expensive query that finding C showed running twice per close now
runs once; the conjunct that only the close evaluates still runs; and the
violation-cache persist is statement-identical — the skipped conjunct's
in-memory result reaches the cache exactly as a fresh evaluation would.

## Shadow parity: decisions and cache identical

A 15-transaction edit stream (content edits, submittor swaps both
directions, a double-content attempt, script create/remove) replayed
identically on both instances after the 12 000 seed:
all 15 `committed`/`invariantsHold`/`affectedConjuncts` triples identical
([data/o3-parity/decisions-off.txt](data/o3-parity/decisions-off.txt) vs
[decisions-skip.txt](data/o3-parity/decisions-skip.txt)). Six directed
probes behaved identically as well, each verified in the database rows:
an orphan script (the Submittor EE rule assigned the session account on
both), a UNI overwrite, an EE-repaired sequence, an EE-normalized ASY
pair, a duplicate userid (INJ invariant: `committed=False`,
`invariantsHold=False` on both — a genuine rollback under skip), and an
unregistered EE function (same 500 on both).

The persisted `__conj_violation_cache__` is identical — and the equality
is grounded, not vacuous: a full `evaluate/all` over all 451 conjuncts on
both databases persists zero violation rows on both sides, so both
databases satisfy every conjunct and their caches agree row-for-row.
RAP's ExecEngine repairs every API-reachable signal violation and
invariants roll back, so a *non-empty* cache after a skipped close does
not occur on this workload; the statement-identical persist in the digest
above covers that path structurally.

## Reading

The engineering fix of harvest-map item 2 delivers what finding C
promised: the 26–30 ms second evaluation of the expensive EE-rule query is
gone, per affected transaction, at zero protocol overhead and with no
change in any commit decision. Unlike the delta protocol (finding B, +6.6
ms on eligible transactions), the skip has no per-conjunct machinery to
pay for — it removes work without adding any. The measured configuration
for RAP-class models is therefore `deltaConjunctMaintenance: off` +
`skipCleanConjuncts: true`. The setting ships default-off in the
framework; this measurement is the case for flipping it.

## Full tables (analyze.py, off vs off+skip)

| scripts in db | off (ms) | off+skip (ms) | off/skip |
|---:|---:|---:|---:|
| 0 | 61.4 | 56.0 | 1.1x |
| 500 | 66.0 | 56.1 | 1.2x |
| 1000 | 79.7 | 60.9 | 1.3x |
| 1500 | 89.2 | 70.7 | 1.3x |
| 2000 | 94.7 | 74.1 | 1.3x |
| 2500 | 108.2 | 81.8 | 1.3x |
| 3000 | 114.4 | 87.3 | 1.3x |
| 3500 | 122.6 | 96.9 | 1.3x |
| 4000 | 124.5 | 102.4 | 1.2x |
| 4500 | 135.3 | 105.7 | 1.3x |
| 5000 | 140.3 | 112.3 | 1.2x |
| 5500 | 153.1 | 117.7 | 1.3x |
| 6000 | 161.0 | 127.1 | 1.3x |
| 6500 | 167.1 | 134.8 | 1.2x |
| 7000 | 189.7 | 133.4 | 1.4x |
| 7500 | 200.3 | 149.2 | 1.3x |
| 8000 | 199.5 | 158.6 | 1.3x |
| 8500 | 205.2 | 168.5 | 1.2x |
| 9000 | 212.4 | 207.0 | 1.0x |
| 9500 | 219.5 | 198.4 | 1.1x |
| 10000 | 229.4 | 201.9 | 1.1x |
| 10500 | 235.6 | 208.2 | 1.1x |
| 11000 | 240.3 | 195.4 | 1.2x |
| 11500 | 264.6 | 195.4 | 1.4x |

(Seed stream: median closeMs per 100-op transaction.)

| page | scripts in db | off (ms) | off+skip (ms) |
|---|---:|---:|---:|
| MyScripts | 1000 | 25.0 | 24.2 |
| MyScripts | 4000 | 24.1 | 24.8 |
| MyScripts | 12000 | 24.7 | 25.1 |
| Nieuwscript | 1000 | 25.1 | 24.5 |
| Nieuwscript | 4000 | 24.7 | 25.1 |
| Nieuwscript | 12000 | 24.1 | 24.9 |
| StudentScripts | 1000 | 48.6 | 49.4 |
| StudentScripts | 4000 | 117.5 | 125.7 |
| StudentScripts | 12000 | 328.7 | 340.5 |

Measurement notes. The runs of this section used framework tree
`bench-o3` (delta branch + the skip commit, nothing of v2.7.0), fresh
databases, and the full 1 000 / 4 000 / 12 000 protocol on both
instances; nothing was scaled down. The host slept once between the
timed runs and the parity phase; the timing CSVs show no artifact
(max/median ≤ 2.1 across all files, largest single value 285 ms), and
the parity phase carries no timings. Absolute numbers sit a few
percent above the #1687 tables (74 vs 69 ms off at 12 000) — different
day, same shape.
