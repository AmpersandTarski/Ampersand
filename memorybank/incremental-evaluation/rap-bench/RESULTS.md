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
