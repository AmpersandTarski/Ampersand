# Cost-gate corpus study (issue #1690)

Measured 2026-08-15. The question under test is R1 of
[issue #1690](https://github.com/AmpersandTarski/Ampersand/issues/1690):
can the compiler see, per violation query, whether incremental evaluation
pays? The instruments in this folder produced every number below; the raw
CSVs stand in [data/](data/).

## Corpus and method

Every conjunct violation query of eight models ran at two database sizes,
timed server-side (`SHOW PROFILES`, query cache off, warm-up run
discarded). The two sizes are the model's own population and an inflated
copy in which every user table holds k copies of its rows, with per-copy
suffixes so join selectivity scales linearly ([inflate.py](inflate.py)).
RAP and FC5 come from dumps of the running #1687 and shadow stacks; the
`testing/` models get their population through `ampersand validate`, which
installs it before term validation starts ([corpus.sh](corpus.sh)).

| model | conjuncts | factor k | largest table at k | median ms | p95 ms | max ms |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| RAP | 451 | 8 | 96 000 (Script) | 0.18 | 5.7 | 218 |
| FC5 | 626 | 20 | 1 720 (tekst) | 0.20 | 0.5 | 4 |
| Kernmodel | 74 | 32 | 576 | 0.16 | 1.1 | 6 |
| CombinedOperators | 59 | 32 | 96 | 0.17 | 5.6 | 4 269 |
| Mandatering | 58 | 32 | 224 | 0.16 | 1.1 | 6 |
| CartesianTest | 54 | 32 | 32 | 0.17 | 0.3 | 1 |
| ChainShortcut | 62 | 32 | 160 | 0.16 | 11.5 | 3 × >25 000 |

Two models of the original selection drop out, stated here so the corpus
has no silent gaps: Arbeidsduur.adl (a pre-existing compiler fatal in
`FSpecAux.hs:38`) and Delivery.adl (`ampersand validate` refuses to build a
database from a population that violates invariants, also with
`--ignore-invariant-violations`).

A query is *measured expensive* when it costs at least 20 ms at the
inflated size — above the 5–15 ms fixed per-transaction cost of the
candidate protocol (#1687), so incremental maintenance pays — or when it
hits the 25 s statement cap.

## Finding 1 — the expensive class exists outside the ExecEngine

RAP at 96 000 scripts has exactly four expensive conjuncts:

| conjunct | rule | ms at 12k → 96k | shape |
| --- | --- | ---: | --- |
| conj_436 | Submission Timestamping (EE) | 24.9 → 218.3 | cartesian `V[DateTime*Script]` |
| conj_127 | UNI `content[Script*ScriptContent]` | 8.4 → 70.4 | linear full scan |
| conj_249 | UNI `submitted[Script*DateTime]` | 7.4 → 64.4 | linear full scan |
| conj_250 | UNI `submittor[Script*Account]` | 7.4 → 62.8 | linear full scan |

Three of the four are *not* ExecEngine rules: they are UNI property checks
whose compiled query scans the wide `Script` table (a `mediumtext` column
among them) once per evaluation. Decision rule 4 of the solution comment
("all expensive queries belong to EE rules → fold the gate into Phase 5")
therefore does **not** fire: the commit-time refresh has its own prize.

The same three queries also carry a sharper observation: `content`,
`submitted` and `submittor` are stored as columns of the `Script` table,
so the relation is UNI *by construction* and the violation set is empty in
every reachable database state. The optimal route for this class is no
query at all. The route vocabulary of the gate needs that third value
(*structural*) next to integral and incremental; the follow-up issue
carries the proof obligation for it.

## Finding 2 — Kleene closures explode at toy sizes

ChainShortcut's `plusUnfoldRight`, `plusUnfoldLeft` and `starLaw` run in
0.5–0.8 ms on 5 nodes and blow through the 25-second statement cap on 160
nodes; CombinedOperators' `unionRedIsBase` grows 0.5 → 4 269 ms over the
same ×32. Neighbouring conjuncts of the same shape on the same 160-row
tables stay at 3–15 ms. Two consequences. The Kleene exponent is real and
bites at sizes three orders of magnitude below RAP's; and within the
Kleene class the spread at one size is ×8 000, so no threshold separates
them — the gate must treat every Kleene term as expensive. The loss is
asymmetric in exactly the right direction: wrongly incremental costs the
bounded protocol fee, wrongly integral costs seconds.

## Finding 3 — shape fixes the exponent, size fixes the crossing

Below the four expensive RAP conjuncts sits a continuous band: the
UNI/INJ/SYM/ASY checks on the 9 600-row Account/Userid tables measure
5–15 ms at k, growing linearly like conj_127 — they simply scan a smaller
table. Essentially every violation query is at least linear in the table
it scans; what the shape decides is the exponent (anchored probe, linear
scan, product, recursion), and what the population decides is when that
curve crosses the protocol cost. A classifier on shape alone can therefore
not reach the 90 % target in both directions. Measured on the corpus:

| classifier | precision | recall | specificity |
| --- | ---: | ---: | ---: |
| v1 — shape only (Kleene, user-cartesian, composition ≥ 4) | 21.7 % | 62.5 % | 98.6 % |
| v2 — shape → scan profile, judged against live table sizes | 29.6 % | **100 %** | 98.6 % |

v2 is the two-stage form the data forces: the compiler derives statically
*which tables the query must read in full* (its scan profile — here
approximated from the generated SQL; the real classifier walks the
`Expression`), and the runtime compares that profile against the table
sizes it already has. On the corpus v2 misses nothing; its false
positives are boundary cases (conj_121 at 15.4 ms, the Kleene cluster at
3–15 ms) where either route sits within ~10 ms of optimal — the bounded
damage that criterion W2 demands. Precision is the wrong yardstick for
the gate; expected regret is, and v2's regret on the corpus is bounded by
the protocol fee while v1's grows with the database (60–70 ms per missed
conjunct per transaction on RAP today).

## Finding 4 — measured cost is environment-sensitive

Three measurement artifacts, kept here because they carry design weight.
Client wall time through `docker exec` carries 10–75 ms of overhead that
drifts with host load (`SELECT 1` itself measured 60 ms under a
concurrent run), which forced the switch to server-side timing. A
`COUNT(*)` wrapper was tried and abandoned: it hands the optimizer a
different statement than the framework runs. And the same conj_269
(*Submittor*) that the #1687 digest showed at 26–29 ms on the organically
grown bench database runs in 0.3 ms on a freshly restored copy of that
same database — restore rebuilds indexes and statistics, and the plan
flips. Any gate that reasons from measured timings inherits this
volatility; a gate that reasons from table sizes does not. This is the
strongest argument the study found for putting the runtime half of the
decision on sizes (v2) rather than on timing bookkeeping (direction C).

## Interface hook (plan step 5)

[interface-hook.py](interface-hook.py) sorts interface objects by the
same logic: RAP has 235 objects — 203 point queries (anchored on a user
atom), 25 session-rooted (anchored only on the session atom, the
StudentScripts class), 7 global (the Overview atlas pages); FC5:
364 / 78 / 12. The session-rooted and global rows are the O8
materialization candidates; their bodies are ordinary terms, so the same
scan-profile classifier judges them once they are generated with
constructor comments. The O8 experiment itself stays under its own
research line (design-heuristics.md).

## Dominance (R4)

The gate chooses per conjunct, so its total cost is the pointwise minimum
and dominance over both uniform modes holds by construction wherever the
classification is right; the corpus numbers bound the damage where it is
wrong (no false negatives; false positives pay at most the protocol fee
on a query of ≤ 20 ms). Concretely on RAP at 96 000 scripts: a
content-touching edit pays ~289 ms in the integral route for conj_436 +
conj_127 alone; the gated route pays the protocol fee (1–10 ms per
conjunct) for those two — or, with the structural route, nothing at all
for conj_127/249/250 — while all cheap conjuncts keep their integral
evaluation and avoid the +6.6 ms uniform-`on` overhead that #1687
measured. The end-to-end run of the gated engine behind a flag belongs to
the implementation issue; the per-conjunct measurements here are what
that run must reproduce.

## Reproducing

```
./setup.sh          # scratch MariaDB on :3493, restore RAP + FC5 dumps
python3 inflate.py rap_xk 8; python3 inflate.py fc5_xk 20
python3 measure.py <generics> <db_x1> <db_xk> data/<model>-scaling.csv
python3 classify.py <model> <generics> data/<model>-classify.csv
bash corpus.sh      # the testing/ models end-to-end
python3 validate-classifier.py <model>:<generics>:<db_xk> ...
```

The scratch stack is disposable (`docker rm -f costgate-db`); the #1687
and FC5 stacks are only read (one mysqldump each).
