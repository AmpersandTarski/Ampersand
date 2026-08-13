# Measurement results — incremental vs. full evaluation

Command: `ampersand incremental-bench` on [ProjectAdmin.adl](ProjectAdmin.adl)
(5 conjuncts, 5 relations, circuit coverage 22/22 nodes incremental, zero
fallback nodes). Machine: Stef's Mac (Apple Silicon, single-threaded
evaluation), compiler branch `incremental-evaluation`, 2026-08-13, seed 42.

## Method

Per scale N, every relation receives N synthetic pairs drawn from pools of N
atoms per concept; a stream of single-pair transactions (inserts and deletes,
50/50) then runs through two evaluators:

- **incremental** — one circuit step (`applyTx`): the delta propagates through
  the maintained circuits; measured is the step plus forcing of the output
  deltas.
- **full-affected** — what generated prototypes do today: full re-evaluation
  (`fullContents`) of every conjunct whose relations were touched, over the
  whole population; measured is evaluation plus forcing of the result sizes.

Both timings cover the same transaction within the same process. Timer:
`getMonotonicTime` (nanoseconds; unit verified against a 100 ms sleep).

**Correctness of the measured runs.** At scales 200, 500 and 1000 every
transaction ran with `--verify`: after each step, the maintained violation set
of every conjunct is compared with a fresh `fullContents` evaluation — zero
mismatches (150 transactions per scale, plus the backfill). The larger scales
run the identical code path; verification there is omitted only because the
oracle itself costs seconds per transaction at those sizes. Separately, the
engine is oracle-verified on ten regression models (Kleene closures, cartesian
products, subtyping, complements, script populations).

## Results (medians over the transaction stream)

| scale (pairs/relation) | incremental | full-affected | speedup | run |
|---:|---:|---:|---:|---|
| 200 | 10.0 µs | 1.65 ms | ×166 | verified, 150 tx |
| 500 | 18.2 µs | 8.69 ms | ×476 | verified, 150 tx |
| 1 000 | 29.8 µs | 32.45 ms | ×1 088 | verified, 150 tx |
| 500 | 24.7 µs | 8.78 ms | ×356 | timing, 100 tx |
| 1 000 | 36.2 µs | 32.77 ms | ×904 | timing, 100 tx |
| 2 000 | 59.1 µs | 300.36 ms | ×5 084 | timing, 100 tx |
| 4 000 | 60.6 µs | 513.75 ms | ×8 480 | timing, 100 tx |
| 8 000 | 64.8 µs | 1 984.33 ms | ×30 626 | timing, 30 tx |

Raw per-transaction data: [results-verified.csv](results-verified.csv),
[results-scaling.csv](results-scaling.csv),
[results-scale8000.csv](results-scale8000.csv) (columns: scale, tx, op,
incremental_us, full_affected_us, affected_conjuncts).

## Reading the numbers

- Across a ×40 growth of the database (200 → 8 000 pairs per relation), the
  incremental step grows ×6 (10 → 65 µs — index lookups pick up logarithmic
  factors and the per-delta fan-out grows slightly with density), while full
  re-evaluation grows ×1 200 (1.65 ms → 1.98 s). The per-transaction cost of
  the incremental route is governed by the size of the change; the full
  route is governed by the size of the database.
- The steep growth of the full route beyond scale 1 000 comes from the
  composition terms: `fullContents` evaluates `r;s` by pairing all source
  rows of `r` with all target rows of `s`, which is quadratic in the number
  of active atoms. The generated SQL avoids that particular quadratic blow-up
  through join plans, so the in-Haskell "full" column overstates what MariaDB
  would spend — but the SQL route still scans tables proportional to database
  size, which is the effect the Phase-4 end-to-end benchmark must quantify.
- Backfill (initializing all circuits from the full population, same code
  path as a transaction) costs 327 ms at scale 8 000 — a one-time cost at
  install/reinstall, in line with the plan's backfill story.
