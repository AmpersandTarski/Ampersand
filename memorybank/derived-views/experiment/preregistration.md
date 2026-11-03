# Falsification experiment for issue #1744: the claims and how they could fail

The research note behind issue #1744 makes qualitative claims about views and constraints,
and the measurement of 27 October 2026 (`../measurement-fc5.md`) was one run on one
population. This experiment tries to refute those claims with measurements that are planned
before they are taken. A claim that survives a serious attempt to refute it carries more
weight than one that a convenient measurement confirms.

This file is committed before the measurements of the experiment are run, so that the git
history shows which outcome would have refuted each claim. Two observations were made
before this file was written, during the preparation; they are recorded at the end, and
they are not counted as tests.

## Setup

- **Model.** `Experiment.adl` next to this file: four base relations (`r`, `t`, `s`,
  `parent`), five derived relations of one shape each (`rs := r;s`, `ru := r \/ t`,
  `rd := r - t`, `ri := r /\ t`, `voorouder := parent+`), four rules that read them, and an
  interface that reads them per atom. The model is synthetic so that the population size
  can be varied while the shape of the model stays the same.
- **Population.** `populatie.py` generates for a size N: N atoms of `A`, `B` and `C`; two
  `r` pairs and one `t` pair per `A` (half of the `t` pairs also in `r`); two `s` pairs per
  `B`; `parent` a random forest over `C` in which every atom has at most one parent and the
  depth is about log₂ N. The seed is fixed per run and recorded.
- **Sizes.** N = 1 000, 4 000, 16 000 and 64 000.
- **Database.** MariaDB 10.6 in a container of its own, no other load, default settings.
- **Timing.** Server-side with `SHOW PROFILES`, as in the earlier measurement; 7 runs per
  query after one warm-up run, the median reported, with minimum and maximum.
- **Two schemas per N.** `stored`: generated without `--derived-views`, the derived tables
  filled from their terms. `views`: generated with `--derived-views`. Both get the same base
  population.

## The claims, with what would refute them

**H1. A view selects the pairs that the materialised relation would hold, for every
population** (claim PRF-19). Test: for 30 random populations of a small N (seeds 1 to 30),
`ampersand validate --derived-views` compares every view and every conjunct with the
evaluator of the compiler, and the views are compared pair for pair with the stored tables
of the `stored` schema. Refuted by one difference.

**H2. Under `--derived-views` a write to a base relation spends no time on keeping a derived
relation up to date; under materialisation that time grows with the population.** Test: per
base relation x and per N, the sum of the violation-query times of the conjuncts that
`relations.json` lists as affected by x, in both schemas (this is what the framework
evaluates when a transaction that writes x closes). Prediction: in `stored` the sum includes
the `Compute` conjuncts and grows with N; in `views` it holds no `Compute` conjunct.
Refuted if the `views` sum for a write is larger than the `stored` sum at N = 64 000 for a
majority of the base relations: then the work has only moved from keeping the copy to
reading the view, at a higher price.

**H3. A view that the database merges into the reading query costs, read for one atom,
about what the stored table costs, whatever the size.** Test: the interface field query of
each derived relation for one atom of `A`, per N, in both schemas. Prediction: for a view
that the compiler calls mergeable, the ratio views/stored stays below 2 at every N and does
not grow with N. Refuted if the ratio exceeds 2 at N = 64 000 or grows by more than a factor
2 from N = 1 000 to N = 64 000.

**H4. A view that the database cannot merge (a union, a recursion, a DISTINCT) is computed
in full at every use, so its read cost for one atom grows with N.** Prediction: for such a
view the read time for one atom grows at least by a factor 8 from N = 1 000 to N = 64 000
(a factor 64 in size). Refuted if it grows by less than a factor 4: then MariaDB pushes the
condition on the atom into the view (condition pushdown into derived tables) and the claim
"computed in full at every use" overstates the cost.

**H5. The report of the compiler predicts H3 and H4.** For each derived relation the
compiler says "merged into every query that reads it" or "computed in full at every use".
Refuted if for some relation the measured behaviour of H3/H4 contradicts the report, or if
`EXPLAIN` of the read shows a derived table (`<derivedN>`) for a relation the report calls
merged.

**H6. The foreign keys of `--foreign-keys` never refuse a write that the prototype
framework makes.** Test: a prototype of `Experiment.adl` with `--foreign-keys` on the
framework branch `derived-relation-views`, driven through its REST API: create atoms, add
and remove links in every relation, delete an atom that occurs in several relations, and
reinstall with population. Refuted by one MariaDB error 1451 or 1452.

**H7. The constraints refuse every write from outside the framework that breaks a reference
or a uniqueness.** Test: direct SQL against the `stored` schema with `--foreign-keys`: a
dangling reference in a wide column and in a link table, the deletion of a referenced atom,
the change of a referenced key, and a duplicate in a unique column. Refuted by one accepted
write.

**H8. A UNIQUE constraint never refuses a transaction that Ampersand accepts.** Test: a
transaction whose end state satisfies every rule but whose intermediate state duplicates a
value in a unique column (two atoms exchange their values of an `INJ` relation), written as
the framework writes it. Refuted if MariaDB refuses it.

**H9. A foreign key costs little per written pair, and that cost does not grow with the
population.** Test: insert N pairs into a relation column with and without its foreign key,
per N, each in one transaction. Prediction: the overhead per pair is below 50% and changes by
less than a factor 2 from N = 1 000 to N = 64 000. Refuted otherwise.

## Observed during the preparation, before this file

- The compiler reports `rs`, `rd`, `ri` and `voorouder` as "merged into every query that
  reads it", while the generated `CREATE VIEW` of each starts with `select distinct`, and
  that of `voorouder` holds a `WITH RECURSIVE`. Cause: `derivedViewBinQuery` in
  `src/Ampersand/FSpec/SQL.hs` matches a `BinSelect`, but `selectExpr` wraps every
  translation in a `BinQEComment`; `setDistinct` removes the comment and sets `DISTINCT`, and
  `derivedViewIsMergeable` then sees a `BinSelect`. H5 is therefore expected to fail on the
  current commit; the experiment runs on the current commit and again after a correction.
- A UNIQUE constraint on `Persoon.paspoort` (`INJ`) of `testing/derivedViews/constraints`
  refused the exchange of two passport numbers in one transaction (MariaDB error 1062). This
  is the test of H8, run once by hand; the experiment repeats it on `Experiment.adl`.
