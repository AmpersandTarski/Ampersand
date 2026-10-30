# Falsification experiment for issue #1744: results

This note reports what came of the attempt to refute the claims of the research note behind
issue #1744.
The claims, the setup and the outcome that would refute each claim are in
[preregistration.md](preregistration.md), committed (12330306e) before the measurements.
The measurements ran on 29 October 2026, on MariaDB 10.6 in a container of its own
(`dv-exp-db`), with the compiler of branch `derived-relations-as-views` and the framework of
branch `derived-relation-views`.
The raw data are in [data/](data/), and every table below is produced by the scripts next to
this note.

## Summary

| claim | verdict |
| --- | --- |
| H1. A view selects the pairs of the materialised relation | survives: 0 differences in 30 random populations and at 4 sizes up to 523 399 pairs |
| H2. Views take the cost of derived relations out of the write path | refuted at every size, for all four base relations |
| H3. A mergeable view reads one atom about as fast as the stored table | refuted on the criterion (ratio 2.5 and 2.7 > 2); the difference is 0.03 ms and does not grow |
| H4. A view the database cannot merge is computed in full at every read | survives for the closure (×192 for ×64 atoms); refuted for the union, which MariaDB filters per atom |
| H5. The report of the compiler predicts H3 and H4 | refuted on the commit before the experiment; survives after the correction it led to |
| H6. The foreign keys never refuse a write of the framework | refuted: every deletion of an atom with a link was refused; survives after framework OK-31 |
| H7. The constraints refuse every broken write from outside | survives: 8 of 8 refused |
| H8. A UNIQUE never refuses a transaction that Ampersand accepts | refuted: an exchange of two `INJ` values is refused |
| H9. A foreign key costs little per pair, independent of size | survives: 3% to 20%, about 2.5 µs per pair, flat in N |

Four defects were found because a claim failed, and three are corrected on the branches:

| defect | found by | correction |
| --- | --- | --- |
| A view the compiler calls mergeable carried `DISTINCT`, so MariaDB never merged it, and the report said "merged" for a closure | H5 | `derivedViewBinQuery` and `derivedViewIsMergeable` in `src/Ampersand/FSpec/SQL.hs` look through the comment that wraps a translation, and a closure is never mergeable |
| `Concept::deleteAtom` deleted the atom before its links, which a foreign key refuses in the middle of the transaction | H6 | framework OK-31: the links go first |
| A view without `DISTINCT` yields a pair twice, and `getAllLinks` passed that on | the stored fill of H1, after the first correction | framework: `getAllLinks` reads a view with `DISTINCT` |
| A UNIQUE on a column a user writes refuses a valid exchange | H8 | open: "Still to decide" in `../DesignChoices.md` |

## H1: equality

`h1val.py` ran `ampersand validate --derived-views` on 30 random populations of 12 atoms per
concept, seeds 1 to 30.
Validate compares 84 terms per population with the evaluator of the compiler; all 30 exit with
0 ([data/h1-validate.txt](data/h1-validate.txt)).
`meet.py` compared every view pair for pair with the stored table filled from its term, at the
four sizes:

| N | rs | ru | rd | ri | voorouder |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 1000 | 0 of 3994 | 0 of 2496 | 0 of 1496 | 0 of 503 | 0 of 8721 |
| 4000 | 0 of 15994 | 0 of 9999 | 0 of 5999 | 0 of 2001 | 0 of 30862 |
| 16000 | 0 of 63989 | 0 of 39997 | 0 of 23997 | 0 of 8003 | 0 of 111975 |
| 64000 | 0 of 255980 | 0 of 159995 | 0 of 95995 | 0 of 32000 | 0 of 523399 |

The same holds for the corrected views (`data/h1-h5-corrected.csv`).

## H2: the cost of a write

Per base relation, the sum in milliseconds of the violation queries of the conjuncts that a
transaction writing it evaluates.
On the stored side that includes the conjuncts of the derived relations that the ExecEngine
rewrites in the same transaction.
A conjunct stopped at the limit of 60 s counts as 60 s, and a sum that contains one is marked ≥.
Corrected views:

| write to | N = 1000 stored | views | N = 4000 stored | views | N = 16000 stored | views | N = 64000 stored | views |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `r` | 132.3 | 54 079.5 | 519.8 | ≥ 61 626.4 | 2 105.6 | ≥ 88 205.5 | 11 241.4 | ≥ 120 166.9 |
| `t` | 8.46 | 104.0 | 35.0 | 1 626.4 | 151.7 | 28 205.5 | 658.6 | ≥ 60 166.9 |
| `s` | 124.4 | 53 977.9 | 487.9 | ≥ 60 008.8 | 1 967.8 | ≥ 60 053.9 | 10 649.2 | ≥ 60 166.9 |
| `parent` | 291.5 | 76 065.6 | 1 430.7 | ≥ 120 000.0 | 6 513.6 | ≥ 120 000.0 | 34 933.4 | ≥ 120 000.0 |

The views as generated before the correction give the same picture (`data/h1-h5-as-generated.csv`).
The cost sits in the rules that join a view the database cannot merge
([data/h2-per-conjunct-corrected.md](data/h2-per-conjunct-corrected.md)).
`BereikViaVoorouder` and `VoorouderTransitief` read the closure `voorouder` and stop at the limit
from N = 4 000 on.
`VerschilInVereniging` reads `rd` and the union `ru` and grows from 102 ms to the limit.
`DoorsnedeInR`, which reads only the mergeable `ri`, costs 167 ms at N = 64 000, against 69 ms
for the same rule on the stored tables.
Inside a join, a view with a union or a closure is a temporary table without an index, and the
join goes block by block, as issue #1708 describes for a computed right-hand side.
So the claim holds only for a derived relation that no rule reads, or that the database merges.

Two remarks on the comparison.
The preregistered sum of the stored side counted only the conjuncts that a base relation affects
directly; `h2stored.py` adds the conjuncts of the derived relations, after the data were in.
That amendment makes the stored side more expensive, so it cannot have rescued the claim.
The stored side still leaves out the inserts of the ExecEngine and its second evaluation round;
that, too, favours the views, which cost 3 to 10 times more at N = 64 000 with the limit as a floor.

## H3, H4 and H5: reading one atom

The interface field query of each derived relation for the atom `a1`, in milliseconds, stored and
view; "report" is what the compiler says per relation.
Before the correction:

| field | report | N = 1000 | N = 4000 | N = 16000 | N = 64000 | growth 1 000 → 64 000 | views/stored at 64 000 | derived table in `EXPLAIN` |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| `rs` | merged | 0.035 / 0.11 | 0.036 / 0.054 | 0.026 / 0.087 | 0.023 / 0.098 | 0.9 | 4.3 | yes |
| `ru` | full | 0.031 / 0.10 | 0.031 / 0.048 | 0.022 / 0.084 | 0.024 / 0.066 | 0.6 | 2.8 | yes |
| `rd` | merged | 0.028 / 0.100 | 0.029 / 0.071 | 0.020 / 0.053 | 0.018 / 0.063 | 0.6 | 3.4 | yes |
| `ri` | merged | 0.028 / 0.093 | 0.029 / 0.067 | 0.018 / 0.056 | 0.018 / 0.066 | 0.7 | 3.7 | yes |
| `voorouders` | merged | 0.064 / 46.2 | 0.071 / 248.7 | 0.049 / 1 183.3 | 0.053 / 6 421.2 | 138.9 | 120 112 | yes |

After the correction:

| field | report | N = 1000 | N = 4000 | N = 16000 | N = 64000 | growth 1 000 → 64 000 | views/stored at 64 000 | derived table in `EXPLAIN` |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| `rs` | merged | 0.026 / 0.053 | 0.027 / 0.052 | 0.033 / 0.097 | 0.046 / 0.087 | 1.7 | 1.9 | no |
| `ru` | full | 0.022 / 0.048 | 0.019 / 0.051 | 0.023 / 0.076 | 0.030 / 0.14 | 2.9 | 4.6 | yes |
| `rd` | merged | 0.019 / 0.040 | 0.021 / 0.043 | 0.021 / 0.065 | 0.022 / 0.056 | 1.4 | 2.5 | no |
| `ri` | merged | 0.018 / 0.046 | 0.017 / 0.047 | 0.023 / 0.059 | 0.020 / 0.054 | 1.2 | 2.7 | no |
| `voorouders` | full | 0.065 / 18.7 | 0.042 / 133.9 | 0.049 / 659.5 | 0.056 / 3 587.0 | 192.0 | 63 814 | yes |

H3 is refuted on its criterion: `rd` and `ri` read 2.5 and 2.7 times slower than the table.
The difference is about 0.03 ms, and it does not grow with N.
H4 holds for the closure, whose read grows 192 times for 64 times as many atoms.
It is refuted for the union `ru`: its read grows 2.9 times, less than the factor 4 of the
criterion, because MariaDB pushes the condition on the atom into both branches of the union.
"Computed in full at every use" is therefore true for a closure and for a union inside a join
(H2), and not for a union read per atom.
H5 is refuted on commit 12330306e, where the report called four views merged and `EXPLAIN` showed
a derived table for every one of them, and the closure "merged" grew 139 times.
After the correction, the report and `EXPLAIN` agree for all five relations.

## H6: the framework against its own foreign keys

`h6.sh` builds a prototype of `Experiment.adl` with `H6-extra.adl` on the framework branch and
drives its REST API with fifteen requests: reinstall with population, create an atom, add and
remove pairs in all four base relations, delete four atoms, and reinstall again.
Before framework OK-31 all four deletions answered 500 with error 1451
([data/h6-before-OK-31.txt](data/h6-before-OK-31.txt)): `Concept::deleteAtom` deleted the row of
the atom and then its links, and MariaDB checks a foreign key at every statement.
With the links deleted first, all fifteen requests commit, with `--foreign-keys` alone and with
`--derived-views` as well (`data/h6-after-OK-31*.txt`).
The test did not merge atoms; `Concept::mergeAtoms` deletes and adds links and then deletes the
atom, which now removes the remaining links first.

## H7 and H8: writes from outside, and the exchange

`h7h8.py` writes directly in SQL ([data/h7h8.txt](data/h7h8.txt)).
All eight broken writes were refused: a dangling reference in a link table and in a wide column,
the deletion and the renaming of a referenced atom, and a duplicate in a unique column.
The database names the constraint in its error, for instance `CONSTRAINT "fk_r_A" FOREIGN KEY ("A")`.
Two writes pass by design: a reference to a concept without a table of its own, and to a scalar
concept, whose table has no primary key.
H8 failed twice: two atoms that exchange their values of an `INJ` relation in one transaction,
written as `addLink` writes, meet error 1062 at the first `UPDATE`, although the end state
satisfies every rule.
H8 ran at the SQL level only: the REST API writes one atom per transaction, so the exchange needs
an interface that writes both atoms at once.

## H9: the cost of a foreign key

The bulk insert of 2N pairs into the link table `r` in one transaction, with and without its
foreign key to `A`:

| N | pairs | without FK (ms) | with FK (ms) | overhead | µs per pair with FK |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 1000 | 1999 | 5.12 | 6.16 | +20% | 3.08 |
| 4000 | 8000 | 18.1 | 21.4 | +18% | 2.67 |
| 16000 | 32000 | 75.0 | 87.9 | +17% | 2.75 |
| 64000 | 127995 | 288.5 | 296.6 | +3% | 2.32 |

## Deviations from the preregistration

- A query stops after 60 s and counts as 60 s, and a query slower than 5 s in its warm-up runs
  twice instead of seven times; the first trial took more than 20 s for one conjunct at N = 1 000.
- H1 compares with `EXCEPT` instead of `NOT IN`, which was too slow at N = 64 000.
- The stored derived tables are filled with `SELECT DISTINCT`, because the corrected views yield
  duplicate rows.
- H2 adds the conjuncts of the derived relations on the stored side (`h2stored.py`), after the data.
- H6 used the REST API only and did not merge atoms; H8 ran at the SQL level only.
- One seed per size, a warm cache, one MariaDB version (10.6), one machine.
