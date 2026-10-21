# Trial: sharing repeated subterms in generated queries (issue #1739)

This folder holds the scripts and the raw results of the experiments of issue #1739.
The branch `violation-sharing-trial` is not for merging:
its compiler writes every violation query and every candidate query in three forms, so that they can be timed side by side.

| Form | Violation query field | Candidate query field |
| --- | --- | --- |
| A, unshared | `violationsSQL` | `candidateSQLUnshared` (empty when longer than 1,000,000 characters) |
| B, shared views with `DISTINCT` (the form of pull request #1734) | `violationsSQLShared` | `candidateSQL` |
| C, shared views without `DISTINCT` | `violationsSQLMergeable` | `candidateSQLMergeable` |

## How to repeat the experiment

1. Copy a populated database of the Landeneisenregister read-only into a container of your own,
   for example `mariadb-dump --single-transaction --quick --databases Register` from `ontdubbel-db`, loaded into `mariadb:10.6` on port 3320.
   For `mariadb:10.4`, drop the first line of the dump (the sandbox comment), which the 10.4 client does not know.
2. Compile the model version that belongs to the data with the compiler of this branch
   (FC5 commit `32d6eb9c` for the data of 9 October 2026): `ampersand proto main.adl --proto-dir <dir> --no-frontend`.
3. `schemadiff.py` compares the tables of the running instance with those of the compiled model;
   `addmissing.py` creates the tables the copy lacks, empty.
   `counts.py` checks that two copies hold the same rows.
4. `measure3.py violations|candidates <generics> <port> Register <out.csv> <reps> <timeout> <delta-pairs>` times the three forms;
   for a candidate query it first puts a sample of pairs of its relation in the delta table.
   `summarize.py <out.csv>` prints the totals and the largest changes.

The files `v106.csv`, `c106.csv`, `v104.csv` and `c104.csv` are the results reported in issue #1739.
