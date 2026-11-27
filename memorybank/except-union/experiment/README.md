# Grid experiment for issue #1763: how a difference is translated to SQL

This note keeps the grid experiment behind the diagnosis of
[issue #1763](https://github.com/AmpersandTarski/Ampersand/issues/1763),
so that the measurement before a change of the translation of `l - r` can repeat it and compare.
The diagnosis and the proposal are in
[the comment of 26 November 2026](https://github.com/AmpersandTarski/Ampersand/issues/1763#issuecomment-7193382350).

## What it measures

Since compiler v5.15.0 (#1708) the generator translates every difference `l - r` with `EXCEPT`,
which computes `r` in full.
The proposal is to use `EXCEPT` only when `r` reads a closure,
and the `LEFT JOIN` of the earlier translation otherwise.
The grid asks, for each shape of `r`, which of three translations is fastest,
at several sizes of `l` and `r` and at two column widths:

- `base` is the translation of v5.15.0, with `EXCEPT` for every difference;
- `noexcept` is the earlier translation, with a `LEFT JOIN` for every difference;
- `closure` uses `EXCEPT` only when `r` reads a closure (`t+`), and a `LEFT JOIN` otherwise.

[grid.adl](grid.adl) has six `ENFORCE` statements:
a large and a small left-hand side (`big`, `small`),
each against a union (`p \/ p~`), an intersection of two unions (`(p \/ p~) /\ (q \/ q~)`)
and a closure (`t+`).
That gives twelve conjuncts, one for insertion and one for deletion per statement.

## The files

- [violations.json](violations.json) holds the violation query of each of the twelve conjuncts in the three translations,
  and [database.sql](database.sql) the schema, which is the same for all three.
  They come from a trial build of the compiler with switches for the three translations,
  on a local branch that is not on `origin`.
  The queries are stored here, so the experiment runs without that build.
- [rooster.py](rooster.py) fills the relations for every size `n` and width `w`,
  gives each derived relation its correct contents (so every violation query returns 0 pairs, as in production),
  and runs each query three times after a first run, reporting the median server time,
  the number of pairs and a checksum.
  A query over 20 s is run once, and one over 60 s is stopped.
- [results-mariadb-10.6.tsv](results-mariadb-10.6.tsv) has the run of 26 November 2026 on MariaDB 10.6,
  at `n` = 1000, 10 000 and 40 000 and `w` = 255 and 124;
  [results-mariadb-12.3.tsv](results-mariadb-12.3.tsv) the run on MariaDB 12.3 at `w` = 255.
  These runs were not preregistered.

## Running it

The script talks to a MariaDB container through `docker exec`;
`DBC` names the container (default `amp1728fc5-db`) and `DBPW` the root password (default `root`).
Each cell gets a database `grid_<n>_<w>`, which the script drops and creates again.

```sh
DBC=<container> python3 rooster.py out.tsv 1000,10000,40000 255,124
```

## Outcome

On MariaDB 10.6 with width 255 every translation loses more than 60× in some cell,
because MariaDB 10.6 puts no index on a materialised subquery whose key exceeds 1000 bytes,
and a pair of `VARCHAR(255)` columns in UTF8MB4 is 2044 bytes.
With width 124 on 10.6, and with width 255 on 12.3, the three translations are within about 2× of each other in every cell.
All three gave the same pairs in every cell.
