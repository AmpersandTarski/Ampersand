# Trial on a copy of a production database: results of phase 2

The predictions are in `preregistration.md`, committed before any data was read (e32da38a6).
The measurements were taken on 27 November 2026 with the compiler of the branch `migration-plan` (88abd7ecd),
on the dump that the server of the pilot made at 19:42:02 UTC on 26 November 2026,
one second after the deployment of release 1.42.0 began, while release 1.41.0 still ran.
The dump holds personal data; it and the work directory stay outside every repository.

## Reading the database as it is

`read_trial.sh` restores the database of the application from the dump into MariaDB 12.3,
creates the views of the migration system on it, and runs `read_compare.py`.
That script reads every relation and concept of release 1.41.0 twice:
through the views of the migration system, and with the queries of release 1.41.0 compiled by itself with the released compiler v5.15.0,
which are the queries by which the application in production reads its data.

The first run refused 34 of the 39 views, because the tables in production have names in lower case.
The database of the pilot runs with `--lower-case-table-names=1` and `--sql-mode=ANSI,TRADITIONAL`;
with those options, as in production, the measurements are these.

| Prediction | Outcome |
| --- | --- |
| 1. Every table that the migration system reads in the database of `old` exists | holds: 39 views, none refused, and no query fails |
| 3. The pairs of every relation are the same | holds: 131 relations, 12,859 pairs, no difference |
| 4. The atoms of every concept are the same | holds: 25 concepts with a table, 2,473 atoms, no difference |

Five concepts have no table in release 1.41.0 and no query in v5.15.0, and the migration system reads them from the columns of their relations;
they have 0, 1, 6, 30 and 417 atoms.
For each, the atoms read are the atoms that the pairs of its relations hold in the existing system.
Six concepts of integers and dates have neither a table nor a query on either side (issue #1727), and are not compared.

## The installation

`install_trial.sh` installs the desired release compiled by itself, and then the migration system, both in framework v2.25.0,
next to the restored database.

| Prediction | Outcome |
| --- | --- |
| 2. The installer of the migration system answers HTTP 200 | refuted: HTTP 500 |

The desired release installs (HTTP 200).
The installer of the migration system fails on an ExecEngine rule of the desired release that gives every session the date of today,
with `VIOLATION (TXT "{EX} SetToday;today;SESSION;", SRC I, TXT ";Date;Y-m-d")` (names translated):
"Concept 'Date' is not defined".
The rule names its relation and concepts in the text of its violation, and in the migration system they are called `new.today` and `new.Date`.
The compiler prefixes the names in the text that it writes itself for an `ENFORCE` (`P2A_Converters.hs`),
but leaves a text that the author wrote, `TXT "{EX} …"`, as it is.
The scripts of the pilot have about 160 such calls, 116 of them of the standard functions `InsPair`, `DelPair`, `InsAtom` and `DelAtom`.
The demonstrations did not show this, because their included contexts have no ExecEngine rules.

The checksums of all 201 tables of `old` are the same before and after.
That shows only that a failed installation leaves the existing database as it was:
the installer stopped inside a transaction of the ExecEngine, which was not committed.
Whether a successful installation writes in `old`, for instance through an ExecEngine rule of `old` on `SESSION`, is still to be measured.

The pilot has ExecEngine functions that send mail.
In the containers of the trial they cannot: the function that sends mail returns without sending when no mail service is configured
in the environment, and the containers were given none; the logs of both installations mention no mail.

The pilot was counted with 201 relations of its own in the experiment on the layout; that was release 1.42.0.
Release 1.41.0, which this trial reads, has 131.
