# RAP benchmark harness (issue #1687)

This folder carries the load harness and the measurement results for the RAP
validation of the incremental-evaluation track:
[issue #1687](https://github.com/AmpersandTarski/Ampersand/issues/1687),
design choice DC-14 in [../DesignChoices.md](../DesignChoices.md).

## The experiment in one paragraph

Two API-level RAP deployments run side by side on one host. They share the
framework code (branch `feat-delta-conjunct-maintenance` of the prototype
framework, mounted worktree), the generics (generated once by the delta-sql
compiler from RAP branch `incremental-evaluation`), and the MariaDB version
(10.4, RAP production parity). They differ in exactly one setting:
`transactions.deltaConjunctMaintenance` stands on `off` (:8191) or `on`
(:8192). The harness seeds student scripts through the framework's full
request pipeline and measures, at growing database sizes, what a transaction
close costs and what a page open costs. The hypothesis under test is the
promise of the incremental track: per-transaction cost follows the size of
the change, not the size of the database.

## Files

| file | role |
|---|---|
| `docker-compose.yml` | the two instances plus their databases |
| `setup.sh` | generate generics, lay out runtime dirs, start the stack |
| `lib.sh` | shared helpers: session, login mimicry, role switch, timed GET |
| `seed.sh` | seed scripts through the replay endpoint; logs every close (E1a) |
| `checkpoint.sh` | at one size: single-edit closes (E1b), page opens (E2), query digest |
| `run-experiment.sh` | reinstall → login → seed to S1/S2/S3 with checkpoints |
| `analyze.py` | aggregate both runs' CSVs into the markdown tables of RESULTS.md |
| `RESULTS.md` | the measurements and their reading |
| `data/` | the raw CSVs and digests both runs produced |

## Reproducing

```
./setup.sh                      # build generics, start containers
./run-experiment.sh 8191 out-off 1000 4000 12000
./run-experiment.sh 8192 out-on  1000 4000 12000
./analyze.py out-off out-on
```

The three page probes: `MyScripts` and `Nieuwscript` are point queries
(index lookups from one atom); `StudentScripts` is a computed expression
(`"_SESSION" # (I[Account] /\ submittor~;submittor)`) whose result grows
with the database — the specimen of an expensive derived interface query.

Login is mimicked at the data level: the replay endpoint links
`sessionAccount`, SIAM's own ExecEngine rules grant the roles, and the
role switcher of the API activates them. The replay endpoint
(`backend/bootstrap/api/replay.php`, untracked in the framework worktree)
is the same one the FC5 shadow run used (DC-12).
