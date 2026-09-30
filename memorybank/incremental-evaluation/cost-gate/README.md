# Cost-gate study harness (issue #1690)

This folder carries the instruments and results of the compile-time
cost-gate research:
[issue #1690](https://github.com/AmpersandTarski/Ampersand/issues/1690),
design choices DC-16 and DC-17 in [../DesignChoices.md](../DesignChoices.md).

The question: can the compiler decide per violation query whether
incremental evaluation pays? The study times every conjunct query of eight
models at two database sizes and confronts a shape classifier with the
measured scaling. The answer stands in [RESULTS.md](RESULTS.md).

| file | role |
| --- | --- |
| `setup.sh` | scratch MariaDB (:3493) plus restored RAP/FC5 dumps |
| `inflate.py` | grow a database ×k with join-preserving row copies |
| `corpus.sh` | `testing/` models end-to-end: proto, validate, inflate, measure |
| `measure.py` | server-side timing of every violation query at two sizes |
| `classify.py` | shape classifier v1 over the constructor comments in the SQL |
| `validate-classifier.py` | v1 and v2 against the measured classes |
| `interface-hook.py` | point / session-rooted / global split of interface objects |
| `data/` | the CSVs every table in RESULTS.md is built from |
