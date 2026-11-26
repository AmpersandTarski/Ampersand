# Layout of an included context: preregistration

Phase 2 of the migration plan (`../plan.md`) changes the compiler so that a migration context
reads the database of an existing system as that system laid it out when it was compiled by itself.
This note registers the claims before the change is built,
so that the experiment can try to refute them.

## The claim

For every relation and every concept of a script,
the table and the columns in which the script stores it when it is compiled by itself
are the table and the columns from which a context that includes the script under a label reads it.
A concept that has no table of its own when the script is compiled by itself
has none in the including context either; the including context reads its atoms from the columns that hold them.

The claim is the layout part of PRF-27 in `docs/proofs/README.md`.

## The experiment

`included_layout.sh <script>` compiles a script by itself and wrapped in
`CONTEXT Migration INCLUDES <name> FROM "<script>" AS old`, both with `ampersand proto --no-frontend`,
and `compare_layout.py` compares the files `relations.json` and `concepts.json` of the two.
`run_all.sh` does that for every script of `testing/` that compiles by itself.

The claim is refuted by one script with one difference.
Scripts of every kind are in the set, among them scripts with `MULTITABLE`, with `NAMED`,
with derived relations as views, and with concepts of integers and dates.

## Predictions

Before the change of the compiler (v5.15.0, measured on `migration-plan`):
a difference in every script that has a concept which no query of its own reads,
because the including context gives every concept a table (DC-15).
No difference in the table or the columns of a relation.

After the change: no difference at all.
A difference that remains is a finding, and the change is not done until it is explained or removed.
