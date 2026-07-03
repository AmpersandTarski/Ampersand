# Regression tests for the Kleene operators

This directory holds all tests that specifically target the Kleene family:
transitive closure `r+`, reflexive-transitive closure `r*`, and transitive
reduction `r%` (sugar for `r - (r;r+)`). Design and machine-checked semantics:
[issue #1651](https://github.com/AmpersandTarski/Ampersand/issues/1651) and
`proofs/kleene/`.

## `semantics/` — Haskell semantics vs SQL semantics

Runs `ampersand validate`: every term in every rule and interface is evaluated
both by the Haskell-based evaluator and by the generated SQL, and the results
must coincide. Requires a reachable MySQL/MariaDB (`MYSQL_HOST`, default
`127.0.0.1`, user `root`). Each script pins the *exact* extension of `r+`/`r%`
with expected populations, plus the proven laws as rules:

| Script | Stresses |
| --- | --- |
| `ChainShortcut.adl` | chain + branch + redundant shortcut; the canonical fixture |
| `Diamond.adl` | multiple witnessing paths for one pair; a transitively closed base (`r+ = r`) |
| `Cycles.adl` | two SCCs, a self-loop, a tail into a cycle; `r%` collapses to empty on cycles |
| `EmptyRelation.adl` | `r = {}`: `r* = I` (regression for the unsound law `r* = r;r*`), `r+ = {}` |
| `LongChain.adl` | 25-node chain: deep `WITH RECURSIVE` recursion; end-to-end reachability |
| `CombinedOperators.adl` | closures of compound terms; nesting (`(s*)*`); interplay with converse (`(s~)% = (s%)~`) |

The `r*` case caught a real bug on first run: the SQL translation of `EKl0`
was `r UNION r+` (which equals `r+`), missing the identity.

## `typecheck/` — type rules of the Kleene family

`shouldPass/`: endorelation operands type-check. `shouldFail/`: heterogeneous
operands must be rejected (exit 10) for each of `+`, `*`, `%`.
