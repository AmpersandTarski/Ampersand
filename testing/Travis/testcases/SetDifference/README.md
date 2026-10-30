# Regression test for the two translations of a difference

The SQL generator translates a difference `l - r` in one of two ways (issue #1708, claim PRF-12).
When `r` contains a closure or a union, it writes a set difference (`EXCEPT`).
Otherwise it writes a left join that keeps the rows of `l` without a partner in `r`.

`DifferenceOntoMaterialised.adl` runs `ampersand validate`:
every term in every rule and interface is evaluated by the Haskell evaluator and by the generated SQL,
and the two results must coincide.
It requires a reachable MySQL or MariaDB (`MYSQL_HOST`, default `127.0.0.1`, user `root`).

The script holds one population and seven rules, each pinning the exact outcome of a difference with an expected relation.

| Rule | Right-hand side | Translation |
| --- | --- | --- |
| `minusClosure` | a closure, `r+` | `EXCEPT` |
| `minusUnion` | a union, `r \/ t` | `EXCEPT` |
| `minusComposedClosure` | a closure inside a composition, `r;r+` | `EXCEPT` |
| `minusConverseClosure` | the converse of a closure | `EXCEPT` |
| `minusComposition` | a composition of declared relations, `r;t` | left join |
| `minusStored` | a declared relation | left join |
| `storedIsClosure` | both directions of `closed = r+` | one of each |

Every pinned outcome is non-empty.
Two translations that both return nothing would agree on a population where the difference is empty, whatever they compute.

The warning about a Cartesian product on `minusConverseClosure` is expected:
the rule is written with an explicit complement, to reach the flipped variant of the translation.
