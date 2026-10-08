# Set difference: left join and EXCEPT select the same pairs

Supports `exceptCase` and `leftJoinCase` in `src/Ampersand/FSpec/SQL.hs` (issue #1708):
the two ways in which the SQL generator translates a difference `l − r`.
It is the artefact of claim PRF-12 in `docs/proofs/README.md`.

`SetDifference.lean` models a two-column table as a list of rows, so that duplicates count as they do in SQL.
The columns of `l` and `r` hold no `NULL`; that is the proviso of the claim.
`NULL` appears where SQL itself introduces it: in the right-hand columns of a left-join row without a partner.

It proves:

- **`mem_leftJoinQuery`:** the left-join translation selects the rows that occur in `l` and not in `r`.
  The proof is a `calc` chain of four steps: the definition of the query, the definition of the left join,
  moving `p ∈ l` out of the quantifier, and the lemma `padding_iff`.
- **`mem_exceptQuery`:** the `EXCEPT` translation selects the same rows.
- **`same_pairs`:** so both translations select the same pairs. This is the claim.
- **`exceptQuery_nodup`:** the result of `EXCEPT` carries no duplicates.
- **`same_pairs_flipped`:** the claim for the flipped variant, in which `r` is replaced by its converse.

The left-join query can return a row more than once, when it occurs more than once in `l`.
The generated query removes those with `select distinct`, so the claim is about the set of rows.
The `example` at the end shows the two results on a table with a duplicate.

Not proved here: that the queries of `l` and `r` never yield `NULL` in a column,
and that the Haskell functions produce the SQL this file models.
The first is an invariant of the generator; the second is guarded by `ampersand validate`,
with the test case `testing/Travis/testcases/SetDifference` reaching both translations.

Build, with Lean 4.34.1 and the core library only:

```
lake build
```

Observed on 7 October 2026: `Build completed successfully`, no `sorry`,
and `#print axioms SetDifference.same_pairs` lists `propext`, `Classical.choice` and `Quot.sound`.
