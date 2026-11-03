# Domain of a relation: the selection on the concept table and the self-join select the same pairs

Supports `domainCase` in `src/Ampersand/FSpec/SQL.hs` (issue #1748):
the special translation of `I[A] /\ e;e~`, the domain of `e` on the atoms of `A`, and of `I[A] /\ e~;e`, its range.
It is the artefact of claim PRF-21 in `docs/proofs/README.md`.

`DomainOfRelation.lean` models a two-column table as a list of rows, so that duplicates count as they do in SQL,
and a concept table as a list of atoms.
The columns hold no `NULL`; that is the proviso of the claim.

It proves:

- **`mem_compose_converse`:** a pair is in `e;e~` exactly when its two atoms share a partner in `e`.
- **`mem_selfJoinQuery`:** the general translation, the rows of `e;e~` on the diagonal with the source among the atoms of `A`,
  selects (x,y) exactly when y = x, x is in `A` and x has a partner in `e`.
  The proof is a `calc` chain of three steps: the definition of the query, the lemma above, and the rewriting of the conjunction.
- **`mem_domainQuery`:** the selection on the concept table selects the same pairs; a `calc` chain of two steps.
- **`same_pairs`:** so both translations select the same pairs. This is the claim.
- **`same_pairs_range`:** the claim for the range form, which is the domain form on the converse of `e`.

The two `example`s at the end show the two results on a table where three rows share a target:
`e;e~` holds ten rows, the domain holds four atoms.

Not proved here: that the query of `e` never yields `NULL` in a column,
and that the Haskell function produces the SQL this file models.
The first is an invariant of the generator; the second is guarded by `ampersand validate`,
with the test case `testing/Travis/testcases/DomainOfRelation` reaching the domain form, the range form,
both orders of the intersection, a smaller concept, a composite term and the domain inside a composition.

Build, with Lean 4.34.1 and the core library only:

```sh
cd proofs/domainOfRelation
lake build
```
