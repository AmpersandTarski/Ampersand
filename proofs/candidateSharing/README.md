# Candidate queries with shared subterms

Supports `candidateTerm` in `src/Ampersand/FSpec/Incremental/DeltaTerms.hs`
and `sqlQuerySharedWithinLimit` in `src/Ampersand/FSpec/SQL.hs` (issue #1728).
It is the artefact of claims PRF-16, PRF-17 and PRF-18 in `docs/proofs/README.md`.

A candidate query returns the pairs whose membership in a rule term may change when a transaction changes one relation.
Claim PRF-7 proves the candidate calculus complete.
This session proves that the compiler may write the candidate set in nested form with shared subterms,
and that this form is linear in the size of the rule term.

`CandidateSharing.lean` models a relation as a binary predicate,
a rule term as the class of terms that the candidate calculus supports,
and the envelopes `W` and `N` of PRF-7 as parameters or, for the size results, as terms.

It proves:

- **`candg_eq_list` (PRF-16):** the nested calculus, one union per node, denotes the union of the list of candidate terms,
  one per occurrence of the changed relation, which is the form the compiler generated until issue #1728.
- **`candg_eq_opt` (PRF-16):** the pruned nested form, in which a branch without a changed relation is left out,
  denotes the same relation as the calculus.
- **`evalWith_eq_inline` (PRF-17):** a term with views bound in order, as in a SQL `WITH` clause,
  denotes what the term with every view inlined denotes.
  The step is the substitution lemma `sem_subst`.
- **`shared_size` (PRF-18, upper bound):** every subterm of the candidate term is one of at most five terms per node of the rule term (`cand_in_catalogue`, `catalogue_length`),
  so a list of its distinct subterms, the nodes of its shared representation, has at most five entries per node of the rule term.
- **`chain_quadratic` (PRF-18, lower bound):** written as a tree, the candidate term of a chain of `k` differences has at least `k (k − 1) / 2` nodes.
- **`candT_denotes`:** the candidate term as syntax denotes the pruned form of PRF-16, with `W` and `N` the relations that the envelope terms denote.

Not proved here: that the Haskell functions produce the terms this file models,
and that MariaDB evaluates a `WITH` clause as `evalWith` does.
The first is guarded by the QuickCheck bridge `Ampersand.Test.Incremental.CandidateProperties`,
whose property `propDeltaQueriesAgree` compares the nested candidate term with the list of candidate terms on random terms and states;
the second by the delta-SQL harness `ampersand incremental-bench --sql`, which compares the maintained violations with full re-evaluation on MariaDB.

Build, with Lean 4.34.1 and the core library only:

```
lake build
lake env lean Axioms.lean
```

Observed on 16 October 2026: `Build completed successfully`, no `sorry`, no warnings,
and `Axioms.lean` lists at most `propext`, `Classical.choice` and `Quot.sound` for every theorem above.
