# The absorption laws of the normaliser

Supports the law set `tceDerivRules` in `src/Ampersand/FSpec/ToFSpec/NormalForms.hs`:
the five absorption laws with which the normaliser rewrites terms.
It is the artefact of claim PRF-22 in `docs/proofs/README.md`.

`RewriteLaws.lean` states the laws over relations as sets of pairs.
The complement is typed as Ampersand types it: `-s` is `V − s`,
where `V` is the universe of the signature.
The premise `r ⊆ V` is the typing premise that every well-typed term satisfies.

It proves:

- **`absorb1`:** `(r ∪ s) ∩ s = s`.
- **`absorb2`:** `(r ∪ ¬s) ∩ s = r ∩ s`.
  This is the corrected second law.
  The law used to read `(r ∪ ¬s) ∩ s = s − r`.
- **`absorb2_old_refuted`:** that former law is false.
  The witness is `r = s = V = {(a,b)}`:
  the left-hand side contains `(a,b)` and the right-hand side is empty.
- **`absorb3`:** `(r ∩ s) ∪ s = s`.
- **`absorb4`:** `(r ∩ ¬s) ∪ s = r ∪ s`, for `r ⊆ V`.
- **`absorb5`:** `(r ∩ s) ∪ ¬s = r ∪ ¬s`, for `r ⊆ V`.

Laws 1, 2 and 3 hold without the typing premise; laws 4 and 5 need it,
because they join a complement to a term by union.

## Build

```
cd proofs/rewriteLaws
lake build
lake env lean Axioms.lean
```

Lean 4.34.1, core library only, no `sorry`.
`Axioms.lean` prints the axioms each theorem depends on;
`lake-build.txt` records the last observed build.
The theorems use `propext` and `Quot.sound` (through `funext`),
and laws 4 and 5 use `Classical.choice` through the excluded middle.

## Boundary

The claim is about the laws as mathematical identities.
Not proved: that the parser `parseRule` and the rewriter `dSteps` apply a law as the identity states it, and the termination of the confluence analysis `dfProofs` that uses the set, which the code marks as unsolved.
The set serves the confluence analysis of `ampersand proofs`;
the normalisation of rules into SQL (`conjNF`) does not use it.
