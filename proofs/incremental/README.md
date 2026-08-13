# Machine-checked proofs for the delta calculus

Isabelle/HOL proofs of the proof obligations in
`memorybank/incremental-evaluation/delta-calculus.md`, section 5.
Session `Incremental_Delta`, parent `HOL`, no axioms beyond HOL, no `sorry`.

Build:

```sh
/opt/homebrew/bin/isabelle build -D proofs/incremental
```

The build reuses the prebuilt HOL heap (see `proofs/spike/install.sh`) and takes
a few seconds.

## Modelling

- `ZSet.thy` models a Z-set over domain `'p` as a function `'p ⇒ int`; the
  implementation invariant "no key maps to 0" corresponds to the finite-support
  predicate `finsupp`. Binary Z-sets are Z-sets over a pair type.
  Composition `zcomp M a b` sums over an explicit finite middle set `M`;
  `zcomp_cover_indep` proves the result independent of the choice of `M` as
  long as `M` covers `midsupp a b`, and `finite_midsupp` proves such a finite
  cover exists for finite-support inputs. This mirrors the implementation,
  which sums over the keys of source-indexed maps.
- `Desugar.thy` uses the set-level house style of
  `proofs/spike/Ampersand_RA.thy`: one universe type `'a`, concepts as sets,
  a relation with signature `[A*B]` as `r ⊆ A×B`, and the TYPED complement
  `cpl A B r = A×B - r`. The adequacy condition of `delta-calculus.md`
  (every relation lies inside the `V` of its signature) appears as the typing
  premises of the lemmas.

## Obligation → lemma

| Obligation | Lemma | Theory |
| --- | --- | --- |
| S1 left residual `l/r = -(-l ; r~)` | `S1_lrs_as_antijoin` (+ `S1_quantifier_domain`) | `Desugar.thy` |
| S2 right residual `l\r = -(l~ ; -r)` | `S2_rrs_as_antijoin` (+ `S2_quantifier_domain`) | `Desugar.thy` |
| S3 diamond as conjunction of both directions | `S3_dia_as_conjunction` | `Desugar.thy` |
| S4 relative addition `l!r = -(-l ; -r)` | `S4_rad_as_double_complement` | `Desugar.thy` |
| S5 typed complement; `V` absorbs | `S5_cpl_as_difference`, `S5_cpl_involution`, `S5_V_absorbs_isc`, `S5_V_absorbs_uni` | `Desugar.thy` |
| Z1 abelian group | `Z1_zplus_assoc`, `Z1_zplus_comm`, `Z1_zplus_zero_left/right`, `Z1_zplus_inverse`, `Z1_zminus_is_plus_neg`, `Z1_finsupp_closure` | `Delta.thy` |
| Z1 flip linear (D1) | `Z1_flip_additive`, `Z1_flip_neg`, `Z1_flip_minus`, `Z1_flip_zero`, `Z1_flip_involution` | `Delta.thy` |
| Z2 pointwise product delta (D5) | `Z2_pointwise_bilinear_delta` | `Delta.thy` |
| Z3 composition delta (D5) | `Z3_zcomp_bilinear_delta` (+ `Z3_zcomp_linear_left/right`, `zcomp_cover_indep`, `finite_midsupp`) | `Delta.thy`, `ZSet.thy` |
| Z4 weighted cartesian product delta (D5) | `Z4_zprod_bilinear_delta` | `Delta.thy` |
| Z5 distinct delta (D6) | `Z5_distinct_delta`, `Z5_H_zero_crossing`, `Z5_H_support` (+ `Z5_H_finsupp`) | `Delta.thy` |
| B1 `distinct(a+b)` = union | `B1_distinct_plus_is_union` | `Bridge.thy` |
| B2 `distinct(a-b)` = difference | `B2_distinct_minus_is_difference` | `Bridge.thy` |
| B3 `a⊙b` = intersection | `B3_mul_is_intersection` | `Bridge.thy` |
| B4 `distinct(a;b)` = relational composition | `B4_distinct_zcomp_is_relcomp` | `Bridge.thy` |
| B5 weighted product + distinct = cartesian product | `B5_distinct_zprod_is_cartesian` | `Bridge.thy` |
| B6 flip = converse | `B6_flip_is_converse` | `Bridge.thy` |

Notes per obligation:

- **Z5** defines `H Z dZ x = distinct(Z+dZ) x - distinct Z x`, so
  `Z5_distinct_delta` holds by construction. The substantive claims are
  `Z5_H_zero_crossing` (H equals the explicit sign-change case form that the
  implementation computes) and `Z5_H_support` (the work is bounded by the
  incoming delta).
- **S1/S2** come with a `*_quantifier_domain` lemma: under the adequacy
  premise, quantifying `z` over the concept `C` equals quantifying `z` over
  the rows of the relation, which is what `fullContents` does.
- **S4** needs no typing premise at all, because both complements in
  `-(-l ; -r)` are typed.

## What is NOT proved

- **D7 recompute nodes** (`Kl0`, `Kl1`, `Bin`): they re-run the specification
  on the new input and emit `new - old`, so they are correct by construction;
  there is nothing to prove beyond the definition.
- **The structural-induction glue** (stretch goal: deep embedding of the core
  term language with the per-node step function and the invariant
  `S = semantics(t)`): not attempted in this session. Until it lands, the glue
  argument is prose (delta-calculus.md, section 4) plus the per-transaction
  oracle check `ampersand incremental-bench --verify`.
- The **`EEqu` finding** of delta-calculus.md (fullContents computes the union
  of the two inclusions): deliberately mirrored, not proved "correct".

## Working notes for future proof sessions

Two Isabelle gotchas cost bisection time in this session; the proofs are
written the way they are to avoid them:

- `blast`/`meson`/`metis`/`force` can diverge without failing in batch builds
  on two goal shapes seen here: `finite`-goals fed subset lemmas (the subset
  unfolds into membership logic), and search over a conditional witness lemma
  that is not yet instantiated. The theories therefore use explicit
  `rule finite_subset[OF …]` chains and instantiate witnesses first
  (`zcomp_nonzero_witness[OF nz]`).
- `fix p` followed by `obtain x z where "p = (x, z)"` before any typed use of
  `p` gives `x`, `z` fresh rigid types, which surfaces later as a type-
  unification error at an unrelated `have`. The theories state the `show`
  first and then open `proof (cases p)` / `case (Pair x z)`.
