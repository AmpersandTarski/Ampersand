# Machine-checked proofs for the delta calculus

Isabelle/HOL proofs of the proof obligations in
`memorybank/incremental-evaluation/delta-calculus.md`, sections 5 and 7 —
including the whole-circuit induction (`Circuit.thy`) and the population
mirror (`Population.thy`) that issue #1683 asked for, and the candidate
calculus of the delta-SQL route (`Candidates.thy`, K-obligations, register
claim PRF-7).
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
- `Circuit.thy` is the deep embedding of the evaluator's circuit language:
  one constructor per `Kind` of `src/Ampersand/FSpec/Incremental.hs`, with the
  node states inline (pre-distinct integrals, the flipped copy for
  composition, the four projection integrals for products). The environment
  holds per relation the pre-distinct pair integral and per concept the
  occurrence integral; a transaction adds to both linearly, as `applyTx`
  does. Every construct without a proven delta rule — fallback terms, `Mp1`,
  `Bin`, `Kl0`, `Kl1` — is a *specification node* `CSpec F`: its step
  re-evaluates `F` on the new environment and emits `new − old` (D7), which
  keeps the theorems' scope honest: they cover every circuit the compiler
  builds. The step function `nxt`/`dlt` is written with the proven
  expansions (Z2/Z3/Z4 asymmetric form, H for every distinct state).
- `Population.thy` models the concept-population bookkeeping in a locale
  over finite sets of declared relations and concepts with an abstract cone
  function `bel` (implementation: `c : smallerConcepts c`).
- `Desugar.thy` uses the set-level house style of
  `proofs/spike/Ampersand_RA.thy`: one universe type `'a`, concepts as sets,
  a relation with signature `[A*B]` as `r ⊆ A×B`, and the TYPED complement
  `cpl A B r = A×B - r`. The adequacy condition of `delta-calculus.md`
  (every relation lies inside the `V` of its signature) appears as the typing
  premises of the lemmas.
- `Candidates.thy` (same set-level house style) is a deep embedding of the
  supported term class of `DeltaTerms.hs` (branch `delta-sql`): relation
  leaves plus state-independent leaves (`Cst`, covering `EDcI`/`EDcV`/
  `EMp1`/`EBin` under the concept-fallback assumption OK-9), closed under
  union, intersection, difference, composition, converse and typed
  complement. The locale `delta_transaction` fixes an old state, a new state
  and per relation a delta set, with one assumption: every changed pair is
  in its relation's delta set. The envelopes `W`/`N` are one recursion with
  a polarity flag, mirroring `widen`/`narrow`; the candidate set `candg` is
  parameterised by the leaf assignment so that the per-relation
  decomposition is a theorem, not a remark.

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
| C1 a step preserves the state invariant (structural induction over all node kinds) | `C1_step_preserves_state` | `Circuit.thy` |
| C2 specification (D7) nodes are correct after every step, unconditionally | `C2_step_establishes_spec` | `Circuit.thy` |
| C3 a well-formed circuit's clipped outputs are the set semantics of its terms | `C3_output_is_semantics`, `C3_setof_output` | `Circuit.thy` |
| C4 the all-zero base state is well-formed | `C4_backfill_base` | `Circuit.thy` |
| C5 every state reachable by ≥1 transaction from the base (backfill included) is correct | `C5_run_correct` (+ `run_wf`, `sem_run`) | `Circuit.thy` |
| P1 the occurrence integral is linear in the transaction (D4; `occDelta` is exact) | `P1_occ_linear` | `Population.thy` |
| P2 its carried set equals the `atomValuesOf` set | `P2_occ_mirrors_atomValuesOf` | `Population.thy` |
| P3 the population-set delta is the zero-crossing H (`cptSetDelta` = `bagH`) | `P3_popset_delta` | `Population.thy` |
| P4 a term relation's integral sums its feeders linearly | `P4_feed_linear` | `Population.thy` |
| P5 its carried set is the `pairsOf` union | `P5_feed_contents` | `Population.thy` |
| W/N envelope invariant (W bounds old∪new from above, N old∩new from below) | `WN_envelope` (+ `W_upper`, `N_lower`) | `Candidates.thy` |
| K1 union candidate rule is complete | `K1_uni` | `Candidates.thy` |
| K2 intersection candidate rule is complete | `K2_isc` | `Candidates.thy` |
| K3 difference candidate rule is complete | `K3_dif` | `Candidates.thy` |
| K4 composition candidate rule is complete | `K4_cps` | `Candidates.thy` |
| K5 converse candidate rule is complete | `K5_flp` | `Candidates.thy` |
| K6 typed-complement candidate rule is complete | `K6_cpl` | `Candidates.thy` |
| K whole-term completeness (every changed pair is a candidate) | `K_complete` | `Candidates.thy` |
| K per-relation decomposition (union of per-relation queries = global candidate set) | `K_per_relation` (+ `candg_UN`, `K_touched_cover`) | `Candidates.thy` |

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
- **C1-C5** split the invariant in two: `wfs` (states are the right functions
  of the children's outputs; holds at the all-zero base) and `sholds`
  (specification nodes' outputs match their semantics; established by any
  step, no premise needed). That split is the formal shape of "backfill is
  the first transaction": the base state need not know the constants (`Mp1`
  content, `I[ONE]`) — the first step emits them. Working out the base case
  exposed that the implementation pre-seeded ONE's population instead of
  letting it travel through `tx0`, which left `I[ONE]`/`V[..*ONE]` circuits
  permanently empty; fixed on this branch and pinned by the engine property.
- **P2**'s set-discipline premise (raw stores hold weights 0/1) is the
  engine's lockstep contract: transaction weights are ±1, insert only absent
  pairs, delete only present ones.
- **K1-K6** were labelled C1-C6 until 2026-08-14; renamed because C1-C5
  already name the whole-circuit obligations above. Completeness is the only
  property the delta-SQL route needs (OK-8, delta-scoped re-evaluation): the
  runtime settles every candidate pair by re-running the conjunct's own
  violation predicate, so a too-large candidate set costs time, never
  correctness. No typing or adequacy premises are needed: the typed
  complement subtracts from a fixed rectangle `A×B`, which drops out of the
  symmetric difference.

## What is NOT proved

- **The dirty-flag shortcuts** of the implementation (a fallback node skips
  recomputation when none of its relations or concepts changed; `Kl0`/`Kl1`
  skip when the child emitted nothing). The model's specification nodes
  recompute every step, so the theorems do not cover the soundness of the
  skip conditions; the per-transaction oracle and the engine property do.
- **Kleene nodes' reading of their child.** `Circuit.thy` models a closure
  node as a specification node over the *semantics* of its child term; the
  implementation computes the closure of the child's *maintained output*.
  Under the proved invariant the two coincide, but the implementation's
  wiring of that equality is covered by the oracle, not by the induction.
- **The Haskell code itself.** The theorems are about the model; the
  QuickCheck bridge (`Ampersand.Test.Incremental.Properties`, run by
  `stack test`) binds the real `ZSet` functions and the engine to the lemmas
  on every build. Verified extraction was considered and rejected (#1683).
- The **`EEqu` finding** of delta-calculus.md (fullContents computes the union
  of the two inclusions): deliberately mirrored, not proved "correct".
- **The candidate side of the SQL route** (`Candidates.thy`): the Haskell
  functions `widen`/`narrow`/`candidateTerms` that mirror the calculus
  (exercised by `ampersand incremental-bench --sql`), the SQL compilation of
  the candidate terms (shared with the full queries; guarded by
  `ampersand validate`), and the delta-table contract itself — that the
  runtime records every changed pair — which belongs to the protocol half of
  register claim PRF-6. Constancy of the concept populations is an
  assumption of the theorems, discharged operationally by the
  concept-affected fallback (OK-9).

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
- The pointwise `*_apply [simp]` rules of `ZSet.thy` make simp unfold a
  partially applied operator into a raw lambda (simp eta-expands such rules),
  after which compositional lemmas about `finsupp`/`isSet`/function equality
  no longer match. `Circuit.thy` therefore removes them from the simp set
  once the reasoning moves to whole Z-set values, and re-adds them by name in
  the few pointwise steps. Related: a partially instantiated `fun_cong` as a
  simp rule sends the simplifier into a divergent search (observed as the
  "Unable to increase stack" batch failure); apply it with `rule` instead.
