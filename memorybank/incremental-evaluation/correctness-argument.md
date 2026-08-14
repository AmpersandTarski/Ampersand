# The correctness argument

How the machine-checked lemmas of `proofs/incremental/` (session
`Incremental_Delta`) combine into a correctness argument for the incremental
evaluator, where that argument is currently weaker than a proof, and what work
follows from those gaps. Posted as comment 5 on issue
[#1682](https://github.com/AmpersandTarski/Ampersand/issues/1682).

## Claim

After every transaction, the output of each conjunct's circuit equals the
violation set that full re-evaluation (`fullContents` over the current
population) computes for that conjunct's term. `fullContents` is the semantic
ground truth of this argument; it is also the reference that `ampersand
validate` holds the generated SQL against, so the claim places the incremental
evaluator on the same footing as the existing query generator.

## The argument, in six steps

**1. Desugaring preserves the semantics (S1–S5).** The compiler rewrites each
violation term before building a circuit: residuals, diamond and relative
addition become antijoin shapes, and complements are pushed inward with the
target signature carried explicitly. Lemmas `S1_lrs_as_antijoin`,
`S2_rrs_as_antijoin`, `S3_dia_as_conjunction` and
`S4_rad_as_double_complement` prove these identities in the typed set model
(`cpl A B r = A×B − r`), under adequacy premises of the form `r ⊆ B×C`. The
`S1/S2_quantifier_domain` lemmas connect the `∀z∈C` reading to the row-based
quantification that `fullContents` actually performs. `S5_cpl_involution`
licenses double-negation elimination — only at equal signatures, which is the
discipline `pushNeg` enforces and the `signLeq` guard protects in the
difference absorption.

**2. Every node's delta rule is proven (Z1–Z5).** Linear nodes pass deltas
through unchanged: the group laws and flip-linearity of Z1. Bilinear nodes
update by the asymmetric expansion `new(a)⊗new(b) − old(a)⊗old(b) =
Δa⊗old(b) + new(a)⊗Δb`, proven as `Z2_pointwise_bilinear_delta`
(intersection), `Z3_zcomp_bilinear_delta` (composition; with
`zcomp_cover_indep` and `finite_midsupp` showing the finite middle set the
implementation iterates over is sound and canonical) and
`Z4_zprod_bilinear_delta` (cartesian products, including `V` over concept
populations). Nodes with pre-distinct state emit through the zero-crossing
function: `Z5_distinct_delta` gives `distinct(Z+ΔZ) = distinct(Z) + H(Z,ΔZ)`,
`Z5_H_zero_crossing` proves H equals the sign-change case form the code
computes, and `Z5_H_support` bounds the emitted work by the incoming delta —
the inequality behind the flat microsecond column in the measurements.

**3. Clipped outputs are the set semantics (B1–B6).** On set-valued inputs
(all weights 0/1), each Z-operation followed by `distinct` computes the
corresponding set operation: union (B1), difference (B2), intersection (B3,
no distinct needed), relational composition (B4, Isabelle's `O`), cartesian
product (B5), converse (B6). So a node whose children carry the correct sets
produces the correct set.

**4. Fallback nodes run the specification (D7).** Constructs without a proven
delta rule — and Kleene closures — re-evaluate their sub-term with
`fullContents` on the current population and emit `new − old`. There is
nothing to prove: they *are* the reference semantics, at recompute cost.

**5. The step preserves the invariant (structural induction).** By induction
over the circuit: leaves receive the transaction's deltas (relations directly;
concept populations via the linear occurrence sums that mirror
`atomValuesOf`); internal nodes combine correct child deltas by step 2 and
read off correct sets by step 3; fallbacks restore correctness outright by
step 4. Hence if every node was correct before the transaction, every node is
correct after it.

**6. The base case is the first step.** Circuits start at the group zero
(empty integrals, empty outputs — trivially correct for the empty population),
and backfill applies the initial population as an ordinary transaction through
the same step function. Steps 5 and 6 together give the claim for every
reachable state.

## Where the argument is weaker than a proof

- **Step 5 is machine-checked since #1683.** `Circuit.thy` carries the deep
  embedding, the step function written with the proven expansions, and the
  theorems C1-C5: a step preserves the invariant, and every state reachable
  from the all-zero base by at least one transaction (backfill included) has
  every node's clipped output equal to its term's set semantics. What the
  induction deliberately does not cover, the README records: the dirty-flag
  skip conditions and the Kleene nodes' reading of their child run on the
  oracle's evidence, not on proof.
- **The code is bound to the model per build, not by refinement proof.**
  `ZSet.thy` models Z-sets as integer-valued functions; the implementation
  uses nested maps with a no-zero invariant. Since #1683,
  `Ampersand.Test.Incremental.Properties` (in `stack test`) re-checks one
  QuickCheck property per proven lemma against the actual
  `bagApply`/`relH`/`composeDeltaOld`/`composeFlipDelta`/... functions, plus
  an engine-level oracle property on random transaction streams. That is a
  per-build statistical link; a refinement proof remains out of scope.
- **The adequacy premises are proven sufficient, not proven satisfied.** The
  S-lemmas assume every relation lies inside the `V` of its signature. The
  population mirror itself is now proven (`Population.thy`, P1-P5: the
  occurrence sums are linear and carry exactly the `atomValuesOf` set, ISA
  cones included) under the engine's set-discipline contract on raw stores —
  a contract the transaction generator of the engine property enforces and
  the runtime must honour.
- **The base-case analysis found two real defects.** Working out C4 (the
  all-zero base) exposed that ONE's population was pre-seeded instead of
  travelling through the backfill transaction, leaving `I[ONE]`/`V[..*ONE]`
  circuits permanently empty; the engine property then found that the
  feeder/cone wiring, derived from initially populated relations, silently
  dropped transactions on initially empty ones. Both are fixed and pinned by
  the property suite — the argument's own machinery caught them, which is
  the argument working as intended.
- **Oracle and fallback share one implementation.** Fallback nodes and the
  verification oracle both call `fullContents`, so an error in `fullContents`
  itself would pass unnoticed by `--verify` (the same blind spot `ampersand
  validate` has). The S-series lemmas reduce this surface for residuals,
  diamond and relative addition by proving the reference semantics equal to
  independent relation-algebra identities; the remaining constructs rest on
  `fullContents` alone. The `EEqu` case of `fullContents` (evaluated as the
  union of the two inclusions) is mirrored, deliberately unjudged, and
  unreachable from conjunct terms.
- **Scope of the measurements.** The timings compare two in-Haskell
  evaluators on synthetic populations; the in-Haskell full evaluation
  overstates what MariaDB spends on composition. Production claims wait for
  the end-to-end benchmark of Phase 4.

## Follow-up work these gaps define

1. **`Circuit.thy`** — done (#1683): C1-C5 in `proofs/incremental/Circuit.thy`.
2. **Phase 0.1** — done (#1683): `Ampersand.Test.Incremental.Properties` in
   `stack test`.
3. **The population mirror** — done (#1683): P1-P5 in
   `proofs/incremental/Population.thy`.
4. **Incremental Kleene closures (Phase 5)** — `proofs/kleene/
   IncrementalDelete.thy` already proves the deletion-side facts; wiring it
   into the engine replaces the closure recompute nodes.
5. **Phase 3 (delta SQL)** — the S/Z/B lemmas carry over unchanged; the new
   proof surface is the correspondence between the generated SQL and the Z-set
   operations, plus the end-to-end Phase 4 measurement.
