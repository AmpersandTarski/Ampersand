# The delta calculus for Ampersand terms (Phase 1)

Status: implemented by `Ampersand.FSpec.Incremental` (Phase 2); proof obligations
in `proofs/incremental/`. Semantic ground truth: `fullContents`
(`src/Ampersand/FSpec/ToFSpec/Populated.hs:61`) — every rule below preserves its
observable behaviour, verified per transaction by the `--verify` oracle mode of
`ampersand incremental-bench`.

## 1. Z-sets

A Z-set over a domain `p` is a finite-support map `p → ℤ`; we write `Z[p]`.
The implementation is `Map p Int` with the invariant that no key maps to 0.
`Z[p]` is an abelian group under pointwise addition. `distinct : Z[p] → Z[p]`
clips: `distinct m x = 1` if `m x > 0`, else `0`. A Z-set is a *set* when every
weight is exactly 1.

Pairs are `(AAtomValue, AAtomValue)`; binary Z-sets are stored indexed by source
(`Map a (Map b Int)`), so rows are cheap to enumerate.

## 2. The core language

The desugaring `core : Expression → CoreTerm` eliminates the derived operators.
Core terms:

```
t ::= Rel r | I c | V sgn | Mp1 v c | Bin op sgn
    | Flp t | Uni t t | Dif t t | Isc t t | Cps t t | Prd t t
    | Kl0 t | Kl1 t
```

Desugaring rules (mirroring `fullContents` case by case):

| Expression | Core |
|---|---|
| `EInc (l,r)` | `core (ECpl l .\/. r)` |
| `EEqu (l,r)` | `core ((l .|-. r) .\/. (r .|-. l))` — mirrors `Populated.hs:82`; see the note below |
| `ECpl x` | `Dif (V (sign x)) (core x)` |
| `ELrs (l,r)` (`l/r`) | `core (ECpl (ECpl l .:. EFlp r))` |
| `ERrs (l,r)` (`l\r`) | `core (ECpl (EFlp l .:. ECpl r))` |
| `EDia (l,r)` (`l<>r`) | `core (ELrs l r ./\. ECpl (l .:. ECpl (EFlp r)))` — see the derivation below |
| `ERad (l,r)` (`l!r`) | `core (ECpl (ECpl l .:. ECpl r))` |
| `EBrk x` | `core x` |
| others | homomorphic |

Derivations against `fullContents` (all complements are typed, i.e. relative to
`V` of the signature, whose atoms come from the concept populations):

- `x (l/r) y ⟺ ∀z: y r z → x l z ⟺ ¬∃z: (x,z)∉l ∧ (y,z)∈r ⟺ (x,y) ∉ (-l ; r~)`.
- `x (l\r) y ⟺ ∀z: z l x → z r y ⟺ (x,y) ∉ (l~ ; -r)`.
- `x (l<>r) y ⟺ ∀z: (x l z ⟷ z r y)`, i.e. the conjunction of `∀z: z r y → x l z`
  (that is `l/r~`, the first conjunct) and `∀z: x l z → z r y` (that is `-(l;-r~)`,
  the second conjunct).
- `x (l!r) y ⟺ ∀z: x l z ∨ z r y ⟺ (x,y) ∉ (-l ; -r)`.

These identities hold *because* every relation is contained in the `V` of its
signature and the concept populations contain every atom that occurs in any
relation (`atomValuesOf`, `Populated.hs:30`). This adequacy condition is part of
the proof obligations (S-series).

**Note on `EEqu`.** `fullContents` computes `EEqu (l,r)` as the *union* of the
two inclusions (`Populated.hs:82`), which is `V` wherever either inclusion
holds — for pair semantics of `l = r` one would expect the *intersection*. The
conjunct path never sees `EEqu` (rules are normalized by `conjNF` first), so the
benchmark is unaffected; the desugaring mirrors the existing behaviour so that
the oracle comparison stays meaningful. Reported as a finding, deliberately not
changed here.

## 3. The circuit and its state

Every core term becomes a circuit node. Each node stores its current **output
set** `S` (a Z-set with all weights 1 — the invariant). Nodes whose raw Z-result
can leave {0,1} additionally store the **pre-distinct integral** `Z`, with
`S = distinct(Z)`:

| Node | raw Z-rule | needs `Z`-state | extra state |
|---|---|---|---|
| `Rel r` | sum of feeding populations | yes (multiple populations may feed one relation) | — |
| `I c` | pop set of `c`, diagonal | no (population sets are maintained globally) | — |
| `V sgn` | `pop(src) ×w pop(tgt)` | yes | — |
| `Mp1 v c` | constant singleton (empty for the SESSION guard) | no | — |
| `Bin op sgn` | recompute node | no | previous output |
| `Flp t` | transpose (linear) | no | — |
| `Uni a b` | `a + b` | yes | — |
| `Dif a b` | `a - b` | yes | — |
| `Isc a b` | `a ⊙ b` (pointwise product) | no (0/1 × 0/1) | — |
| `Cps a b` | `(a ; b)(x,y) = Σ_m a(x,m)·b(m,y)` | yes (witness counts) | flipped copy of `a`'s output |
| `Prd a b` | `dom_w(a) ×w cod_w(b)` | yes | unary integrals `dom_w`, `cod_w` |
| `Kl0/Kl1 t` | recompute node (`transClosureMap`) | no | previous output |

`Prd` follows `fullContents`: the product of the atoms *occurring* in `a`'s
domain and `b`'s codomain (not the concept populations). `dom_w(a)(x) = Σ_y a(x,y)`
is a linear projection, so `Prd` is bilinear over two linear images.

**Concept populations** are maintained globally, exactly as `atomValuesOf`
derives them: for concept `c`, the unary integral
`popZ(c) = Σ_{r : source r ≼ c} leftOccurrences(r) + Σ_{r : target r ≼ c} rightOccurrences(r) + Σ_{explicit atom populations ≼ c} atoms`
(`≼` = concept or one of its smaller concepts), and `popSet(c) = distinct(popZ(c))`.
Every term of these sums is linear in the transaction delta. `DISJT/UNION/ISECT`
concepts follow `atomValuesOf` (intersection resp. union of member populations)
as recompute-style derived sets.

## 4. The delta rules

A transaction delta is a Z-set per relation (pairs, weights ±1) plus a Z-set per
concept (explicit atom insertions/deletions). One step propagates deltas
bottom-up; each node emits its **output-set delta** and updates its state.

Linear nodes (D-series, their own delta):

- **D1** `Δ(Flp t) = flip(Δt)`
- **D2** `ΔZ(Uni) = Δa + Δb`
- **D3** `ΔZ(Dif) = Δa - Δb`
- **D4** `ΔZ(Rel r) = Σ feeding population deltas`, `Δ popZ` likewise (linear sums)

Bilinear nodes use the exact asymmetric expansion (**D5**, no `Δa·Δb` term):

```
new(a)⊗new(b) − old(a)⊗old(b)  =  Δa ⊗ old(b)  +  new(a) ⊗ Δb
```

applied with `⊗` = pointwise product (`Isc`), composition (`Cps`), weighted
cartesian product (`Prd`, `V`). For `Cps` the two terms are computed by index
lookups: `Δa ; old(b)` iterates `Δa` against `b`'s source-indexed output;
`new(a) ; Δb` iterates `Δb` against the node's incrementally maintained flipped
copy of `a`'s output. Work is proportional to the deltas times the matching
rows, never a full scan.

Distinct nodes (**D6**): given the stored integral `Z` and an incoming `ΔZ`,
the output-set delta is the zero-crossing function

```
H(Z, ΔZ)(x) = +1  if Z(x) ≤ 0 < Z(x)+ΔZ(x)
              −1  if Z(x) > 0 ≥ Z(x)+ΔZ(x)
               0  otherwise
```

with `support(H) ⊆ support(ΔZ)`, so the work is bounded by the incoming delta.

Recompute nodes (**D7**, fallback — correct by construction): `Kl0`, `Kl1`
(closure of the child's new output via `transClosureMap`) and `Bin`
(re-filtered when a source/target population changed). Their delta is
`new − old`. They run only when their input delta is non-empty. Incremental
closure maintenance (via `proofs/kleene/IncrementalDelete.thy`) is Phase 5.

## 5. Proof obligations (Isabelle/HOL, `proofs/incremental/`)

Set level, finite universe, typed relations (`r ⊆ A×C`, `s ⊆ B×C`, …),
complement relative to the signature's `V`:

- **S1** `l/r = −(−l ; r~)` under the adequacy condition.
- **S2** `l\r = −(l~ ; −r)`.
- **S3** `l<>r = (∀-both-directions) = conjunction of S1-style terms`.
- **S4** `l!r = −(−l ; −r)`.
- **S5** complement as difference from `V`; `V` absorbs every well-typed relation.

Z-set level:

- **Z1** `Z[p]` is an abelian group; `+`, `−`, `flip` are linear (their own delta).
- **Z2** pointwise product: `(a+da)⊙(b+db) = a⊙b + da⊙b + (a+da)⊙db`.
- **Z3** composition: `(a+da);(b+db) = a;b + da;b + (a+da);db` (finite support).
- **Z4** weighted cartesian product of unary Z-sets: same bilinear expansion.
- **Z5** `distinct(Z+ΔZ) = distinct(Z) + H(Z,ΔZ)` and `support(H(Z,ΔZ)) ⊆ support(ΔZ)`.

Bridge (inputs are sets, i.e. 0/1-weighted):

- **B1** `distinct(a+b)` = set union; **B2** `distinct(a−b)` = set difference;
- **B3** `a⊙b` = set intersection (no distinct needed);
- **B4** `distinct(a;b)` = relational composition;
- **B5** unary weighted product + distinct = cartesian product of the underlying sets;
- **B6** `flip` = converse.

The former stretch goal — a deep embedding of the core language with the step
function and the invariant `S = semantics(t)` preserved by every step — is
proven since issue #1683: obligations C1-C5 in `proofs/incremental/Circuit.thy`
(whole-circuit induction, backfill as the first step), with the population
bookkeeping proven as P1-P5 in `Population.thy`. The per-transaction oracle
check in `incremental-bench --verify` is thereby a diagnostic, and the
QuickCheck bridge in `stack test` binds the implementation to these lemmas on
every build.

## 6. What falls outside this phase

Symbolic delta *terms* (Δ as `Expression`, OK-2) for SQL generation are Phase 3;
this phase implements the circuit interpreter that the benchmark and the oracle
run in pure Haskell. Incremental Kleene closure and incremental `EBin` are
Phase 5 (D7 covers them correctly, at recompute cost).

## 7. The candidate calculus for delta SQL (Phase 3, issue #1684)

The SQL side uses delta-scoped re-evaluation (OK-8): the cache update re-runs
the existing violation predicate on a candidate set, so only candidate
**completeness** matters — no weights, no intermediate state. With `Δs` the
delta table of relation s (the pairs touched by the transaction; empty when s
is untouched), the widened and narrowed envelopes bound the relation's old and
new state per position polarity:

```
W(s) = s ∪ Δs                N(s) = s − Δs
W(a∪b) = W(a) ∪ W(b)         N likewise
W(a∩b) = W(a) ∩ W(b)         N likewise
W(a−b) = W(a) − N(b)         N(a−b) = N(a) − W(b)
W(a;b) = W(a) ; W(b)         N likewise      W(a~) = W(a)~
W(-a)  = -(N(a))             N(-a)  = -(W(a))
W = N = id on I, V, atom literals, EBin
```

The candidate terms for a change in r (each a superset of the pairs whose
membership may have changed via one occurrence of r):

```
D(s)      = [Δr]  if s = r, else []
D(a∪b)    = D(a) ++ D(b)
D(a∩b)    = [d ∩ W(b) | d∈D(a)] ++ [W(a) ∩ d | d∈D(b)]
D(a−b)    = [d − N(b) | d∈D(a)] ++ [W(a) ∩ d | d∈D(b)]
D(a;b)    = [d ; W(b) | d∈D(a)] ++ [W(a) ; d | d∈D(b)]
D(a~)     = [d~ | d∈D(a)]         D(-a) = D(a)
D = []    on I, V, atom literals, EBin
```

Any other constructor makes the conjunct unsupported: it keeps full
re-evaluation. Concept-population changes (which move `I`, `V` and `EBin`)
also keep full re-evaluation, via the existing concept-affected trigger (OK-9).

**Proof obligations (K-series, machine-checked 2026-08-14):** for every rule
above, if a pair's membership in the term differs between the old and the new
database state, then the pair is in the union of the candidate terms evaluated
over the new state plus the delta tables. K1 union, K2 intersection,
K3 difference, K4 composition, K5 converse, K6 complement. Proven in
`proofs/incremental/Candidates.thy` (branch `incremental-evaluation`, register
claim PRF-7): one lemma per rule, the W/N envelope invariant, the whole-term
theorem `K_complete`, and the per-relation decomposition `K_per_relation`
that justifies taking the union of per-relation candidate queries. The series
was labelled C1..C6 until 2026-08-14; it is renamed to K (kandidaat) because
C1..C5 name the whole-circuit obligations of `Circuit.thy`. Two tests bind
the code to the lemmas: the QuickCheck bridge
`Ampersand.Test.Incremental.CandidateProperties` (in `stack test`, one
property per lemma over the real `widen`/`narrow`/`candidateTerms`, with an
independent reference evaluator) re-checks the correspondence on every
build, and the delta-SQL harness (`ampersand incremental-bench --sql`) binds
the generated SQL to it against a real MariaDB.
