# DBSP: Automatic Incremental View Maintenance for Rich Query Languages — study note

| | |
|---|---|
| **Authors** | Mihai Budiu, Frank McSherry, Leonid Ryzhyk, Val Tannen |
| **Venue** | arXiv 2203.16684 (2022); published version in PVLDB 16(7), VLDB 2023 — **Best Research Paper award**; 2024 ACM SIGMOD Research Highlights |
| **Links** | [abstract](https://arxiv.org/abs/2203.16684) · [PDF](https://arxiv.org/pdf/2203.16684) · [HTML](https://arxiv.org/html/2203.16684v1) · [VLDB version](https://docs.feldera.com/vldb23.pdf) |
| **Implementation** | [Feldera](https://github.com/feldera/feldera) (open source, Rust) |

Section numbers below refer to the arXiv v1 rendering.

## 1. Z-sets: relations whose elements carry integer weights (§4.1)

A **Z-set** (also called Z-relation) over a domain `A` is a function `A → Z` with
*finite support*: only finitely many elements map to a non-zero integer. The paper
writes the type as `Z[A]` and suggests reading a Z-set as a key-value map: keys are
records, values are integer **weights** (multiplicities).

Because `Z` is an abelian group, `Z[A]` is one too, with pointwise operations:

```
(f + g)(x) = f(x) + g(x)        (-f)(x) = -f(x)        0 = the empty Z-set
```

Three special classes matter (Definitions 4.1–4.3):

- a Z-set is a **set** when every present element has weight exactly 1;
- it is **positive** (a bag) when every weight is > 0;
- `distinct(m)[x] = 1 if m[x] > 0, else 0` — the function that collapses any
  Z-set back to a set, discarding negative-weight elements.

**Why this matters.** In ordinary set semantics, "the difference between yesterday's
database and today's" is not itself a relation — you need two sets (inserted, deleted)
and bookkeeping around them. In `Z[A]` a **delta is a first-class value**: an insertion
is an element with weight `+1`, a deletion the same element with weight `-1`, and
"apply the change" is just group addition. Every operator can now be asked "what do you
do to a difference?", and calculus-style reasoning (differentiation, integration,
linearity) becomes available. This single move is what makes the whole incremental
theory compositional.

## 2. The stream calculus (§2)

- **Stream** (Def 2.1): a function `N → A`, written `s ∈ S_A`, with `s[t]` the value
  at time step `t`. Time steps are transaction boundaries, not wall-clock time.
- **Lifting** (Def 2.3): a scalar function `f : A → B` becomes a stream operator
  `↑f` applied pointwise: `(↑f)(s)[t] = f(s[t])`. Lifting distributes over
  composition: `↑(f ∘ g) = ↑f ∘ ↑g` (Prop 2.4).
- **Delay** `z^-1` (Def 2.5): `z^-1(s)[0] = 0_A`, `z^-1(s)[t] = s[t-1]` for `t ≥ 1`.
  The name comes from the z-transform in signal processing.
- **Time-invariant** (Def 2.6): `S(z^-1(s)) = z^-1(S(s))` — the operator behaves the
  same at every time step. **Causal** (Def 2.8): output at `t` depends only on inputs
  at times `≤ t`. All DBSP operators are causal and time-invariant. **Strict**
  operators (output at `t` depends only on inputs at times `< t`; `z^-1` is the
  prototype) guarantee unique fixed points for feedback loops (Prop 2.10).
- **Differentiation** `D` (Def 2.17): `D(s) = s - z^-1(s)`, i.e.
  `D(s)[t] = s[t] - s[t-1]` — the stream of changes.
- **Integration** `I` (Def 2.19, Prop 2.20): `I(s)[t] = Σ_{i≤t} s[i]` — the running
  sum, definable inside the calculus as the feedback circuit `fix α. (s + z^-1(α))`.
- Both are causal, **linear**, and time-invariant (LTI), and they are mutual inverses
  (Theorem 2.22): `I(D(s)) = D(I(s)) = s`.

**The incremental version of an operator** (Def 3.1):

```
Q^Δ  =  D ∘ Q ∘ I
```

For a binary operator, `Q^Δ(a, b) = D(Q(I(a), I(b)))`. Read it as: `Q^Δ` consumes
streams of *changes*, internally reconstitutes full snapshots (`I`), applies the
original query, and emits only the *change* of the output (`D`). By Theorem 2.22 this
is semantics-preserving by construction: integrate the output of `Q^Δ` and you get
exactly the stream of full outputs of `Q`. The definition alone is useless as an
implementation (it materializes full snapshots); the theorems below are what turn it
into an efficient one.

## 3. The key theorems (§3)

**Properties of `^Δ` (Proposition 3.2):**

- *Inversion*: `Q ↦ Q^Δ` is a bijection (you can always recover `Q`).
- *Invariance*: `+^Δ = +`, `(z^-1)^Δ = z^-1`, `I^Δ = I`, `D^Δ = D`.
- *Chain rule*: `(Q1 ∘ Q2)^Δ = Q1^Δ ∘ Q2^Δ`.
- *Add rule*: `(Q1 + Q2)^Δ = Q1^Δ + Q2^Δ`.
- *Cycle rule*: incrementalizing a feedback loop = incrementalizing its body.

The **chain rule is the workhorse**: to incrementalize a composite query it suffices
to incrementalize each operator separately and compose the results. Incrementalization
is therefore modular — no whole-program analysis, no per-query cleverness.

**Linear operators are their own incremental version (Theorem 3.3).** If `Q` is LTI
then `Q^Δ = Q`. A linear operator maps a change to a change directly; it keeps *no
state*. Selection, projection, union-as-addition, converse/renaming, and linear
aggregates all fall in this class.

**Bilinear operators — the join case (Theorem 3.4).** For a bilinear time-invariant
`×` (linear in each argument separately):

```
(a × b)^Δ  =  a × b  +  z^-1(I(a)) × b  +  a × z^-1(I(b))
```

where `a, b` are the change streams. In database words, with `Δa, Δb` today's changes
and `A⁻, B⁻` yesterday's full snapshots:

```
Δ(A ⋈ B)  =  Δa ⋈ Δb  +  A⁻ ⋈ Δb  +  Δa ⋈ B⁻
```

— the classical incremental-join delta formula, here *derived* rather than postulated.
The needed state is `z^-1(I(a))` and `z^-1(I(b))`: each input's integral (the full
relation so far), kept indexed on the join key.

**Non-linear operators: `distinct` (Prop 4.7, §4.2.1).** `distinct` is neither linear
nor bilinear, but `(↑distinct)^Δ` has an efficient direct implementation. It keeps the
integral `I(d)` of its input and, for each element `x` occurring in the incoming change
`d`, applies

```
H(i, d)[x] = -1  if i[x] > 0  and (i+d)[x] ≤ 0
              1  if i[x] ≤ 0  and (i+d)[x] > 0
              0  otherwise
```

i.e. it emits an output change only for elements whose weight crosses zero. Only
elements mentioned in `d` can cross, so the work is bounded by `|d|`.

**The efficiency argument (§4.5).** For each operator, per-step *time* cost:

- linear operators: `O(|change|)`, no state;
- `I`: `O(|change|)` time (only changed keys are touched), `O(|database|)` memory;
- `(↑distinct)^Δ`: `O(|change|)` time, `O(|database|)` memory;
- bilinear operators: the `Δa ⋈ Δb` term costs `O(|Δa|·|Δb|)`; the delta-vs-integral
  terms cost time proportional to the delta times the matching records, via index
  lookups into the stored integrals — never a rescan of the full relations.

So the incremental circuit does work proportional to the **size of the change**, not
the size of the database. The database size shows up only as *memory* held in the
integrals — which is the same information a conventional DBMS holds as tables plus
indexes.

## 4. Relational algebra as DBSP circuits (§4.2, Table 4.2)

The compositional scheme: implement each set operator on Z-sets, prove that on
"set-typed" inputs the Z-set circuit computes the same result as the set semantics
(commutative-diagram proofs in §4.3), inserting `distinct` where negative or >1
weights could arise.

| RA operator | Z-set implementation | class |
|---|---|---|
| selection `σ_P` | `σ_P(m)[x] = m[x] if P(x), else 0` | linear |
| projection `π` | `π(i)[y] = Σ_{x: x↾c = y} i[x]` (weights add up) | linear |
| cartesian product `×` | `(a×b)[(x,y)] = a[x]·b[y]` | bilinear |
| equi-join `⋈` | product restricted to matching keys | bilinear |
| union `∪` | `distinct(a + b)` | linear + distinct |
| intersection `∩` | special case of equi-join | bilinear |
| difference `\` | `distinct(a - b)` | linear + distinct |
| distinct | as above | neither; own `^Δ` |

Note how **negation costs nothing extra**: `a - b` is plain group subtraction (linear!);
the only non-linear ingredient is the trailing `distinct` that clips negative weights.
Propositions 4.5–4.6 let the compiler *consolidate* `distinct` operators — push them
around and merge them so a chain of operators needs only one at the end.

**Aggregation (§7.2).** An aggregate is a function `Z[A] → B` into an abelian group.
`SUM` and `COUNT` are group homomorphisms, hence linear, hence their own incremental
version (a deletion contributes its value negatively). `MIN`/`MAX` are not linear: on
a deletion of the current minimum the operator must consult the remaining group, so it
keeps its integral (per group, e.g. ordered) as auxiliary state. Grouping is modeled
with *indexed Z-sets* (Z-sets of Z-sets).

**Recursion (§5–6).** Recursive queries (e.g. transitive closure, stratified Datalog)
use two operators that move between a value and a stream: `δ0` (emit the value, then
zeros) and `∫` (sum a stream that becomes zero). A recursive rule body `R` is put in a
feedback loop with `z^-1`; strictness of the delay guarantees a unique fixed point,
and Theorem 5.4 shows the circuit computes `fix x. R(I, x)` — naïve Datalog
evaluation. To incrementalize a recursive circuit the construction moves to **nested
streams** `S_{S_A}` (streams of streams, two-dimensional time: outer = database
transactions, inner = fixpoint iterations). Applying `^Δ` at both levels yields —
automatically — the classical **semi-naïve evaluation** algorithm, and beyond it:
incremental maintenance *of* recursive views. Stratified negation is supported;
non-monotone recursion over unbounded domains may fail to converge.

## 5. What "compile-time, semantics-preserving transformation" means: Algorithm 4.8

The paper's central deliverable is a *deterministic rewriting algorithm*, running in
time proportional to the number of operators in the query, that turns any
relational-algebra query `Q` into an efficient incremental circuit:

1. **Translate** `Q` into a Z-set circuit using the per-operator rules of Table 4.2
   (inserting `distinct` where set semantics requires it).
2. **Optimize**: apply the `distinct`-consolidation rewrites (Props 4.5–4.6) until
   convergence, minimizing the number of `distinct` operators.
3. **Lift** the whole circuit (`Q ⇒ ↑Q`, using Prop 2.4) so it operates on streams
   of database snapshots.
4. **Incrementalize**: surround the lifted circuit with `I` at the inputs and `D` at
   the output — by definition this is `(↑Q)^Δ`, consuming and producing change
   streams. Correct, but not yet efficient.
5. **Push `^Δ` inward** using the chain rule and the other Prop 3.2 identities:
   replace each linear operator by itself, each bilinear operator by its three-term
   Theorem 3.4 expansion, each `distinct` by the Prop 4.7 circuit. All explicit `I`/`D`
   pairs at internal edges cancel; what remains of the integrals is exactly the
   indexed state inside joins and distincts.

"Semantics-preserving" is literal: at every step the output stream, once integrated,
equals the brute-force re-evaluation of `Q` on every snapshot. No approximation, no
restriction to a query subset — this contrasts with prior IVM work where each operator
class needed a bespoke, separately proved delta rule.

## 6. Practical notes: state, and Feldera

**State.** An incremental circuit is not stateless; the transformation makes the
minimal necessary state explicit and local:

- each bilinear operator (join, intersection, product) stores the integral of each
  input, indexed on the join key;
- each `(↑distinct)^Δ` stores the integral of its input (element → weight);
- linear operators store nothing;
- recursion adds per-iteration nested-stream state.

These integrals play the role of a DBMS's tables-plus-indexes; total memory is
proportional to database size, while per-transaction time is proportional to
change size.

**Feldera** (the authors' company; open source at `github.com/feldera/feldera`)
implements DBSP as a Rust runtime (`dbsp` crate) plus a SQL compiler that translates
full SQL (including outer joins, aggregates, window functions, recursion) into DBSP
circuits and then applies exactly this incrementalization algorithm. Integrals are
stored in LSM-like batch/trace data structures (an idea inherited from Differential
Dataflow's arrangements — McSherry's earlier system, of which DBSP is a simplified,
one-dimensional-time theory), can spill to disk, and circuits process one
transaction-batch of changes per step. Sources: [Feldera blog](https://www.feldera.com/blog/Best-Research-Paper-VLDB-2023),
[Feldera papers page](https://docs.feldera.com/literature/papers/),
[GitHub](https://github.com/feldera/feldera).

## 7. Relevance to Ampersand

Ampersand compiles rules — heterogeneous relation-algebra terms over binary relations —
into SQL that re-evaluates each rule's violation set against the whole database on
every transaction. DBSP is a theory for making precisely this incremental: a
transaction *is* a Z-set delta (inserts `+1`, deletes `-1`), and the violation set of
each rule is a view to maintain. Operator by operator:

- **Composition `r;s`** = join on the middle atom followed by projection on the outer
  pair: bilinear ∘ linear. Theorem 3.4 applies directly; the circuit keeps `r` indexed
  by target and `s` by source — the indexes Ampersand's generated SQL wants anyway,
  but here maintained once, incrementally. Note that the projection makes weights add
  up (two witnesses `b` for `(a,c) ∈ r;s` give weight 2), which is *correct* Z-set
  bookkeeping: deleting one witness leaves weight 1, and the trailing `distinct`
  still reports the pair. Weights implement witness counting for free.
- **Converse `r~`** = a pointwise pair-swap: linear, stateless, `Q^Δ = Q`.
- **Union `∪`**, **intersection `∩`**, **difference `-` (binary)**: directly in
  Table 4.2. Intersection is bilinear; union and difference are linear plus `distinct`.
- **Complement `-r` and the universal flavor.** This is the one construct that needs
  care. DBSP has no complement against an infinite domain — a Z-set must have finite
  support. Ampersand's semantics already saves the day: complement is defined against
  `V[A×B]`, the cartesian product of the *concept populations*, which is finite (an
  active-domain semantics). Two routes, in order of preference:
  1. **Rewrite the complement away.** Rules are typically inclusions `r ⊢ s`, whose
     violation set is `r ∩ -s = r - s`: a difference/antijoin, no complement
     materialized. Likewise the residuals `s\r`, `r/s` (universal quantification)
     expand to complement-of-composition-of-complements; pushed through, the
     violation queries become antijoin shapes ("pairs in X with *no* witness in Y"),
     which DBSP handles as `distinct(a - b)` or join-plus-subtraction — all
     incremental. Ampersand's compiler already normalizes toward such shapes.
  2. **Materialize `V[A×B] - r`** where a complement truly must be a first-class
     term: `V` is the cartesian product of two unary Z-sets (the concept
     populations), itself bilinear. Feasible but expensive: adding one atom to `A`
     produces a delta of size `|B|`, and the stored integral is `O(|A|·|B|)`.
     Complements should stay inside antijoins, not become materialized views.
- **Kleene closures `r+`, `r*`**: exactly DBSP's recursion story (§5–6); the
  incrementalized nested-stream circuit is semi-naïve transitive closure, maintained
  under both insertions *and deletions* — something the classical semi-naïve
  algorithm alone does not give.
- **Set semantics**: Ampersand relations are sets of pairs, so `distinct` appears
  after every weight-producing operator; the Algorithm 4.8 step-2 consolidation keeps
  that to roughly one `distinct` (with its integral) per rule.

**What this would buy Ampersand.** Per transaction, each rule's violation *delta*
would arrive in time proportional to the transaction size, instead of a per-rule SQL
query over full tables. For blocking invariants the delta plus the maintained
violation integral answers "any violations now?" immediately; for signal rules the
delta is precisely the list of newly appeared/disappeared violations to show a role.

**What could block or complicate it.**

1. *Architecture*: DBSP is an engine holding state between transactions, not a query
   generator. Adopting it means either embedding Feldera/DBSP beside MariaDB (dual
   state, synchronization at transaction boundaries) or generating the delta queries
   from Theorem 3.4 into SQL by hand — recovering classical IVM with its per-operator
   proof obligations, which is exactly what DBSP exists to avoid.
2. *Complement discipline*: every complement must be rewritten into an antijoin
   against concrete relations before translation; a rule that forces a materialized
   complement of a near-empty relation over big concepts costs `O(|A|·|B|)` state.
3. *Changing concept populations*: `V[A×B]` and typing predicates depend on concept
   populations, so atom creation/deletion must flow through the circuits as deltas of
   the unary population Z-sets — an extra input per concept, straightforward but
   pervasive.
4. *Transaction semantics*: DBSP time steps must align with Ampersand's
   transaction/ExecEngine cycle; ExecEngine repair actions are new deltas fed back in,
   i.e. an outer feedback loop around the whole circuit — expressible in DBSP (it is
   a fixpoint), but it must be designed, not assumed.

## Sources

- [arXiv abstract](https://arxiv.org/abs/2203.16684) · [PDF](https://arxiv.org/pdf/2203.16684) · [HTML v1](https://arxiv.org/html/2203.16684v1)
- [VLDB 2023 version (PDF)](https://docs.feldera.com/vldb23.pdf)
- [Feldera: Best Research Paper VLDB 2023](https://www.feldera.com/blog/Best-Research-Paper-VLDB-2023)
- [Feldera publications](https://docs.feldera.com/literature/papers/) · [feldera/feldera on GitHub](https://github.com/feldera/feldera)
