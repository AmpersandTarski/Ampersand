# From rules to running code

This chapter follows a single ADL rule all the way to the artifact that enforces
it at runtime, and then proves one property of that machinery. It is written for
contributors and is deliberately layered, so you can stop at the depth you need:

- **[Part I — The big picture](#part-i--the-big-picture)** answers the question
  most people arrive with: when you write a rule, where does its code end up —
  Angular, PHP, or the database?
- **[Part II — Under the hood](#part-ii--under-the-hood)** is for die-hards who
  want the mechanism in detail: how the violation SQL is built, what quads are,
  how repair and signals work, and why all of this lives in the back-end rather
  than in the database.
- **[Part III — Do the two sides agree?](#part-iii--do-the-compiler-and-the-back-end-agree)**
  proves that the compiler and the back-end agree, exactly, on which conjuncts to
  re-check after a change — no missed violations and no wasted work.
- **[Part IV — Paying for the change, not the database](#part-iv--paying-for-the-change-not-the-database)**
  follows the work in progress that makes each re-check itself incremental: a
  delta computation whose cost follows the size of the change, with the proofs
  and measurements that exist today and the steps that remain before prototypes
  run it.

---

## Part I — The big picture

### The short answer

The compiler turns each rule into a SQL query that finds all violations of that rule.
A rule never becomes Angular code, and it never becomes hand-written PHP.
The compiler ships the query as data in `generics/conjuncts.json`.
The generic PHP back-end runs it in MariaDB.
Each tier plays exactly one role:

- **MariaDB** executes the rule: the violation query runs here and returns the
  offending pairs.
- **PHP back-end** decides what to do with the result: roll back the transaction
  (invariant) or present the violations as work to be done (signal).
- **Angular front-end** only displays: it shows signals and error messages the
  back-end returns through the API. It contains no rule logic.

### The pipeline: rule → conjuncts → violation SQL

The compiler normalises every rule into **conjuncts**, the smallest queryable
units of a rule's violation condition. For each conjunct it generates a SQL query
that returns the tuples violating that conjunct: it takes the negation of the
conjunct, brings it to conjunctive normal form, and compiles that to SQL.

These queries are written to `generics/conjuncts.json`, one entry per conjunct:

```json
{
  "id": "conj_0",
  "signalRuleNames": ["ruleProcessApproval"],
  "invariantRuleNames": ["ruleTotalFunction"],
  "violationsSQL": "SELECT ... -- returns the violating pairs"
}
```

A companion file, `generics/rules.json`, lists the rules themselves — split into
`invariants` and `signals` — with each rule's meaning, violation message, and the
ids of the conjuncts that make it up. One conjunct can belong to several rules,
and one rule can decompose into several conjuncts.

So the "code" of a rule is the `violationsSQL` string. It is data, not program
text: the PHP framework is generic and identical for every application; only the
JSON files change.

### When the SQL runs: quads

The compiler also computes **quads**, which wire each relation to the rules it can
break. A quad records: *if this relation changes, then this rule may be violated,
so check these conjuncts.* The same wiring ships to the back-end as
`affectedConjuncts` lists in `concepts.json` and `relations.json`. During a
transaction the back-end only records which relations and concepts were touched;
when the transaction closes it unions their lists and runs exactly those conjunct
queries — not every rule. The wiring is computed once at compile time; the
checking happens at runtime, inside the PHP transaction.

### Invariants versus signals

A rule is a **signal** when a role is assigned to maintain it (it has
*maintainers*), and an **invariant** otherwise. Both use the same violation SQL;
they differ only in what the back-end does with a non-empty result.

| | Invariant | Signal (process rule) |
| --- | --- | --- |
| Assigned maintainer | none | one or more roles |
| On violation | the transaction is rolled back | the violations are shown as work to do |
| Who resolves it | the system keeps it true automatically | the responsible role acts on it |
| Lives in `rules.json` under | `invariants` | `signals` |

### Where to look in the compiler

- `Ampersand/Output/ToJSON/Conjuncts.hs` — builds `conjuncts.json`, including the
  `violationsSQL` for each conjunct.
- `Ampersand/Output/ToJSON/Rules.hs` — builds `rules.json`, splitting invariants
  from signals.
- `Ampersand/FSpec/SQL.hs` — compiles a rule expression to SQL.
- `Ampersand/Prototype/GenBackend.hs` — writes the `generics/` files the back-end
  consumes.

See also [Architecture of an Ampersand Application](./architecture-of-an-ampersand-application.md)
for how these generated files fit into the whole application.

---

## Part II — Under the hood

Part I gave the shape; this part traces the mechanism in the source. It also
answers a tempting question — *could rule enforcement move into the database as
triggers, so that violations never have to travel back to PHP?* — because the
answer is what explains the current split. Source paths below are Haskell modules
under `src/` in the compiler repository, and PHP classes under
`backend/src/Ampersand/` in the Prototype framework.

### 1. The violation SQL is arbitrarily complex

The pipeline from Part I is realised in one line of
`Ampersand/Output/ToJSON/Conjuncts.hs`:

```haskell
cnjJSONviolationsSQL = sqlQuery fSpec . conjNF env . notCpl . rcConjunct $ conj
```

The real machine behind `sqlQuery` is `Ampersand/FSpec/SQL.hs`, whose `selectExpr`
translates relation-algebra expressions into SQL. It emits left joins over
subqueries for complement and intersection, and **recursive common table
expressions** for transitive closure (the Kleene operators). There is no upper
bound on the complexity of a conjunct's query: union, composition, complement and
converse all compile recursively, so a single conjunct can span arbitrary joins
and subqueries across many tables.

This is the first reason enforcement is hard to push into a row-level trigger: the
"condition" of a rule is a set-based query over the whole population, not a check
on the row that just changed.

### 2. Quads: Ampersand is already ECA — structurally

A **quad** ties a relation to a rule it can break
(`Ampersand/FSpec/FSpec.hs`):

```haskell
data Quad = Quad
  { qDcl       :: Relation,            -- the relation whose change triggers a check
    qRule      :: Rule,                -- the rule that may then be violated
    qConjuncts :: NE.NonEmpty Conjunct -- the conjuncts to check
  }
```

`quadsOfRules` / `makeAllQuads` in `Ampersand/FSpec/ToFSpec/Calc.hs` build one quad
per (relation, rule) pair, for every relation that occurs in the rule. The data
type even reads as a rule: *ONCHANGE `qDcl` FIX `qRule`*. That is precisely an
Event–Condition–Action shape: **event** = a relation changes, **condition** = a
conjunct is violated, **action** = repair.

So the idea of treating rules as ECA is not foreign to Ampersand — it is already
the internal model. What the compiler deliberately does *not* do is turn the
*action* into executable database code: a grep of the compiler for `CREATE
TRIGGER`, `CREATE PROCEDURE`, `AFTER INSERT` or `BEFORE UPDATE` returns nothing.
The action is emitted as data and interpreted by the back-end.

### 3. The database schema carries data, not behaviour

`databaseStructureSql` in `Ampersand/Output/FSpec2SQL.hs` (with
`Ampersand/Prototype/TableSpec.hs`) generates the schema. It contains only `CREATE
TABLE` with primary keys and nullability, `CREATE INDEX`, and a bookkeeping
`ts_insertupdate` timestamp column. There are **no triggers, no stored procedures,
and no foreign-key constraints**.

Relations map onto tables in two shapes:

- **`TblSQL`** — a "wide" table per concept kernel, with a primary-key column and
  one column per functional attribute (and columns for specialisations).
- **`BinSQL`** — a "narrow" two-column table per non-functional relation.

Because each relation lives in exactly one plug (`getRelationTableInfo` in
`Ampersand/FSpec/FSpecAux.hs`), a violation that spans several relations becomes a
join across several plug tables — which is exactly the SQL that section 1
generates.

### 4. Repair is data, interpreted by the back-end

A rule's `VIOLATION` clause is stored on the rule as a structured `PairView`
(`rrviol` in `Ampersand/Core/AbstractSyntaxTree.hs`), for example:

```text
VIOLATION (TXT "Custom display: ", SRC expr1, TXT " conflicts with ", TGT expr2)
```

For automated rules this clause carries `{EX}` primitives — `InsPair`, `DelPair`,
`DelAtom`, `MrgAtoms`. `Ampersand/Output/ToJSON/Rules.hs` serialises the whole
`PairView` into `rules.json` as text (`rulJSONpairView`); the back-end's ExecEngine
parses the `{EX}` markers at runtime and calls the matching repair functions. The
compiler only ever *reads* these scripts, never executes them — for instance
`Ampersand/FSpec/Oscillation.hs` parses them statically to analyse oscillation
risk. Repair, in other words, is emitted as data and interpreted by PHP, never
compiled to SQL.

### 5. Invariants hold at transaction boundaries, not continuously

The intended semantics are documented in `docs/conceptual/automated-rules.md`:

> A rule has to be satisfied at all times. Such rules are called invariants. When
> violated, the system produces an error message and blocks the transaction in
> which the violation has occurred.

"At all times" means *at every transaction boundary*, not continuously in the
background. A transaction may pass through intermediate states that violate an
invariant, as long as the invariant holds again when it commits. The check runs
inside the PHP transaction, at commit; nothing evaluates rules between
transactions.

### 6. Why this does not move into the database

Sections 1–5 already explain the architecture, but they also settle the tempting
question. Moving enforcement into MariaDB triggers fails on several independent
points:

| Obstacle | Why it blocks DB-side enforcement |
| --- | --- |
| **Deferred semantics** | Invariants are checked at commit (section 5). MariaDB triggers fire immediately, per statement, and MariaDB has no deferred constraints (no `DEFERRABLE INITIALLY DEFERRED`). Legitimate multi-step transactions would be rejected mid-way. |
| **Set-based vs per-row** | Violation SQL is a whole-population query (section 1); triggers are `FOR EACH ROW`. Re-running a conjunct's join per changed row is both wrong (still immediate) and expensive. |
| **Iterative repair** | The ExecEngine repairs violations in a loop until a fixpoint, calling PHP closures (section 4). A trigger cannot run that loop, cannot call PHP, and cannot even modify the table that fired it (MariaDB error 1442). |
| **Oscillation** | Repairs that delete or merge are non-monotone and can cycle; the compiler analyses this risk at design time (`Ampersand/FSpec/Oscillation.hs`). Recursive trigger firing would reproduce the same divergence inside the engine, with no design-time guard. |
| **Signals are reports, not constraints** | Signal violations are allowed states surfaced to a role after commit, with UI links. Triggers prevent or react; they cannot answer "list the current violations for role X." |
| **Violation messages** | Messages are built in PHP from `PairView` segments (section 4), aggregated per rule. A trigger's `SIGNAL` yields a flat string of at most 128 characters — the structured, navigable message is lost. |

The one place DB-side enforcement *is* sound is the **monotone, insert-only**
subset, where `Ampersand/FSpec/Oscillation.hs` can already certify termination.
That subset is small and partly already enforced structurally by keys. The
genuine win for "less back-end ↔ database traffic" is not triggers but **batching
detection into a single round trip** — a stored procedure or one `UNION ALL` query
that evaluates all affected conjuncts server-side — while rollback, repair and
messaging stay in the back-end. Part IV attacks the same cost from the other
side: not fewer round trips, but smaller queries.

This layering is deliberate: the compiler reasons about rules algebraically,
generates *data* (JSON + SQL), and delegates *interpretation* to a generic
runtime. The next part proves that this division is not just clean but correct.

---

## Part III — Do the compiler and the back-end agree?

Part I said the back-end re-checks only the conjuncts *affected* by a change, using
a list the compiler ships. That should make a careful reader nervous. The affected
set is the product of two cooperating computations: the compiler builds an index at
compile time, and the back-end selects from it at run time. If they disagreed in
either direction we would have a real defect — **a missed conjunct is a silent
integrity hole**, and **a spurious conjunct is wasted work on every transaction**.

This part proves they agree exactly. We first pin down both halves from the source,
state what "correct" means, and then prove the correspondence.

### The two halves, from the source

**The specialisation order.** Write `c ⊑ d` for "`c` is-a `d`": every atom of `c`
is also an atom of `d`, so `c` is a *specialisation* of `d` and `d` a
*generalisation* of `c`. It is a partial order (reflexive and transitive). Its
down-set is

```text
down(d) = { c | c ⊑ d }   -- d together with all its specialisations
```

In the compiler this is `smaller` (`Ampersand/FSpec/ToFSpec/ADL2FSpec.hs`), built
from `smallerConcepts` (`Ampersand/Core/AbstractSyntaxTree.hs`), whose own comment
reads *"all predecessors (more specific concepts)."*

**Modifiable subexpressions.** For a conjunct `K`, let `modif(K)` be its
subexpressions whose population a single insert or delete can change
(`modifyablesByInsOrDel` in the compiler). Each `e ∈ modif(K)` has a source concept
`src(e)` and a target concept `tgt(e)`.

**The compiler index** (`fSpecAllConjsPerConcept` and `fSpecAllConjsPerDecl` in
`ADL2FSpec.hs`, shipped as `affectedConjuncts` in `concepts.json` and
`relations.json`). A conjunct `K` is filed under a concept `c`, and under a
relation `r`, by:

```text
RelatedConj(c) = { K | ∃ e ∈ modif(K).  c ⊑ src(e)  ∨  c ⊑ tgt(e) }      (I)
RelatedConj(r) = { K | r occurs in K }                                    (I′)
```

Note that **(I) has already applied the typology closure**: `K` is filed under the
endpoint concept *and under every specialisation of it*, because the index walks
`down(src e) ∪ down(tgt e)`.

**The runtime marking** (Prototype framework). A transaction records which atoms and
links it touched:

- inserting or deleting an atom of concept `A` marks exactly `{A}` —
  `Core/Concept.php` (`addAtom`/`removeAtom`/`deleteAtom`) calls
  `Transaction::addAffectedConcept($this)`, where `$this` is `A` itself;            (R)
- inserting or deleting a link in relation `r` marks exactly `{r}` —
  `Core/Relation.php` calls `Transaction::addAffectedRelations`.                    (R′)

Both `addAffectedConcept` and `addAffectedRelations` are plain idempotent
set-insertions (`Transaction.php`); they perform **no typology walk**. The back-end
then unions the precomputed lists (`getAffectedConjuncts`):

```text
Retrieved = ⋃ { RelatedConj(c) | c marked } ∪ ⋃ { RelatedConj(r) | r marked }
```

### What "correct" means

A conjunct `K` can change its violation set under a change `δ` only if `δ` changes
the population of some `e ∈ modif(K)`. Spell that out per kind of change:

```text
atAtom(A) = { K | ∃ e ∈ modif(K).  A ⊑ src(e)  ∨  A ⊑ tgt(e) }            (T)
atLink(r) = { K | r occurs in K }                                          (T′)
```

(T) holds because a freshly created or removed atom of `A` changes the population
of a concept `C` exactly when the atom is a `C`-atom, i.e. exactly when `A ⊑ C`.
The ground truth for a transaction is the union of `atAtom`/`atLink` over its
changes. **Correctness** is the claim `Retrieved = ground truth`: every conjunct
that could have changed is re-checked (safety) and no other is (no waste).

### The correspondence theorem

> **Theorem.** For every transaction, `Retrieved` equals the union of `atAtom(A)`
> over its atom changes and `atLink(r)` over its link changes.

**Proof.** `Retrieved` is a union over marked items, and the ground truth is a union
over changes, so it suffices to prove the two primitive cases; the transaction case
follows by taking unions.

*Atom case.* A change to an atom of `A` marks exactly `{A}` by (R), so it
contributes `RelatedConj(A)`. Unfolding the index (I):

```text
RelatedConj(A) = { K | ∃ e ∈ modif(K).  A ⊑ src(e) ∨ A ⊑ tgt(e) } = atAtom(A)   by (T).
```

The two sides are the *same predicate*: equality is definitional, not an
inclusion. The only order fact used is reflexivity `A ⊑ A`, which lets the marked
concept `A` match itself inside the down-set the compiler already expanded.

*Link case.* A change to a link in `r` marks exactly `{r}` by (R′), contributing
`RelatedConj(r) = { K | r occurs in K } = atLink(r)` by (I′) and (T′).

Taking the union over all of the transaction's changes gives
`Retrieved = ⋃ atAtom ∪ ⋃ atLink`. ∎

### What the theorem buys

- **Safety.** Every conjunct that a change could violate is re-checked, so an
  invariant violation can never slip through unchecked. Integrity is not
  best-effort; it is exact.
- **No redundancy.** No conjunct that *cannot* have changed is ever re-evaluated,
  so the per-transaction query count is the minimum the model allows.
- **One source of truth.** All typology reasoning lives in the compiler, in the
  single word `smaller`. The back-end performs *no* typology inference to select
  conjuncts; it records the bare concept or relation of each edit and unions
  precomputed lists. The two sides therefore *cannot drift*: the runtime supplies
  the change-set (which the compiler cannot know in advance), the compiler supplies
  the index (which the runtime never recomputes). There is no duplicated derivation
  to get out of step — which is the strongest form of the "compute once, ship the
  result" principle.

*Proof track: [PRF-1 — the runtime re-checks exactly the affected conjuncts](../proofs/README.md#prf-1).*

---

## Part IV — Paying for the change, not the database

Parts I–III describe every prototype Ampersand generates today, and Part III
proved that the *selection* of conjuncts to re-check is exact. The check itself,
however, is not proportional to the change: each selected conjunct re-runs its
full violation query over the whole population. Insert one pair into a database
of a million pairs, and the joins of Part II section 1 run over the million.

The **incremental evaluation** work (issue
[#1682](https://github.com/AmpersandTarski/Ampersand/issues/1682), branch
`incremental-evaluation`) removes that mismatch: it computes the *change* of
each violation set directly from the *change* in the population, in time
proportional to the change. The theory is DBSP ("DBSP: Automatic Incremental
View Maintenance for Rich Query Languages", arXiv 2203.16684, VLDB 2023) — a
compile-time transformation that turns any relational-algebra query into its
incremental form. Ampersand's rule terms are relational algebra over binary
relations, so the theory applies without translation loss. This part gives the
mechanism at the level of Part II and states precisely what exists today; the
full rule table, the proofs and the raw measurements live in the repository,
under `memorybank/incremental-evaluation/` and `proofs/incremental/`.

### 1. Z-sets: sets that can express a change

A **Z-set** maps each pair to an integer weight; pairs not mentioned have
weight 0. An ordinary relation is a Z-set with all weights 1. A *change* is a
small Z-set too: weight +1 means "this pair was inserted", −1 "this pair was
deleted". One transaction thus becomes one small Z-set per touched relation,
plus one per concept for atom creation and deletion — atom changes are deltas
like any other, which is how the typology of Part III joins the calculus.

The weights are not bookkeeping decoration; composition needs them. The pair
`(x,y)` is in `r;s` when *some* intermediate `z` witnesses it, and there may be
several. Deleting one witness must not delete the pair while another witness
remains. The weight counts the witnesses, and a `distinct` step clips positive
weights back to 1 where set semantics is required. Getting that clip right is
the crux of the whole construction (rule D6 below).

### 2. The delta rules

Each conjunct's violation term is first desugared into a small core language:
complements become differences from `V`, and the residuals `l/r`, `l\r`, the
diamond and the relative addition become "no witness" compositions — the
antijoin discipline that keeps `V[A×B]` from ever being materialized. The core
term then becomes a **circuit**: one node per subterm, each holding exactly the
state its delta rule needs. A transaction enters at the leaves (the changed
relations and concepts) and propagates upward; each node emits the delta of its
output. Three groups of rules cover the core:

| Operators | Delta rule | State kept per node |
| --- | --- | --- |
| converse, union, difference | *linear*: the delta passes straight through, e.g. `Δ(a∪b) = Δa + Δb` | the weighted sum (for the `distinct` step) |
| composition, intersection, cartesian product | *bilinear*: `Δ(a⊗b) = Δa⊗b_old + a_new⊗Δb` — evaluated by index lookups against the maintained inputs, never a full scan | composition also keeps a converse copy of its left input, so both lookup directions are indexed |
| `distinct` (weights → set) | *zero-crossing*: a pair enters the set when its weight rises above 0 and leaves when it drops to 0; all other weight changes are invisible | the pre-`distinct` weights |

Kleene closures (`*`, `+`) and the built-in operators have no delta rule yet:
their nodes recompute from their — incrementally maintained — input whenever
that input changed. That is the general discipline of the design: **a construct
without a proven delta rule falls back to full evaluation**, so the route is
never incorrect, only locally slower. Incremental closure maintenance is a
planned phase of its own.

The full table, with the desugaring identities and their derivations, is
`memorybank/incremental-evaluation/delta-calculus.md`.

### 3. What exists today: a proven, measured evaluator in the compiler

`Ampersand.FSpec.Incremental` (with `Ampersand.FSpec.Incremental.ZSet`)
implements the circuits in pure Haskell, next to the existing full evaluator
`fullContents` — the compiler's in-memory equivalent of the violation SQL. The
command `ampersand incremental-bench` builds the circuits for a model, runs a
stream of random single-pair transactions through both evaluators, and in
`--verify` mode compares the maintained violation set of every conjunct against
a fresh full evaluation after every single transaction.

The measured gap is the point of the whole exercise. Across a ×40 growth of the
benchmark database (200 → 8 000 pairs per relation), the incremental step grows
×6 (10 → 65 µs per transaction) while full re-evaluation grows ×1 200
(1.65 ms → 1.98 s) — a speedup between ×166 and ×30 626, with zero mismatches
on the verified runs. The incremental cost follows the size of the change; the
full cost follows the size of the database. Raw data and method:
`memorybank/incremental-evaluation/bench/RESULTS.md`.

The delta rules are machine-checked. The Isabelle/HOL session
`Incremental_Delta` in `proofs/incremental/` (six theories, no unproven gaps)
covers the Z-set algebra, the bilinear expansions, the zero-crossing rule, the
desugaring identities, and a whole-circuit induction: every state reachable
from the empty database by transactions — the loading of the initial
population included —
yields exactly the set semantics of every term. The obligation-to-lemma map is
in `proofs/incremental/README.md`. A QuickCheck suite in `stack test` states
the same lemmas as properties over the actual Haskell functions, so the code
is bound to the proofs on every build.

*Proof track: [PRF-2 — the incremental evaluator is exact](../proofs/README.md#prf-2).*

### 4. What remains before a prototype runs it

The runtime seam is already in place. The prototype framework materializes
violations per conjunct in a database table (`__conj_violation_cache__`) and
refreshes it wholesale at each commit: delete all rows of the conjunct, re-run
the full query, insert the result. Signal rules are *read* entirely from that
table. Incremental evaluation therefore changes the refresh strategy of an
existing store, not the architecture around it. Two steps remain:

- **Delta SQL (compiler).** Derive, per (conjunct, affected relation), the
  delta term as an ordinary `Expression` and compile it with the same
  `selectExpr` machinery of Part II — delta terms are just terms over the
  transaction's changed pairs. `conjuncts.json` grows an optional per-relation
  delta-query field; the framework ignores unknown fields, so the contract
  stays backward compatible.
- **Runtime adoption (prototype framework).** Maintain the violation table by
  applying row deltas instead of the wholesale refresh, keep the full queries
  as fallback and as periodic self-check, and switch per application — after a
  shadow-run period on a real application in which both routes run, every
  divergence is logged, and users see only the old route.

Until those steps land, Parts I–III describe every prototype in production,
unchanged; this part describes the compiler's proven core and the route by
which it reaches the runtime.

*Proof track: [PRF-6 — the delta SQL maintains the violation records exactly](../proofs/README.md#prf-6).*

### Where to look

- The [proof track](../proofs/README.md) — the register of claims behind this
  chapter, with the narrative trail
  [Incremental evaluation](../proofs/incremental-evaluation.md).
- `src/Ampersand/FSpec/Incremental.hs` and
  `src/Ampersand/FSpec/Incremental/ZSet.hs` — the circuits, the delta
  propagation, and the oracle comparison.
- `src/Ampersand/Commands/IncrementalBench.hs` — the `incremental-bench`
  command.
- `src/Ampersand/Test/Incremental/Properties.hs` — the QuickCheck bridge that
  runs in `stack test`.
- `proofs/incremental/` — the Isabelle/HOL theories, with the
  obligation-to-lemma table in its `README.md`.
- `memorybank/incremental-evaluation/` — the delta calculus, the design-choice
  register, the plan with phase status, and the benchmark data.
