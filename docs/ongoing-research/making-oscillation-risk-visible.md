# Investigative Report: Making Oscillation Risk Visible in Ampersand

*Literature-grounded analysis and plan. Search id 1; 16 references in
`literature_search.csv`. Citations use `[@cite_key]`.*

---

## 0. What this report does

The oscillation guide (`docs/guides/oscillations/README.md`) establishes that an
oscillation (`Maximum reruns exceeded`) is the runtime telling you that, *given the
data*, your automated rules are jointly unsatisfiable. It also states the open problem:
we cannot mathematically predict from the code whether an oscillation *will* occur, but
we would like to make the *risk* of one visible.

This report reads the literature on that exact question, reformulates the problem in the
vocabulary the literature uses, weighs the candidate methods against each other, and ends
with an advised approach for Ampersand.

---

## 1. Interpreting the problem through the literature

The same phenomenon — a repair loop that may never reach a fixpoint — has been studied
independently in four communities. Each gives Ampersand a precise name for one facet of
the ExecEngine.

### 1.1 The ExecEngine repair loop *is* the chase

The closest formal match is the **chase** from database dependency theory. The chase is a
fixpoint procedure that repeatedly enforces dependencies on an instance, inserting tuples
(possibly with fresh nulls) and equating values until no dependency is violated
[@Fagin2005DataExchange; @Onet2013ChaseProcedure]. Two dependency kinds drive it, and they
line up one-to-one with Ampersand's `{EX}` primitives:

| Chase dependency | Ampersand `{EX}` primitive | Effect |
| --- | --- | --- |
| **TGD** (tuple-generating, often existential) | `InsAtom`, `InsPair`, `NewStruct` | *adds* facts (a fresh atom = a fresh null) |
| **EGD** (equality-generating) | `MrgAtoms` | *equates* two atoms |
| (no standard analogue) | `DelPair`, `DelAtom` | *removes* facts |

The FC5 case is textbook: the create-rule `eppoCodeMaaktOrganisme` is an existential TGD
(it invents a fresh `Organisme`), and the merge-rule `OrganismeUniekeEPPO` is an EGD
(it equates two `Organisme` atoms). "Maximum reruns exceeded" is exactly **non-termination
of the chase**. This matters because chase termination is the single most-studied version
of our question, and crucially it is studied **data-independently**: the standard criteria
guarantee termination *for every instance* [@Fagin2005DataExchange].

### 1.2 The ExecEngine is also an active (ECA) rule system

Viewed operationally, the ExecEngine is an **active database**: each automated rule is an
event–condition–action rule whose action can re-trigger other rules [@Paton1999ActiveDB].
This community framed precisely our three questions — is a rule set **guaranteed to
terminate**, to reach a **unique final state** (confluence), and to produce a unique
observable behaviour — and answered them by *static analysis of the rule set*
[@Aiken1995StaticAnalysis]. This is the literature that gives us the **rule-level
dependency graph** and the language of "rules that re-trigger each other," which is also
how the guide already reasons ("Rules fixed in last run", "a write that feeds another
antecedent").

### 1.3 The ExecEngine is also a graph-rewriting system

An Ampersand population is a labelled graph (atoms = nodes, pairs = edges), and every
`{EX}` action is a graph rewrite. In that setting termination and confluence are the
standard well-behavedness properties, and **critical-pair analysis** detects pairs of
rules that conflict — one rule disabling or undoing what another did
[@Lambers2006ConflictDetection]. That is precisely the guide's "opposing actions" signature
(one rule adds a fact, another removes/merges it on the same relation or atom).

### 1.4 The unifying lens: monotonicity and stratification

Underneath all three views is one distinction the guide already draws: **monotone**
(insert-only) repair converges to a least fixpoint by Knaster–Tarski/Kleene, whereas
delete/merge make the repair operator **non-monotone** and it can cycle. Logic programming
made this operational long ago: build the predicate dependency graph with *positive* and
*negative* edges; if no cycle runs through a **negative** edge, the program is **stratified**
and has a well-defined, terminating semantics [@Apt1988DeclarativeKnowledge]. Translated to
Ampersand: a repair that **removes or merges** (`DelPair`, `DelAtom`, `MrgAtoms`) is a
*negative-polarity* edge; **a cycle through a negative edge is the formal signature of
oscillation risk.** This single idea reappears as "no special-edge cycle" in the chase
(§2) and as "no triggering cycle" in active rules (§1.2).

---

## 2. A more precise statement of the problem

The literature lets us sharpen the original question into something implementable.

> **Given** a set `R` of automated rules, each a relation-algebra inclusion
> `antecedent ⊆ consequent` with a repair script of `{EX}` primitives, **construct a
> static analysis `A(R)`, computed from the rule text alone (data-independent), such that:**
>
> 1. **Soundness for termination (no false "safe").** If `A(R)` reports *safe*, then for
>    *every* population the ExecEngine repair loop terminates. (This is exactly the
>    guarantee weak acyclicity gives the chase [@Fagin2005DataExchange].)
> 2. **Explanatory risk reports.** When `A(R)` cannot certify safety, it returns the
>    *minimal* set of mutually-triggering rules and the relation(s)/atom(s) on which
>    *opposing* writes meet — i.e. it reproduces, statically and before any data is loaded,
>    the "Rules fixed in last run" diagnosis the runtime gives after the fact.
> 3. **Conservative by design.** False positives (flagging a safe rule set as risky) are
>    acceptable; false negatives (missing a real oscillation) are not.

Two consequences follow directly from the abstracts.

**(a) We cannot ask for an exact decision — and now we know why.** Joosten proves that
entailment over invariants written with composition, converse and intersection of binary
relations (with equality) — *the core idea behind languages such as Ampersand* — is
**undecidable**; his "graph saturation" procedure is therefore only a *semi-decision*
procedure that need not terminate [@Joosten2018GraphSaturation]. The same verdict comes from
the chase: termination is undecidable in general [@Deutsch2008ChaseRevisited;
@Greco2011StratificationCriteria]. So the guide's claim ("we cannot mathematically predict
oscillation from the code") is not a gap in our cleverness; it is a theorem. The only
coherent goal is a **sound over-approximation**: a sufficient condition for termination
whose *negation* we surface as risk. This is the design stance every method below shares.

**(b) "Risk" has two distinguishable structural causes**, matching the guide's two
signatures:
- a **triggering cycle** — rule A's repair feeds rule B's antecedent and vice versa
  (§1.2); and
- an **opposing-write conflict** — within such a cycle, at least one edge is *negative*
  (a `Del`/`Mrg` undoes an `Ins`), which is what turns a benign monotone cycle into a
  non-monotone, potentially oscillating one (§1.4).

A useful analysis must detect the cycle *and* its polarity, because a purely positive
(monotone) cycle still converges by Knaster–Tarski and must **not** be flagged — otherwise
the analysis drowns the user in false alarms.

---

## 3. The candidate methods, compared on equal footing

Four families of methods can supply `A(R)`. They differ mainly in **granularity** (do they
reason about whole rules or about individual relation positions?), which drives their
**precision** (false-positive rate) and **cost**.

### 3.1 Rule-level triggering / activation graph (active databases)

Build a directed graph with one node per automated rule; an edge `A → B` when A's action
can change a relation occurring in B's antecedent. **No cycle ⇒ guaranteed termination**;
a cycle is a *necessary* condition for non-termination [@Aiken1995StaticAnalysis]. The
*refined* triggering graph prunes edges where the action cannot actually re-satisfy the
other rule's condition, removing spurious cycles [@Urban1999RefinedTriggering], and later
refinements delete still more edges that cannot lead to actual re-execution, certifying
termination for strictly more rule sets [@Couchot2002ImprovingRTG]. An
*algebraic* reformulation expresses the test in terms of properties of the action operators
(e.g. commutativity ⇒ confluence) rather than syntax [@Baralis2000AlgebraicApproach], and
modularization keeps it tractable on large rule sets [@Baralis1996Modularization].

- **+** Granularity matches the user's mental model and the guide's diagnosis exactly: it
  names *rules*. Cheap to compute; cycle detection on a small graph.
- **+** The algebraic variant is the natural way to lift the test onto relation-algebra
  operators, and Ampersand already manipulates rules as relation-algebra terms.
- **−** Coarse: it over-approximates at the rule level, so it flags cycles that the data can
  never actually drive — more false positives than position-level analysis.
- **−** Plain triggering graphs ignore polarity; without the signed refinement (§1.4) they
  flag monotone cycles that always converge.

### 3.2 Position-level acyclicity (the chase)

Build a dependency graph over **relation positions** (argument slots). Mark edges that carry
a freshly invented value ("special"/existential edges). **Weak acyclicity** = no cycle
through a special edge ⇒ the chase terminates on every instance [@Fagin2005DataExchange].
Successively more permissive criteria certify strictly larger classes: **stratification**
and **c-stratification** [@Deutsch2008ChaseRevisited], **super-weak acyclicity**
[@Marnette2009GeneralizedSchemaMappings], and a unifying **local-stratification** hierarchy
with rewriting techniques that massage a rule set so more criteria apply
[@Greco2011StratificationCriteria; @CuencaGrau2013AcyclicityNotions]. Critically for us,
most early criteria *ignore EGDs*; later work shows EGDs (= `MrgAtoms`) materially affect
termination and gives criteria that **exploit** them [@Calautti2016EGDChaseTermination]. A
complementary line keeps reasoning decidable even when the chase does *not* terminate, via
structural restrictions such as guardedness [@Cali2013TamingChase].

- **+** Strongest precision: position-level value-flow analysis flags far fewer safe sets
  than rule-level graphs. Decidable in polynomial time for several criteria
  [@Marnette2009GeneralizedSchemaMappings].
- **+** EGD-aware variants target exactly the create-vs-merge interaction that caused FC5
  [@Calautti2016EGDChaseTermination].
- **+** Soundness-for-all-instances is the literal form of requirement (1) in §2.
- **−** Granularity is *positions*, not rules; turning a "special-edge cycle" back into a
  human-readable "these two rules collide" report needs extra work.
- **−** Heavier to implement: it requires modelling each `{EX}` script as TGD/EGD bodies and
  tracking value provenance across positions.

### 3.3 Critical-pair analysis (graph rewriting)

Enumerate the minimal population fragments on which two rules conflict — one rule's rewrite
disables or undoes another's. No critical pairs ⇒ local confluence (Newman's lemma); the
AGG tool computes these automatically [@Lambers2006ConflictDetection].

- **+** Directly formalises the guide's "opposing actions" signature; tells you *which two
  repairs* undo each other and *on what overlap*.
- **+** Aims at **confluence** (unique result), a property the rule-level and position-level
  termination tests do not directly give.
- **−** Critical-pair enumeration is combinatorially heavier and is about confluence, not
  primarily termination; on its own it does not bound the number of reruns.
- **−** Tooling lives in the model-transformation world; porting to Ampersand's
  relation-algebra rewrites is a non-trivial bridge.

### 3.4 Signed dependency graph / stratification (logic programming)

The conceptual unifier of §1.4: a predicate (here: relation) dependency graph with positive
and negative edges; a cycle through a negative edge means *unstratified* and is the formal
risk signature [@Apt1988DeclarativeKnowledge].

- **+** Captures the one thing the bare triggering graph misses — **polarity** — and does so
  with almost no extra machinery.
- **+** Explains *why* monotone rule sets are always safe (Knaster–Tarski), so it naturally
  suppresses the false alarms of §3.1.
- **−** Stratification by itself is a yes/no property; it does not localise *which atoms* nor
  bound reruns. It is best used as a *refinement layer* on top of §3.1, not standalone.

### 3.5 Comparison summary

| Method | Granularity | Precision (fewer false +) | Cost | Names colliding *rules*? | EGD/merge-aware? | Gives confluence? |
| --- | --- | --- | --- | --- | --- | --- |
| 3.1 Triggering graph [@Aiken1995StaticAnalysis; @Urban1999RefinedTriggering] | rule | low–medium | low | **yes** | only if encoded | partial (algebraic [@Baralis2000AlgebraicApproach]) |
| 3.2 Chase acyclicity [@Fagin2005DataExchange; @Calautti2016EGDChaseTermination] | position | **high** | medium–high | not directly | **yes** | no |
| 3.3 Critical pairs [@Lambers2006ConflictDetection] | rule-pair + graph overlap | high (for conflict) | high | **yes** | yes | **yes** |
| 3.4 Signed graph / stratification [@Apt1988DeclarativeKnowledge] | relation/rule | refinement only | very low | yes (as layer) | yes (sign of `Mrg`) | no |

No single row dominates. The pragmatic reading: **3.1 + 3.4 give the cheapest, most
explanatory first cut; 3.2 (EGD-aware) buys precision exactly where Ampersand bleeds; 3.3
is the principled future answer for confluence.**

---

## 4. Selection: an advised approach for Ampersand

Given the trade-offs, a **single layered analysis built in stages** dominates any one method
used alone. Each stage is independently shippable and strictly increases precision.

**Stage 0 — Frame and classify (concept work, no code).**
Adopt the chase as the official semantics of the ExecEngine
[@Fagin2005DataExchange; @Joosten2018GraphSaturation], and classify each `{EX}` primitive
once: `Ins*`/`NewStruct` = monotone (positive), `Del*` = non-monotone (negative),
`MrgAtoms` = EGD (negative). This classification is the input to every later stage and is
already implicit in the guide.

**Stage 1 — Signed, refined rule-level triggering graph (primary, user-facing).**
Combine §3.1 and §3.4. Nodes are automated rules. Add edge `A → B` when A's repair writes a
relation that occurs in B's antecedent; **label the edge `+` if A only inserts into it and
`−` if A can delete-or-merge it** [@Aiken1995StaticAnalysis; @Apt1988DeclarativeKnowledge].
Prune edges that cannot actually re-trigger B, as in the refined triggering graph and its
later refinements [@Urban1999RefinedTriggering; @Couchot2002ImprovingRTG]. **Report a risk for every cycle that contains at least one
`−` edge**, naming the rules and the shared relation — statically reproducing the runtime's
"Rules fixed in last run." Purely positive cycles are *not* flagged, because monotone repair
converges (Knaster–Tarski). Use the algebraic operator view to compute edge labels directly
from the relation-algebra terms [@Baralis2000AlgebraicApproach], and modularize per pattern
to scale [@Baralis1996Modularization].

*Why first:* highest value-to-effort ratio. It speaks in *rules* (the user's and the guide's
language), it is cheap, and with the sign refinement it already separates the dangerous
non-monotone cycles from the harmless monotone ones. Its known weakness is false positives.

**Stage 2 — EGD-aware position-level acyclicity (precision layer).**
For each rule cycle Stage 1 flags, run a weak-acyclicity / local-stratification check on the
underlying TGD/EGD encoding, with `MrgAtoms` treated as a first-class EGD
[@Fagin2005DataExchange; @Greco2011StratificationCriteria; @Calautti2016EGDChaseTermination].
If the cycle is weakly acyclic (no fresh value flows around it), **downgrade** it from
"risk" to "safe," removing the false alarm. This is exactly the FC5 discriminator: the
create→merge cycle is *not* weakly acyclic because the merge's EGD feeds a fresh organism
back into the create rule's antecedent.

*Why second:* it directly attacks Stage 1's only real weakness and targets the
create-vs-merge interaction that caused both FC5 oscillations, but it costs more to build
(value-flow modelling), so it is worth adding only after Stage 1 proves the concept.

**Stage 3 (optional, research) — Critical-pair confluence and native saturation.**
For rule sets that pass termination but where *order-independence* matters, add critical-pair
analysis to certify confluence [@Lambers2006ConflictDetection], and use Joosten's graph
saturation as the executable reference semantics that defines "triggers" precisely in
Ampersand's own relation algebra [@Joosten2018GraphSaturation]. Where neither termination nor
confluence can be shown, guardedness-style restrictions indicate which rule shapes remain
well-behaved [@Cali2013TamingChase].

**What this deliberately is not.** It is not a decision procedure — that is impossible
[@Joosten2018GraphSaturation; @Deutsch2008ChaseRevisited]. It is a *sound over-approximation*:
"safe" is a guarantee; "risk" is an honest "I cannot rule out an oscillation here, and these
are the colliding rules." That is precisely what the guide asks for.

---

## 5. Conclusions and advice

1. **The problem is solved in principle, not by us first.** The ExecEngine repair loop is a
   chase over TGDs (`Ins*`/`NewStruct`) and EGDs (`MrgAtoms`)
   [@Fagin2005DataExchange; @Onet2013ChaseProcedure], equivalently an active-rule system
   [@Aiken1995StaticAnalysis; @Paton1999ActiveDB] and a graph-rewriting system
   [@Lambers2006ConflictDetection; @Joosten2018GraphSaturation]. Each community already offers
   a *data-independent, syntactic* termination test. Ampersand can adopt one rather than
   invent one.

2. **Exact prediction is provably out of reach,** so aim for a sound over-approximation. The
   undecidability is established both in Ampersand's own relation algebra
   [@Joosten2018GraphSaturation] and for the chase [@Deutsch2008ChaseRevisited]. This justifies
   the guide's framing and sets the realistic target: certify *safe* soundly, and otherwise
   emit an explanatory *risk*.

3. **Advice — build it in two shippable stages.** Start with the **signed, refined
   rule-level triggering graph** (Stage 1): it is cheap, it speaks in rules, and the
   positive/negative edge labelling already encodes the monotone-vs-non-monotone insight the
   guide teaches [@Aiken1995StaticAnalysis; @Urban1999RefinedTriggering;
   @Apt1988DeclarativeKnowledge; @Baralis2000AlgebraicApproach]. Then add the **EGD-aware
   weak-acyclicity check** (Stage 2) to prune false positives on exactly the create-vs-merge
   interaction that caused the FC5 oscillations [@Calautti2016EGDChaseTermination;
   @Fagin2005DataExchange; @Greco2011StratificationCriteria]. Treat critical-pair confluence
   and graph saturation as a later research track [@Lambers2006ConflictDetection;
   @Joosten2018GraphSaturation].

4. **Pedagogical bonus.** The signed-cycle report is the *static* twin of the guide's
   *runtime* "Rules fixed in last run" message. Shipping it would let Ampersand warn a
   modeller about an oscillation **before** a single row is loaded — turning the guide's
   after-the-fact diagnosis into design-time feedback, which is the stated goal.

---

## 6. Stage 1 — implementation and validation (2026-06-06)

Stage 1 is implemented in the compiler. This section records what was built, the design
decisions taken, and how the result was validated, so that the choices are auditable and
Stage 2 can build on them.

### 6.1 What was built

A new module `Ampersand.FSpec.Oscillation` computes the signed, refined rule-level
triggering graph and emits one compiler warning per risky cycle. It runs inside
`pCtx2Fspec` (in `Ampersand.FSpec.ToFSpec.CreateFspec`), right after the existing
Cartesian-product check, so the analysis runs on every command that builds an FSpec —
`check`, `validate`, `proto`. The warning is constructed by `mkOscillationWarning` in the
central `CtxError` module, alongside the other compiler warnings. The analysis is always
on (not gated behind `--verbose`), because oscillation risk is a design-time concern the
modeller should always see.

The graph is built as follows.

- **Nodes** are the automated rules: the rules maintained by the `ExecEngine` role
  (`fRoleRuls`). Invariants and human-role process rules are excluded, because only the
  ExecEngine repairs data automatically and can therefore loop.
- **Writes per rule.** Each rule's `VIOLATION` repair script is parsed from its `PairView`
  by splitting the concatenated text segments on the `{EX}` marker and then on `;`. The
  function name and the relation/concept names are always literal text in a `{EX}`
  instruction (they can never be computed from an expression), so this text-level parse is
  robust even though the atom-value arguments are expression segments. Each instruction
  maps to signed writes, following the classification of Stage 0:
  `InsPair` → positive write on the named relation; `DelPair` → negative write;
  `MrgAtoms;C` → both a positive and a negative write on every relation on concept `C`
  (a merge re-routes pairs: it removes `(b,x)` and adds `(a,x)`); `DelAtom;C` → negative
  write on every relation on `C`; `InsAtom;C`/`NewStruct;C` → positive writes on the
  relations on `C`. "Relations on `C`" is restricted to the relations actually read by
  some automated rule, since only those can carry an edge.
- **Edges.** There is an edge `A → B`, signed with `A`'s write, whenever a write of `A` can
  *increase* `B`'s set of violations. Which writes can do so is decided by the monotonicity
  refinement (§6.2). The edge is negative iff the triggering write deletes or merges.
- **Risk.** The graph's strongly-connected components are computed (`Data.Graph`). A
  component is reported iff it is cyclic *and* contains at least one internal **negative**
  edge. A purely positive (insert-only) cycle converges to a least fixpoint by
  Knaster–Tarski and is deliberately not flagged.

### 6.2 The monotonicity refinement, and why it is essential

A bare triggering graph — "edge `A → B` whenever `A` writes a relation that occurs in `B`" —
flags far too much. The decisive example: a uniqueness rule `key;key~ |- I[E]` repaired by
`MrgAtoms;E`. The merge writes `key`, and the rule reads `key`, so the bare graph draws a
*negative self-loop* and reports every uniqueness rule in every model as an oscillation
risk. That is a false alarm: a merge strictly reduces the number of atoms, so it always
terminates on its own.

The fix is the refinement that the literature attaches to Stage 1 ("prune edges that cannot
actually re-trigger `B`", §3.1, §4). A rule `a |- c` is violated by the pairs in `a - c`,
so its violation set grows only when a relation that occurs **positively** in `a - c` grows,
or one that occurs **negatively** shrinks. We compute, by a standard polarity walk over the
relation-algebra term (composition, union, intersection, Kleene and converse preserve sign;
difference flips its right argument; complement flips; residuals and the diamond are treated
conservatively as both signs), the set of signs with which each relation occurs in `a - c`.
An **insert** can re-trigger `B` only on a relation that occurs positively there; a
**delete** only on one that occurs negatively. In the uniqueness example `key` occurs only
positively in `key;key~`, so the merge's delete cannot grow the rule's own violations — the
self-loop disappears, exactly as it should.

### 6.3 Design decisions and deliberate deviations

- **Soundness target: whole rule expression, not only the antecedent.** §2 and §4 phrase the
  edge as "`A` writes a relation in `B`'s *antecedent*". We use `B`'s whole expression
  (antecedent and consequent), because a repair that shrinks `B`'s *consequent* also creates
  a violation of `B`. Restricting to the antecedent would miss those triggers — a false
  negative, which requirement (3) forbids. The monotonicity walk already handles antecedent
  and consequent with the correct signs, so this is the natural sound reading.
- **Reporting granularity: strongly-connected components.** We report one warning per SCC
  that contains a negative edge, rather than enumerating elementary cycles. An SCC is exactly
  the "minimal set of mutually-triggering rules" of requirement (2), it is cheap and
  deterministic, and every negative edge inside an SCC lies on some cycle. Enumerating
  individual elementary cycles (finer, but exponential in the worst case) is left for later.
- **Atom-level effects modelled through relations.** Merge, delete-atom and insert-atom are
  modelled by their effect on the *relations* on the concept, which is how oscillations
  actually propagate (the FC5 loop runs through `voorkeursNaam` and `eppoCode`). We do *not*
  model the rarer trigger in which the bare *existence* of a fresh atom violates a rule
  phrased purely on `I[C]` or `V` (e.g. `I[C] |- r`) without any relation pair changing.
  `InsAtom` almost always comes paired with an `InsPair` that does the real triggering, so
  this keeps the analysis simple at a small, documented soundness cost. See the open
  question in §7.
- **No `--verbose` gate.** Unlike the Cartesian-product warning, the risk warning is always
  emitted. It is a design-time signal, not a performance hint.

### 6.4 Validation

Five scripts were checked with `stack exec ampersand -- check`. The first two are the guide's
own minimal reproductions; the last three are crafted to probe the refinement and live in
`testing/oscillation/` with a `testinfo.yaml`, so the regression suite type-checks them on
every run.

| Script | Automated rules | Expected | Result |
| --- | --- | --- | --- |
| `docs/guides/oscillations/oscillatie-buggy.adl` | `maakOrganisme`, `uniekeCode`, `uniekeNaam` | risk | **risk** — one SCC of all three, naming the `voorkeursNaam`/`eppoCode` collisions |
| `docs/guides/oscillations/oscillatie-fixed.adl` | same names, code-driven creation + `maakSynoniem` | risk (see note) | **risk** — same SCC |
| `testing/oscillation/mono-cycle.adl` | two insert-only rules in a cycle | no risk | **silent** |
| `testing/oscillation/merge-alone.adl` | one `MrgAtoms` uniqueness rule | no risk | **silent** |
| `testing/oscillation/insdel-cycle.adl` | an `InsPair`/`DelPair` pair on one relation | risk | **risk** — names the `light` collision |

The two negative cases (`mono-cycle`, `merge-alone`) confirm the analysis is not the trivial
"flag every cycle": the monotone cycle and the standalone merge rule are both correctly left
silent. The `insdel-cycle` case confirms a genuine non-monotone cycle is caught.

**Note on `oscillatie-fixed.adl`.** Stage 1 *does* flag the fixed script, and this is the
correct Stage-1 behaviour rather than a bug. The fix in the guide (code-driven creation)
makes the rules terminate for a *value-flow* reason: after the merge, the code still has an
Organisme, so the create rule's antecedent stays empty. A signed rule-level graph cannot see
that — `voorkeursNaam` still occurs negatively in the create rule's consequent, so the merge's
delete is, at the rule level, a possible re-trigger. Certifying the fixed script *safe* is
exactly the job of **Stage 2** (EGD-aware weak acyclicity), which §4 introduces for precisely
this discriminator. Stage 1 honestly reports "I cannot rule out an oscillation here"; Stage 2
will downgrade it to "safe". This matches the staged design and the undecidability result: a
sound over-approximation must over-flag, and Stage 2 is where the false positive is removed.

## 7. Open questions for the next session

1. **Bare-atom triggering (the §6.3 soundness gap).** Should Stage 1 also model the trigger
   where a freshly created atom violates a rule phrased on `I[C]`/`V` with no relation pair
   involved? Doing so soundly reintroduces the merge/delete self-loop noise unless handled
   with care. The pragmatic choice was to leave it to Stage 2 (which tracks atom/value
   provenance anyway). Confirm this is acceptable, or ask for it in Stage 1.
2. **Whether to flag `oscillatie-fixed.adl` now.** Stage 1 flags it (correctly, see §6.4). If
   that is judged too noisy before Stage 2 lands, an interim option is to lower the
   fixed-script-style false positives by also requiring the cycle's negative edge to be a
   *merge/delete of a relation the partner rule can re-create* — but that starts to anticipate
   Stage 2. Preference?
3. **Severity and gating.** The risk is emitted as a normal compiler *warning*, always on.
   Should it instead be `--verbose`-gated (like the Cartesian-product warning), or promoted to
   something more prominent?
4. **Wording and locale.** The warning text is English and points to
   `docs/guides/oscillations/README.md`. Is that the right destination, and is English right
   given the FC5 audience?

---

*Reference list: see `literature_search.csv` (search id 1). The open-access copy of
[@Joosten2018GraphSaturation] is archived under `copies/`; the remaining 15 items are queued
in `literature_search/worklist.md` for OU-library retrieval.*
