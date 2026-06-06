# Understanding oscillations in Ampersand — a worked case

> **Who is this for?** Students and developers who work with Ampersand and want to understand
> why the ExecEngine sometimes gets stuck with *"Maximum reruns exceeded"*.
>
> **Learning goals.** You learn to (1) recognise the phenomenon, (2) trace it back to your
> rules, which collide, (3) understand that an oscillation expresses a mathematical inconsistency, and
> (4) use such an oscillation as an *opportunity* to make your rules (or your data)
> consistent. By the end you can reproduce an example by yourself with a minimal script.
>
> **Key message.** An oscillation is not an annoying bug that you silence by raising the
> rerun limit. It is the runtime telling you: *your rules contradict each other.* Use
> that to make them consistent.
>
> **About the example.** This case comes from a Dutch project (FC5, plant-health inspection),
> so the rule and relation names are Dutch — for instance `Organisme` (organism),
> `voorkeursNaam` (preferred name), `eppoCode`, and `WetenschappelijkeNaam` (scientific name).
> The reasoning is language-independent.

---

## Before you start: rules and the ExecEngine

In Ampersand you write **rules**, and you write them with one of two statements:

- a **`RULE`** statement — a constraint, optionally followed by `ROLE ... MAINTAINS`, which says
  who keeps the rule satisfied;
- an **`ENFORCE`** statement — a syntax-sugared `RULE` that the ExecEngine keeps satisfied:
  shorter to write, same meaning.

See [the RULE statement](../../reference-material/syntax-of-ampersand.md#the-rule-statement) and
[the ENFORCE statement](../../reference-material/syntax-of-ampersand.md#the-enforce-statement) in
the reference material for the full syntax.

A rule of the form `antecedent |- consequent` ("antecedent is a subset of consequent") is broken
by **violations**: the pairs in the antecedent that are missing from the consequent. What happens
next depends on the `ROLE ... MAINTAINS`:

- **Invariant** — a `RULE` without `ROLE ... MAINTAINS`. It must hold after every transaction. A
  violation makes Ampersand block the transaction and roll everything back. For a data import,
  the whole import then fails.
- **Process rule** — a `RULE` with `ROLE <role> MAINTAINS` for a human role. A violation is
  allowed; the user in that role gets a **signal**, which stays until that user resolves it. The
  transaction proceeds.
- **Automated rule** — a `RULE` with `ROLE ExecEngine MAINTAINS`, or the shorter `ENFORCE` that
  expands to one. The ExecEngine is a role the runtime plays itself, so a violation is repaired
  automatically, with no person involved.

An automated rule can therefore be written two ways — a `RULE` plus `ROLE ExecEngine MAINTAINS`,
or the shorter `ENFORCE` — and both hand the rule to the ExecEngine. Automated rules are the crux
of oscillations: because the ExecEngine **changes the data by itself** to satisfy them, it can
keep changing it forever. That is why understanding oscillations starts with the ExecEngine.

In the `RULE ... MAINTAINS` form, the repair is the rule's `VIOLATION` script. Each instruction
in it starts with `{EX}` and calls a built-in function:

| Function | Does |
| --- | --- |
| `InsAtom;C` | creates a new atom in concept `C` (placeholder `_NEW`) |
| `DelAtom;C;a` | deletes atom `a` from concept `C` |
| `InsPair;r;A;a;B;b` | adds pair `(a,b)` to relation `r[A*B]` |
| `DelPair;r;A;a;B;b` | removes pair `(a,b)` from `r` |
| `MrgAtoms;C;a;C;b` | merges atom `b` into `a`: all links of `b` move to `a`, `b` disappears |

**The rerun loop.** The ExecEngine works iteratively:

1. Evaluate all automated rules and collect the violations.
2. Run their `{EX}` fixes.
3. Those fixes change the population → *new* violations may arise, or old ones may be solved.
   So: **rerun** (back to step 1).
4. Stop once a round fixes nothing more (a **fixpoint**: violations = 0).

If this never converges, the loop would run forever. So there is a
**maximum number of reruns**. When it is reached, the engine stops with the error:

```text
Maximum reruns exceeded. Rules fixed in last run: <rule A>, <rule B>
```

That last line names your suspects: **"Rules fixed in last run"** lists exactly the rules
still fixing in the last round.

---

## 1. The phenomenon — how do you spot it?

In the FC5 case this surfaced while loading the EPPO code list. The prototype log
(`docker compose logs prototype`) shows:

```text
EXECENGINE.ERROR: Maximum reruns exceeded. Rules fixed in last run:OrganismeUniekeEPPO, eppoCodeMaaktOrganisme)
APPLICATION.ERROR: Maximum reruns exceeded for ExecEngine
   {"Rules fixed in last run":["...Rule: OrganismeUniekeEPPO","...Rule: eppoCodeMaaktOrganisme"]}
```

Around it you see a typical rhythm of fixes alternating:

```text
EXECENGINE.INFO: InsAtom(Organisme)
EXECENGINE.INFO: InsPair(voorkeursNaam,Organisme,_NEW,WetenschappelijkeNaam,'Candidatus Phytoplasma solani')
EXECENGINE.INFO: InsPair(eppoCode,Organisme,_NEW,EPPOcode,PHYPSO)
...
EXECENGINE.INFO: MrgAtoms(Organisme, <id-1>, Organisme, <id-2>)
EXECENGINE.NOTICE: ExecEngine fixed 16 violations for rule 'OrganismeUniekeEPPO'
...
EXECENGINE.NOTICE: ExecEngine fixed 8 violations for rule 'eppoCodeMaaktOrganisme'
EXECENGINE.ERROR: Maximum reruns exceeded ...
```

> **Why is this already suspicious?** A healthy ExecEngine run *descends* towards zero
> violations: each round fixes less. Here you see the opposite — `MrgAtoms` (merge) and
> `InsAtom`/`InsPair` (create) keep alternating. Something is created, removed, created again.
> That back-and-forth is the **oscillation**.

A second, serious consequence you do not see right away: the import ran inside a
**transaction**. The error rolls it back. Concretely, in the FC5 database the source relation
`eppoCode[WetenschappelijkeNaam*EPPOcode]` stayed at **1 row** (instead of ~1400) — so the
whole EPPO list was not loaded, while the app still "seemed" to run.

> **Lesson 1.** An oscillation is doubly damaging: it stops the engine, and because it leaves
> violations unresolved, the surrounding transaction rolls back. "The app starts" does not mean
> "the data is loaded".

---

## 2. The analysis — how do you trace it back?

This section works from symptom to cause: it establishes the cause first, and only then
chooses a fix.

**Step 1 — Identify the colliding rules.** The error's "Rules fixed in last run" line lists the
suspects: the automated rules still firing when the engine gave up. Often that is the whole
conflict, as here — `OrganismeUniekeEPPO` and `eppoCodeMaaktOrganisme`. When the line names more
rules, three signals from the log narrow it down:

- **Opposing actions.** A cycle needs two fixes that undo each other: one that *adds* a fact
  (`InsAtom`, `InsPair`) and one that *removes* it (`DelPair`, `DelAtom`, `MrgAtoms`), touching
  the same relation or atom. The pair whose actions alternate is the conflict.
- **Counts that do not fall.** Each round logs `ExecEngine fixed N violations for rule X`. A rule
  in the cycle keeps reporting violations round after round; a rule outside it drops to zero and
  stays there.
- **A write that feeds another antecedent.** For each suspect, see what its `VIOLATION` script
  changes, and whether that change lands in another suspect's antecedent. If rule A's fix creates
  a violation of rule B, and B's fix re-creates one of A, those two are the pair.

Find the definitions of the colliding rules in the `.adl` source:

```text
-- create rule
ROLE ExecEngine MAINTAINS eppoCodeMaaktOrganisme
RULE eppoCodeMaaktOrganisme : eppoCode - voorkeursNaam~;V[Organisme*EPPOcode] |- voorkeursNaam~;I[Organisme];eppoCode
VIOLATION ( TXT "{EX} InsAtom;Organisme"
          , TXT "{EX} InsPair;voorkeursNaam;Organisme;_NEW;WetenschappelijkeNaam;", SRC I[WetenschappelijkeNaam]
          , TXT "{EX} InsPair;eppoCode;Organisme;_NEW;EPPOcode;", TGT I )

-- merge rule
ROLE ExecEngine MAINTAINS OrganismeUniekeEPPO
RULE OrganismeUniekeEPPO : eppoCode[Organisme*EPPOcode];eppoCode[Organisme*EPPOcode]~ |- I[Organisme]
VIOLATION ( TXT "{EX} MrgAtoms;Organisme;", SRC I, TXT ";Organisme;", TGT I )
```

**Step 2 — Translate each rule into plain language.**

- `eppoCodeMaaktOrganisme`: "for every scientific name in the source that is not yet the
  `voorkeursNaam` of an Organisme: create an Organisme with that name and that EPPO code."
  → this is **name-driven**: one Organisme per *name*.
- `OrganismeUniekeEPPO`: "two Organismen with the same EPPO code are the same" → **merge per
  code**: one Organisme per *code*.

**Step 3 — Read the `{EX}` actions in the log as evidence.** The `InsPair(voorkeursNaam,…)`
and `InsPair(eppoCode,…)` show which names and codes are involved. Follow them and you watch
the same Organisme appear, merge, and appear again.

**Step 4 — Check the input (the data), not just the logic.** Ask: *can the source violate the
assumptions of these rules?* Query the source:

```sql
SELECT WetenschappelijkeNaam, COUNT(DISTINCT EPPOcode)
FROM <source table> GROUP BY WetenschappelijkeNaam HAVING COUNT(DISTINCT EPPOcode) > 1;  -- one name, several codes?
-- and the reverse: one code, several names?
```

In the FC5 source it turned out: **several names point to the same EPPO code** (synonyms).
That is the trigger.

---

## 3. The cause — concrete and mathematical

### 3a. Concrete: the script of the loop

Take one code `X` with two names `A1` and `A2` (synonyms). Follow the ExecEngine round by
round:

1. **Round 1, create.** `eppoCodeMaaktOrganisme` is name-driven. `A1` and `A2` are not yet a
   voorkeursNaam, so the rule fires twice: `Org1(voorkeursNaam=A1, eppoCode=X)` and
   `Org2(voorkeursNaam=A2, eppoCode=X)` appear.
2. **Round 1, merge.** `OrganismeUniekeEPPO` sees two Organismen with code `X` → `MrgAtoms`.
   The result is one Organisme. But `voorkeursNaam` is `[UNI]` (at most one per Organisme), so
   after the merge only one name survives, say `A1`. **`A2` has lost its voorkeursNaam.**
3. **Round 2, create.** Now `A2` is again the voorkeursNaam of no Organisme → the create rule
   fires again for `(A2, X)` → another Organisme appears for `A2`.
4. **Round 2, merge.** Two Organismen with code `X` → merge → `A2` loses its name again.
5. → back to step 3. **Create → merge → create → merge → …** Forever.

The engine never reaches zero violations and stops at the rerun limit.

### 3b. Mathematical: the rules are jointly unsatisfiable

Write the requirements as statements about
relations (in relation algebra; read `;` as composition and `~` as converse):

- `voorkeursNaam` is `[UNI,INJ]` → an **injective partial function** `Organisme →
  WetenschappelijkeNaam`: each Organisme has at most one name, and each name belongs to at most
  one Organisme.
- `eppoCode` is `[UNI]`, and `OrganismeUniekeEPPO` adds that different Organismen may not share
  a code → `eppoCode` is **also** an injective partial function `Organisme → EPPOcode`.
- `eppoCodeMaaktOrganisme` requires that *every* `(name, code)` from the source has an
  Organisme with exactly that name as voorkeursNaam and that code.

Add up these requirements. Through the Organisme as an intermediate step, together they force
a **bijection** between the names and the codes that occur in the source:

```text
WetenschappelijkeNaam  <--(voorkeursNaam, bijective)-->  Organisme  <--(eppoCode, bijective)-->  EPPOcode
```

A composition of two bijections is a bijection. So the rules require the source mapping
**name ↔ code to be one-to-one**.

But the **data** says: `A1 ↦ X` and `A2 ↦ X` — two names, one code. That is by definition
*not* injective, so *not* a bijection. Therefore **no population** satisfies all rules at
once. Given this data, the specification is **unsatisfiable (inconsistent)**.

> **Why does an infinite loop follow, and not "just" an error message?**
> The ExecEngine looks for a **fixpoint**: a state it no longer needs to repair. That works
> cleanly when repairs are *monotone* (they only add facts): the population then grows towards
> a least fixpoint and stops (Kleene/Tarski theorem). Here it does not: `MrgAtoms`
> **removes** a fact (a voorkeursNaam) that the create rule immediately **adds** again. The
> repair mapping is non-monotone and has no common fixpoint; instead it describes a **cycle
> with period 2** (create ↔ merge). No fixpoint means no termination. The rerun limit cuts off
> the infinite loop.
>
> In short: **unsatisfiable rules + automatic repair = oscillation.** The oscillation is
> the *observable consequence* of a *logical contradiction*.
>
> **Lesson 2.** Each individual rule was reasonable ("codes unique", "names unique",
> "everything gets an Organisme"). The *combination* is the contradiction. Inconsistency is a
> property of the *set* of rules, not of one rule.

---

## 4. The solution choices — and the principle behind them

When rules are jointly unsatisfiable with the data, you have three choices:

**Choice A — Weaken the model so the requirements can hold together.**
The contradiction arose because we demanded both "one Organisme per name" and "one Organisme
per code". Drop one side: allow one code to have several names, with one *preferred name* and
the rest as *synonyms*. Then the mapping code → {names} is legal and the bijection requirement
disappears. → Fits **synonyms**, a genuine domain phenomenon.

**Choice B — Repair the data so it can satisfy the requirements.**
If the contradiction stems from an *error* in the source (not a genuine domain phenomenon),
make the mapping correct. → Fits **typos and placeholders** (see §6, oscillation #2).

**Choice C — Demote an overly strict invariant to a signal.**
Sometimes you do not want to repair automatically; you want a human to decide. Change the rule
from an invariant (or an automated rule) into a **process rule** under a human role. The
violation then crashes nothing; it appears as a worklist. → Fits **checks** that really mean
"report this to the administrator".

> **The principle.** The goal is not "make the oscillation go away" but **make the rule set
> jointly satisfiable** — by adjusting the model, the data, or the strictness. The oscillation
> told you exactly which rules to examine.
>
> **Anti-pattern:** raising the rerun limit. That hides the contradiction; it stays and breaks
> your system elsewhere (a rolled-back transaction, missing data).

---

## 5. The solution (oscillation #1: synonyms)

Chosen: **A** (weaken the model) — because synonyms are legitimate.

**Change 1 — make creation code-driven instead of name-driven.** No longer "one Organisme per
name", but "one Organisme per code that does not have one yet". Only the subtracted term in
the antecedent changes:

```text
-- was (name-driven): subtract = "name is already a voorkeursNaam"
RULE eppoCodeMaaktOrganisme : eppoCode - voorkeursNaam~;V[Organisme*EPPOcode] |- voorkeursNaam~;I[Organisme];eppoCode

-- becomes (code-driven): subtract = "code is already on an Organisme"
RULE eppoCodeMaaktOrganisme : eppoCode - V[WetenschappelijkeNaam*Organisme];eppoCode[Organisme*EPPOcode] |- voorkeursNaam~;I[Organisme];eppoCode
```

**Change 2 — keep the non-chosen names as synonyms** (otherwise the merge loses them):

```text
ROLE ExecEngine MAINTAINS eppoCodeSynoniem
RULE eppoCodeSynoniem : eppoCode;eppoCode[Organisme*EPPOcode]~ - voorkeursNaam~ |- synoniem~
VIOLATION ( TXT "{EX} InsPair;synoniem;Organisme;", TGT I, TXT ";WetenschappelijkeNaam;", SRC I )
```

**Why does this terminate now?** Follow `X` with names `A1, A2` again:

1. Round 1: code-driven creation still makes two Organismen for `X` this round;
   `OrganismeUniekeEPPO` merges them into one (voorkeursNaam = `A1`); `eppoCodeSynoniem` sets
   `A2` as a synonym.
2. Round 2: code `X` now *has* an Organisme → the create rule fires **no more** (that is the
   difference!); the merge is satisfied; the synonym is already there. **Zero violations →
   stop.**

The crucial change: creation now hangs on the **code** (which the merge leaves intact), not on
the **name** (which the merge throws away). So creation no longer "restores" what the merge
just did. The loop breaks because the rules are now **jointly satisfiable**: one code → one
Organisme → one preferred name + zero-or-more synonyms. No bijection requirement anymore.

---

## 6. Same phenomenon, different cause (oscillation #2)

After fixing #1 a **second** oscillation appeared — now between `OrganismeUniekeNaam` and
`eppoCodeMaaktOrganisme`. This is the **mirror image**: not several names per code, but
**several codes per name**.

`OrganismeUniekeNaam` merges Organismen with the same voorkeursNaam. Because creation is now
code-driven, the model makes one Organisme per code; if two codes carry the same name, two
Organismen with identical voorkeursNaam appear → merge → but `eppoCode` is `[UNI]`, so the
merge throws away one code → that code gets no Organisme again → creation makes it again →
loop. Mathematically: the same bijection requirement, now violated on the **name side**.

The cause turned out to be **dirty data** in `EPPOcodes.xlsx`:

- six non-existent codes shared the placeholder name `(code niet gevonden in EPPO)`;
- an O/0 typo: `Begomovirus coheni` appeared under both `TYLCV0` (digit zero) and `TYLCVO`
  (letter O).

Here **choice B (repair the data)** is right, not A. An important methodological point:
**consult the source of truth.** The EPPO database (`data.eppo.int`) settled it:

| Code | EPPO `/names` | Verdict |
| --- | --- | --- |
| `TYLCV0` | Begomovirus coheni | real → keep |
| `TYLCVO` | `null` | does not exist → typo, remove |
| the 6 placeholder codes | `null` | do not exist → remove |

> **Lesson 3.** One symptom (oscillation), two different causes, two different correct fixes.
> Synonyms are a *model issue* (weaken the model); typos are a *data issue* (repair the data).
> The mathematical diagnosis — which side of the bijection is violated, and is that a genuine
> domain phenomenon or an error? — points you to the right choice.

### Robustness afterwards: invariant → process rule

What if contradictory data arrives again later? Then you want the import to **report** it
instead of crash. That is **choice C**, applied to the invariant `checkEPPOcode` (which checks
whether the EPPO code of a POcombinatie matches the code list):

```text
ROLE IMPORTER MAINTAINS checkEPPOcode      -- from invariant to process rule
RULE checkEPPOcode : ...
```

Now an inconsistency no longer blocks the import; it appears as a signal for the role
`IMPORTER`. (Note: do this *only* for *readable checks*. An `{EX}` fix makes a rule an automated
rule, which belongs to the ExecEngine, not to a human role — otherwise the `{EX}` text shows up
as an unreadable signal.)

---

## 7. Validation — how do you know it really works?

A fix is verified by measurement, not by the impression that it works. After the rebuild
(`./nvwa_prototype_init.sh`):

1. **No more oscillation** in the log:

   ```bash
   docker compose logs prototype 2>&1 | grep -ci "maximum reruns exceeded"   # expect: 0
   ```

2. **The transaction no longer rolls back** — the data is now really loaded:

   | measurement | before | after |
   | --- | --- | --- |
   | `eppoCode[WN*EPPO]` (rows) | 1 | 1400 |
   | Organismen | 792 | 1496 |
   | synonym pairs | 0 | 4295 |

3. **The requirements now demonstrably hold** (the ones that caused the oscillation):

   ```sql
   -- every code belongs to exactly one Organisme? -> expect 0
   SELECT COUNT(*) FROM (SELECT eppoCode FROM Organisme WHERE eppoCode IS NOT NULL
                         GROUP BY eppoCode HAVING COUNT(*)>1) t;
   -- no Organisme without a code? -> expect 0
   SELECT COUNT(*) FROM Organisme WHERE eppoCode IS NULL;
   ```

4. **Spot checks against the source of truth:** `TYLCV0 → Begomovirus coheni` (one record, no
   duplicate), and no Organisme left with the name `(code niet gevonden in EPPO)`.

> **Lesson 4.** An oscillation fix is only done when you can show that (a) the loop is gone,
> (b) the surrounding transaction now succeeds, and (c) the rules that collided now actually
> hold. Point (c) proves you *solved* the contradiction rather than *hid* it.

---

## 8. Reproduce it yourself (minimal example)

Alongside this lesson sit two self-contained scripts that compile cleanly:

- **`oscillatie-buggy.adl`** — reproduces the oscillation. It holds the name-driven creation
  plus the two merge rules (`uniekeCode`, `uniekeNaam`), and a population with two names on one
  code. At the bottom sits a second population (one name, two codes) that you can switch on to
  see oscillation #2.
- **`oscillatie-fixed.adl`** — the solved version (code-driven creation + `maakSynoniem`). It
  converges on the synonym case.

These minimal scripts use short rule names; mapped to the case above, `maakOrganisme` is
`eppoCodeMaaktOrganisme`, `uniekeCode` is `OrganismeUniekeEPPO`, `uniekeNaam` is
`OrganismeUniekeNaam`, and `maakSynoniem` is `eppoCodeSynoniem`.

**Type-check** (this validates only the syntax and types, not the runtime loop). Run it in the
folder that holds the scripts:

```bash
docker run --rm --platform linux/amd64 -v "$PWD:/scripts" \
  ampersandtarski/ampersand-compiler:latest check /scripts/oscillatie-buggy.adl
# => "contains no type errors and no population errors."
```

**To really see the oscillation** you need the *runtime* (the ExecEngine does not run on
`check`, only when you run a prototype). Generate and start a prototype of the script —
easiest in a [RAP environment](../../tutorial-rap4.md), or with a local
[prototype deployment](../deploying-your-prototype.md). Then trigger the ExecEngine (the
"run execengine" action or a data import) and read the log:

- with `oscillatie-buggy.adl` the message `Maximum reruns exceeded` appears, naming the two
  colliding rules;
- with `oscillatie-fixed.adl` the run descends cleanly to zero violations.

**Experiments to try:**

1. In the buggy version, change the population to two *different* codes for two *different*
   names. → No oscillation. (Why? The bijection requirement is not violated.)
2. In the fixed version, remove the rule `maakSynoniem`. → No oscillation, but the second name
   is *lost*. (Lesson: terminating is not the same as correct.)
3. In the buggy version, switch on the second population (one name, two codes). → Oscillation
   #2, now with `uniekeNaam` as the colliding rule. Repair it by removing the typo from the
   population (choice B), not by deleting a rule.

---

## 9. Summary — the oscillation as an opportunity

- An oscillation (`Maximum reruns exceeded`) means: **given the data, your rules are jointly
  unsatisfiable.** The automatic repair finds no fixpoint and cycles.
- The error message points at the **colliding rules** (the ones still being fixed). That is a
  gift: it locates the contradiction.
- Diagnose **mathematically**: which impossible requirement do the rules impose together
  (here: a bijection name ↔ code), and which side does the data violate?
- Choose with oscillation risks in mind: **weaken the model** (A, for a genuine domain phenomenon such as
  synonyms), **repair the data** (B, for errors — consult the source of truth), or **demote an
  invariant to a signal** (C, when a human must decide).
- **Validate** that the loop is gone, the transaction succeeds, and the rules now really hold.

> An oscillation is not a setback but **feedback**: the runtime proves that your rules
> contradict each other and points to where. Use that knowledge to make your specification
> consistent — then your model is demonstrably the better for it.

---

*Related documents:* [Automating Rules in Ampersand](../../conceptual/automated-rules.md)
(how ExecEngine rules repair the population automatically — the background under this case),
and [Best practices for Ampersand modellers](../best-practices.md).
