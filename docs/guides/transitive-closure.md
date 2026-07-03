# Computing Transitive Closures

## The problem

Ampersand supports `r+` and `r*` as operators in relational expressions. The prototype generator does not translate these operators to the database layer. This means that if you need the transitive closure of a relation at runtime — to query it, enforce rules on it, or display it in an interface — you must store it in a separate relation.

The solution is to maintain that relation using the ExecEngine. The ExecEngine recomputes the transitive closure whenever the base relation changes. This uses the `TransitiveClosure` function, which is implemented in PHP using Warshall's algorithm. For the theoretical background on automated rules, see [Automating Rules in Ampersand](../conceptual/automated-rules.md).

## The pattern

You need three relations and one rule. Suppose your base relation is `r[C*C]`.

```ampersand
RELATION r[C*C]          -- the relation whose transitive closure you need
RELATION rCopy[C*C]      -- a copy of r; used to detect changes
RELATION rPlus[C*C]      -- the transitive closure of r, i.e. r+
```

The rule detects changes in `r` by comparing it to `rCopy`. When they differ, the ExecEngine recomputes `rPlus` from scratch.

```ampersand
ROLE ExecEngine MAINTAINS "Compute transitive closure of r"
RULE "Compute transitive closure of r" : r = rCopy
VIOLATION (TXT "{EX} TransitiveClosure;r;C;rCopy;rPlus")
```

The call `TransitiveClosure;r;C;rCopy;rPlus` tells the ExecEngine to:
1. empty `rCopy` and `rPlus`,
2. copy all pairs from `r` into `rCopy`,
3. compute the transitive closure of `r` and store it in `rPlus`.

After this rule fires, `rPlus` contains exactly the pairs reachable via one or more steps through `r`. This is the irreflexive transitive closure `r+`. It excludes self-loops unless they exist in `r`.

Use `r = rCopy` rather than `r |- rCopy`. The `=` form detects both additions and deletions in `r`. The `|-` form only detects additions and misses the case where a pair is removed from `r`.

## Adding the reflexive transitive closure

If you also need `r*` — the reflexive transitive closure that includes every atom paired with itself — add a fourth relation and keep it synchronized with `ENFORCE`.

```ampersand
RELATION rStar[C*C]      -- the reflexive transitive closure of r, i.e. r*

ENFORCE rStar := I[C] \/ rPlus
```

The `ENFORCE` statement keeps `rStar` up to date automatically whenever `rPlus` changes. The difference in use is:

- Use `rPlus` when the item itself must not appear in the result.
- Use `rStar` when the item itself must appear in the result.
- Use `I \/ rPlus` as an inline expression when you do not want to store `rStar` as a separate relation.

## Application 1: Management chain

In an organization, `reports[Employee*Employee]` records who directly reports to whom. You need to know who reports indirectly to a given manager, for example to determine access rights or to send notifications through the chain.

```ampersand
RELATION reports[Employee*Employee] [UNI]
MEANING "An employee reports directly to another employee."

RELATION reportsCopy[Employee*Employee]
RELATION reportsPlus[Employee*Employee]
MEANING "`reportsPlus` is the transitive closure of `reports`."
RELATION reportsStar[Employee*Employee]
MEANING "`reportsStar` is the reflexive transitive closure of `reports`."

ROLE ExecEngine MAINTAINS "Compute transitive closure of reports"
RULE "Compute transitive closure of reports" : reports = reportsCopy
VIOLATION (TXT "{EX} TransitiveClosure;reports;Employee;reportsCopy;reportsPlus")

ENFORCE reportsStar := I[Employee] \/ reportsPlus
```

The expression `reportsPlus~` gives all employees who (directly or indirectly) report to a given employee, not including that employee themselves. The expression `reportsStar~` includes the employee themselves.

## Application 2: Category taxonomy

A webshop organizes products in a category hierarchy. `parentCat[Category*Category]` records the direct parent of each category. To show all products in a category including those in subcategories, you need the transitive closure.

```ampersand
RELATION parentCat[Category*Category] [UNI,IRF]
MEANING "A category is contained in a parent category."

RELATION parentCatCopy[Category*Category]
RELATION parentCatPlus[Category*Category]
MEANING "`parentCatPlus` is the transitive closure of `parentCat`."
RELATION parentCatStar[Category*Category]
MEANING "`parentCatStar` is the reflexive transitive closure of `parentCat`."

ROLE ExecEngine MAINTAINS "Compute transitive closure of parentCat"
RULE "Compute transitive closure of parentCat" : parentCat = parentCatCopy
VIOLATION (TXT "{EX} TransitiveClosure;parentCat;Category;parentCatCopy;parentCatPlus")

ENFORCE parentCatStar := I[Category] \/ parentCatPlus

RELATION productIn[Product*Category]
MEANING "A product belongs to a category."
```

The expression `productIn;parentCatStar~` gives all products in a category and all its subcategories. Because `parentCatStar` includes the identity, products that belong directly to the category appear in the result as well.

## Application 3: Task dependencies

In project management, `dependsOn[Task*Task]` records that one task must be completed before another can start. You need all indirect dependencies to detect circular dependencies or to compute the order in which tasks must be done.

```ampersand
RELATION dependsOn[Task*Task] [IRF]
MEANING "A task depends directly on another task."

RELATION dependsOnCopy[Task*Task]
RELATION dependsOnPlus[Task*Task]
MEANING "`dependsOnPlus` is the transitive closure of `dependsOn`."

ROLE ExecEngine MAINTAINS "Compute transitive closure of dependsOn"
RULE "Compute transitive closure of dependsOn" : dependsOn = dependsOnCopy
VIOLATION (TXT "{EX} TransitiveClosure;dependsOn;Task;dependsOnCopy;dependsOnPlus")

RULE "No circular dependencies" : dependsOnPlus |- -I[Task]
MEANING "A task cannot depend on itself, directly or indirectly."
```

The rule `"No circular dependencies"` uses `dependsOnPlus` to detect cycles. When it is violated, the task listed in the violation depends on itself through a chain of dependencies. The reflexive closure `rStar` is not needed here; `dependsOnPlus` alone is sufficient because a cycle shows up as a pair `(a, a)` in `dependsOnPlus`.

## The transitive reduction `r%`

Since the `%` operator, Ampersand also offers the *transitive reduction*: `r%` keeps only the pairs that no longer path already provides — the Hasse diagram of `r`. It is syntactic sugar for `r - (r;r+)`, so everything on this page about computing `r+` applies to `r%` as well.

The reduction is meaningful for acyclic relations only: every pair on a cycle has a longer alternative path, so it disappears from `r%`. Guard acyclicity with a rule, in the same style as `"No circular dependencies"` above:

```ampersand
RULE acyclicR : r+ /\ I |- -V
MEANING "r stays acyclic, the scope in which r% is the transitive reduction."
```
