# From rules to running code

This chapter is for programmers who want to know where a RULE ends up in the
application Ampersand generates. It answers one question: when you write a rule
in ADL, which runtime artifact enforces it, and in which tier — Angular, PHP, or
MariaDB?

## The short answer

A rule never becomes Angular code, and it never becomes hand-written PHP. The
compiler turns each rule into a SQL query that finds its violations, ships that
query as data in `generics/conjuncts.json`, and the generic PHP back-end runs it
against MariaDB. Each tier plays exactly one role:

- **MariaDB** executes the rule: the violation query runs here and returns the
  offending pairs.
- **PHP back-end** decides what to do with the result: roll back the transaction
  (invariant) or present the violations as work to be done (signal).
- **Angular front-end** only displays: it shows signals and error messages the
  back-end returns through the API. It contains no rule logic.

## The pipeline: rule → conjuncts → violation SQL

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

## When the SQL runs: quads

The compiler also computes **quads**, which wire each relation to the rules it can
break. A quad records: *if this relation changes, then this rule may be violated,
so check these conjuncts.* When the back-end mutates a relation, it uses the quads
to run only the affected conjunct queries — not every rule. The wiring is computed
once at compile time; the checking happens at runtime, inside the PHP transaction.

## Invariants versus signals

A rule is a **signal** when a role is assigned to maintain it (it has
*maintainers*), and an **invariant** otherwise. Both use the same violation SQL;
they differ only in what the back-end does with a non-empty result.

| | Invariant | Signal (process rule) |
| --- | --- | --- |
| Assigned maintainer | none | one or more roles |
| On violation | the transaction is rolled back | the violations are shown as work to do |
| Who resolves it | the system keeps it true automatically | the responsible role acts on it |
| Lives in `rules.json` under | `invariants` | `signals` |

## Where to look in the compiler

- `Ampersand/Output/ToJSON/Conjuncts.hs` — builds `conjuncts.json`, including the
  `violationsSQL` for each conjunct.
- `Ampersand/Output/ToJSON/Rules.hs` — builds `rules.json`, splitting invariants
  from signals.
- `Ampersand/FSpec/SQL.hs` — compiles a rule expression to SQL.
- `Ampersand/Prototype/GenBackend.hs` — writes the `generics/` files the back-end
  consumes.

See also [Architecture of an Ampersand Application](./architecture-of-an-ampersand-application.md)
for how these generated files fit into the whole application.
