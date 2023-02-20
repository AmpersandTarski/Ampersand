# The IDENT statement

## Purpose:

This statement is a rule, which defines an identity on a concept. It is syntactic sugar for specifying a set of relations that identify atoms in a specific concept. For example, if relations `pi` and `rho` determine an atom of concept `T` uniquely, you can write:

```text
IDENT "T uniqueness" :  T (pi, rho)
```

As the IDENT statement defines a rule, it can be in the same places as any other RULE.

## Syntax

```text
`IDENT` (<label> `:`)? <Concept> `(` <term>* `)`
```

where:

* `<label>` is the name of the rule. It can be a single word or a string \(enclosed by double brackets\). It is followed by a colon \(`:`\) to distinguish the label from the concept that follows.
* `<Concept>` is the name of the Concept for atoms of which the rule specifies an identity
* Between brackets are terms whose source concept must be `<Concept>`. This is enforced by the type system.

## Informal Semantics

```text
IDENT "Rule Name" : C (e1, e2, ...)
```

translates into the following rule:

```text
  RULE "Rule Name":  {e1}<>{e1}~ /\ {e2}<>{e2}~ /\ ... |- I[C]
```

Note that

* in case every`e`is both univalent and total, `e<>e~` equals `e;e~`, and the rule is equivalent to:

```text
   RULE "Rule Name":  {e1};{e1}~ /\ {e2};{e2}~ /\ ... |- I[C]
```

* in case every `e` is univalent but not total, you should use the `IDENT` statement \(or the rule that it implements\), because that also works when an `e` is not populated.

