# The TABLE statement

## Disclaimer

This statement will be available in Ampersand vs. 4.

## Purpose

This statement provides syntactic sugar for defining tabular information conveniently. It introduces a number of relations and rules in a single statement, to simplify a script.

## Syntax

```text
`TABLE` <Concept> `(` (<term> `:` <Concept> <Multiplicities>?)* `)`
```

where:

* `<label>` is the name of the rule. It can be a single word or a string \(enclosed by double brackets\). It is followed by a colon \(`:`\) to distinguish the label from the concept that follows.
* `<Concept>` is the name of the Concept for atoms of which the rule specifies an identity
* Between brackets are terms whose source concept must be `<Concept>`. This is enforced by the type system.

## Semantics by example

```text
TABLE  C (e1:C1, e2:C2)
```

translates into the following declarations:

```text
RELATION E1[C*C1]
RELATION E2[C*C2]
```

Multiplicity annotations are allowed. For example:

```text
TABLE  C (e1:C1[UNI,TOT], e2:C2, e3:C[ASY], ...)
```

translates into the following declarations:

```text
RELATION E1[C*C1] [UNI,TOT]
RELATION E2[C*C2]
RELATION E3[C*C] [ASY]
...
```

## Practice

This statement makes nice combinations with the IDENT statement. For example to define two identities for persons:

```text
TABLE Person(name:String, ssn:SSN, birthplace:City, birthdate:Date)
IDENT Person(ssn)
IDENT Person(name,birthplace,birthdate)
```

This states that a person is uniquely defined by `ssn`, but also by the combination of `name`, `birthplace`, and `birthdate`. This statement can also be used to objectify \(reify\) an term `e`

```text
TABLE T(pi:P, rho:R)
IDENT T(pi,rho)
RULE "Create T" : e |- pi~;rho
RULE "Delete T" : pi~;rho |- e
```

If a user is tempted to replace the Create/Delete pair with a single equivalence, this becomes:

```text
TABLE T(pi:P, rho:R)
IDENT T(pi,rho)
RULE e = pi~;rho
```

