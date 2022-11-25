# The RELATION statement

## Purpose

A _**relation statement**_ says that a relation exists. It introduces (defines, declares) the relation in the context that uses the relation statement.

A _**population statement**_ specifies which pairs (of atoms) are in a relation.

## Description

A relation is a set that contains pairs of atoms. Over time, pairs can be inserted into or deleted from a relation, for example by a user typing data into an Ampersand application. So the content of a relation is changing over time.

When discussing relations, an arbitrary relation is referred to as $$r$$, $$s$$, or $$t$$. To say that a pair $$(a,b)$$ belongs to a relation $$r$$, we write $$a\ r\ b$$ or alternatively $$(a,b)\in r$$.

## Examples

```
RELATION soldBy[Order*Person]
```

```
RELATION contract[Order*ContractID] [UNI,TOT]
PRAGMA "Order " " has contract " " as its legal basis."
MEANING
{+ Every Order has a unique ContractID which specifies the legal basis
   for that particular order.
+}
```

In this example:

* `contract` is the _**name**_ of the relation,
* `Order` is the _**source concept**_ of the relation,
* `ContractID` is the _**target concept**_ of this relation, and
* `UNI` and `TOT` are _**constraints**_ of this relation.

## Syntax and meaning

Each relation used in Ampersand has to be declared. This means that the developer tells the system that this particular relation exists. A relation declaration can have one of the following formats:

```
RELATION <lower case identifier>
         '[' <upper case identifier> '*' <upper case identifier> ']'
         <properties>? <pragma>? <meaning>?
```

In the _**declaration**_ `RELATION owner[Person*Building]`, `owner` is the _**name**_ and `[Person*Building]` is the _**type**_ of the relation. Relation names start with a lower case character, to avoid confusion with concept names. The _**signature**_ of this relation is `owner[Person*Building]`. The signature identifies the relation within its context. The left hand concept, `Person`, is called the _**source**_ of the relation and the right concept, `Building`, is called the _**target**_.

All three formats define a relation by its name, its source concept and its target concept. By convention, the name of a relation is a single word that starts with a lower case letter. The source and target concepts start with an upper case letter. This convention avoids confusion between concepts and relations.

A relation statement means that there exists a relation in the current context with the specified name, source concept and target concept.

A relation statement may occur anywhere inside a context, both inside and outside a pattern.

The optional `<properties>` and `<pragma>`-parts are discussed in the sequel. The `<meaning>`-part is discussed [here](meaning-statements.md).

The name, source concept and target concept together identify a relation uniquely within its context. As a consequence, the name of a relation does not have to be unique. E.g. `name[Book*Name]` can be specified in the same context as `name[Person*Name]`. Because they have different source concepts, these are different relations.

## Properties

The `<properties>`-part is meant for writing multiplicity constraints in a comma separated list between square brackets '\[' and ']'. E.g. `[UNI,TOT]` . The following properties can be specified on any relation `r[A*B]`

| &   | property   | semantics                                                                                                                                                                      |
| --- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| UNI | univalent  | For any `a` in `A` there can be not more than one `b` in `B` in the population of `r`. This implies that every `a` occurs not more than once (is unique) in the source of `r`. |
| INJ | injective  | For any `b` in `B` there can be not more than one `a` in `A` in the population of `r`. So, every `b` occurs not more than once in the target of `r`.                           |
| SUR | surjective | For any `b` in `B` there must be at least one `a` in `A` in the population of `r`.                                                                                             |
| TOT | total      | For any `a` in `A` there must be at least one `b` in `B` in the population of `r`.                                                                                             |

There are additional relations that can be specified on endo relations. An endo relation is a relation where the source and target concepts are equal. `r[A*A]`.

| &    | property      | semantics                                                                 |
| ---- | ------------- | ------------------------------------------------------------------------- |
| SYM  | symmetric     | For each (`a`,`b`) in `r`, (`b`,`a`) is in `r`.                           |
| ASY  | antisymmetric | If (`a`,`b`) and (`b`,`a`) are both in `r`, then `a` = `b`                |
| TRN  | transitive    | If (`a`,`b`) and (`b`,`c`) are both in `r`, then (`a`,`c`) is in `r`.     |
| RFX  | reflexive     | For each `a` in `A`, the pair (`a`,`a`) is in the population of `r`       |
| IRF  | irreflexive   | For each `a` in `A`, the pair (`a`,`a`) is _not_ in the population of `r` |
| PROP | -             | shortcut for the combination of symmetric and antisymmetric.              |

Let's assume that we want to express that any person can live in one city only. So under this constraint "_Joe Smith lives in New York_" and "_Joe Smith lives in Denver_" cannot both be true at the same time.

In relation algebra, we say that the relation is univalent, which means that every atom in the source concept can only be paired with a single atom in the target concept. This is modeled as

```
RELATION lives[Person*City][UNI]
MEANING "A person can live in one city only."
```

## PRAGMA

A pragma is optional and is characterized by the reserved word `PRAGMA`. The `PRAGMA` is followed by two or three strings. It is used to construct sentences in natural language, using pairs from the actual population of a relation. A pragma specifies how we speak (in natural language) about any pair in the relation. Ampersand also uses pragmas to generate examples in the functional specification. Example of a pragma with three strings:

```
PRAGMA "Student " " flies the flag of " " in top."
```

To use this pragma on the pair `(John,Amsterdam)` results in the sentence `"Student John flies the flag of Amsterdam in top."`. The two atoms are fitted in between the three strings. A pragma with two strings is identical to a pragma in which the third string is empty.

(The `PRAGMA` keyword will become obsolete in a future version of Ampersand. It will be replaced by the `VIEW`-statement which offers more flexibility in composing sentences.)

Example:

```
RELATION accepted[Provider * Order] [INJ] PRAGMA "Provider " " has accepted order "
```

The `PRAGMA` tells us that it makes sense to utter the phrase `"Provider Mario's Pizza's has accepted order 12345."`

## MEANING

For a full discussion of meaning, we refer to [`this page`](meaning-statements.md).

## Miscellaneous

*
