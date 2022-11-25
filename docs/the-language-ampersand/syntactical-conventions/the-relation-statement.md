# The RELATION statement

## Purpose

A relation statement says that there exists a relation in a context. It introduces \(defines, declares\) the relation in the context. Each relation is a set that contains pairs of atoms. Over time, pairs can be inserted into or deleted from a relation.

## Examples

```text
RELATION soldBy[Order*Person]
```

```text
RELATION contract[Order*ContractID] [UNI,TOT]
PRAGMA "Order " " has contract " " as its legal basis."
MEANING "Every Order has a unique ContractID which specifies the legal basis for that particular order."
```

## Syntax

A relation statement can have one of the following forms:

```text
RELATION <lower case identifier> '[' <upper case identifier>'*' <upper case identifier>']'
<properties>? <pragma>? <meaning>?
```

```text
<lower case identifier> '::' <upper case identifier> '*' <upper case identifier>
<properties>? <pragma>? <meaning>?
```

```text
<lower case identifier> '::' <upper case identifier> '->' <upper case identifier>
<properties>? <pragma>? <meaning>?
```

The second and third ways will become obsolete in future versions of Ampersand.

All three ways define a relation by its name, its source concept and its target concept. The name of a relation is a single word that starts with a lower case letter. The source and target concepts start with an upper case letter.

A relation statement may occur anywhere inside a context, both inside and outside of a pattern.

The optional `<properties>`, `<pragma>`, and `<meaning>`-parts are discussed in the sequel.

## Semantics

A relation statement means that there exists a relation in the current context with the specified name, source concept and target concept.

The name, source concept and target concept together identify a relation uniquely within its context. As a consequence, the name of a relation does not have to be unique. E.g. `name[Book*Name]` can be specified in the same context as `name[Person*Name]`. Because they have different source concepts, these are different relations.

## Properties

The `<properties>`-part is meant for writing multiplicity constraints in a comma separated list between square brackets '\[' and '\]'. E.g. `[UNI,TOT]` . The following multiplicity constraints are available:

* `UNI` \(univalent\)
* `INJ` \(injective\)
* `SUR` \(surjective\)
* `TOT` \(total\)
* `SYM` \(symmetric\)
* `ASY` \(antisymmetric\)
* `TRN` \(transitive\)
* `RFX` \(reflexive\)
* `IRF` \(irreflexive\)
* `PROP` \(property\)

Let's assume that we want to express that any person can live in one city only. So under this constraint "_Joe Smith lives in New York_" and "_Joe Smith lives in Denver_" cannot both be true at the same time.

In relation algebra, we say that the relation is univalent, which means that every atom in the source concept can only be paired with a single atom in the target concept. This is modeled as

```text
RELATION lives[Person*City][UNI]
MEANING "A person can live in one city only."
```

## PRAGMA

A pragma is optional and is characterized by the reserved word `PRAGMA`. The `PRAGMA` is followed by two or three strings. It is used to construct sentences in natural language, using pairs from the actual population of a relation. A pragma specifies how we speak \(in natural language\) about any pair in the relation. Ampersand also uses pragmas to generate examples in the functional specification. Example of a pragma with three strings:

```text
PRAGMA "Student " " flies the flag of " " in top."
```

To use this pragma on the pair `(John,Amsterdam)` results in the sentence `"Student John flies the flag of Amsterdam in top."`. The two atoms are fitted in between the three strings. A pragma with two strings is identical to a pragma in which the third string is empty.

\(The `PRAGMA` keyword will become obsolete in a future version of Ampersand. It will be replaced by the `VIEW`-statement which offers more flexibility in composing sentences.\)

Example:

```text
RELATION accepted[Provider * Order] [INJ] PRAGMA "Provider " " has accepted order "
```

The `PRAGMA` tells us that it makes sense to utter the phrase `"Provider Mario's Pizza's has accepted order 12345."`

## MEANING

A meaning is optional and is characterized by the reserved word `MEANING`. It specifies the meaning of a relation in natural language. It is is meant to say in natural language what it means for a pair to be in the relation. The meaning is used to generate documentation with and is printed in the functional specification. A `<meaning>` has the following form:

```text
MEANING <language>? <markup>? <text>
```

The `<text>`-part is where the the meaning is written down. We support both:

* a string, enclosed by double quotes,

  e.g.

  ```text
  MEANING "This is an example, which means nothing."
  ```

* any text, starting with `{+` and ending with `+}`

  e.g.

  ```text
  MEANING
  {+This is an example that is
  spread over multiple lines. A string cannot
  be multi-line, so we use curly-brackets with plus.
  +}
  ```

The optional `<language>` is specified as

* `IN ENGLISH` or 
* `IN DUTCH`.

Example :

```text
MEANING IN ENGLISH {+This is a single line example.+}
```

This is a way to override the default language.

Sometimes you need formatting in the meaning, such as dotted lists, italics, or mathematical symbols. For this purpose you have a choice in which syntax you specify the meaning. The optional `<markup>` is one of :

* `REST` \(Restructured text\)
* `HTML`
* `LATEX` 
* `MARKDOWN`

Example :

```text
MEANING LATEX {+This is a {\em mathematical} formula $\frac{3}{x+7}$.+}
```

Ampersand uses Pandoc to offer a choice for your markup. See [pandoc.org](http://pandoc.org/) for details.

