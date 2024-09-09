# Context

## Purpose

Contexts exist in Ampersand for the purpose of dealing with [truth](truth.md). Within one context there may be no contradictions.

## Description

Any statement can be true in context only. Within one context, there are no contradictions.  
As facts are true statements, we say that facts must exist inside a context.

## Examples

Examples of contexts:

- a single lawsuit in which all case data is contained;
- the financial administration of a repair shop;
- the life insurance department of a bank.

The world is full of contradictions. Examples:

- Bob's personal income over March 2013 according to Bob's employer differs from Bob's personal income over March 2013 according to the National Tax Authority. \(To resolve this, we must distinguish between the context of Bob's employer and the context of the National Tax Authority.\)
- The police can be convinced that Peter X commited the crime, yet his attorney is convinced he is innocent. \(To make sense of the situation, a judge distinguishes the reasoning of the defense from the reasoning of the prosecution as different contexts. In fact, the judge will construct her own context to create the verdict \)
- A computer system can tell that the person with social security number 721-07-4426 was born on April 27th, 1943, while the same computer system tells in another screen that this person was born on May 3rd, 1952. This is inconsistent, because every person has only one birth date. \(This situation should be reported as a software mistake.\)

## [Syntax](https://github.com/AmpersandTarski/Ampersand/blob/main/src/Ampersand/Input/ADL1/Parser.hs) and meaning

```text
CONTEXT <name> <language>? <markup>? <context element>* ENDCONTEXT
```

A context is specified by the context elements between the keywords `CONTEXT` and `ENDCONTEXT`. A context has a name. You can optionally specify the language and markup \(see below\).

A context represents a set of [true statements in a given language](truth.md), which is the meaning of that context.

### Language

To tell Ampersand what language your context is in, you can append a language directive to your context. Currently English and Dutch are supported. To do so, directly following the name of your context, you can specify

```text
IN <language>
```

Where `<language>` can be `ENGLISH` or `DUTCH`.

### Markup

Directly following the optional language definition, you can optionally specify the format of texts in your `PURPOSE` statements and `MEANING` blocks. This allows you to use your favourite markup language within Ampersand, as long as it is one of `REST` \(Restructured Text\), `HTML`, `LATEX` or `MARKDOWN`. If you specify one of these words in your `CONTEXT` definition, that value will be the default markup of all purposes and meanings in your context.

\(For details on these formats, see [pandoc.org](http://pandoc.org/)\).

### Context elements

A context may contain different types of statements. The order of statements in a context is irrelevant for the software that Ampersand generates. However, the order is maintained when documentation is generated.

|                    |                                                                                                                         |
| :----------------- | :---------------------------------------------------------------------------------------------------------------------- |
| `<meta>`           | a statement to provide metadata to a script, such as author, company, etc.                                              |
| `<pattern>`        | a block of code that represents rules on a single and specific topic, at the user's discretion                          |
| `<rule>`           | a statement that declares a [rule](./syntax-of-ampersand#the-rule-statement)                                                        |
| `<classify>`       | a statement that specifies generalization/specialization of [concepts](./syntax-of-ampersand#the-concept-statement)                 |
| `<relation>`       | a declaration of a relation, stating the existence of a [relation](./syntax-of-ampersand#the-relation-statement) within the context |
| `<conceptDef>`     | a description of a [concept](./syntax-of-ampersand#the-concept-statement), to document its meaning                                  |
| `<representation>` | a statement that defines the atomic type of a [concept](./syntax-of-ampersand#the-concept-statement)                                |
| `<roleRule>`       | a statement that makes a role responsible for satisfying a rule                                                         |
| `<viewDef>`        | a statement for presenting facts in a readable sentence                                                                 |
| `<interface>`      | a unit of code that can be run independently and specifies interaction with a user or a computer                        |
| `<purpose>`        | a statement to describe the [purpose](./syntax-of-ampersand#the-purpose-statement) of a context or a context element                |
| `<population>`     | a statement that sums up the initial [population](./syntax-of-ampersand#the-population-statement) of a relation                     |
| `<include>`        | a statement to [include](./syntax-of-ampersand#the-include-statement) another file in the context                                   |
