# Module

{% hint style="danger" %}
The module system in Ampersand is currently being developed. It isn't yet in the main branch. This page is being created as work progresses.&#x20;

See [https://github.com/AmpersandTarski/Ampersand/issues/1307](https://github.com/AmpersandTarski/Ampersand/issues/1307) for details.
{% endhint %}

## Purpose

An Ampersand specification consists of a CONTEXT file and a set of Modules. A module in Ampersand serves the dual purpose of controlling name-spaces and ordering definitions in multiple files.&#x20;

We have been inspired by the [module system of Haskell](https://www.haskell.org/tutorial/modules.html).&#x20;

## Description



## Examples



## [Syntax](https://github.com/AmpersandTarski/Ampersand/blob/development/src/Ampersand/Input/ADL1/Parser.hs) and meaning

```
MODULE <name> <language>? <markup>? <import>* <module element>* MODULE
```



### Language

To tell Ampersand what language your module is in, you can append a language directive to your module. Currently English and Dutch are supported. To do so, directly following the name of your context, you can specify

```
IN <language>
```

Where `<language>` can be `ENGLISH` or `DUTCH`.

### Markup

Directly following the optional language definition, you can optionally specify the format of texts in your `PURPOSE` statements and `MEANING` blocks. This allows you to use your favourite markup language within Ampersand, as long as it is one of `REST` (Restructured Text), `HTML`, `LATEX` or `MARKDOWN`. If you specify one of these words in your `CONTEXT` definition, that value will be the default markup of all purposes and meanings in your context.

(For details on these formats, see [pandoc.org](http://pandoc.org/)).

### Import

An IMPORT statement contains a reference to another module and contains information of what definitions are imported from that module together with information about possible name-changes.&#x20;

### Module elements

A module may contain different types of statements. The order of statements in a context is irrelevant for the software that Ampersand generates. However, the order is maintained when documentation is generated.

|                    |                                                                                                          |
| ------------------ | -------------------------------------------------------------------------------------------------------- |
| `<pattern>`        | a block of code that represents rules on a single and specific topic, at the user's discretion           |
| `<rule>`           | a statement that declares a [rule](the-rule-statement.md)                                                |
| `<gen>`            | a statement that specifies generalization/specialization of [concepts](the-concept-statement.md)         |
| `<relation>`       | a declaration of a relation, stating the existence of a [relation](relations.md) within the context      |
| `<conceptDef>`     | a description of a [concept](the-concept-statement.md), to document its meaning                          |
| `<representation>` | a statement that defines the atomic type of a [concept](../tutorial-rap3/conceptual-model-enrollment.md) |
| `<roleRule>`       | a statement that makes a role responsible for satisfying a rule                                          |
| `<viewDef>`        | a statement for presenting facts in a readable sentence                                                  |
| `<service>`        | a unit of code that can be run independently and specifies interaction with a user or a computer         |
| `<purpose>`        | a statement to describe the purpose of a context or a context element                                    |
| `<population>`     | a statement that sums up the initial population of a relation                                            |
