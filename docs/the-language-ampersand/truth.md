---
description: >-
  An information system should represent the truth. So, as a designer you must
  know a thing or two about truth.
---

# Truth

Let us introduce some language to talk about truth. Consider a fact "Joe Smith lives in New York." from an Ampersand perspective. In Ampersand, we can analyse this as follows:

* Let `Person` and `City` be [_**concepts**_](the-concept-statement.md)_\*\*\*\*_
* Let `"Joe Smith"` be an [_**atom**_](atoms.md) of the concept `Person` and `"New York"` an atom of the concept `City`.
* Let us use the [_**relation**_](relations.md) `livesIn[Person*City]` to contain our fact.
* `livesIn` is the _**relation name**_ and `[Person*City]` is the _**signature**_ of this relation.
* `Person` is the _**source**_ of this relation and `City` is the _**target**_.
* If the pair `("Joe Smith","New York")` is an element of this relation, Ampersand considers the statement `"Joe Smith" livesIn "New York"` to be true. So all pairs in a relation represent _**facts**_, i.e. true statements.

## Language that makes sense to the business

Ampersand takes a pragmatic stance on truth: You model only things that make sense to the business. This [video clip](https://player.ou.nl/wowzaportlets/#!production/VDvSFqQ) illustrates the distinction between sensible and senseless statements. A sensible statement (we say: "It makes sense.") is a statement that can be true or false. Sentences that are not sensible (we can say: it is non-sense) are to be avoided. The Ampersand type system helps you to make sensible statements only.

## Truth in context

Truth always has context. If we say "Jack was married to Jackie", this statement is true in a [context](context.md) where "Jack" refers to the 35th president of the United States, John F. Kennedy. However, this statement is not true in a context where there is no Jack. And in a context where marriage doesn't exist, this statement makes no sense.
