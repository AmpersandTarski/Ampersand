# Boolean operators in natural language

## Purpose of boolean operators

To say things such as pair `("peter","macbook")` is either in relation `ownsa`  or `wantsa`,  requires us to use boolean operators $$\cup$$, $$\cap$$, and $$-$$ .

## Meaning

Let us explain the meaning of relational operators $$\cup$$, $$\cap$$, and $$-$$ by means of examples.

Assume we have a relation, `ownsa[Person*LaptopType]`, which contains the persons who own a particular type of laptop. A fact `"peter" ownsa "macbook"` means that Peter owns a MacBook.

Also assume another relation `wantsa[Person*LaptopType]`, which contains the persons who want a particular type of laptop. A fact `"peter" wantsa "macbook"` means that Peter wants a MacBook.

## Union

The sentence: "Peter owns a MacBook or Peter wants a MacBook." is represented as\
&#x20;`"peter"` (`ownsa` $$\cup$$ `wantsa`) `"macbook"`.

## Intersection

The sentence: "Peter owns a MacBook and Peter wants a MacBook." is represented as\
&#x20;`"peter"` (`label` $$\cap$$ `colour`) `"macbook"`.

## Difference

The sentence: "Peter owns a MacBook and Peter does not want a MacBook." is represented as\
&#x20;`"peter"` (`label` $$-$$ `colour`) `"macbook"`.

## Natural language templates

There is a pattern to this. A computer can generate a literal translation from the formula to natural language. However, that translation looks clumsy, verbose and elaborate. It is up to you to turn that in normal language. For examples [click here](https://ampersandtarski.gitbook.io/documentation/\~/drafts/-LKR7o8ALsxT8aQfWugs/primary/ampersands-own-language/semantics-visualized/semantics-visualized). The systematic translation is given in the following table:

| Formally              | Natural language template |
| --------------------- | ------------------------- |
|  $$a\ (r\cup s)\ b$$  |  `a r b `or `a s b`.      |
| $$a\ (r\cap s)\ b$$   | `a r b `and `a s b`.      |
| $$a\ (r-s)\ b$$       | `a r b `and not`a s b`.   |

## Other explanation

Would you like a different explanation of the relational operators? [This page](../semantics-in-sets/boolean-operators-sets.md) explains the boolean operators in terms of set theory. An explanation in logic is given [here](../semantics-in-logic/boolean-operators.md). [Click here](../semantics-in-algebra/boolean-operators-in-algebra.md) for some algebraic rules about boolean operators. If you want to see it explained visually in Venn-diagrams, [click here](../semantics-visualized/semantics-of-boolean-operators-visualized.md).
