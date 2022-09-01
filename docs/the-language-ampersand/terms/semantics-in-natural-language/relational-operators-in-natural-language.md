# Relational operators in natural language

## Purpose of relational operators

To say things such as "the name of the owner", we want to string together multiple relations (viz. `name` and `owner`). Relational operators allow us to make such statements.

## Meaning

The meaning of relational operators $$\smallsmile$$ and $$;$$ is best explained by means of examples.

Assume we have a relation, `label[Contract*Colour]`, which contains the colour of labels on contracts. A fact `"1834" label "blue"` means that contract 1834 has a blue label.

Also assume another relation `stored[Contract*Location]`, which gives the location where a contract is stored. Fact `"1834" store "cabinet 42"` means that contract 1834 is stored in cabinet 42.

## Converse

A relation can be altered by swapping the elements of every pair in the relation. Mathematically, $$(a, b)$$ is a different from $$(b,a)$$. In natural language, however, the meaning does not change.  So if`"1834" label "blue"` means that contract 1834 has a blue label, `"blue" label~ "1834"` also means that contract 1834 has a blue label.

* The sentence: "All contracts with a blue label are stored in cabinet 42." is represented as `"blue" (label\stored) "cabinet 42"`. Literally it says: For every contract, if it has a blue label, then it is stored in cabinet 42.

## Composition

The sentence "A _contract with a blue label is stored in cabinet 42_." can be represented as `"blue" (label~;stored) "cabinet 42"`.  Literally it says: There is a contract that has a blue label and is stored in cabinet 42.

## Natural language templates

There is a pattern to this. A computer can generate a literal translation from the formula to natural language. However, that translation looks clumsy, verbose and elaborate. It is up to you to turn that in normal language. For examples [click here](https://ampersandtarski.gitbook.io/documentation/\~/drafts/-LKR7o8ALsxT8aQfWugs/primary/ampersands-own-language/semantics-visualized/semantics-visualized). The systematic translation is given in the following table:

| Formally    | Natural language template                   |
| ----------- | ------------------------------------------- |
| `a (r;s) b` | There exists an x : if `a r x`then `x s b`. |
| `b r~ a`    | `a r b`.                                    |

The natural language translation for `b r~ a`is the same as language translation for `a r b`.

## Other explanation

Would you like a different explanation of the relational operators? [This page](../semantics-in-sets/relational-operators-in-set-theory.md) explains the relational operators in terms of set theory. An explanation in logic is given [here](relational-operators-in-natural-language.md). [Click here](../semantics-in-algebra/relational-operators-in-algebra.md) for some algebraic rules about relational operators.
