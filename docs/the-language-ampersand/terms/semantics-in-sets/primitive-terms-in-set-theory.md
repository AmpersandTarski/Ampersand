# Primitive terms in set theory

## Relations

When a [relation](../../relations.md) is used in a term, it stands for the set of pairs it contains at the moment it is evaluated. That set \(also referred to as the contents of the relation\) can change over time as users add or delete pairs from it.

When a relation is used in a term, we can simply use its name if that is unambiguous. For instance  the name `owner`  refers to `RELATION owner[Person*Building]` if that is the only relation the ampersand-compiler can link it to. In some cases, however the name alone is ambiguous. For example if there are two relations with the same name and different signatures. In such cases Ampersand will try to infer the type from the context. That however does not always succeed. In such cases, Ampersand generates an error message that asks you to remove the ambiguity by adding the correct type.

If a pair $$(a,b)$$ is an element of a relation $$r$$, we write $$(a,b)\in r$$. Alternatively we may write $$a\ r\ b$$.

## Identity

For every concept $$C$$, the term $$I_{[C]}$$ represents the _**identity relation**_. It is defined by:

$$
I_{[C]}\ =\ \{(c,c) |\ c\in C\}
$$

The type of $$I_{[C]}$$ is $$[C*C]$$. In Ampersand code you write `I[C]`.

## Complete relation

For every pair of concepts $$A$$ and $$B$$ the term $$V_{[A*B]}$$ represents the _**complete relation**_. It is defined by:

$$
V_{[A*B]}\ =\ \{(a,b) |\  a\in A\ \wedge\ b\in B\}
$$

The type of $$V_{[A*B]}$$ is $$[A*B]$$. In Ampersand code you write `V[A*B]`.

## Other explanation

Would you like a different explanation of the primitive terms? [This page](../semantics-in-logic/primitive-terms.md) explains the primitive terms in logic. [Click here](../semantics-in-natural-language/primitive-terms-in-natural-language.md) for the explanation of primitive terms in natural language.

