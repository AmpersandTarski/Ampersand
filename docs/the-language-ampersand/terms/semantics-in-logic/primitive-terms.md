# Primitive terms

## Relations

When a [relation](../../relations.md) is used in a term, it stands for all pairs it contains at the moment it is evaluated. Those pairs (also referred to as the _**contents**_ or _**population**_ of the relation) can change over time as users add or delete pairs from it.

When a relation is used in a term, we can just use its name if that is unambiguous. For instance  the name `owner`  refers to `RELATION owner[Person*Building]` if that is the only relation the ampersand-compiler can link it to. In some cases, however the name alone is ambiguous. For example if there are two relations with the same name and different signatures. In such cases Ampersand will try to infer the type from the context. That however does not always succeed. In such cases, Ampersand generates an error message that asks you to remove the ambiguity by adding the correct type.

If a pair $$(a,b)$$ is an element of a relation $$r$$, we write $$a\ r\ b$$. Alternatively we may write $$(a,b)\in r$$ , since we know that $$r$$ is a set.

## Identity

For every concept $$C$$, the term $$I_{[C]}$$ exists. It refers to the _**identity relation**_. It means that for every $$a\in C$$ and $$b\in C$$ we have:

$$
a = b\ \Leftrightarrow\ a\ I_{[C]}\ b
$$

The type of $$I_{[C]}$$ is $$[C*C]$$. In Ampersand code you write `I[C]`.

## Complete relation

For every pair of concepts $$A$$ and $$B$$ the term $$V_{[A*B]}$$ refers to the _**complete relation**_. For every $$a\in A$$ and $$b\in B$$ we have:

$$
a\ V_{[A*B]}\ b
$$

The type of $$V_{[A*B]}$$ is $$[A*B]$$. In Ampersand code you write `V[A*B]`.

## Other explanation

Would you like a different explanation of the primitive terms? [This page](../semantics-in-sets/primitive-terms-in-set-theory.md) explains the primitive terms in terms of set theory. [Click here](../semantics-in-natural-language/primitive-terms-in-natural-language.md) for the explanation of primitive terms in natural language.
