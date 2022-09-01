# Primitive terms in natural language

## Relations

When a [relation](../../relations.md) is used in a term, it stands for a set of facts that are assumed true on the current time in the current context. Those facts \(also referred to as the contents or the population of the relation\) can change over time as users add or delete facts from it.

When a relation is used in a term, we can simply use its name if that is unambiguous. For instance  the name `owner`  refers to `RELATION owner[Person*Building]` if that is the only relation the ampersand-compiler can link it to. In some cases, however the name alone is ambiguous. For example if there are two relations with the same name and different signatures. In such cases Ampersand will try to infer the type from the context. That however does not always succeed. In such cases, Ampersand generates an error message that asks you to remove the ambiguity by adding the correct type.

If a pair $$(a,b)$$ is an element of a relation $$r$$, we write $$a\ r\ b$$ to denote the fact. It means that we consider $$a\ r\ b$$ to be true \(within the current context\).

## Identity

Every atom in a concept $$C$$ identifies itself. If for example concept "Person" contains atoms {"Ann", "Bob", "Cecil"}, "Ann" identifies "Ann", "Bob" identifies "Bob", and "Cecil" identifies "Cecil". This makes "Ann" and "Bob" different atoms \(unequal\).

## Other explanation

Would you like a different explanation of the primitive terms? [This page](../semantics-in-logic/primitive-terms.md) explains the primitive terms in logic. [Click here](../semantics-in-sets/primitive-terms-in-set-theory.md) for the explanation in set theory.

