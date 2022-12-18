# The CLASSIFY statement

## Purpose

A _**classify statement**_ is also called a _**specialization**_. It specifies that atoms of one concept are atoms of another concept as well. You can use it to buils classifications like [Linnaeus](https://www.britannica.com/science/taxonomy/The-Linnaean-system) did.

## Syntax and meaning

```
CLASSIFY <upper case identifier> ISA <upper case identifier>
```

In a specialization, e.g. `CLASSIFY Sedan ISA Car`, we call the first concept (`Sedan`) the specific concept and the second (`Car`) the generic concept. The meaning of a specialization is that every atom from the specific concept is an atom from the generic concept as well. So every (atom that is a) Sedan is a Car as well.

So in general:  `CLASSIFY` $$A$$ `ISA` $$B$$ means: $$\forall a: a\in A\Rightarrow a\in B$$.

## Examples

```
CLASSIFY Monkey ISA Mammal
```

```
CLASSIFY Sedan ISA Car
```



To save some writing, you may specify

```
CLASSIFY Monkey, Cow, Human ISA Mammal
```

`This means exactly the same as`

```
CLASSIFY Monkey ISA Mammal
CLASSIFY Cow ISA Mammal
CLASSIFY Human ISA Mammal
```

## Best practice

A specialization is a static relationship. If you want to say that a student is a person, please consider whether you want this to be static. If a person can enroll to become a student, or graduate or drop out to become non-student again, the dynamics of that cannot be captured in a specialization. Use a relationship instead to model the state of being a student. \
E.g. `RELATION student[Person*Enrollment]`

By adding and removing pairs to that relation, it continuously reflects which persons are a student.
