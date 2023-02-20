# The CONCEPT statement

## Purpose:

A concept statement defines a concept in natural language. A concept is a name for similar things. For example: `Peter`, `John`, and `Barack` are things you might want to call `Person`, whereas `45-NP-88` and `KD-686-D` could be instances of the concept `LicensePlate`.

## Syntax:

```text
CONCEPT <Uppercase identifier> <String> <String>?
```

This statement may occur anywhere within a context, either inside or outside a pattern.

## Semantics

This statement means that there exists a concept called `<Uppercase identifier>` in the current context.

* `<Uppercase identifier>` specifies the name of the concept.
* `String` contains a definition of the concept. This definition is used by the documentation generator, which expects it to be a grammatically correct and complete sentence.
* `String?` is an \(optional\) reference to the source of the definition. It is meant for traceability.

## Examples

```text
CONCEPT Person "A person is a human creature." "Ventroli1997"
```

```text
CONCEPT Organization "An organization is a collection of persons that work together to achieve specific objectives."
```

```text
CONCEPT Criterion "A criterion is a standard on which a judgment or decision may be based." "Merriam-Webster"
```

## Miscellaneous

* The name of a concept starts with an uppercase.
* A concept should be used for immutable concepts. E.g. use a concept `Person` to express that a person will always be a person and will not change in, let us say, a table. However, don't use `Employee`, because termination of an employee's contract causes a person to be an employee no longer. So employees are not immutable. To be an employee is a dynamic property, so model it as a relation.
* The description will be printed in the functional specification, so please check that your definition is a complete sentence.
* Concepts need not be defined. If you use a concept without a definition, Ampersand defines it for you \(regardless of whether you defined it or not\).

