# The CONCEPT statement

## Purpose:

A concept statement defines a concept in natural language. A concept is a name for similar things. For example: `Peter`, `John`, and `Barack` are things you might want to call `Person`, whereas `45-NP-88` and `KD-686-D` could be instances of the concept `LicensePlate`.

## Syntax:

```
CONCEPT <Uppercase identifier>
MEANING  {+ <Text> +}
```

## Semantics

This statement means that there exists a concept called `<upper case identifier>` in the current context.

* `<upper case identifier>` specifies the name of the concept. It starts with an upper case character and may subsequently have any combination of upper case (ABCDEFGHIJKLMNOPQRSTUVWXYZ), lower case (abcdefghijklmnopqrstuvwxyz), digits (0123456789) and underscore (\_).
* `<String>` defines the concept. Please describe in natural language the conditions that make an atom belong to this concept. Your definition is used by the documentation generator, which expects it to be a grammatically correct and complete sentence.

## Examples

```
CONCEPT Person MEANING {+ A person is a human creature. +}
```

```
CONCEPT Organization MEANING
{+ An organization is a collection of persons that work together
   to achieve specific objectives.
+}
```

```
CONCEPT Criterion
MEANING
{+ A criterion is a standard on which a judgment or decision may be based.
   [Merriam-Webster]
+}
```

## Miscellaneous

* The name of a concept starts with an uppercase character.
* A concept should be used for immutable concepts. E.g. use a concept `Person` to express that a person will always be a person and will not change in, let us say, a table. However, don't use `Employee`, because termination of an employee's contract causes a person to be an employee no longer. So employees are not immutable. To be an employee is a dynamic property, so model it as a relation.
* The description will be printed in the functional specification, so please check that your definition is a complete sentence.
* Concepts need not be defined. If you use a concept without a definition, Ampersand makes it for you.

## Markup

For the purpose of documentation, you may state the language in which the meaning is written. You may also state in which markup you have written your meaning. Examples:

```
CONCEPT Person MEANING IN ENGLISH {+ A person is a human creature. +}
```

If you specify the language, Ampersand can restrict the documentation for the language you choose. Currently, you can only choose `DUTCH` or `ENGLISH`. The default language is English

```
CONCEPT Organization MEANING MARKDOWN
{+ An organization is a **collection of persons** that work together
   to achieve specific objectives.
+}
```

By specifying a markup language, Ampersand interprets the text as specified. If you do not specify the markup language, your text is interpreted as restructured text. The available markup languages are `LATEX`, `MARKDOWN`, `HTML`, and `REST`. The default markup language is REStructured Text (REST).

```
CONCEPT Criterion
MEANING IN ENGLISH LATEX
{+ A criterion is a standard on which a judgment or decision may be based.
   \cite{Merriam-Webster}
+}
```

