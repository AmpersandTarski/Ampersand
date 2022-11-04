---
description: >-
  MEANING can be used with CONCEPT-statements, RELATION-statements, and
  RULE-statements, to define the meaning of your concepts, relations, and rules.
---

# MEANING statements

A meaning is optional and is characterized by the reserved word `MEANING`. It specifies the meaning of a concept, a relation, or a rule in natural language. The meaning is used to generate documentation and is printed in the functional specification. A `<meaning>` can be any text, starting with `{+` and ending with `+}` e.g.

```text
MEANING
{+ This is an example that is
   spread over multiple lines. 
+}
```

The optional `<language>` is specified as

* `IN ENGLISH` or 
* `IN DUTCH`.

Example :

```text
MEANING IN DUTCH {+ Dit is een voorbeeld in een (1) regel.+}
```

This is a way to override the default language \(which is English\).

Sometimes you need formatting in the meaning, such as dotted lists, italics, or mathematical symbols. For this purpose you have a choice in which syntax you specify the meaning. The optional `<markup>` is one of :

* `REST` \(Restructured text. This is the default\)
* `HTML`
* `LATEX` 
* `MARKDOWN`

Example :

```text
MEANING LATEX {+This is a {\em mathematical} formula $\frac{3}{x+7}$.+}
```

Ampersand uses Pandoc to offer a choice for your markup. See [pandoc.org](http://pandoc.org/) for details.

## 

