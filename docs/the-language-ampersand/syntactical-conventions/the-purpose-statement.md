# The PURPOSE statement

## Semantics

Most things in your model are in it for a reason. To document these, you should use the PURPOSE statement.

## Syntax

`PURPOSE` `<type of thing>` `<name>` `<language>?` `<markup>?`

`{+` `<anything>` `+}`

Where `<type of thing>` and `<name>` are the type and name of the thing that is refered to. This could be one of: `CONCEPT`, `RELATION`, `RULE`, `IDENT`, `VIEW`, `PATTERN`, `INTERFACE`, `CONTEXT`

The optional and can be used to override the settings for language and markup. If omitted, these are inherited from the pattern of context where the PURPOSE statement is specified in.

## 

Examples:

```text
PURPOSE CONCEPT Person {+The concept Person keeps all personal data together.+}
```

```text
PURPOSE RELATION accountOwner
{+ The system shall register all accounts to an owner,
   so accounts with the same owner are linked in this way.
+}
```

When defining the purpose of a relation, make sure that Ampersand can identify the relation unambiguously. If you have multiple relations `accountOwner`, add the signature to disambiguate it. For instance:

```text
PURPOSE RELATION accountOwner[Account*Owner]
{+ The system shall register all accounts to an owner,
   so accounts with the same owner are linked in this way.
+}
```

## Markup

For the purpose of documentation, you may state the language in which you write a purpose. You may also state in which markup language you use. Examples:

```text
PURPOSE CONCEPT Person IN ENGLISH {+ The concept PERSON keeps all personal data together, which we need to comply with the GDPR.  +}
```

If you specify the language, Ampersand can restrict the documentation for the language you choose. Currently, you can only choose `DUTCH` or `ENGLISH`. The default language is English.

```text
PURPOSE RELATION accountOwner LATEX
{+ The system {\em shall} register all accounts to an owner, so accounts with the same owner are linked in this way.
+}
```

By specifying a markup language, Ampersand interprets the text as specified. If you do not specify the markup language, your text is interpreted as REStructured Text \(`REST`\). The available markup languages are `LATEX`, `MARKDOWN`, `HTML`, and `REST`.

```text
PURPOSE RULE "Check Digit Character"
IN ENGLISH MARKDOWN
{+ This rule enforces the use of a check digit character
   as described in [ISO 7064](en.wikipedia.org/wiki/ISO/IEC_7064).
   This is applicatble to IBAN bank account numbers.
+}
```





