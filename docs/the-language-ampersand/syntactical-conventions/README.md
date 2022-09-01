# Syntactical Conventions

To keep this chapter as readable as possible, we have chosen to omit some details that are irrelevant for practically all &-modelers. In the very rare case that these technicalities are of interest, the reader could have a look in [the sourcecode of the parser](https://github.com/AmpersandTarski/Ampersand/blob/master/src/Ampersand/Input/ADL1/Parser.hs), where all EBNF statements are in comments.

## Purpose

This page is meant as a reference for syntactical details and conventions, reserved words, etc.

## Learning the syntax

The most effective way to learn Ampersand's syntax is to copy from existing scripts. This is learning by examples. This reference chapter is suitable to check things, and less suitable for learning.

## Symbols

Ampersand has _reserved words_, such as `RELATION`, `CONTEXT`, `CONTAINS`. All reserved words are written in capital letters. They are introduced on the fly. You will find an exhaustive list of reserved words at the end of this page.

Untyped atoms are written between double quotes, e.g. `"Peter"` or `"KD-686-D"`. If you want to introduce a double quote inside an atom, escape it with a backslash, e.g. `"the symbol \" is called double quote"`.  
Numeric atoms always start with a digit, e.g. `4711` or `75.88E3`. The boolean atoms are `TRUE` and `FALSE`. Dates and timestamps follow the Excel-syntax, e.g. ??? The atom `_SESSION` indicates the current user session, and is an instance of concept `SESSION`. It is used in services.

Brackets must always match. For terms, we use round brackets `(` and `)`. For populations and services we use square brackets `[` and `]`.

Constructs that contain ampersand statements are contexts and patterns. They always come in pairs: `PATTERN` and `ENDPATTERN`, and `CONTEXT` and `ENDCONTEXT`.

White space characters \(spaces, tabs, CRLF\) are meaningless. You can use them freely to layout your script in a way that helps you to recognize its structure.

A comment on a single line starts with `--`. Everything after a `--` symbol is ignored until the line ends. Multiline comments are wrapped between comment brackets `{-` and `-}`. Multiline comments may be nested.

Identifiers always start with a letter. Concepts start with a capital letter, as in `Person`, `Case`, `A`, and `Order`. Relation names start with a lower case letter, as in `contains`, `attr`, `sessionLogin`, or `r`.

## terms

terms are combined with operators. Binary operators may require brackets to avoid ambiguity. To save writing unneccessary brackets, some precedence rules are in place.

| operator category | precedence | operators |
| :--- | :--- | :--- |
| logic | 1 \(weakest\) | \|-  \(subset\),  `=` \(equal\) |
| binary boolean | 2 | `\/` \(union\), `/\` \(intersect\), `-` \(difference\) |
| binary relational | 3 | `;` \(composition\), `!` \(relational addition\), `\` \(right residual\), `/` \(left residual\), `<>` \(diamond operator\) |
| unary prefix, unary postfix | 4 \(strongest\) | `-` \(complement\), `~` \(converse\) |

Within an operator category, you must place brackets to disambiguate. E.g. `r/\s\/t` is not allowed. You have to write either `(r/\s)\/t` or `r/\(s\/t)`. Across categories, you may omit brackets because a higher precedence binds stronger. So `r;s\/t` means `(r;s)\/t`. \(Note that `(r;s)\/t` and `r;(s\/t)` have different meanings\). Associative operators \(`\/`, `/\`, `;`\) need not be disambiguated with brackets. So `r\/s\/t` and `(r\/s)\/t` and `r\/(s\/t)` all mean exactly the same.

## List of reserved words

Keywords in Ampersand are always written in CAPITALS.

* Keywords for the main structure of the code
  * [`CONTEXT`](./)
  * `ENDCONTEXT`
  * [`IN`](language-support.md)
  * `ENGLISH`
  * `DUTCH`
  * [`INCLUDE`](the-include-statement.md)
  * `META`
  * `THEMES`
  * [`PATTERN`](patterns.md)
  * `ENDPATTERN`
  * [`CONCEPT`](../the-concept-statement.md)
* Keywords for [relations](../relations.md)
  * [`RELATION`](../relations.md)
  * `PRAGMA`
  * `UNI`
  * `INJ`
  * `SUR`
  * `TOT`
  * `SYM`
  * `ASY`
  * `TRN`
  * `RFX`
  * `IRF`
  * `PROP`
  * [`POPULATION`](the-population-statement/)
  * `CONTAINS`
* Keywords for [rules](../the-rule-statement.md)
  * `RULE`
  * `MESSAGE`
  * `VIOLATION`
  * `TXT`
  * `SRC`
  * `TGT`
  * `I`
  * `V`
  * `ONE`
  * `ROLE`
  * `MAINTAINS`
* Keywords for documentation
  * [`PURPOSE`](the-purpose-statement.md)
  * [`MEANING`](../relations.md)
  * `REF`
  * `REST`
  * `HTML`
  * `LATEX`
  * `MARKDOWN`
* Keywords for [services](https://github.com/ampersandtarski/documentation/tree/662a3e7bdf67bf950cfc029e4c51efc919c0bf53/syntax/the_interface_statement.md)
  * `INTERFACE`
  * `FOR`
  * `LINKTO`
  * `BOX`
* Keywords for identities
  * [`IDENT`](the-ident-statement.md)
* Keywords for views
  * `VIEW`
  * `ENDVIEW`
  * `DEFAULT`
  * `TEMPLATE`
  * `HTML`
* Keywords for generalisations:
  * `CLASSIFY`
  * `ISA`
  * `IS`
* Keywords for TType:
  * `REPRESENT`
  * `TYPE`
  * `ALPHANUMERIC`
  * `BIGALPHANUMERIC`
  * `HUGEALPHANUMERIC`
  * `PASSWORD`
  * `BINARY`
  * `BIGBINARY`
  * `HUGEBINARY`
  * `DATE`
  * `DATETIME`
  * `BOOLEAN`
  * `INTEGER`
  * `FLOAT`
  * `AUTOINCREMENT`
* Reserved words for values of atoms:
  * `TRUE`
  * `FALSE` --for booleans
  * `_SESSION`
* Reserved words for concepts
  * `ONE`
  * `SESSION`
* Experimental keywords:
  * `SERVICE`
  * `EDITS`
* Deprecated keywords:
  * `SPEC`
  * `KEY`
  * `PROCESS`
  * `ENDPROCESS`

