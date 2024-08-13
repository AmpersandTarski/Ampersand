# Syntactical conventions

This section is a reference for meta syntax, syntactical conventions, reserved words, etc.

To keep this documentation readable, the documentation omits some details that we deem irrelevant for most Ampersand modelers. The definitive syntax definition is [the source code of the parser](https://github.com/AmpersandTarski/Ampersand/blob/master/src/Ampersand/Input/ADL1/Parser.hs), where all EBNF statements are fully detailed in comments.

## How to read syntax statements

Sometimes, in describing the syntax, we use EBNF-like notation with the following meaning:

| Operator | meaning                             |
| -------- | ----------------------------------- |
| `<foo>?` | Zero or one occurrence of `<foo>`   |
| `<foo>+` | One or more occurrences of `<foo>`  |
| `<foo>*` | Zero or more occurrences of `<foo>` |

## Syntactical Conventions

## Symbols

Ampersand has _reserved words_, such as `RELATION`, `CONTEXT`, `CONTAINS`. All reserved words are written in capital letters. They are introduced on the fly. You will find an exhaustive list of reserved words [here](## List of reserved words).

Untyped atoms are written between double quotes, e.g. `"Peter"` or `"KD-686-D"`. If you want to introduce a double quote inside an atom, escape it with a backslash, e.g. `"the symbol \" is called double quote"`.  
Numeric atoms always start with a digit, e.g. `4711` or `75.88E3`. The boolean atoms are `TRUE` and `FALSE`. Dates and timestamps follow the Excel-syntax, e.g. ??? The atom `_SESSION` indicates the current user session, and is an instance of concept `SESSION`. It is used in interfaces.

Brackets must always match. For terms, we use round brackets `(` and `)`. For populations and interfaces we use square brackets `[` and `]`.

Constructs that contain Ampersand statements are contexts and patterns. They always come in pairs: `PATTERN` and `ENDPATTERN`, and `CONTEXT` and `ENDCONTEXT`.

White space characters \(spaces, tabs, CRLF\) are meaningless. You can use them freely to layout your script in a way that helps you to recognize its structure.

A comment on a single line starts with `--`. Everything after a `--` symbol is ignored until the line ends. Multiline comments are wrapped between comment brackets `{-` and `-}`. Multiline comments may be nested.

Identifiers always start with a letter. Concepts start with a capital letter, as in `Person`, `Case`, `A`, and `Order`. Relation names start with a lower case letter, as in `contains`, `attr`, `sessionLogin`, or `r`.

## Terms

Terms specify calculations with relations. They combine relations with operators to produce new relations. There are unary and binary operators. Binary operators may require brackets to avoid ambiguity. To save writing unneccessary brackets, some precedence rules are in place.

| operator category           | precedence      | operators                                                                                                                  |
| :-------------------------- | :-------------- | :------------------------------------------------------------------------------------------------------------------------- |
| logic                       | 1 \(weakest\)   | \|- \(subset\), `=` \(equal\)                                                                                              |
| binary boolean              | 2               | `\/` \(union\), `/\` \(intersect\), `-` \(difference\)                                                                     |
| binary relational           | 3               | `;` \(composition\), `!` \(relational addition\), `\` \(right residual\), `/` \(left residual\), `<>` \(diamond operator\) |
| unary prefix, unary postfix | 4 \(strongest\) | `-` \(complement\), `~` \(converse\)                                                                                       |

Within an operator category, you must place brackets to disambiguate. E.g. `r/\s\/t` is not allowed. You have to write either `(r/\s)\/t` or `r/\(s\/t)`. Across categories, you may omit brackets because a higher precedence binds stronger. So `r;s\/t` means `(r;s)\/t`. \(Note that `(r;s)\/t` and `r;(s\/t)` have different meanings\). Associative operators \(`\/`, `/\`, `;`\) need not be disambiguated with brackets. So `r\/s\/t` and `(r\/s)\/t` and `r\/(s\/t)` all mean exactly the same.

## List of reserved words

Keywords in Ampersand are always written in CAPITALS.

- Keywords for the main structure of the code
  - [`CONTEXT`, `ENDCONTEXT`](./syntax-of-ampersand#the-context-statement)
  - [`IN`, `ENGLISH`, `DUTCH`](./syntax-of-ampersand#language-support)
  - [`INCLUDE`](./syntax-of-ampersand#the-include-statement)
  - [`PATTERN`, `ENDPATTERN`](./syntax-of-ampersand#the-pattern-statement)
  - [`CONCEPT`](./syntax-of-ampersand#the-concept-statement)
- Keywords for [relations](./syntax-of-ampersand#the-relation-statement)
  - [`RELATION`](./syntax-of-ampersand#the-relation-statement)
  - `PRAGMA`
  - [`UNI`, `INJ`, `SUR`, `TOT`, `SYM`, `ASY`, `TRN`, `RFX`, `IRF`, `PROP`, `MAP`, `BIJ`](./syntax-of-ampersand.md#properties)
  - [`POPULATION`, `CONTAINS`](./syntax-of-ampersand#the-population-statement)
- Keywords for [rules](./syntax-of-ampersand#the-rule-statement)
  - `RULE`
  - `MESSAGE`
  - `VIOLATION`
  - `TXT`
  - `SRC`
  - `TGT`
  - `I`
  - `V`
  - `ONE`
  - `ROLE`
  - `MAINTAINS`
- Keywords for documentation
  - [`PURPOSE`](./syntax-of-ampersand#the-purpose-statement)
  - [`MEANING`](./syntax-of-ampersand#the-meaning-sub-statement)
  - `META`
  - `REF`
  - `REST`
  - `HTML`
  - `LATEX`
  - `MARKDOWN`
- Keywords for [interfaces](./interfaces.md)
  - `INTERFACE`
  - `FOR`
  - `LINKTO`
  - `BOX`
- Keywords for identities
  - [`IDENT`](./syntax-of-ampersand#the-ident-statement)
- Keywords for views
  - `VIEW`
  - `ENDVIEW`
  - `DEFAULT`
  - `TEMPLATE`
  - `HTML`
- Keywords for generalisations:
  - [`CLASSIFY`](./syntax-of-ampersand#the-classify-statement)
  - `ISA`
  - `IS`
- Keywords for TType:
  - `REPRESENT`
  - `TYPE`
  - `ALPHANUMERIC`
  - `BIGALPHANUMERIC`
  - `HUGEALPHANUMERIC`
  - `PASSWORD`
  - `BINARY`
  - `BIGBINARY`
  - `HUGEBINARY`
  - `DATE`
  - `DATETIME`
  - `BOOLEAN`
  - `INTEGER`
  - `FLOAT`
  - `AUTOINCREMENT`
- Reserved words for values of atoms:
  - `TRUE`
  - `FALSE` --for booleans
  - `_SESSION`
- Reserved words for concepts
  - `ONE`
  - `SESSION`
- Experimental keywords:
  - `SERVICE`
  - `API`
