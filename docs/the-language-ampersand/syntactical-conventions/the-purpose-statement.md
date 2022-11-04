# The PURPOSE statement

## Semantics

Most things in your model are in it for a reason. To document these, you should use the PURPOSE statement.

## Syntax

`PURPOSE` `<type of thing>` `<name>` `<language>?` `<markup>?` `<list of references>?`

`{+` `<anything>` `+}`

Where `<type of thing>` and `<name>` are the type and name of the thing that is refered to. This could be one of: `CONCEPT`, `RELATION`, `RULE`, `IDENT`, `VIEW`, `PATTERN`, `INTERFACE`, `CONTEXT`

The optional  and  can be used to override the settings for language and markup. If omitted, these are inherited from the pattern of context where the PURPOSE statement is specified in.

