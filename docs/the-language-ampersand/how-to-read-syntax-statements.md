# How to read syntax statements

Sometimes, in describing the syntax, EBNF-like notation is used, with the following meaning:

| Operator | meaning                             |
| -------- | ----------------------------------- |
| `<foo>?` | Zero or one occurrence of `<foo>`   |
| `<foo>+` | One or more occurrences of `<foo>`  |
| `<foo>*` | Zero or more occurrences of `<foo>` |

To keep this chapter as readable as possible, we have chosen to omit some details that are irrelevant for practically all Ampersand modelers. In the very rare case that these technicalities are of interest, the reader could have a look in [the sourcecode of the parser](https://github.com/AmpersandTarski/Ampersand/blob/master/src/Ampersand/Input/ADL1/Parser.hs), where all EBNF statements are fully detailed in comments.
