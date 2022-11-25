# The Preprocessor

## Purpose

In order to enable multiple configurations of some functionality, Ampersand allows developers to specify code fragments that are conditionally ignored \(or included, depending on your point of view\). This allows you e.g. to include code that you need for debugging/development, but that is not needed in demonstrations. It also allows you to create modules, e.g. SIAM, that developers can re-use yet have control over the functionality that is included.

## Description

The preprocessor allows for

* setting or clearing preprocessor variables. This is done by means of an extension of the `INCLUDE` statement.
* testing a preprocessor variable, and including or ignoring a subsequent text depending on the value of that variable.

The preprocessor syntax has been designed in such a way that scripts that use the preprocessor syntax can also be compiled by versions of Ampersand that do not support preprocessor syntax. In such cases, the preprocessor syntax is treated as comment \(and hence ignored\). Obviously, compiling scripts whose consistency relies on preprocessor statements cannot be expected to compile with versions of Ampersand that do not support the preprocessor syntax.

## Examples

Here is an example of how preprocessor variables can be used within a script. In this example, a list of usernames and passwords is shown when the preprocessor variable `Developing` is true. If that variable is false, all texts between the `--#IF` and `--#ENDIF` are treated as comments \(ignored\)

```text
INTERFACE "Login": I[LoginForm] BOX
   [ "Userid": login_Userid cRUd
   , "Password": login_Password crUd
--#IF Developing
   , "Possible U/PW combinations": V[LoginForm*Account] COLS
      [ "User": accPerson cRud
      , "Userid" : accUserid cRud
      , "Password" : accPassword cRud
      ]
--#ENDIF
   ]
```

Preprocessor variables are assigned values in `INCLUDE` statements, e.g. as follows:

```text
INCLUDE "foo.adl" --# [ "firstvar", "!secondvar", "Developing" ]
```

Variables are set or cleared by specifying their names \(in quotes\) in a list following the `--#` preprocessor directive. When a variable name is preceeded with a `!`-character, its value is cleared \(set to 'false'\). Otherwise, its value is set \(to 'true'\).

## Syntax and meaning

At the time of writing, the preprocssor specifications are as follows:

A line of text, that \(a\) starts with optional whitespace, \(b\) is followed by 2 or more `-` characters, \(nu is dat 2 `-` chars\) \(c\) is followed by optional whitespace, \(dat mag nu nog niet\) \(d\) is followed by `#` and \(e\) is followed with optional whitespace, is not interpreted by the Ampersand parser, but is passed to the Preprocessor instead. The text behind the `#`-character up till the end of the line is the `TextToBePreprocessed`. `TextToBePreprocessed` is defined by the \(PCRE\) regex `^\s*--+\s*#\s*(?P<TextToBePreprocessed>.*)$`. Note that Ampersand versions that do not support preprocessing will treat such texts as comment.

A preprocessor Keyword is the first word in `TextToBePreprocessed`. `Keyword` is defined by the \(PCRE\) regex `(?P<Keyword>\w+\b)` when it is applied to `TextToBePreprocessed`. In absolute terms, that would be `^\s*--+\s*#\s*(?P<Keyword>\w+\b).*$` followed by optional whitespace and keyword consisting of alphanumeric characters. Keywords are \(thus\) case sensitive. Examples: `--#IF Debugging` or `-- # ENDIF`

Currently, valid keyword syntax is as follows \(the \(PCRE\) regexes are assumed to be applied on `TextToBePreprocessed`\):

* `IF`, `IFNOT` each take one argument - a variable.

  Formally, this is defined by \(PCRE\) regex `(?P<Keyword>IF|IFNOT)\s+(?P<Variable>\w+)`.

  Examples `--#IF Debugging` or `--#IFNOT UserSpecifiesLoginMethod`

* `ELSE` and `ENDIF` do not take arguments.

  The preprocessor treats any text following these syntaxes as comments \(i.e.: ignores such texts\)

Also the syntax of `INCLUDE` statements is extended to include an optional comment that specifies a list of quoted variable names. Example `INCLUDE "../SIAMv3/Login.ifc" --# [ "Debugging", "NoLogout" ]`. The \(PCRE\) regex is `\bINCLUDE\s+"(?P<fileid>[^"]+)"(\s+(--#\s*)?\[\s*"(?P<var1>!?\w+)"(\s*,\s*"(?P<var2>!?\w+)")*\])?` where any text in groups `var1` or `var2` are variable names that may \(optionally\) be preceeded with a `!` character. For each such variable names, a variable is created that can be referenced by its name. When the variable name was preceeded with a `!` character, its value is initialized as 'false'; When the variable name was not preceeded with a `!` character, its value is initialized as 'true'; If a variable with a specified name was already created, the newly created variable takes precedence. After a file inclusion terminates, the variables that the INCLUDE statement created are all destroyed.

When the preprocessor parses the file that is `INCLUDE`d, preprocessor commands that evaluate a variable \(such as `IF` or `IFNOT`\) will use the value as defined in that `INCLUDE` statement or \(recursively\) in that of a 'higher' `INCLUDE` statement.

