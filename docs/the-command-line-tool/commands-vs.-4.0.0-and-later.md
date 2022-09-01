---
description: >-
  As from version 4.0.0 Ampersand has a command structure that allows you to use
  Ampersand from the command line.
---

# Commands (vs. 4.0.0 and later)

## Commands without a script

1. `ampersand --version` \
   to find out which version of Ampersand you are using.
2. `ampersand --help`\
   to summarize all level one commands

## Commands with a script

In practice, run `ampersand --help` instead of consulting the following list, just to be sure that the commands match the version you are using. The following list has been taken from version 4.0.0.

The general structure of commands is `ampersand [options]* COMMAND`. The following commands are available:

1.  `ampersand [options]* check <filename>`

    Check your model for syntax errors and type errors.
2.  `ampersand [options]* daemon <filename>`

    Check your model continuously while editing it.
3.  `ampersand [options]* data-analysis <filename>`

    Export a data model as a plain Ampersand script, for analyzing Excel data.
4.  `ampersand [options]* dev-output <filename>`

    Generate some diagnostic files, intended for developers of Ampersand.
5.  `ampersand [options]* documentation <filename>`

    Generate a functional design document, to kick-start your functional specification.
6.  `ampersand [options]* population <filename>`

    Generate a .xmlx file containing the populations of your script.
7.  `ampersand [options]* proofs <filename>`

    Generate a report containing proofs.
8.  `ampersand [options]* proto <filename>`

    &#x20;Generate a prototype from your specification.
9.  `ampersand [options]* export <filename>`

    Generate a single `.adl`-file of your script (prettyprinted)
10. `ampersand [options]* uml <filename>`

    &#x20;Generate a data model in UML 2.0 style.
11. `ampersand [options]* validate <filename>`

    Compare results of rule evaluation in Haskell and SQL, for testing expression semantics. This requires command line PHP with MySQL support.
12. `ampersand [options]* test <filename>`

    &#x20;Run test suites in a given directory. This is meant to do regression testing during automatic build (e.g. Travis-ci)

## Filename

The `filename` in a command typically has suffix `.adl` (which stands for Ampersand Description Language). If you omit the suffix `.adl`, Ampersand does NOT add it for you. The filename conventions of your operating system apply (whether you have Windows, macOS, or Linux).

## Options

You may use one of the first three options to specify how verbose your output will be. The other options can be used at random and mixed freely.

1. `--help` Show the help text. This is always up to date with the version of the compiler you are running. So consider `ampersand --help` to be authoritative.
2.  `--verbosity VERBOSITY`

    Verbosity: silent, error, warn, info, debug
3.  `-V, --verbose`

    Enable verbose mode: verbosity level "debug"
4.  `--silent`

    Enable silent mode: verbosity level "silent"
5.  `--[no-]time-in-log`

    Enable/disable inclusion of timings in logs, for the purposes of using diff with logs (default: enabled)
6.  `--[no-]terminal`

    Enable/disable overriding terminal detection in the case of running in a false terminal
7.  `--terminal-width INT`

    Specify the width of the terminal, used for pretty-print messages
8.  `--outputDir DIR`

    Specify the directory where your output will be written to.

## Options per command

Every command can have options of its own. Use the command and add `--help` to see which command specific options there are.

Example:&#x20;

```
documentation % ampersand documentation --help
Usage: ampersand documentation [--blackWhite] [--[no-]Intro] [--[no-]SharedLang]
                               [--[no-]Diagnosis] [--[no-]ConceptualAnalysis] 
                               [--[no-]DataAnalysis] [--[no-]graphics] 
                               [--[no-]text] --format FORMAT AMPERSAND_SCRIPT 
                               [--sql-bin-tables] [--interfaces] 
                               [--namespace NAMESPACE] [--crud-defaults CRUD] 
                               [--[no-]trim-cellvalues] [--build-recipe RECIPE] 
                               [--ignore-invariant-violations] 
                               [--language OUTPUTLANGUAGE] [--[no-]legal-refs] 
                               [--verbosity VERBOSITY | (-v|--verbose) | 
                                 --silent] [--[no-]time-in-log] 
                               [--[no-]terminal] [--terminal-width INT] 
                               [--output-dir DIR] [--help]
  Generate a functional design document, to kick-start your functional
  specification.

Available options:
  --blackWhite             avoid coloring conventions to facilitate readable
                           pictures in black and white.
  --[no-]Intro             Do or do not include chapter Intro in the generated
                           document.
  --[no-]SharedLang        Do or do not include chapter SharedLang in the
                           generated document.
  --[no-]Diagnosis         Do or do not include chapter Diagnosis in the
                           generated document.
  --[no-]ConceptualAnalysis
                           Do or do not include chapter ConceptualAnalysis in
                           the generated document.
  --[no-]DataAnalysis      Do or do not include chapter DataAnalysis in the
                           generated document.
  --[no-]graphics          Enable/disable generation of graphics before
                           generating the document. (default: enabled)
  --[no-]text              Enable/disable generation the document file.
                           (default: enabled)
  --format FORMAT          The format in which the output is written.
  AMPERSAND_SCRIPT         The root file of your Ampersand model.
  --sql-bin-tables         Generate binary tables instead of broad tables in SQL
                           database, for testing purposes.
  --interfaces             Generate interfaces, which currently does not work.
  --namespace NAMESPACE    Prefix database identifiers with this namespace, to
                           isolate namespaces within the same
                           database. (default: "")
  --crud-defaults CRUD     Temporary switch to learn about the semantics of crud
                           in interface expressions. (default: "CRUD")
  --[no-]trim-cellvalues   Enable/disable ignoring the leading and trailing
                           spaces in .xlsx files that are INCLUDED in the
                           script. (default: enabled)
  --build-recipe RECIPE    Build the internal FSpec with a predefined recipe.
                           Allowd values are:
                           [Standard,Grind,Prototype,RAP] (default: "Standard")
  --ignore-invariant-violations
                           ignore invariant violations. In case of the prototype
                           command, the generated prototype might not behave as
                           you expect. Documentation is not affected. This means
                           that invariant violations are reported anyway. (See
                           https://github.com/AmpersandTarski/Ampersand/issues/728)
  --language OUTPUTLANGUAGE
                           Pick 'NL' for Dutch or 'EN' for English, as the
                           language to be used in your output. Without this
                           option, output is written in the language of your
                           context.
  --[no-]legal-refs        Enable/disable generation of a table of legal
                           references in Natural Language chapter of the output
                           document. (default: disabled)
  --verbosity VERBOSITY    Verbosity: silent, error, warn, info, debug
  -v,--verbose             Enable verbose mode: verbosity level "debug"
  --silent                 Enable silent mode: verbosity level "silent"
  --[no-]time-in-log       Enable/disable inclusion of timings in logs, for the
                           purposes of using diff with logs (default: enabled)
  --[no-]terminal          Enable/disable overriding terminal detection in the
                           case of running in a false terminal
  --terminal-width INT     Specify the width of the terminal, used for
                           pretty-print messages
  --output-dir DIR         Specify the directory where your output will be
                           written to
  --help                   Show this help text
```

