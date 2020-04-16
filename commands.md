# Changes to the command line interface

In the previous years, Ampersand has accumulated a bunch of command line options.
There were no commands, and it was sometimes unclear what options could be used
together and what options couldn't be used together. Since version 4 of Ampersand,
we decided to introduce _commands_. Most of the commands ask the compiler to
generate some specific output from your model.

We decided not te be backwards compatible, because that would make the implementation
even more complex than it already was.

## What happend to the command line options from before version 4.0.0

* Discontinued because they did not do anything (useful)

  * ***--ECA***
          generate documentation with ECA rules, for future purposes.
  * ***--altGraphics***
          generate graphics in an alternate way. (you may experiment with
          this option to see the differences for yourself)
  * ***--atlas-without-expressions***
          Temporary switch to create Atlas without expressions, for use in
          RAP3
  * ***--crowfoot***
          generate crowfoot notation in graphics, to please crowfoot
          addicts.
  * ***--fpa***
          Generate Function Point Analysis chapter.
  * ***--fpa-excel*** `fpa`
          Generate an Excel workbook (FPA_\<filename\>.xml).
  * ***--oldNormalizer***
          Use the old normalizer at your own risk.
  * ***--newNormalizer***
          Use the new normalizer at your own risk.
  * ***--test***
          Used for test purposes only.
  * ***--testRule=RULE***
          Show contents and violations of specified rule, for testing a
          single rule in your Ampersand-script.

* Changed into commands (in `this style` the new command is given)
  * With no specific output requested, you could run ampersand in the past as well.
          With the introduction of the commands, a command has become mandatory.
          For this reason, we introduced the command `check`, which will
          assess the given script.
  * ***--daemon[=configfile]*** `daemon --daemonconfig=CONFIGFILE`
          Run ampersand as daemon, for use by the vscode
          ampersand-language-extention. An optional parameter may be
          specified to tell what config file is used. This defaults to
          _.ampersand_ .
  * ***--fspec=FORMAT*** `documentation --format=FORMAT`
          generate a functional design document in specified format
          (FORMAT=[Asciidoc, Context, Docbook, Docx, Html, Latex, Man,
          Markdown, Mediawiki, Opendocument, Org, Pandoc, Pdf, Plain, Rst,
          Rtf, Texinfo, Textile]), to kick-start your functional
          specification.
  * ***--proto[=DIRECTORY]*** `proto --proto-directory=DIRECTORY`
          generate a functional prototype, so you can experiment with the
          information system specified in your script.
  * ***--dataAnalysis[=file]*** `data-analysis`
          export a data model as plain Ampersand script, for analysing
          Excel-data.

  * ***--export[=file]*** `export`
          export as prettyprinted plain Ampersand script, for round-trip testing of the
          Ampersand compiler.
  * ***--pop-xlsx*** `population`
          Generate an .xlsx file containing the populations of your script.
  * ***--proofs*** `proofs`
          generate derivations, for testing the generation of rules.
  * ***--uml*** `uml`
          Generate a data model in UML 2.0 style.
  * ***--validate*** `validate`
          Compare results of rule evaluation in Haskell and SQL, for
          testing expression semantics. This requires command line php with
          MySQL support.
  * ***--haskell*** `devoutput`
          generate internal data structure, written in Haskell (for
          debugging).
  * ***--sqldump*** `devoutput`
          generate a dump of SQL queries (for debugging).

* Removed

  * ***--sampleConfigFile*** 
          We depend on docker lately to run ampersand scripts. Therefor, we use
          its features to specify the command line options for Ampersand. Because
          of this, we no longer support configuration files for Ampersand.
  * ***--skip-composer***
          skip installing php dependencies (using Composer) for prototype
          framework.
          
* Changed in some way

  * We used to have verbose and not-verbose. Now we have 4 levels of verbosity. This effects the former swich --verbose. We now have
    * ***--verbosity VERBOSITY***    Verbosity: `silent`, `error`, `warn`, `info`, `debug`
    * ***-v,--verbose***             Enable verbose mode: verbosity level "debug"
    * ***--silent***                 Enable silent mode: verbosity level "silent"
    * ***--[no-]time-in-log***       Enable/disable inclusion of timings in logs, for the purposes of using diff with logs (default: enabled)
  * We used to have a number of switches. Most of them now can be put on or off, using a generic way of prefixing the name with `no-`
  * The flags ***--noDiagnosis***, ***--noGraphics***, ***--diagnosis*** have been replaced by a generic mechanism of a switch per chapter. This gives full control what chapters you want in your generated document.
  * ***--do-not-trim-cellvalues*** has been replaced by `--[no-]trim-cellvalues`
  * ***--help*** Now gives information about the level where it is called:
    * `ampersand --help` gives help about the commands and global options.
    * `ampersand <command> --help` gives information about the command-specific options.
  * We used to have the possibility to fiddle around with the formal ampersand metamodel. This
    has been generalized, and we now have another metamodel as well, called PrototypeContext. The
    latter is used during the generation of prototypes. This is still an experimental feature.
    * ***--add-semantic-metamodel, --meta-tables*** Changed to `--build-recipe RECIPE`.
          All relations, views, idents etc. from the specified metamodel will be
          available for use in your model. These artefacts do not have to
          be declared explicitly in your own model.

* Unchanged (but now only available for relevant commands)
  * ***--outputDir=DIR***
          output directory. Not used for prototypes.
  * ***--blackWhite***
          avoid coloring conventions to facilitate readable pictures in
          black and white.
  * ***--crud-defaults=CRUD***
          Temporary switch to learn about the semantics of crud in
          interface expressions.
  * ***--interfaces (-x)***
          generate interfaces, which currently does not work.
  * ***--language=LANG***
          Pick 'NL' for Dutch or 'EN' for English, as the language to be
          used in your output. Without this option, output is written in
          the language of your context.
  * ***--namespace=NAMESPACE***
          prefix database identifiers with this namespace, to isolate
          namespaces within the same database.
  * ***--reference-table***
          generate a table of references in the Natural Language chapter,
          for instance for legal traceability.
  * ***--sql-bin-tables***
          generate binary tables only in SQL database, for testing
          purposes.
  * ***--customizations=DIRECTORY***
          copy a directory into the generated prototype, overriding the
          default directory called 'customizations'.
  * ***--dbName=NAME (-d)***
          database name (This overrules environment variable CCdbName,
          defaults to filename) to which the prototype will connect for
          persistent storage.
  * ***--force-reinstall-framework***
          re-install the prototype framework. This discards any previously
          installed version.
  * ***--ignore-invariant-violations***
          Allow to build a prototype, even if there are invariants that are
          being violated. (See
          <https://github.com/AmpersandTarski/Ampersand/issues/728)>)
  * ***--prototype-framework-version=VERSION***
          tag, branch or SHA of the prototype framework on Github. Normally you
          shouldn't need to change this. It refers to the version of the frontend
          code at <https://github.com/AmpersandTarski/prototype>.
  * ***--sqlHost=HOSTNAME***
          set SQL host name (Defaults to `localhost`), to identify the host
          on which the persistent store resides
  * ***--sqlLogin=USER***
          set SQL user name (Defaults to `ampersand`), to let your
          application login to the database.
  * ***--sqlPwd=PASSWORD***
          set SQL password (Defaults to `ampersand`), to let your
          application login to the database.
  * ***--version (-V)***
          show version and exit.
  
## working with meta-models

We now have `recipes` to deal with metamodels such as Formal Ampersand and PrototypeContext. These recipes will be used based on the chose command. There might be a need to modify those recipes by the user. This still has to be seen. In any case, the following options will be removed:

* ***--gen-as-rap-model***
          Generate populations for use in RAP3.
* ***--meta-file***
          Generate an .adl file that contains the relations of
          formal-ampersand, populated with the the meta-population of your
          own .adl model, in case you want a metamodel.
