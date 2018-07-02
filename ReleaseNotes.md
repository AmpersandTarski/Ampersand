# Release notes of Ampersand

## Unreleased changes

* Upgrade to LTS-11.13
* PHP Backend: implement [PSR6 caching interface](https://www.php-fig.org/psr/psr-6/) for conjunct violation cache
* [Issue #789](https://github.com/AmpersandTarski/Ampersand/issues/789) Introduction `API` keyword as synonym for `INTERFACE`
* [Issue #411](https://github.com/AmpersandTarski/Ampersand/issues/411) Fix issue with conjunct violation cache for src/tgt atom identifiers longer than 255 chars
* Upgrade to pandoc vs 2.0
* [Issue #482](https://github.com/AmpersandTarski/Ampersand/issues/482) Using checksum from generator to check if Ampersand model files are changed
* [Issue #792](https://github.com/AmpersandTarski/Ampersand/issues/792) Add possibility to extend TXT in interfaces
* [Issue #801](https://github.com/AmpersandTarski/Ampersand/issues/801) Fix a problem with empty values in .xlsx files during compilation of an .adl file

## v3.10.1 (8 june 2018)

* [Issue #779](https://github.com/AmpersandTarski/Ampersand/issues/779) Removed GenBericht module (project specific extension which is not used anymore)
* [Issue #482](https://github.com/AmpersandTarski/Ampersand/issues/482) Add checksum for generated Ampersand model files to detect changes

## v3.10.0 (11 may 2018)

* PHP Backend: integrated and refactored ExecEngine (not an extension anymore)
* PHP Backend: integrated and refactored ExcelImporter (not an extension anymore)
* PHP Backend: added functionality to import json population files via UI of ExcelImporter
* PHP Backend: now requires PHP 7.x (5.6 is not supported anymore)
* PHP Backend: added [Pimple](https://pimple.symfony.com/) as Dependency Injection Container library
* PHP Backend: refactored creation of AngularApp, AmpersandApp and MysqlDB using Pimple container
* PHP Backend: mysql database class placed under Plug namespace
* PHP Backend: added function/method parameter and return type specifications
* PHP Backend: update from Slim v2 to v3 api framework
* [Issue #770](https://github.com/AmpersandTarski/Ampersand/issues/770) Add configuration option to specify login page for prototype

## v3.9.5 (15 april 2018)

* [Issue #760](https://github.com/AmpersandTarski/Ampersand/issues/760) Fixed a bug causing wrong results in the prototype.

## v3.9.4 (16 march 2018)

* Better performance of the generator, due to using Haskell's sets rather than lists.
* [Issue #758](https://github.com/AmpersandTarski/Ampersand/issues/758) Fixed a bug that could cause a runtime error in edgecases

## v3.9.3 (16 february 2018)

* [Issue #752](https://github.com/AmpersandTarski/Ampersand/issues/752) Fixed a bug that could cause failure without proper error message
* Better (more uniform) diagnostic comments in SQL output

## v3.9.2 (19 january 2018)

* [Issue #739](https://github.com/AmpersandTarski/Ampersand/issues/739) Better parser error message for mismatch of values
* [Issue #744](https://github.com/AmpersandTarski/Ampersand/issues/744) Better lexer error message for unterminated string
* [Issue #741](https://github.com/AmpersandTarski/Ampersand/issues/741) Solved a bug that could cause invalid database state during runtime of a prototype

## v3.9.1 (22 december 2017)

* Security fix: use PHP session strict mode. This prevents a user defined session ID that is never generated
* [Issue #707](https://github.com/AmpersandTarski/Ampersand/issues/707) Bugfix issue where user input '_SESSION' was also replaced by the session id
* [Issue #723](https://github.com/AmpersandTarski/Ampersand/issues/723) THEMES have been removed from the syntax.
* [Issue #729](https://github.com/AmpersandTarski/Ampersand/issues/729) Bugfix issue where REPRESENT statements with DATE, DATETIME etc. is involved
* [Issue #732](https://github.com/AmpersandTarski/Ampersand/issues/732) Create own prelude, to guarantee correct use of UTF8 everywhere in the Haskell code

## v3.9.0 (24 november 2017)

It has taken some time since the last release. This release has some major work in it, and also a change of syntax of scripts. Unfortunately this is not downward compatible.

* Introduction of SystemContext.adl, which will be required for new interface generation, which will become available in some future release
* Upgraded to LTS-9.2
* [Issue #713](https://github.com/AmpersandTarski/Ampersand/issues/713) New syntax for singleton expressions

## v3.8.8 (1 september 2017)

* Somewhat more verbose test output
* [Issue #692](https://github.com/AmpersandTarski/Ampersand/issues/692) Better allignment between Formal Ampersand and meatgrinder
* [Issue #693](https://github.com/AmpersandTarski/Ampersand/issues/693) Fixed a bug about cyclic defined CLASSIFY statements

## v3.8.7 (4 august 2017)

* Upgraded to LTS-8.22
* Made the meatgrinder do better static analisys, inspired by issue #684

## v3.8.6 (7 july 2017)

* [Issue #674](https://github.com/AmpersandTarski/Ampersand/issues/674) New feature for ExecEngine to navigate user to other interface
* [Issue #675](https://github.com/AmpersandTarski/Ampersand/issues/675) Extend scope of _NEW in ExecEngine from NewStruct to complete violation statement

## v3.8.5 (12 may 2017)

* [Issue #666](https://github.com/AmpersandTarski/Ampersand/issues/666) A hint message is supplied when a case-incorrect url is used.
* Upgrade LTS Haskell 8.5, which includes GHC 8.0.2

## v3.8.4 (not released, there was nothing new in april 2017)

## v3.8.3 (17 march 2017)

* Minor modifications on frontend

## v3.8.2 (17 february 2017)

* [Issue #621](https://github.com/AmpersandTarski/Ampersand/issues/621) Disabled --fpa-excel switch, because it resulted in a fatal error.
* [Issue #617](https://github.com/AmpersandTarski/Ampersand/issues/617) Minor fix in functional design document.
* [Issue #624](https://github.com/AmpersandTarski/Ampersand/issues/624) New feature: ExecEngine can merge atoms to fix violations of univalence and other identity violations.
* [Issue #625](https://github.com/AmpersandTarski/Ampersand/issues/625) Comparison of origins now based on canonicalized paths
* [Issue #627](https://github.com/AmpersandTarski/Ampersand/issues/627) Fixed a bug in generation of queries for frontend

## v3.8.1 (20 january 2017)

* [Issue #605](https://github.com/AmpersandTarski/Ampersand/issues/605) Added modules "Modules.adl" and "Patterns.adl" in FormalAmpersand as preparatory work for issue #605.
* [Issue #603](https://github.com/AmpersandTarski/Ampersand/issues/603) Removed obsolete switches about what normalization is used.
* [Issue #600](https://github.com/AmpersandTarski/Ampersand/issues/600) When a configuration file exists (.yaml) with the same name as the file being compiled, it will be used if no config file is explicitly mentioned in the command.
* Renamed "functional specification" to "functional design" for more realistic expectations with users.
* Restructured internal structure of modules of Ampersand.

## v3.8.0 (20 december 2016)

* [Issue #587](https://github.com/AmpersandTarski/Ampersand/issues/587) There is no need any longer to explicitly specify the language in a script. If omitted, IN ENGLISH will be used as default.
* [Issue #588](https://github.com/AmpersandTarski/Ampersand/issues/588) PURPOSE sytax changed: Now matching brackets are: `{+` and `+}`   (no backwards compatibility, to enable nested comment blocks)
* Added some additional diagnosis info in settings.json
* Added support for project/application and extension specific composer dependencies.
* Added 'customization' folder to prototype generation process. This folder can be used to e.g. overwrite generated views.
* Includes for frontend app must be placed in 'app' folder now. The include folder thereby directly matches the destination directory structure.
* Ampersand version info is printed in verbose mode
* New switch, to add all relations, concepts and generalisation relations of formal ampersand into your script: --add-semantic-metamodel

## v3.7.3 (25 november 2016)

* Alternative definition of univalence and injectivity to get better violations on runtime.

## v3.7.2 (28 october 2016)

* Some updates in the meatgrinder. (still experimental)
* Several changes in the generation of the functional design document. (Less LaTex specific)

## v3.7.1 (30 september 2016)

* Upgrade version of Haskell compiler to 8.0.1 (automatically used via Stack)
* Progress on meatgrinder. Basic meatgrinder functionality is available now.
* Several bugfixes

## v3.7.0 (2 september 2016)

* [Issue #506](https://github.com/AmpersandTarski/Ampersand/issues/506) Bugfix in chapter diagnosis of Func. spec.
* [Issue #483](https://github.com/AmpersandTarski/Ampersand/issues/483) Introduction of proper way to produce exit codes of Ampersand
* [Issue #502](https://github.com/AmpersandTarski/Ampersand/issues/502) More stable way to generate .pdf file (LaTeX output)
* [Issue  #72](https://github.com/AmpersandTarski/Ampersand/issues/72) Symmetry of relations will be checked by a generated application
* [Issue #256](https://github.com/AmpersandTarski/Ampersand/issues/256) Surjectivity of relations will be checked by a generated application
* [Issue #345](https://github.com/AmpersandTarski/Ampersand/issues/345) Surjectivity of relations will be checked by a generated application

## v3.6.1 (5 august 2016)

* [Issue #488](https://github.com/AmpersandTarski/Ampersand/issues/488) Performance enhancement: Added indexes on table columns when possible
* [Issue #486](https://github.com/AmpersandTarski/Ampersand/issues/486) Performance enhancement: Removed DISTINCT in subqueries
* [Issue #459](https://github.com/AmpersandTarski/Ampersand/issues/459) fix for underscores in Concept name
* [Issue #489](https://github.com/AmpersandTarski/Ampersand/issues/489) Implemented markdown for rule violation messages in frontend
* [Issue #412](https://github.com/AmpersandTarski/Ampersand/issues/412) Partial fix for removing rows from COLS template
* [Issue #373](https://github.com/AmpersandTarski/Ampersand/issues/373) Fix breadcrumb
* ExecEngine extension: improved logging for debugging
* Added new reporting functionality for backend framework
* DB performance: less queries because 'I[Concept]'-expression is not queried anymore
* Minor backend fixes
* Fix issues regarding CRUD specifications: missing interfaces in navbar +menu, missing crudR check in templates, return content after create

## v3.6.0 (8 july 2016)

* [Issue #406](https://github.com/AmpersandTarski/Ampersand/issues/406) Minor changes on syntax of INTERFACE statement.
* [Issue #438](https://github.com/AmpersandTarski/Ampersand/issues/438) New switch: `--include`
* [Issue #438](https://github.com/AmpersandTarski/Ampersand/issues/438) Enhancement: Introduction of a configuration file. A sample configuration file is generated when you use the switch `--sampleConfigFile`. To use a config file, use the switch `--config=MyConfig.yaml`
* [Issue #468](https://github.com/AmpersandTarski/Ampersand/issues/468) Enhancement: default configuration file (when it exists)
* [Issue #414](https://github.com/AmpersandTarski/Ampersand/issues/414) Enhancement: From now on, all text values read from .xlsx files are trimmed (leading and trailing spaces are removed), unless the switch --do-not-trim-cellvalues is given.
* More automation on releasing Ampersand.
* Frontend: Options in frontend navbar now can be defined for certain roles only (e.g. the installer and excelimporter).
* [Issue #103](https://github.com/AmpersandTarski/Ampersand/issues/103) Fix overlap by multiple rows in navbar
* [Issue #423](https://github.com/AmpersandTarski/Ampersand/issues/423) Database is automatically installed when it does not exists yet (first time use)

## v3.5.2 (10 juni 2016)

* Work on meatgrinder (still experimental!)
* Bug fix: Issue with SQL query [Issue #152](https://github.com/AmpersandTarski/Ampersand/issues/152)
* Bug fix: minor issue with SQL query [Issue #436](https://github.com/AmpersandTarski/Ampersand/issues/436)
* Bug fix: Nontermination of functional document generator. [Issue #231](https://github.com/AmpersandTarski/Ampersand/issues/231)
* SQL query performance improvements: [Issue #426](https://github.com/AmpersandTarski/Ampersand/issues/426) and [Issue #217](https://github.com/AmpersandTarski/Ampersand/issues/217)
* Back end performance: Postpone calculation of view and label for Atoms untill really needed (e.g. in interfaces)
* Added frontend switch to turn on/off auto saving changes

## v3.5.1 (17 may 2016)

* Minor enhancement of generation of Logical Data Model
* More consisten use of views in interface definitions: [Issue #416](https://github.com/AmpersandTarski/Ampersand/issues/416)
* Re-enabled output format for `--fSpec=asciidoc`
* Added depth parameter in API resources call (?depth=\<int\>). This provides functionality to specify the depth of subinterfaces for which the content must be returned and is especially usefull for recursive (sub)interfaces using the LINKTO statement.
* Added this release notes file.
* Bug fixes: [Issue #413](https://github.com/AmpersandTarski/Ampersand/issues/413)

## v3.5.0 (28 apr 2016)

This is the first release of 2016. We are happy announce that we have much more possibilities on creating nice interfaces for the prototype application. Apart from that, lots of bugfixes have been made, and some other features have been introduced.

## releases prior to v3.5.0

We didnt make proper release notes. If you are interested, have a look at the Git history.
