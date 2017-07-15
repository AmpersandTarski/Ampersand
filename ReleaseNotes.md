# Release notes of Ampersand
## Unreleased changes
  * Upgraded to LTS-8.22

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
  * SQL query performance improvements: [Issue #426](https://github.com/AmpersandTarski/Ampersand/issues/426) and 
    [Issue #217](https://github.com/AmpersandTarski/Ampersand/issues/217)
  * Back end performance: Postpone calculation of view and label for Atoms untill really needed (e.g. in interfaces)
  * Added frontend switch to turn on/off auto saving changes
  
## v3.5.1 (17 may 2016)
  * Minor enhancement of generation of Logical Data Model
  * More consisten use of views in interface definitions: [Issue #416](https://github.com/AmpersandTarski/Ampersand/issues/416)
  * Re-enabled output format for `--fSpec=asciidoc`
  * Added depth parameter in API resources call (?depth=<int>). This provides functionality to specify the depth of subinterfaces for which the content must be returned and is especially usefull for recursive (sub)interfaces using the LINKTO statement.
  * Added this release notes file. 
  * Bug fixes: [Issue #413](https://github.com/AmpersandTarski/Ampersand/issues/413) 


## v3.5.0 (28 apr 2016)
This is the first release of 2016. We are happy announce that we have much more possibilities on creating nice interfaces for the prototype application. Apart from that, lots of bugfixes have been made, and some other features have been introduced.

## releases prior to v3.5.0 
We didnt make proper release notes. If you are interested, have a look at the Git history.  
