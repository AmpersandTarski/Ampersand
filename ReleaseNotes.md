# Release notes of Ampersand

## unreleased changes
  * Performance enhancement: Added indexes on table columns when possible

## v3.6.0 (8 july 2016)
  * [Issue #406](https://github.com/AmpersandTarski/Ampersand/issues/406) Minor changes on syntax of INTERFACE statement. 
  * [Issue #438](https://github.com/AmpersandTarski/Ampersand/issues/438) New switch: --include
  * [Issue #438](https://github.com/AmpersandTarski/Ampersand/issues/438) Enhancement: Introduction of a configuration file. A sample configuration file is generated when you use the switch --sampleConfigFile. To use a config file, use the switch --config=MyConfig.yaml
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
  * Re-enabled output format for --fSpec=asciidoc
  * Added depth parameter in API resources call (?depth=<int>). This provides functionality to specify the depth of subinterfaces for which the content must be returned and is especially usefull for recursive (sub)interfaces using the LINKTO statement.
  * Added this release notes file. 
  * Bug fixes: [Issue #413](https://github.com/AmpersandTarski/Ampersand/issues/413) 


## v3.5.0 (28 apr 2016)
This is the first release of 2016. We are happy announce that we have much more possibilities on creating nice interfaces for the prototype application. Apart from that, lots of bugfixes have been made, and some other features have been introduced.

## releases prior to v3.5.0 
We didnt make proper release notes. If you are interested, have a look at the Git history.  
