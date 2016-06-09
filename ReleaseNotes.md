# Release notes of Ampersand

## unreleased changes 
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
