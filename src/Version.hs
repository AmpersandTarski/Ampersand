{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.753" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Free texts (for use in EXPLAINATION etc.) now can be parsed in various formats. (use --help for info on the option --freeTextFormat)
   :`freeTextFormat`: as prefix of some free text overrules the default value.
-}
