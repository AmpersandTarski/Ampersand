{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.732" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Kleine foutjes uit hoofdstuk 2 opgelost:
  * (EXPLAIN PATTERN werkt nu)
  * Alleen user-defined rules worden nog getoond in hoofdstuk 2
  * verbeteringen aan XML output
  
-}
