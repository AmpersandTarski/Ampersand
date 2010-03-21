{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1-645" -- #1.#2-#3 : #1 major version; #2 student release version; #3 SVN revision number.
{-
SVN Version text:
Fouten in de codegenerator verholpen...
en het genereren van code vermijden wanneer blokkade te voorspellen is (preEmpt in ADL2Fspec).
-}
