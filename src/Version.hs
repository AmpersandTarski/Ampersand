{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.780" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
mLkpTbl bepaalt (plug,fld0,fld1) voor (sqlRelPlug Mph{})
of (fldnull fld0) waar is of niet, zou niet uit moeten maken
-}
