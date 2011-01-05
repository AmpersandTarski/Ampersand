{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.811" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
sqlPlugFields -> plug -> expr -> [(sqlfld,sqlfld)] in Prototype.RelBinGenSql
based on
plugpath -> plug -> sqlfield -> sqlfield -> expr in Data.Plug
-}
