{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1-668" -- #1.#2-#3 : #1 major version; #2 student release version; #3 SVN revision number.
{-
SVN Version text:
Upgrade naar nieuwe Graphviz versie. We zijn nu afhankelijk van: 
graphviz  >= 2999.9.0.0,  hdbc-odbc >= 2.2.3.0  ,  pandoc    >= 1.5.1.1
-}
