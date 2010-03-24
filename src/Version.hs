{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1-646" -- #1.#2-#3 : #1 major version; #2 student release version; #3 SVN revision number.
{-
SVN Version text:
PandocAux toegevoegd voor Pandoc gerelateerde functies die gedeeld kunnen worden door meerdere pandoc documenten.
Type inferentie bewijs toegevoegd aan Rule en ObjectDef. Het bewijs voor een type fout is nog niet toegevoegd, alleen het bewijs voor het type dat geinfereerd is. In InfTree2Pandoc moeten functies komen die van een bewijs een Pandoc iets maken.
-}
