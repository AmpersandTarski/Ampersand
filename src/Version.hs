{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-403" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
extra feature in Prototype: $obj->isNew() levert een Boolean
Verder: bug opgelost in readObj($objNr) als $objNr wel van het juiste type is, maar toch niet in Obj zit, dwz:
readDocent($studentNr) (wanneer zowel service Docent als Student van concepttype User zijn)
-}
