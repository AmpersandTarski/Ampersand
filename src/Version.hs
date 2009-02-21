{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-262" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Bug uit de Equal van ConceptDef gehaald. Het is echter niet de laatste, want ik heb nog steeds een loop...
Maar toch is dit een mooi voorbeeld van dat het nuttig is om warnings ALLEMAAL te bekijken...
-}