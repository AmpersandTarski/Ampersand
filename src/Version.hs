{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-545" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Mp1 (de 'Atom-notatie') in de scanner en de parser verwerkt. Gerard, wil jij de typechecker bijwerken?
Het voorbeeld ADLtool.adl bevat enkele voorbeelden.
-}
