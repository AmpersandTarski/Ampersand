{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-522" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
ShowADL verbeterd.
Ook onderscheid tussen "declarations" en "decls" aangebracht. De functie "declarations" laat de gedeclareerde relaties zien
en "decls" laat de gebruikte relaties zien.
-}
