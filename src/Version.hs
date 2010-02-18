{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-608" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Ge-upgrade naar nieuste packages. Helaas is dit niet backwards compatible...
Packages die problemen gaven:
  Graphviz (nu versie graphviz-2999.8.0.0)
  Pandoc   (nu versie pandoc-1.4)
-}
