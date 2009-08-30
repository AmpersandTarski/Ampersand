{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-401" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
ShowXML uitgebreid met Plug informatie. Het blijkt dat showXML niet goed omgaat met '&' en met '"' characters. 
Ik heb OUNL.adl hierom een beetje aangepast. Eigenlijk zou showXML hier uiteindelijk wel tegen moeten kunnen....
-}
