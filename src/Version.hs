{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "0.8.10-619" -- the number behind the last digit is the SVN revision number.
{-
SVN Version text:
ADL myScript.adl -f latex   werkt weer, maar nu met PanDoc 1.4 (en dus met het template mechanisme van PanDoc)
-}
