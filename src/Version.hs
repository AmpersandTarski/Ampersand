{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "0.8.10-615" -- the number behind the last digit is the SVN revision number.
{-
SVN Version text:
enkele bugs en verbeteringen
-}
