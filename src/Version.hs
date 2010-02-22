{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "0.8.10-613" -- the number behind the last digit is the SVN revision number.
{-
SVN Version text:
op verzoek van Rieks:
environment variabelen "CCPreVersion" en "CCPostVersion" worden getoond bij --version optie, als ze tenminste bestaan....
-}
