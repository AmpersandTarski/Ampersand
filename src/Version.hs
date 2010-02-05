{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-588" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Type checker doorgekamd en moet correcte types/errors geven. Nu ga ik de errormeldingen nog uitbreiden met nuttige informatie.
-}
