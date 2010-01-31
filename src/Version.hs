{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-568" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Kleine opruimacties en herstel van gedrag bij gebruik van --help en --version flags.
-}
