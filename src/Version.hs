{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-557" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Tex header optie default op Nothing, zodat je niet gedwongen wordt een PATH variabele te zetten.
Added instructions to pdflatex command instead of changing directory.
-}
