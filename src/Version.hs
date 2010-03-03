{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "0.8.10-625" -- the number behind the last digit is the SVN revision number.
{-
SVN Version text:
added theme option to differentiate in style and content etc. implemented "student" theme for func. spec.
-}
