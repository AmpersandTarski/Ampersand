{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-493" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Added a DISPLAY directive on services to be able to use an id for links, and display a user-friendly representation via a relation function.
-}
