{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "0.8.10-632" -- the number behind the last digit is the SVN revision number.
{-
SVN Version text:
SQL plugs zijn gerepareerd en het class diagram is nu een correcte afspiegeling van de SQL-plugs.
-}
