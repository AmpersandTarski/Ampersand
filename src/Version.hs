{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1-644" -- #1.#2-#3 : #1 major version; #2 student release version; #3 SVN revision number.
{-
SVN Version text:
door de gebruiker gespecificeerde properties op declaraties zijn constant
decprps_calc i.e. multiplicities bevatten alle properties
-}
