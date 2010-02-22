{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "0.8.10-612" -- the number behind the last digit is the SVN revision number.
{-
SVN Version text:
Hersteld: 
 + prototypes navigeren op /(name fSpec).php?content=(name o) [php functie serviceref()]
 + session_start()

foutmelding in type checker als concept niet bestaat

wat gespeel met sqlRelPlugs
-}
