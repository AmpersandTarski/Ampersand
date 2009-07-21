{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-355" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
brede tabellen worden gebruikt. Maar let op! Standaard worden numerieke id's verwacht.. Onverwacht gedrag dat optreed: je krijgt getallen (standaard 0) te zien waar je teksten verwacht. Bovendien komt ook de populatie nog niet mee
-}
