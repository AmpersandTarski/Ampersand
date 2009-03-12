{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-277" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Fouten hersteld waardoor het parsen verkeerd leek te gaan. Fouten werd geïntroduceerd door het opschonen van code...
Gelukkig hebben we subclipse achter de hand... 
Herschrijven blijft preciesiewerk.... Bummer!
-}