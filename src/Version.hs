{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-246" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
{-# OPTIONS_GHC -Wall #-} toegevoegd in bepaalde modules. Deze optie zet voor het desbetreffende file alle waarschuwingen aan. Het idee is om zoveel mogelijk waarschuwingen te voorkomen. 
Modules waarin deze optie nu aanstaat, zijn momenteel 'overtredingsvrij'!
-}