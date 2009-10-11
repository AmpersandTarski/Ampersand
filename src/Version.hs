{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-441" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Ik heb nieuwe plugs gedefinieerd, maar nog niet geactiveerd. Bas, even kijken in ADL2Fspec.hs.
Vlak boven de definitie van "allplugs" heb ik een definitie van "newplugs" neergezet.
Als ik newplugs gebruik in plaats van allplugs (allplugs wordt oldplugs en newplugs wordt allplugs...)
dan krijg ik rommel in de database.
-}
