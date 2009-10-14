{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-444" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Fout in de codegenerator provisorisch hersteld. Het gaat hier om unieke attribuutnamen van plugs.
Dat is nu niet gegarandeerd en dus heb ik er een rd omheen gezet zodat ze in elk geval uniek zijn.
Het werkt... zonder dat ik weet waarom (oei..)-}
