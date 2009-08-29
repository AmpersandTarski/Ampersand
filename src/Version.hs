{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-397" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Output naar bestanden meer portable gemaakt. Mogelijkheden van Options toegepast voor generatie van prototype. dbName moet nu nog worden gebruikt. 
ook nieuwe option: --test in combinatie met --verbose.
-}
