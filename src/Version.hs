{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-427" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
added field to Fspc containing violations in the population (to be implemented)
-}
