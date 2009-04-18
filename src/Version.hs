{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-295" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Imports en Exports van modules expliciet gemaakt. Dit voorkomt vervuiling. 
'loshangende' modules op zijspoor gezet.
-}