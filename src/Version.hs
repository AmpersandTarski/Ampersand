{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-398" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Her en der aanpassingen gemaakt dat er meer gebruik gemaakt wordt van mogelijkheden van opties. 
Het zou nu mogelijk moeten zijn om de naam van de gegenereerde executable te wijzigen
Je kan nu ook een specifieke database naam opgeven voor gebruik van de prototype. 
Ook her en der wat opgeruimd.
-}
