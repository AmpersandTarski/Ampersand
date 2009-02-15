{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-252" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Optie aangebracht om de typechecker over te slaan. De bedoeling is om dit weer te slopen zodra de typechecker stabiel is. 
Op deze manier heb ik geen last van onderhanden werk. Uiteraard staat de optie standaard uit...
-}