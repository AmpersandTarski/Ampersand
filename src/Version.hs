{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.737" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
* Foutje verholpen in ampersand.cabl , met betrekking tot gebruik van UTF8
*AGPL- license toegevoed.
* class-diagram in hoofdstuk 4 beter passend gemaakt. 
-}
