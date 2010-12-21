{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.791" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Ik ben aan het puzzelen geweest, maar ik kom er niet uit. In het mapje ADL2 heb ik enkele modules gezet, met daarin wat vingeroefeningen. Helaas is het allemaal nog niet erg consistent. Hulp is welkom.... 
-}
