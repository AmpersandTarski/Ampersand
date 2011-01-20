{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "Ampersand vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.823" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Begin gemaakt van schuifacties met modules. Graag even niet te veel code verplaatsen. Dat heb ik onderhanden. Ik zal de komende tijd nog wel meer verplaatsacties doen. 
-}
