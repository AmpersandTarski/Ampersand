{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.669" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
ampersand.cabal toegevoegd. Er zijn nog wel de nodige besluiten te nemen over de inhoudelijke invulling. Daar moeten Stef en ik het nog maar eens over hebben met elkaar. 
-}
