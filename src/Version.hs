{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.770" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Enkele functies verwijderd / verplaats naar aanleiding van de plaatjes uit SourceGraph
Het resultaat is te vinden op: han_joosten.users.sourceforge.net/SourceGraph/Main.html 
-}
