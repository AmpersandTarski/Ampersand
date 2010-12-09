{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.778" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Syntax aanpassingen, zodat de meeste code ook kan worden geparsed door haskell-src-exts. Dit is nodig om plaatjes te genereren. 
Zie voor een analyse van onze eigenste sourcecode:
http://han_joosten.users.sourceforge.net/SourceGraph/Ampersand.html
-}
