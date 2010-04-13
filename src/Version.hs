{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1-653" -- #1.#2-#3 : #1 major version; #2 student release version; #3 SVN revision number.
{-
SVN Version text:
ShowHS zo goed mogelijk neergezet. Er ontbreken nog de berekende signaaldeclaraties in de Fspc. Klopt dat, Stef?
-}
