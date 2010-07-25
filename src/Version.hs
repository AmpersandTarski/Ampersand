{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.682" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Minor detais for explanations have been changed further.
Gerard, kun jij de typering op explanations laten werken?
 - Alleen explanations toestaan waar definities bij horen.
 - Alle explanations waar een morfisme in zit correct invullen.
-}
