{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.723" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
UTF8 support toegevoegd. Ik heb hiermee LaTeX documenten gegenereerd, met echte diacrieten in de PDF! YESSS!
Helaas lijkt er wel iets omgevallen te zijn, ik denk dat dat komt door dubbele labels die worden gegenereerd of door labels met diacrieten er in. Moet ik nog naar kijken. 
Even opletten: Zorg er voor dat je ADL script is opgeslagen als UTF8.
-}
