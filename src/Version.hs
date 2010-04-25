{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1-660" -- #1.#2-#3 : #1 major version; #2 student release version; #3 SVN revision number.
{-
SVN Version text:
Wat zitten klooien met de URL voor het .map bestand. Ik heb redelijke hoop dat de plaatjes nu clikable zijn, maar kan dit zelf niet makkelijk testen. 
-}
