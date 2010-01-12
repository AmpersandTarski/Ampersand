{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-537" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Bug die Rieks had gevonden verholpen. had te maken met ten onrechte gebruik van mphyin in plaats van inline. inline is een veiliger functie, omdat die volledig is. mphyin is geen functie: hij is niet gedefinieerd op elke morphisme. 
Stef, wil jij nog eens goed kijken naar het attribuut mphyin bij I{} ? ofwel netjes gaan gebruiken, ofwel verwijderen uit de datastructuur...
-}
