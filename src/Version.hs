{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1-663" -- #1.#2-#3 : #1 major version; #2 student release version; #3 SVN revision number.
{-
SVN Version text:
ShowHS doet het nu ook, onder voorwaarde dat de signaal-regels mee worden gegenereerd in ADL2Fspec.hs. Dat moet nog even gebeuren.
-}
