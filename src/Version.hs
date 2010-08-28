{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.724" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
UTF8 doet het nu voor LaTeX. De PDF generatie hobbelt nu door, als je er maar voor zorgt dat je input bestand UTF8 is. (In notepad++ kan je dat gemakkelijk controleren.)
-}
