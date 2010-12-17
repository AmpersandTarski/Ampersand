{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.784" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
PlugSQL opgesplitst in TblSQL,BinSQL en ScalarSQL.
Kleine aanpassingen en commentaar op verschillende plekken (grep '151210' *.hs)
Functies m.b.t. plug verhuist naar Data.ADL2Plug of Data.Plug.hs.
-}
