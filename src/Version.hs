{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.716" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Plug datatypes zijn opgesplitst in PlugPHP en PlugSQL, als je beide wilt, kun je ze combineren met makePlug, en [Plug] kun je omzetten naar [PlugPHP] of [PlugSQL] via pickTypedPlug
-}
