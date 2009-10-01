{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-429" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Implemented Fspec{violations} and enabled printing of violations when generating a prototype. All typed morphisms in the context are populated in the typechecker. 
-}
