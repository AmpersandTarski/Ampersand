{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-440" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
target directories een beetje aangepast in Options. Nu hoef je alleen nog maar een algemene prototype directory aan te geven, en dat kan in een environment variabele. 
-}
