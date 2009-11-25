{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-465" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Embedding pictures in prototype.
Add instruction "PICTURE" to SERVICE label containing picture links.
SERVICE Xxx : I[Something]
  = [myLabel {"PICTURE"}: picture[Something*PictureLink] ] -}
