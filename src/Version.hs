{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-595" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
De plug van expressie I[X] tbv (SELECT .. FROM PlugX) kan alleen plug op X (source van plug) zijn en niet meer een plug met een attribuut van type X (target van Plug veld)
-}
