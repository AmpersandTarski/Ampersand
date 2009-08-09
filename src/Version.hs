{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-366" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Crowfoot notation enabled. However, the current implementation of the Graphviz library
does not support the combinators for arrowheads. For the time being, I just use different
symbols for different cardinalities. Whenever the Graphviz library will support the needed arrowheads,
it is very easy to use these symbols. 
-}
