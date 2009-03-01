{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-271" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
NormalForms ontdaan van alle warnings. Tevens opschoning gehouden van dode code. Ruimt lekker op.
Start gemaakt met ombouwen van Graphics. Ik gebruik daar nu het graphviz package.
-}