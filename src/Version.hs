{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-428" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Population completely linked under the set of declarations by typechecker. Relation population was already linked, now source(R) and target(R) (not domain and range) are added to declaration of R.
Small change to installer.hs because some binary plugs (for R) have field expressions I[A] and R[A*B]. Contents of I[A] were added to binary plug too. 
-}
