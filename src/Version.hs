{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "Ampersand vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.818" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
SJ: Types have been parameterized.
For example, Expression is now of type (Expression (Relation Concept)).
The reason for doing this is to force ourselves to keep the key algorithms generic.
For instance, normalization should work for Expression r, and not be specific for (Expression (Relation Concept)).
-}
