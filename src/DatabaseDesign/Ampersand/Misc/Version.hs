{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Misc.Version (ampersandCoreVersionBanner,versionNumber)
where

ampersandCoreVersionBanner :: String
ampersandCoreVersionBanner = "Ampersand vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.2.0.863" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
import files as population
new format .pop-files contain only POPULATION blocks

cmd: adl --importfile=xxx --importformat=pop yyy
 -> the populations in xxx.pop overwrite the POPULATION blocks of the first CONTEXT in yyy.adl

cmd: adl --importfile=xxx --importformat=adl yyy
 -> xxx.adl as data Fspc will be transformed as if it were POPULATION blocks in the first context in yyy.adl
(yyy.adl is likely to be atlas.adl)

-}
