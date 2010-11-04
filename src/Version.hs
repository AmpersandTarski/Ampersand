{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.765" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Added command line option: blackWhite  (on special request of Stef) @Stef: Voor zover ik zo in de gouwigheid zie, worden er nu nooit kleurtjes gebruikt in de graphics. Was dit de bedoeling???)
-}
