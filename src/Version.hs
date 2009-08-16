{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-375" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Kleine bug verwijderd uit Options.hs.
Het syptoom was: als er geen output redirection plaatsvindt, maar de huidige directory-string bevat per ongeluk een spatie, dan ging het hartstikke fout omdat de string met de huidige directory als twee argumenten wordt opgevat.
Oplossing was: als er geen output redirection plaatsvindt, dan wordt (in regel 75 van Options.hs) dirOutput = ""    (in plaats van dirOutput = currdir)
-}
