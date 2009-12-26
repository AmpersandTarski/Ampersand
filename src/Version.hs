{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-508" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Fr constructor systematisch verwijderd uit alles. Deze was geheel functieloos. (Kennelijk bedoeld voor toekomstige functionaliteit. Dan komt hij tzt wel weer terug)
-}
