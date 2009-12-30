{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-513" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Alles was laatst vrijwel warningvrij. De unificatie van Ru en Sg tot Ru heeft nog op enkele plaatsen nieuwe warnings gegeven. Stef, wil je die nog even oplossen? Ik neem aan dat je nu nog opperbest in de materie zit. (enkele triviale warnings heb ik al voor je opgelost...)
Verder heb ik typ verwijderd. Die was ook al niet meer nodig...
-}
