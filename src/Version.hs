{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner,versionNumber)
where

versionbanner :: String
versionbanner = "ADL vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.713" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Explanations worden nu geparsed als "restructured text". (http://docutils.sourceforge.net/docs/user/rst/quickref.html) 
In het Viro script van Stef moeten er nu nog wel weer de explainations bij langs, om dat recht te zetten. Maar dan heb je ook wat moois!
-}
