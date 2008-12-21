  -- | Deze module bevat operaties op strings.
  module Strings where

   import Char (toUpper, toLower)

   -- | firstCaps verwijdert spaties uit een string, en maakt van elk eerste letter van een woord een hoofdletter.
   firstCaps :: String -> String
   firstCaps "" = ""
   firstCaps "_" = ""
   firstCaps ('_':'_':str) = firstCaps ('_':str)
   firstCaps ('_':c:str) = toUpper c:firstCaps str
   firstCaps (c:str) = c:firstCaps str

   unCap [] = [] ; unCap (h:t) = toLower h:t
   upCap [] = [] ; upCap (h:t) = toUpper h:t


   chain :: String -> [String] -> String
   chain str [] = []
   chain str xs = foldl f (head xs) (tail xs) where f x y = x++str++y


   idNam :: String -> String
   idNam c = "\\id{"++concat [if c `elem` [' ','_'] then "\\ " else [c]| c<-firstCaps c]++"}"


