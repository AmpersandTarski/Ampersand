  {-# OPTIONS_GHC -Wall #-}
  -- | Deze module bevat operaties op strings.
  module Strings (chain,unCap,firstCaps,remSpaces,spaces)
  where

   import Char (toUpper, toLower)

   -- | firstCaps verwijdert spaties uit een string, en maakt van elk eerste letter van een woord een hoofdletter.
   firstCaps :: String -> String
   firstCaps "" = ""
   firstCaps "_" = ""
   firstCaps ('_':'_':str) = firstCaps ('_':str)
   firstCaps ('_':c:str) = toUpper c:firstCaps str
   firstCaps (c:str) = c:firstCaps str

   unCap :: String -> String
   unCap [] = [] ; unCap (h:t) = toLower h:t
   

   chain :: [b] -> [[b]] -> [b]
   chain _ [] = []
   chain str xs = foldl f (head xs) (tail xs) where f x y = x++str++y

   remSpaces :: String -> String
   remSpaces [] = []
   remSpaces (' ':c:str) = toUpper c:remSpaces str 
   remSpaces xs = xs

   spaces :: Int -> String
   spaces x 
      | x < 1     = ""
      | otherwise = " "++spaces (x - 1)
