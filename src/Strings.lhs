
> module Strings where

  This module is ment for all kind of string handling functionality

>  import Char (toUpper)

>  firstCaps :: String -> String
>  firstCaps "" = ""
>  firstCaps "_" = ""
>  firstCaps ('_':'_':str) = firstCaps ('_':str)
>  firstCaps ('_':c:str) = toUpper c:firstCaps str
>  firstCaps (c:str) = c:firstCaps str

>  tt :: String -> String
>  tt a = "{\\tt "++a++"}"

>  chain :: String -> [String] -> String
>  chain str [] = []
>  chain str xs = foldl f (head xs) (tail xs) where f x y = x++str++y


>  idNam :: String -> String
>  idNam c = "\\id{"++concat [if c `elem` [' ','_'] then "\\ " else [c]| c<-firstCaps c]++"}"

  