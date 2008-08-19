
> module Strings where

  This module is ment for all kind of string handling functionality

>  import Char (toUpper, toLower)
>  -- import CommonClasses(Collection(rd))  

>  firstCaps :: String -> String
>  firstCaps "" = ""
>  firstCaps "_" = ""
>  firstCaps ('_':'_':str) = firstCaps ('_':str)
>  firstCaps ('_':c:str) = toUpper c:firstCaps str
>  firstCaps (c:str) = c:firstCaps str

>  tt :: String -> String
>  tt a = "{\\tt "++a++"}"


>  idNam :: String -> String
>  idNam c = "\\id{"++concat [if c `elem` [' ','_'] then "\\ " else [c]| c<-firstCaps c]++"}"

  