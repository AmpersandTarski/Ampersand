  {-# OPTIONS_GHC -Wall #-}
  -- | This module contains some common String funcions
  module DatabaseDesign.Ampersand.Basics.String
   (unCap,upCap,escapeNonAlphaNum)
  where
   import Data.Char 

   -- | Converts the first character of a string to lowercase, with the exception that there is a second character, which is uppercase. 
   -- uncap "AbcDe" == "abcDe"
   -- uncap "ABcDE" == "ABcDE"  
   unCap :: String -> String
   unCap [] = []
   unCap [h] = [toLower h]
   unCap (h:h':t) | isUpper h' = h:h':t
                  | otherwise  = toLower h:h':t
   -- | Converts the first character of a string to uppercase
   upCap :: String -> String
   upCap [] = []  
   upCap (h:t) = toUpper h:t

   
   -- | escape anything except regular characters and digits to _<character code>
   -- e.g. escapeNonAlphaNum "a_é" = "a_95_233"
   escapeNonAlphaNum :: String -> String
   escapeNonAlphaNum = concatMap escapeNonAlphaNumChar
    where escapeNonAlphaNumChar c 
            | isAlphaNum c && isAscii c = [c]
            | otherwise                 = '_' : show (ord c)
   