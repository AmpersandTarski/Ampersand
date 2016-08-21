  -- | This module contains some common String funcions
module Ampersand.Basics.String (unCap,upCap,escapeNonAlphaNum,escapeIdentifier) where

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

-- Create an identifier that does not start with a digit and consists only of upper/lowercase ascii letters, underscores, and digits.
-- This function is injective.
escapeIdentifier :: String -> String
escapeIdentifier ""      = "_EMPTY_"
escapeIdentifier (c0:cs) = encode False c0 ++ concatMap (encode True) cs
  where encode allowNum c | isAsciiLower c || isAsciiUpper c || allowNum && isDigit c = [c]
                          | c == '_'  = "__" -- shorthand for '_' to improve readability
                          | otherwise = "_" ++ show (ord c) ++ "_"
