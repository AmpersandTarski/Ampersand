{-# LANGUAGE OverloadedStrings #-}
-- | This module contains some common Text funcions
module Ampersand.Basics.String 
        ( unCap,upCap
        , urlEncodedName
        , escapeIdentifier
        , escapeLatex
        , toLatexVariable
        , optionalQuote
        , mapText
        , toBaseFileName
        ) where

import           Ampersand.Basics.Prelude
import           RIO.Char
import qualified RIO.Text as T
-- | Converts the first character of a string to lowercase, with the exception that there is a second character, which is uppercase.
-- uncap "AbcDe" == "abcDe"
-- uncap "ABcDE" == "ABcDE"
unCap :: Text -> Text
unCap txt = case T.uncons txt of
  Nothing -> mempty
  Just (h,tl) -> case T.uncons tl of
    Nothing -> T.toLower . T.singleton $ h
    Just (h',tl')
      | isUpper h' -> T.cons h (T.cons h' tl')
      | otherwise  -> T.cons (toLower h) (T.cons h' tl')
-- | Converts the first character of a Text to uppercase
upCap :: Text -> Text
upCap txt = case T.uncons txt of
  Nothing -> mempty
  Just (h,tl) -> T.cons (toUpper h) tl

-- | escape anything except regular characters and digits to _<character code>
-- e.g. urlEncodedName "a_é" = "a_95_233"
urlEncodedName :: Text -> Text
urlEncodedName txt = case T.uncons txt of
  Nothing -> mempty
  Just (h,tl)
    | isAlphaNum h && isAscii h -> T.singleton h              <> urlEncodedName tl
    | otherwise                 -> T.cons '_' (tshow (ord h)) <> urlEncodedName tl
 
-- | Make sure that the text can safely be used in LaTeX
escapeLatex :: Text -> Text
escapeLatex txt = case T.uncons txt of
  Nothing -> mempty
  Just (h,tl)
    | isAlphaNum h -> T.singleton h              <> escapeLatex tl
    | otherwise    -> T.cons '_' (tshow (ord h)) <> escapeLatex tl
-- | Make sure that a text can be used safely as a Latex variable.
toLatexVariable :: Text -> Text
toLatexVariable txt = 
   case T.uncons txt of
     Nothing -> mempty
     Just (h,tl) 
      | h `elem` specialLaTeXChars -> "\\"<> T.singleton h <> toLatexVariable tl
      | otherwise                  ->        T.singleton h <> toLatexVariable tl
   where specialLaTeXChars :: [Char] -- Based on https://tex.stackexchange.com/questions/34580/escape-character-in-latex 
         specialLaTeXChars = "&%$#_{}~^\\" 
-- Create an identifier that does not start with a digit and consists only of upper/lowercase ascii letters, underscores, and digits.
-- This function is injective.
escapeIdentifier :: Text -> Text
escapeIdentifier txt = case T.uncons txt of
  Nothing -> "_EMPTY_"
  Just (c0,cs) -> encode False c0 <> mapText (encode True) cs
  where encode :: Bool -> Char -> Text
        encode allowNum c | isAsciiLower c || isAsciiUpper c || allowNum && isDigit c = T.singleton c
                          | c == '_'  = "__" -- shorthand for '_' to improve readability
                          | otherwise = "_" <> tshow (ord c) <> "_"

-- | convenient function like map on String
mapText :: (Char -> Text) -> Text -> Text
mapText fun txt = case T.uncons txt of
  Nothing -> mempty
  Just (h,tl) -> fun h <> mapText fun tl
optionalQuote :: Text -> Text
optionalQuote str
  | needsQuotes = tshow str
  | otherwise   = str 
 where
  needsQuotes =
   case T.words str of
    []  -> True
    [_] -> False
    _   -> True

-- | This function tries to create a valid filename based on a given text. 
--   see https://stackoverflow.com/questions/1976007/what-characters-are-forbidden-in-windows-and-linux-directory-names
toBaseFileName :: Text -> FilePath
toBaseFileName txt = concatMap convertChar $ T.unpack txt
  where convertChar :: Char -> [Char]
        convertChar c
          | isSpace c = ['_']
          | c `elem` ['<','>',':','\"','/','\\','|','?','*'] = '%': show (ord c)
          | otherwise = [c]