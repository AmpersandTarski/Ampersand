-- | This module contains some common Text funcions
module Ampersand.Basics.String
  ( unCap,
    upCap,
    urlEncode,
    escapeIdentifier,
    escapeLatex,
    isSafeIdChar,
    toLatexVariable,
    optionalQuote,
    mapText,
    toBaseFileName,
    toText1Unsafe,
    text1ToText,
    camel,
    pascal,
  )
where

import Ampersand.Basics.Prelude
import Ampersand.Basics.Version (fatal)
import qualified Network.URI.Encode as URI
import RIO.Char
import qualified RIO.Text as T
import qualified Text.Casing as Casing

-- | Converts the first character of a string to lowercase, with the exception that there is a second character, which is uppercase.
-- uncap "AbcDe" == "abcDe"
-- uncap "ABcDE" == "ABcDE"
unCap :: Text -> Text
unCap txt = case T.uncons txt of
  Nothing -> mempty
  Just (h, tl) -> case T.uncons tl of
    Nothing -> T.toLower . T.singleton $ h
    Just (h', tl')
      | isUpper h' -> T.cons h (T.cons h' tl')
      | otherwise -> T.cons (toLower h) (T.cons h' tl')

-- | Converts the first character of a Text to uppercase
upCap :: Text -> Text
upCap txt = case T.uncons txt of
  Nothing -> mempty
  Just (h, tl) -> T.cons (toUpper h) tl

-- | escape anything except regular characters and digits to _<character code>
-- e.g. urlEncodedName "a_é" = "a_95_233"
urlEncode :: Text -> Text
urlEncode = URI.encodeText

-- urlEncode txt = case T.uncons txt of
--  Nothing -> mempty
--  Just (h, tl)
--    | isAlphaNum h && isAscii h -> T.singleton h <> urlEncode tl
--    | otherwise -> T.cons '_' (tshow (ord h)) <> urlEncode tl

-- | Make sure that the text can safely be used in LaTeX
escapeLatex :: Text -> Text
escapeLatex txt = case T.uncons txt of
  Nothing -> mempty
  Just (h, tl)
    | isAlphaNum h -> T.singleton h <> escapeLatex tl
    | otherwise -> T.cons '_' (tshow (ord h)) <> escapeLatex tl

-- | Make sure that a text can be used safely as a Latex variable.
toLatexVariable :: Text -> Text
toLatexVariable txt =
  case T.uncons txt of
    Nothing -> mempty
    Just (h, tl)
      | h `elem` specialLaTeXChars -> "\\" <> T.singleton h <> toLatexVariable tl
      | otherwise -> T.singleton h <> toLatexVariable tl
  where
    specialLaTeXChars :: [Char] -- Based on https://tex.stackexchange.com/questions/34580/escape-character-in-latex
    specialLaTeXChars = "&%$#_{}~^\\"

-- Create an identifier that does not start with a digit and consists only of upper/lowercase ascii letters, underscores, and digits.
-- This function is injective.
escapeIdentifier :: Text1 -> Text1
escapeIdentifier (Text1 c0 cs) =
  case T.uncons cs of
    Nothing -> encode False c0
    Just (h, tl) -> encode False c0 <> mapText (encode True) (Text1 h tl)
  where
    encode :: Bool -> Char -> Text1
    encode allowNum c
      | isAsciiLower c || isAsciiUpper c || allowNum && isDigit c = Text1 c mempty
      | c == '_' = toText1Unsafe "__" -- shorthand for '_' to improve readability
      | otherwise = Text1 'Ð' $ tshow (ord c) <> "Ð"

-- | Tells if a character is valid as character in an identifier. Because there are
--   different rules for the first character of an identifier and the rest of the
--   characters of an identifier, a boolean is required that tells if this is the
--   first character.
isSafeIdChar :: Bool -> Char -> Bool
isSafeIdChar isFirst c = isLower c || isUpper c || (not isFirst && isAlphaNum c)

toText1Unsafe :: Text -> Text1
toText1Unsafe txt = case T.uncons txt of
  Nothing -> fatal "toText1Unsafe must not be used unless you are certain that it is safe!"
  Just (h, tl) -> Text1 h tl

text1ToText :: Text1 -> Text
text1ToText (Text1 h tl) = T.cons h tl

-- | convenient function like map on String
mapText :: (Char -> Text1) -> Text1 -> Text1
mapText fun (Text1 h tl) =
  case T.uncons tl of
    Nothing -> fun h
    Just (h', tl') -> fun h <> mapText fun (Text1 h' tl')

optionalQuote :: Text -> Text
optionalQuote str
  | needsQuotes = tshow str
  | otherwise = str
  where
    needsQuotes =
      case T.words str of
        [] -> True
        [_] -> False
        _ -> True

-- | This function tries to create a valid filename based on a given text.
--   see https://stackoverflow.com/questions/1976007/what-characters-are-forbidden-in-windows-and-linux-directory-names
toBaseFileName :: Text -> FilePath
toBaseFileName txt = concatMap convertChar $ T.unpack txt
  where
    convertChar :: Char -> [Char]
    convertChar c
      | isSpace c = ['_']
      | c `elem` ['<', '>', ':', '\"', '/', '\\', '|', '?', '*'] = '%' : show (ord c)
      | otherwise = [c]

camel :: Text -> Text
camel = T.pack . Casing.camel . T.unpack

pascal :: Text -> Text
pascal = T.pack . Casing.pascal . T.unpack
