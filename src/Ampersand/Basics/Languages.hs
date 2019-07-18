{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields,OverloadedLabels  #-}
-- | This module does some string manipulation based on natural languages
module Ampersand.Basics.Languages
         where
              
import Ampersand.Basics.Prelude
import RIO.Char (toLower)
import qualified RIO.List as L

data Lang = Dutch | English deriving (Show, Eq, Ord,Typeable, Data, Enum, Bounded)

-- | Returns the plural of a given word based on a specific language
plural :: Lang -> String -> String
plural English str =
  case reverse str of
    []   -> str
    'y':cs -> reverse cs<>"ies"
    's':_  -> str<>"es"
    'x':_  -> str<>"es"
    'f':cs -> reverse cs<>"ves"
    _      -> case lookup str exceptions of
                Just a -> a
                Nothing -> str<>"s"
    where exceptions = [("child","children"),("Child","Children"),("mouse","mice"),("Mouse","Mice"),("sheep","sheep"),("Sheep","Sheep")]
plural Dutch str = 
  case str of 
    [] -> str
    h:tl -> case matches of
              m:_ -> m
              []  -> foo
      where 
            foo 
                | take 3 (reverse str)== reverse "ium" = (reverse.drop 3.reverse) str++"ia"
                | take 2 (reverse str) `elem` map reverse ["el", "em", "en", "er", "um", "ie"] = str++"s"
                | "ij" `L.isSuffixOf` str = str++"en"
                | "io" `L.isSuffixOf` str = str++"'s"
                | klinker last = str++"s"
                | (take 2.drop 1.reverse) str `elem` ["aa","oo","ee","uu"] = (reverse.drop 2.reverse) str++mede (drop (length str-1) str)++"en"
                | otherwise                  = str++"en"
            last = case reverse tl of
                     [] -> h
                     c:_ -> c
            mede "f" = "v"
            mede "s" = "z"
            mede x = x
            klinker c = c `elem` "aeiou"
            matches = [(reverse.drop (length s).reverse) str++p |(s,p) <-exceptions, (map toLower.reverse.take (length s).reverse) str==s]
            exceptions = [ ("aanbod", "aanbiedingen")
                        , ("beleg", "belegeringen")
                        , ("dank", "dankbetuigingen")
                        , ("gedrag", "gedragingen")
                        , ("genot", "genietingen")
                        , ("lof", "loftuitingen")
                        , ("lende", "lendenen")
                        , ("onderzoek", "onderzoekingen")
                        , ("archiefstuk", "archiefbescheiden")
                        , ("titel", "titels")
                        , ("plan", "plannen")
                        , ("kind", "kinderen")
                        ]

-- Utility types and functions for handling multiple-language strings

-- If you declare a local function:   l lstr = localize (fsLang fSpec) lstr
-- you can use:  l (NL "Nederlandse tekst", EN "English text")
-- to specify strings in multiple languages.

newtype NLString = NL String
newtype ENString = EN String

type LocalizedStr = (NLString, ENString)

localize :: Lang -> LocalizedStr -> String
localize Dutch   (NL s, _) = s
localize English (_, EN s) = s



