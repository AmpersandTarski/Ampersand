{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Utility types and functions for handling multiple-language strings
module Ampersand.Basics.Languages
    (Lang(..)
    ,LocalizedStr
    ,NLString(..)
    ,ENString(..)
    ,localize
    ,plural
    )
where
              
import Ampersand.Basics.Prelude
import qualified RIO.Text as T

-- | An enumeration of supported natural languages.
data Lang = Dutch | English deriving (Show, Eq, Ord,Typeable, Data, Enum, Bounded)

-- | Returns the plural of a given word based on a specific language
plural :: Lang -> Text -> Text
plural English str =
  case T.uncons $ T.reverse str of
    Nothing    -> str
    Just ('y',cs) -> T.reverse cs<>"ies"
    Just ('s',_ ) -> str<>"es"
    Just ('x',_ ) -> str<>"es"
    Just ('f',cs) -> T.reverse cs<>"ves"
    _      -> case lookup str exceptions of
                Just a -> a
                Nothing -> str<>"s"
    where exceptions = [("child","children"),("Child","Children"),("mouse","mice"),("Mouse","Mice"),("sheep","sheep"),("Sheep","Sheep")]
plural Dutch str = 
  case T.uncons str of 
    Nothing     -> str
    Just (h,tl) -> case matches of
              m:_ -> m
              []  -> foo
      where 
            foo :: Text
            foo 
                | T.take 3 (T.reverse str)== T.reverse "ium" = (T.reverse.T.drop 3.T.reverse) str<>"ia"
                | T.take 2 (T.reverse str) `elem` map T.reverse ["el", "em", "en", "er", "um", "ie"] = str<>"s"
                | "ij" `T.isSuffixOf` str = str<>"en"
                | "io" `T.isSuffixOf` str = str<>"'s"
                | klinker last = str<>"s"
                | (T.take 2.T.drop 1.T.reverse) str `elem` ["aa","oo","ee","uu"] = (T.reverse.T.drop 2.T.reverse) str<>mede (T.drop (T.length str-1) str)<>"en"
                | otherwise                  = str<>"en"
            last = case T.uncons . T.reverse  $ tl of
                     Nothing -> h
                     Just (c,_) -> c
            mede "f" = "v"
            mede "s" = "z"
            mede x = x
            klinker :: Char -> Bool
            klinker c = c `elem` ['a','e','i','o','u']
            matches = [ (T.reverse.T.drop (T.length s).T.reverse) str<>p |(s,p) <-exceptions
                      , (T.toLower.T.reverse.T.take (T.length s).T.reverse) str==s
                      ]
            exceptions :: [(Text,Text)]
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


-- If you declare a local function:   l lstr = localize (outputLang env fSpec) lstr
-- you can use:  l (NL "Nederlandse tekst", EN "English text")
-- to specify strings in multiple languages.

-- | Dutch text
newtype NLString = NL Text
-- | English text
newtype ENString = EN Text

-- | Type of text containing all localized variations
type LocalizedStr = (NLString, ENString)

-- | Select the correct text based on the chosen language
localize :: Lang -> LocalizedStr -> Text
localize Dutch   (NL s, _) = s
localize English (_, EN s) = s
