-- | This module does some string manipulation based on natural languages
module Ampersand.Misc.Languages
         ( Lang(English,Dutch)
         , allLangs
         , plural
         ) where
              
import Data.Char (toLower)
import Data.List (isSuffixOf)
import Ampersand.Core.ParseTree      (Lang(..))

allLangs :: [Lang]
allLangs = [Dutch,English] -- All supported natural languages in Ampersand

-- | Returns the plural of a given word based on a specific language
plural :: Lang -> String -> String
plural English str
  | null str = str
  | last str=='y' = init str++"ies"
  | last str=='s' = str++"es"
  | last str=='x' = str++"es"
  | last str=='f' = init str++"ves"
  | otherwise     = head ([p |(s,p)<-exceptions, s==str]++[str++"s"])
  where exceptions = [("child","children"),("Child","Children"),("mouse","mice"),("Mouse","Mice"),("sheep","sheep"),("Sheep","Sheep")]
plural Dutch str
  | null str = str
  | not (null matches)    = head matches
  | take 3 (reverse str)== reverse "ium" = (reverse.drop 3.reverse) str++"ia"
  | take 2 (reverse str) `elem` map reverse ["el", "em", "en", "er", "um", "ie"] = str++"s"
  | "ij" `isSuffixOf` str = str++"en"
  | "io" `isSuffixOf` str = str++"'s"
  | klinker (last str)    = str++"s"
  | (take 2.drop 1.reverse) str `elem` ["aa","oo","ee","uu"] = (reverse.drop 2.reverse) str++mede (drop (length str-1) str)++"en"
  | otherwise                  = str++"en"
  where mede "f" = "v"
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
