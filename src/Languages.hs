{-# LINE 1 "Languages.lhs" #-}
#line 1 "Languages.lhs"


  module Languages
              (  Lang(English,Dutch)
               , ShowLang(showLang)
               , plural
              ) where
   import Char (toLower)

   data Lang = Dutch | English deriving (Show, Eq)
   class ShowLang a where
    showLang :: Lang -> a -> String



   plural English str
    | null str = str
    | last str=='y' = init str++"ies"
    | last str=='s' = str++"es"
    | last str=='x' = str++"es"
    | last str=='f' = init str++"ves"
    | otherwise     = head ([p|(s,p) <-exceptions, s==str]++[str++"s"])
    where exceptions = [("mouse","mice"),("sheep","sheep"),("Mouse","Mice"),("Sheep","Sheep")]
   plural Dutch str
    | null str = str
    | not (null matches)         = head (matches++[str++"en"])
    | take 2 (reverse str)=="ei" = str++"s"
    | take 2 (reverse str)=="ji" = str++"en"
    | take 2 (reverse str)=="oi" = str++"'s"
    | last str `elem` "aeiou" = str++"s"
    | (take 2.drop 1.reverse) str `elem` ["aa","oo","ee","uu"] = (reverse.drop 2.reverse) str++mede (drop (length str-1) str)++"en"
    | otherwise                  = str++"en"
    where mede "f" = "v"
          mede "s" = "z"
          mede x = x
          matches = [(reverse.drop (length s).reverse) str++p|(s,p) <-exceptions, (map toLower.reverse.take (length s).reverse) str==s]
          exceptions = [ ("aanbod", "aanbiedingen")
                       , ("beleg", "belegeringen")
                       , ("dank", "dankbetuigingen")
                       , ("gedrag", "gedragingen")
                       , ("genot", "genietingen")
                       , ("lof", "loftuitingen")
                       , ("rede", "redenen")
                       , ("lende", "lendenen")
                       , ("onderzoek", "onderzoekingen")
                       , ("archiefstuk", "archiefbescheiden")
                       , ("titel", "titels")
                       ]
