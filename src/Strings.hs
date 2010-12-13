  {-# OPTIONS_GHC -Wall #-}
  -- | Deze module bevat operaties op strings.
  module Strings (unCap,upCap,trim,spacesToUnderscores,spaces,commaEng,commaNL,preciesEen)
  where
   import Char (isUpper, toUpper, toLower)
   import Char(chr)

   unCap :: String -> String
   unCap [] = []
   unCap [h] = [toLower h]
   unCap (h:h':t) | isUpper h' = (h:h':t)
                  | otherwise  = toLower h:h':t

   upCap :: String -> String
   upCap [] = [] ; upCap (h:t) = toUpper h:t

   trim      :: String -> String
   trim      = f . f
     where f = reverse . dropWhile (' '==)

   spacesToUnderscores :: String -> String
   spacesToUnderscores  = map translate 
      where translate :: Char -> Char
            translate ' ' = '_'
            translate c = c
            
   spaces :: Int -> String
   spaces x = replicate x ' '

   commaEng :: String -> [String] -> String
   commaEng str [a,b,c]= a++", "++b++", "++str++" "++c
   commaEng str [a,b]  = a++" "++str++" "++b
   commaEng _ [a]    = a
   commaEng str (a:as) = a++", "++commaEng str as
   commaEng _ []     = ""

   commaNL :: String -> [String] -> String
   commaNL str [a,b,c]= a++", "++b++" "++str++" "++c
   commaNL str [a,b]  = a++" "++str++" "++b
   commaNL _ [a]    = a
   commaNL str (a:as) = a++", "++commaNL str as
   commaNL _ []     = ""

   preciesEen :: String
   preciesEen = [chr 233]++[chr 233]++"n"
    