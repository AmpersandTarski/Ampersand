  {-# OPTIONS_GHC -Wall #-}
  -- | Deze module bevat operaties op strings.
  module DatabaseDesign.Ampersand.Basics.String
   (unCap,upCap,trim,spacesToUnderscores,spaces,commaEngString,commaNLString,preciesEen)
  where
   import Data.Char (isUpper, toUpper, toLower, chr)

   unCap :: String -> String
   unCap [] = []
   unCap [h] = [toLower h]
   unCap (h:h':t) | isUpper h' = h:h':t
                  | otherwise  = toLower h:h':t

   upCap :: String -> String
   upCap [] = []  
   upCap (h:t) = toUpper h:t

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

   commaEngString :: String -> [String] -> String
   commaEngString str [a,b,c]= a++", "++b++", "++str++" "++c
   commaEngString str [a,b]  = a++" "++str++" "++b
   commaEngString _ [a]    = a
   commaEngString str (a:as) = a++", "++commaEngString str as
   commaEngString _ []     = ""

   commaNLString :: String -> [String] -> String
   commaNLString str [a,b,c]= a++", "++b++" "++str++" "++c
   commaNLString str [a,b]  = a++" "++str++" "++b
   commaNLString _ [a]    = a
   commaNLString str (a:as) = a++", "++commaNLString str as
   commaNLString _ []     = ""

   preciesEen :: String
   preciesEen = [chr 233]++[chr 233]++"n"
    