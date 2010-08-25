  {-# OPTIONS_GHC -Wall #-}
  -- | Deze module bevat operaties op strings.
  module Strings (chain,unCap,upCap,remSpaces,trim,spacesToUnderscores,spaces,spread,commaEng,commaNL,noCollide)
  where
   import Char (isUpper, toUpper, toLower)
   import Char(isDigit,digitToInt,intToDigit)

   -- | changes its second argument by appending a digit, such that it does not occur in its first argument 
   noCollide :: [String] -- ^ forbidden names
             -> String -- ^ preferred name
             -> String -- ^ a unique name (does not occur in forbidden names)
   noCollide names nm | nm `elem` names = noCollide names (namepart (reverse nm) ++ changeNr (numberpart (reverse nm)))
                      | otherwise = nm
    where
      namepart str   = reverse (dropWhile isDigit str)
      numberpart str = reverse (takeWhile isDigit str)
      changeNr x     = int2string (string2int x+1)
      --  changeNr x = show (read x +1)
      string2int :: String -> Int
      string2int  = enc.reverse
       where enc "" = 0
             enc (c:cs) = digitToInt c + 10* enc cs
      int2string :: Int -> String
      int2string 0 = "0"
      int2string n = if n `div` 10 == 0 then [intToDigit (n `rem` 10)|n>0] else int2string (n `div` 10)++[intToDigit (n `rem` 10)]


{- -- not used
   -- | firstCaps verwijdert spaties uit een string, en maakt van elk eerste letter van een woord een hoofdletter.
   firstCaps :: String -> String
   firstCaps "" = ""
   firstCaps "_" = ""
   firstCaps ('_':'_':str) = firstCaps ('_':str)
   firstCaps ('_':c:str) = toUpper c:firstCaps str
   firstCaps (c:str) = c:firstCaps str
-}
   unCap :: String -> String
   unCap [] = []
   unCap [h] = [toLower h]
   unCap (h:h':t) | isUpper h' = (h:h':t)
                  | otherwise  = toLower h:h':t

   upCap :: String -> String
   upCap [] = [] ; upCap (h:t) = toUpper h:t

   chain :: [b] -> [[b]] -> [b]
   chain _ [] = []
   chain str xs = foldl f (head xs) (tail xs) where f x y = x++str++y

   trim      :: String -> String
   trim      = f . f
     where f = reverse . dropWhile (' '==)

   remSpaces :: String -> String
   remSpaces [] = []
   remSpaces (' ':c:str) = toUpper c:remSpaces str 
   remSpaces xs = xs

   spacesToUnderscores :: String -> String
   spacesToUnderscores  = map translate 
      where translate :: Char -> Char
            translate ' ' = '_'
            translate c = c
            
   spaces :: Int -> String
   spaces x 
      | x < 1     = ""
      | otherwise = " "++spaces (x - 1)

-- De functie spread verspreidt strings over kolommen met een breedte van n.
-- Deze functie garandeert dat alle strings worden afgedrukt in de aangegeven volgorde.
-- Hij probeert daarbij zo weinig mogelijk regels te gebruiken,
-- en alleen de grens van n te overschrijden als een string zelf langer is dan n.
   spread :: Int -> String -> [String] -> [String]
   spread n str = f ""
    where f stored []       = [stored| not (null stored)]
          f [] (cs:css)     = f cs css
          f stored (cs:css) = if length stored > n then stored: f cs css else
                              if length new <= n then f new css else stored: f cs css
                              where new = stored++str++cs

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
