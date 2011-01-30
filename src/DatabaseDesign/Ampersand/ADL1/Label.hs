{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.Label (Label(..))
where
   import DatabaseDesign.Ampersand.ADL1.FilePos  (FilePos)
   
   data Label = Lbl { lblnm   :: String
                    , lblpos  :: FilePos
                    , lblstrs :: [[String]]
                    }
   instance Eq Label where
    l==l' = lblnm l==lblnm l'

{- For future use: classes Identified and Named should be removed. Their functionality should be replaced
by something like this (similar to Named)
---------------------------------------
   import Char

   data Labeled a = Labeled { labl :: String, unlabeled :: a} deriving (Eq)
   instance Show a => Show (Labeled a) where
     show a = labl a ++ ": "++  show (unlabeled a)
  
   reLabel :: (Eq a) => String->Labeled a -> Labeled a
   reLabel s o = Labeled s (unlabeled o)
   
   freshLabel :: [Labeled a] -> String -> a1 -> Labeled a1
   freshLabel vars nm obj
     = Labeled realname obj
       where realname = noCollide (map labl vars) nm
  
   noCollide :: [String] -- ^ forbidden names
             -> String   -- ^ preferred name
             -> String   -- ^ a unique name (does not occur in forbidden names)
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
      int2string n = if n `div` 10 == 0
                     then [intToDigit (n `rem` 10)|n>0]
                     else int2string (n `div` 10)++[intToDigit (n `rem` 10)]

   ----------------------------------------------
-}