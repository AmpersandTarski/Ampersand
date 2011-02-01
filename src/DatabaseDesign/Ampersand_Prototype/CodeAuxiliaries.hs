{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types, KindSignatures, GeneralizedNewtypeDeriving #-}
module DatabaseDesign.Ampersand_Prototype.CodeAuxiliaries
       (Named(..),atleastOne,reName,nameFresh,noCollide) 
where
 import Data.Char
 data (Eq a) => Named a = Named { nName :: String, nObject :: a} deriving (Eq)
 instance (Show a,Eq a)=> Show (Named a) where
   show a = "$"++(nName a)++(show (nObject a))

 reName :: (Eq a) => String->Named a -> Named a
 reName s o = Named s (nObject o)
 
 nameFresh :: (Eq a, Eq a1) => [Named a] -> String -> a1 -> Named a1
 nameFresh vars nm obj
   = Named realname obj
     where realname = noCollide (map nName vars) nm

 -- | make sure a function returns at least one item (run-time check) or give a debug error
 atleastOne :: forall t. [Char] -> [t] -> [t]
 atleastOne errormsg [] = error errormsg
 atleastOne _ (a:as) = a:as

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

 