{-# LANGUAGE Rank2Types, KindSignatures, GeneralizedNewtypeDeriving #-}

module Prototype.CodeAuxiliaries (Named(..),atleastOne,reName,nameFresh) where
 import Strings(noCollide)

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
