{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Basics.Auxiliaries 
        ( eqClass,
          eqCl,
          transClosureMap, transClosureMap',
          converse,
          commaEng, commaNL,
          Flippable(..),
        ) where

import           Ampersand.Basics.Prelude
import           RIO.List(foldl,intersect,nub,union)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map 
import qualified Data.Set as Set 

-- | The 'eqClass' function takes an equality test function and a list and returns a list of lists such
-- that each sublist in the result contains only equal elements, and all equal elements are in
-- the same sublist.  For example,
--
-- Example> eqClass "Mississippi" = ["M","iiii","ssss","pp"]
--
eqClass :: (a -> a -> Bool) -> [a] -> [NEL.NonEmpty a]
eqClass _ [] = []
eqClass f (x:xs) = (x NEL.:| [e |e<-xs, f x e]) : eqClass f [e |e<-xs, not (f x e)]

-- | eqCl is used for gathering things that are equal wrt some criterion f.
--   For instance, if you want to have persons with the same name:
--    'eqCl name persons' produces a list,in which each element is a list of persons with the same name.
-- Example> eqCl (=='s') "Mississippi" = "ssss"

eqCl :: Ord b => (a -> b) -> [a] -> [[a]]
eqCl _ [] = []
eqCl f lst = Map.elems (Map.fromListWith (++) [(f e,[e]) | e <- lst])

-- |  Warshall's transitive closure algorithm
transClosureMap' :: Ord a => Map.Map a [a] -> Map.Map a [a]
transClosureMap' xs
  = foldl f xs (Map.keys xs `intersect` nub (concat (Map.elems xs)))
    where
     f :: Ord a => Map.Map a [a] -> a -> Map.Map a [a]   -- The type is given for documentation purposes only
     f q x = Map.unionWith union q (Map.fromListWith union [(a, q Map.! x) | (a, bs) <- Map.assocs q, x `elem` bs])
-- |  Warshall's transitive closure algorithm
transClosureMap :: Ord a => Map.Map a (Set.Set a) -> Map.Map a (Set.Set a)
transClosureMap xs
  = foldl f xs (Map.keysSet xs `Set.intersection` mconcat (Map.elems xs))
    where
     f :: Ord a => Map.Map a (Set.Set a) -> a -> Map.Map a (Set.Set a)
     f q x = Map.unionWith Set.union q (Map.fromListWith Set.union [(a, q Map.! x) | (a, bs) <- Map.assocs q, x `elem` bs])

-- Convert list of a's with associated b's to a list of b's with associated a's.
-- Each b in the result is unique, and so is each a per b, eg.: 
-- converse [("foo",[2,2,3]),("foo",[3,4]),("bar",[4,5])]  == [(2,["foo"]),(3,["foo"]),(4,["foo","bar"]),(5,["bar"])]
converse :: forall a b . (Ord a, Ord b) => [(a, [b])] -> [(b, [a])]
converse aBss = let asPerB :: Map.Map b (Set.Set a)
                    asPerB = foldl (.) id [ Map.insertWith Set.union b (Set.singleton a)  | (a,bs) <- aBss, b <- bs ] Map.empty
                in Map.toList $ fmap Set.toList asPerB -- first convert each Set to a list, and then the whole Map to a list of tuples

commaEng :: String -> [String] -> String
commaEng str [a,b,c] = a++", "++b++", "++str++" "++c
commaEng str [a,b]   = a++" "++str++" "++b
commaEng _   [a]     = a
commaEng str (a:as)  = a++", "++commaEng str as
commaEng _   []      = ""

commaNL :: String -> [String] -> String
commaNL str [a,b]  = a++" "++str++" "++b
commaNL  _  [a]    = a
commaNL str (a:as) = a++", "++commaNL str as
commaNL  _  []     = ""

class Flippable a where
  flp :: a -> a

