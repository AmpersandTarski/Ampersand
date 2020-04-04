{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Basics.Auxiliaries 
        ( eqClass,eqClassNE,
          eqCl,eqClNE,
          getCycles,
          transClosureMap, transClosureMap',
          converse, converseNE, converseSet,
          commaEng, commaNL,
          liftFst,liftSnd,
          Flippable(..)
        ) where

import           Ampersand.Basics.Prelude hiding (to)
import           Ampersand.Basics.Version
import           Data.Graph (stronglyConnComp, SCC(CyclicSCC))
import           Data.Typeable
import           RIO.List(foldl,intersect,nub,union)
import qualified RIO.NonEmpty as NE
import qualified RIO.NonEmpty.Partial as PARTIAL
import qualified RIO.List as L
import qualified RIO.Map as Map 
import qualified RIO.Map.Partial as PARTIAL --TODO: Get rid of partial functions.
import qualified RIO.Set as Set
import qualified RIO.Text as T
 
-- | The 'eqClass' function takes an equality test function and a list and returns a list of lists such
-- that each sublist in the result contains only equal elements, and all equal elements are in
-- the same sublist.  For example,
--
-- Example> eqClass (==) "Mississippi" = ["M","iiii","ssss","pp"]
--
eqClass :: (a -> a -> Bool) -> [a] -> [NE.NonEmpty a]
eqClass _ [] = []
eqClass f (x:xs) = (x NE.:| [e |e<-xs, f x e]) : eqClass f [e |e<-xs, not (f x e)]

-- | eqCl is used for gathering things that are equal wrt some criterion f.
--   For instance, if you want to have persons with the same name:
--    'eqCl name persons' produces a list,in which each element is a list of persons with the same name.
-- Example> eqCl (=='s') "Mississippi" = "ssss"

eqCl :: Ord b => (a -> b) -> [a] -> [NE.NonEmpty a]
eqCl _ [] = []
eqCl f lst = Map.elems (Map.fromListWith (<>) [(f e,e NE.:| []) | e <- lst])

-- NonEmpty variants of eqClass and eqCl
eqClassNE :: (a -> a -> Bool) -> NE.NonEmpty a -> NE.NonEmpty (NE.NonEmpty a)
eqClassNE f = PARTIAL.fromList . eqClass f . NE.toList
eqClNE :: Ord b => (a -> b) -> NE.NonEmpty a -> NE.NonEmpty (NE.NonEmpty a)
eqClNE f = PARTIAL.fromList . eqCl f . NE.toList

-- | getCycles returns a list of cycles in the edges list (each edge is a pair of a from-vertex
--   and a list of to-vertices)
getCycles :: (Ord a, Show a,Typeable a) => [(a, [a])] -> [[a]]
getCycles edges' = 
  [ vs | CyclicSCC vs <- stronglyConnComp graphEdges ]
  where
      edges = L.nub $ map nubbed edges'
        where nubbed :: (Eq a) => (a, [a]) -> (a, [a]) 
              nubbed (a,xs)= (a,nub xs)
      allVertices = Set.toList . Set.fromList . concat $ [ from : to | (from, to) <- edges ]
      graphEdges = [ (v, keyFor v , map keyFor vs)  | (v, vs) <- edges ]
      keyFor v = fromMaybe fatalError $ L.elemIndex v allVertices
        where
          fatalError = fatal $ T.intercalate "\n" $
              [ "v ("<>tshow (typeOf v) <>") = "<>tshow v
              , "length edges = "<>tshow (length edges)
              , "edges = "
              ]<>map (("  "<>) .tshow) edges<>
              [ "allVertices ="
              ]<>map (("  "<>) .tshow) allVertices<>
              [ "graphEdges ="
              ]<>map (("  "<>) .tshow) graphEdges

-- |  Warshall's transitive closure algorithm
transClosureMap' :: Ord a => Map.Map a [a] -> Map.Map a [a]
transClosureMap' xs
  = foldl f xs (Map.keys xs `intersect` nub (concat (Map.elems xs)))
    where
     f :: Ord a => Map.Map a [a] -> a -> Map.Map a [a]   -- The type is given for documentation purposes only
     f q x = Map.unionWith union q (Map.fromListWith union [(a, q PARTIAL.! x) | (a, bs) <- Map.assocs q, x `elem` bs])
-- |  Warshall's transitive closure algorithm
transClosureMap :: Ord a => Map.Map a (Set.Set a) -> Map.Map a (Set.Set a)
transClosureMap xs
  = foldl f xs (Map.keysSet xs `Set.intersection` mconcat (Map.elems xs))
    where
     f :: Ord a => Map.Map a (Set.Set a) -> a -> Map.Map a (Set.Set a)
     f q x = Map.unionWith Set.union q (Map.fromListWith Set.union [(a, q PARTIAL.! x) | (a, bs) <- Map.assocs q, x `elem` bs])

-- Convert list of a's with associated b's to a list of b's with associated a's.
-- Each b in the result is unique, and so is each a per b, eg.: 
-- converse [("foo",[2,2,3]),("foo",[3,4]),("bar",[4,5])]  == [(2,["foo"]),(3,["foo"]),(4,["foo","bar"]),(5,["bar"])]
converse :: forall a b . (Ord a, Ord b) => [(a, [b])] -> [(b, [a])]
converse aBss = let asPerB :: Map.Map b (Set.Set a)
                    asPerB = foldl (.) id [ Map.insertWith Set.union b (Set.singleton a)  | (a,bs) <- aBss, b <- bs ] Map.empty
                in Map.toList $ fmap Set.toList asPerB -- first convert each Set to a list, and then the whole Map to a list of tuples
converseNE :: (Ord a,Ord b) => [(a, NE.NonEmpty b)] -> [(b, NE.NonEmpty a)]
converseNE = (fmap $ liftSnd PARTIAL.fromList) . converse . (fmap $ liftSnd NE.toList)
converseSet :: (Ord a,Ord b) => [(a, Set b)] -> [(b, Set a)]
converseSet = (fmap $ liftSnd Set.fromList) . converse . (fmap $ liftSnd Set.toList)
liftFst :: (a -> b) -> (a, c) -> (b, c)
liftFst f (a,c) = (f a, c)

liftSnd :: (a -> b) -> (c, a) -> (c, b)
liftSnd f (c,a) = (c, f a)

commaEng :: (Semigroup str, IsString str) => str -> [str] -> str
commaEng str [a,b,c] = a<>", "<>b<>", "<>str<>" "<>c
commaEng str [a,b]   = a<>" "<>str<>" "<>b
commaEng _   [a]     = a
commaEng str (a:as)  = a<>", "<>commaEng str as
commaEng _   []      = ""

commaNL :: (Semigroup str, IsString str) => str -> [str] -> str
commaNL str [a,b]  = a<>" "<>str<>" "<>b
commaNL  _  [a]    = a
commaNL str (a:as) = a<>", "<>commaNL str as
commaNL  _  []     = ""

class Flippable a where
  flp :: a -> a
