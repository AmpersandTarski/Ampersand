{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Basics.Auxiliaries 
        ( eqClass,
          eqCl,
          getCycles,
          transClosureMap, transClosureMap',
          combinations,
          converse,
          commaEng, commaNL,
          fst3, snd3, thd3,
          Flippable(..),
          showTrace,
          showTraceTag,
          blockParenthesize,
          addToLastLine,
          indent,
          module Debug.Trace
        ) where

import Data.List
import Data.Graph (stronglyConnComp, SCC(CyclicSCC))
import Data.Maybe (fromMaybe)
import Data.Map (Map) 
import qualified Data.Map as Map 
import Data.Set (Set)
import qualified Data.Set as Set 
import Debug.Trace

-- | The 'eqClass' function takes an equality test function and a list and returns a list of lists such
-- that each sublist in the result contains only equal elements, and all equal elements are in
-- the same sublist.  For example,
--
-- Example> eqClass "Mississippi" = ["M","iiii","ssss","pp"]
--
eqClass :: (a -> a -> Bool) -> [a] -> [[a]]
eqClass _ [] = []
eqClass f (x:xs) = (x:[e |e<-xs, f x e]) : eqClass f [e |e<-xs, not (f x e)]

-- | eqCl is used for gathering things that are equal wrt some criterion f.
--   For instance, if you want to have persons with the same name:
--    'eqCl name persons' produces a list,in which each element is a list of persons with the same name.
-- Example> eqCl (=='s') "Mississippi" = "ssss"

eqCl :: Ord b => (a -> b) -> [a] -> [[a]]
eqCl _ [] = []
eqCl f lst = Map.elems (Map.fromListWith (++) [(f e,[e]) | e <- lst])

-- | getCycles returns a list of cycles in the edges list (each edge is a pair of a from-vertex
--   and a list of to-vertices)
getCycles :: Eq a => [(a, [a])] -> [[a]]
getCycles edges =
  let allVertices = nub . concat $ [ from : to | (from, to) <- edges ]
      keyFor v = fromMaybe (error "FATAL") $ elemIndex v allVertices
      graphEdges = [ (v, keyFor v , map keyFor vs)  | (v, vs) <- edges ]
  in  [ vs | CyclicSCC vs <- stronglyConnComp graphEdges ]


-- |  Warshall's transitive closure algorithm
transClosureMap' :: Ord a => Map a [a] -> Map a [a]
transClosureMap' xs
  = foldl f xs (Map.keys xs `intersect` nub (concat (Map.elems xs)))
    where
     f :: Ord a => Map a [a] -> a -> Map a [a]   -- The type is given for documentation purposes only
     f q x = Map.unionWith union q (Map.fromListWith union [(a, q Map.! x) | (a, bs) <- Map.assocs q, x `elem` bs])
-- |  Warshall's transitive closure algorithm
transClosureMap :: Ord a => Map a (Set a) -> Map a (Set a)
transClosureMap xs
  = foldl f xs (Map.keysSet xs `Set.intersection` mconcat (Map.elems xs))
    where
     f :: Ord a => Map a (Set a) -> a -> Map a (Set a)
     f q x = Map.unionWith Set.union q (Map.fromListWith Set.union [(a, q Map.! x) | (a, bs) <- Map.assocs q, x `elem` bs])

-- The following function can be used to determine how much of a set of alternative expression is already determined
-- | The 'combinations' function returns all possible combinations of lists of list.
-- For example,
--
-- > combinations [[1,2,3],[10,20],[4]] == [[1,10,4],[1,20,4],[2,10,4],[2,20,4],[3,10,4],[3,20,4]]
combinations :: [[a]] -> [[a]]
combinations []       = [[]]
combinations (es:ess) = [ x:xs | x<-es, xs<-combinations ess]

-- Convert list of a's with associated b's to a list of b's with associated a's.
-- Each b in the result is unique, and so is each a per b, eg.: 
-- converse [("foo",[2,2,3]),("foo",[3,4]),("bar",[4,5])]  == [(2,["foo"]),(3,["foo"]),(4,["foo","bar"]),(5,["bar"])]
converse :: forall a b . (Ord a, Ord b) => [(a, [b])] -> [(b, [a])]
converse aBss = let asPerB :: Map b (Set a)
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

fst3 :: (a,b,c)->a
snd3 :: (a,b,c)->b
thd3 :: (a,b,c)->c
fst3 (a,_,_) = a
snd3 (_,b,_) = b
thd3 (_,_,c) = c

class Flippable a where
  flp :: a -> a


-- Trace shorthands

showTrace :: Show a => a -> a
showTrace = traceShowId

showTraceTag :: Show a => String -> a -> a
showTraceTag tag x = trace (tag ++ ": " ++ show x) x


-- Code formatting utils

blockParenthesize :: String -> String -> String -> [[String]] -> [String]
blockParenthesize open close sep liness =
  case liness of
    [] -> [open ++ close]
    _  -> concat [ zipWith (++) (pre:repeat "  ") linez
                 | (pre, linez) <- zip ((open++" "): repeat (sep++" ")) liness ] ++ [close]
-- [["line"], ["line1", "line2", "line3"],["linea", "lineb"] ->
-- ( line
-- , line1
--   line2
--   line3
-- , linea
--   lineb
-- )

addToLastLine :: String -> [String] -> [String]
addToLastLine str [] = [str]
addToLastLine str liness = init liness ++ [last liness ++ str]

indent :: Int -> [String] -> [String]
indent n = map (replicate n ' ' ++)
