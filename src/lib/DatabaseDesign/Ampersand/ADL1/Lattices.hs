{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module DatabaseDesign.Ampersand.ADL1.Lattices (findExact,findSubsets,optimize1,addEquality,emptySystem) where
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, partition)

-- optimisations possible for the EqualitySystem(s):
-- (1) apply optimize1 directly
-- (2) include the transitively dependent rules
data EqualitySystem a
 = ES (Map.Map a Int) -- whatever this is a system of
      (IntMap.IntMap  -- map for: whenever you encounter this element i in your set y
         [( Set.Set Int -- when you find this set (that is: if it is a subset of y)
          , Set.Set Int -- add this set
          )]
      )

emptySystem :: EqualitySystem a
emptySystem = ES Map.empty IntMap.empty

findExact :: (Ord a, SetLike x) => Op1EqualitySystem a -> FreeLattice a -> x a -- returns the empty set on a failure
findExact = findWith (lookupInRevMap)
findSubsets :: (Ord a, SetLike x) => Op1EqualitySystem a -> FreeLattice a -> [x a] -- returns a list of largest subsets
findSubsets = findWith findSubsetInRevMap

findWith :: (SetLike x, Ord a) => ((x Int) -> RevMap a -> b) -> Op1EqualitySystem a -> FreeLattice a -> b
findWith f es@(ES1 _ back _) trm
  = f (fromSet (intersections (map it (latticeToTranslatable es trm)))) back
  where it = simplifySet es
        intersections [] = Set.empty
        intersections x = foldr1 Set.intersection x

simplifySet :: (SetLike x) => Op1EqualitySystem t -> x Int -> Set.Set Int
simplifySet (ES1 _ _ imap) x = imapTranslate imap (toSet x) Set.empty

latticeToTranslatable :: Ord a => Op1EqualitySystem a -> FreeLattice a -> [Set.Set Int]
latticeToTranslatable (ES1 m _ _) lt = t lt
 where
   t (Atom a) = [(Map.!) m a]
   t (Meet a b) = [Set.union ta tb | ta <- (t a), tb <- (t b)]
   t (Join a b) = t a ++ t b
   

-- how to lookup something in a RevMap (Precondition: list is sorted!)
lookupInRevMap :: (Ord a, SetLike x) => [Int] -> RevMap a -> x a
lookupInRevMap [] (RevMap st _) = fromSet st
lookupInRevMap (a:as) (RevMap _ mp)
 = case IntMap.lookup a mp of
    Nothing -> slEmpty
    Just rm -> lookupInRevMap as rm

-- a bit slower: suppose we could not find our element in the RevMap, we find all subsets of it (as a RevMap)
findSubsetAsRevMap :: (Ord a) => [Int] -> RevMap a -> RevMap a
findSubsetAsRevMap [] (RevMap st _) = RevMap st IntMap.empty
findSubsetAsRevMap lst (RevMap st mp)
 = RevMap st (IntMap.fromList [ (l, rm)
                              | (l, rst) <- listAndRest lst
                              , (Just mp') <- [IntMap.lookup l mp]
                              , let rm@(RevMap st2 mp2) = findSubsetAsRevMap rst mp'
                              , not (Set.null st2 && IntMap.null mp2)
                              ] )

-- fins the largest subsets! (endpoints only)
findSubsetInRevMap :: (Ord a, SetLike x) => [Int] -> RevMap a -> [x a]
findSubsetInRevMap lst rm = largestSubset (findSubsetAsRevMap lst rm)

largestSubset :: (Ord a, SetLike x) => RevMap a -> [x a]
largestSubset i
 = elimSubsets (endPoints i)
 where elimSubsets ((a,v):as) = v : elimSubsets (filter (\x -> not (Set.isSubsetOf (fst x) a)) as)
       elimSubsets [] = []

endPoints :: (Ord a, SetLike x) => RevMap a -> [(Set.Set Int,x a)]
endPoints (RevMap st im)
 = if (IntMap.null im) then [(Set.empty,fromSet st)] else concat (map endPoints' (IntMap.toList im))
 where endPoints' (i,rm) = map addi (endPoints rm)
        where addi (lst,elm) = (Set.insert i lst,elm)

listAndRest :: [t] -> [(t, [t])]
listAndRest [] = []
listAndRest (a:rst) = (a,rst):listAndRest rst


data RevMap a
 = RevMap (Set.Set a) -- all elements equivalent to this point in the map
          (IntMap.IntMap (RevMap a)) -- recursive
          deriving Show

data Op1EqualitySystem a
 = ES1 (Map.Map a (Set.Set Int))
       (RevMap a)
       (IntMap.IntMap  -- map for: whenever you encounter this element i in your set y
         [( Set.Set Int -- when you find this set (that is: if it is a subset of y)
          , Set.Set Int -- add this set
          )]
      )

-- TODO: this function can be optimised a lot
reverseMap :: (Ord a) => [(a,[Int])] -> RevMap a
reverseMap lst
 = RevMap (Set.fromList (map fst empties)) (buildMap rest)
 where
   (empties,rest) = partition (null . snd) lst
   buildMap [] = IntMap.empty
   buildMap o@((_,~(f:_)):_)
     = IntMap.insert f (reverseMap (map tail2 h)) (buildMap tl)
     where tail2 (a,b) = (a, tail b)
           (h,tl) = partition ((== f) . head . snd) o
     
optimize1 :: Ord a => EqualitySystem a -> Op1EqualitySystem a
optimize1 (ES oldmap oldimap)
 = ES1 newmap
       (reverseMap (Map.toList (Map.map getList newmap)))
       (IntMap.mapMaybe (\x -> notEmpty [ (s1,imapTranslate oldimap s2 (Set.empty)) 
                                        | (s1,s2) <- x
                                        , not (Set.null s1)
                                        , not (Set.null s2)
                                        ]) oldimap)
 where notEmpty [] = Nothing
       notEmpty a = Just a
       -- newmap :: Map.Map a (Set.Set Int)
       newmap = Map.map (\x -> imapTranslate oldimap (Set.singleton x) (Set.empty)) oldmap

addEquality :: (Ord a, SetLike x) => x a -> x a -> EqualitySystem a -> EqualitySystem a
addEquality  set1 set2 eqSys0
 = addEquality' eqSys2 ns1 ns2
 where
   (eqSys1, ns1) = translateWith eqSys0 set1
   (eqSys2, ns2) = translateWith eqSys1 set2

addEquality' :: Ord a => EqualitySystem a -> Set.Set Int -> Set.Set Int -> EqualitySystem a
addEquality' ~(ES nms imap) set1 set2
 = ES nms (addRule (addRule imap set1 set1 uni) set2 (Set.difference set2 set1) uni)
 where
   uni = Set.union set1 set2
   addRule :: IntMap.IntMap [(Set.Set Int, Set.Set Int)] -> Set.Set Int -> Set.Set Int -> Set.Set Int -> IntMap.IntMap [(Set.Set Int, Set.Set Int)]
   addRule oldimap origSet triggers newSet
    = foldl updateMapForTrigger oldimap (getList triggers)
    where dif = Set.difference newSet origSet
          updateMapForTrigger :: IntMap.IntMap [(Set.Set Int, Set.Set Int)] -> Int -> IntMap.IntMap [(Set.Set Int, Set.Set Int)]
          updateMapForTrigger mp trigger
           = IntMap.insertWith (++) trigger [(Set.delete trigger origSet, dif)] mp

translateWith :: (Ord a, SetLike x) => EqualitySystem a -> x a -> (EqualitySystem a, Set.Set Int)
translateWith ~(ES nomenclature imap) inSet
 = ( ES newNomenclature imap
   , fromList$ map (newNomenclature Map.!) (getList inSet)
   )
 where
  newNomenclature
   = foldr (\x y -> if Map.member x y then y else Map.insert x (Map.size y) y) nomenclature (getList inSet)

imapTranslate :: IntMap.IntMap [(Set.Set Int, Set.Set Int)] -> Set.Set Int -> Set.Set Int -> Set.Set Int
imapTranslate imap tds doneSet
 = case Set.minView tds of
    Nothing -> doneSet
    Just (todo,set) -> imapTranslate imap (newSet todo set) (slInsert todo doneSet)
 where
  newSet todo set
   = case IntMap.lookup todo imap of
       Nothing -> set
       Just lst -> slUnions (set:[Set.difference tl doneSet | (fl,tl) <- lst, Set.isSubsetOf fl doneSet])

data FreeLattice a
 = Join (FreeLattice a) (FreeLattice a)
 | Meet (FreeLattice a) (FreeLattice a)
 | Atom a
 
instance SetLike [] where
  fromList = id
  fromSet = Set.toList
  toSet = Set.fromList
  getList = id
  slUnion a b = mrgUnion a b
  slIsect a b = mrgIsect a b
  slFold = foldl
  slNull = null
  slSize = length

instance SetLike Set.Set where
  slIsect = Set.intersection
  slUnion = Set.union
  slEmpty = Set.empty
  slUnions = Set.unions
  slMap = Set.map
  getList = Set.toList
  fromList = Set.fromList
  fromSet = id
  isElemOf = Set.member
  slFold f = Set.fold (flip f)
  slNull = Set.null
  slSize = Set.size
  slInsert = Set.insert
  toSet = id

class SetLike x where -- I dislike having to put Ord everywhere, is there another way? (Without including a in the class)
  slIsect :: Ord a => x a -> x a -> x a
  slUnion :: Ord a => x a -> x a -> x a
  getList :: Ord a => x a -> [a]
  fromList :: Ord a => [a] -> x a
  fromSet  :: Ord a => Set.Set a -> x a
  slMap :: (Ord a,Ord b) => (a -> b) -> x a -> x b
  slMap f = fromList . nub' . sort . (map f) . getList
  slEmpty :: Ord a => x a
  slEmpty = fromList []
  slUnions :: Ord a => [x a] -> x a
  slUnions = foldr slUnion slEmpty
  isElemOf :: Ord a => a -> x a -> Bool
  isElemOf e mp = (e `elem` getList mp)
  slFold :: Ord b => (a -> b -> a) -> a -> x b -> a
  slFold f u xs = foldl f u (getList xs)
  slSize :: Ord a => x a -> Int
  slSize = length . getList
  slNull :: Ord a => x a -> Bool
  slNull = null . getList
  slInsert :: Ord a => a -> x a -> x a
  slInsert x = slUnion (fromList [x])
  toSet :: Ord a => x a -> Set.Set a
  

nub' :: Eq a => [a] -> [a]
nub' (a:b:bs) | a == b = nub' (b:bs)
              | otherwise = a:nub' (b:bs)
nub' rst = rst

mrgUnion :: (Ord a) => [a] -> [a] -> [a]
mrgUnion (a:as) (b:bs) | a<b       = a:mrgUnion as (b:bs)
                       | a==b      = a:mrgUnion as bs
                       | otherwise = b:mrgUnion (a:as) bs
mrgUnion a b = a ++ b -- since either a or b is the empty list
{-# SPECIALIZE mrgUnion :: [Int] -> [Int] -> [Int] #-}

mrgIsect :: (Ord a) => [a] -> [a] -> [a]
mrgIsect (a:as) (b:bs) | a<b       = mrgIsect as (b:bs)
                       | a==b      = b: mrgIsect as bs
                       | otherwise = mrgIsect (a:as) bs
mrgIsect _ _ = [] -- since either a or b is the empty list
{-# SPECIALIZE mrgIsect :: [Int] -> [Int] -> [Int] #-}

