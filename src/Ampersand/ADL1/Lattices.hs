{-|
Module      : Lattices
Description : Efficient membership for a lattice
Copyright   : (c) Sebastiaan Joosten, 2014 - 2015
License     : same as the rest of Ampersand
Maintainer  : sjcjoosten

This module allows you to build a finite semi-Lattice using equalities over intersections of atoms, see @addEquality@.
After changing the data type, see @optimize1@, the structure allows you to perform several queries, such as finding (sets of) least/greatests bounds.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.ADL1.Lattices 
    ( findExact,findUpperbounds,optimize1
    , Op1EqualitySystem,addEquality,emptySystem
    , FreeLattice(..),getGroups,isInSystem
    ) where

import           Ampersand.Basics hiding (toList)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified RIO.List    as L
import qualified RIO.Map    as Map
import qualified RIO.Map.Partial as PARTIAL --TODO: Get rid of partial functions
import qualified RIO.Set    as Set

-- optimisations possible for the EqualitySystem(s):
-- (1) apply optimize1 inline, that is: don't use EqualitySystem but use ES1 instead
-- (2) include the transitively dependent rules recursively
data EqualitySystem a
 = ES (Map.Map a Int) -- whatever this is a system of
      (IntMap.IntMap  -- map for: whenever you encounter this element i in your set y
         [( IntSet.IntSet -- when you find this set (that is: if it is a subset of y)
          , IntSet.IntSet -- add this set
          )]
      )

-- | An empty system of equalities, that is: x ~ y only if x = y.
emptySystem :: EqualitySystem a
emptySystem = ES Map.empty IntMap.empty

-- | Test whether the given concept is present in the equality system.
--   This is important when using getGroups, since that function only returns concepts @x@ for which @isInSystem x@ (and exactly those).
isInSystem :: (Ord a) => Op1EqualitySystem a -> a -> Bool
isInSystem (ES1 t _ _) a = Map.member a t

-- | getGroups: create groups of concepts (type variable: concept).
--   1. Each concept is in precisely one group.
--   2. Two concepts are in the same group if there is a path of classify-rules between those concepts.
--   The purpose of this is to know whether two concepts are comparable or not. Atoms of concepts within a group can be compared.
--   Atoms of concepts in different groups may never be compared.
getGroups :: (Ord concept, SetLike set) => Op1EqualitySystem concept -> [set concept]
getGroups (ES1 tran _ imap)
 = [fromList [a | (a,i) <- Map.toList tran, not . IntSet.null $ IntSet.intersection i r] | r <- IntMap.elems res]
 where
   iml :: [(Int,[(IntSet.IntSet,IntSet.IntSet)])]
   iml = IntMap.toList imap
   (_, _, res) = foldr getLists (0, IntMap.empty, IntMap.empty) ([IntSet.insert a (IntSet.union b c) | (a, bc) <- iml, (b, c) <- bc] ++ Map.elems tran)
   getLists :: IntSet.IntSet -> (Int, IntMap.IntMap Int, IntMap.IntMap IntSet.IntSet) -> (Int, IntMap.IntMap Int, IntMap.IntMap IntSet.IntSet)
   getLists im (acc, allElems, rev) -- TODO: this might be made more efficiently by using Array as the last element
    = if not (IntMap.null overlap) then
       (acc, newElems, newRev)
      else (acc+1, IntMap.union (IntMap.fromSet (const acc) im) allElems, IntMap.insert acc im rev)
    where
      overlap = IntMap.intersection allElems (IntMap.fromSet id im) -- overlap between im and the previously treated elements
      oldKeys = IntMap.elems overlap -- sets to which the overlapping items belong
      newKey = head oldKeys -- get any key name
        where head [] = fatal "head used on empty list."
              head (x:_) = x
      oldKeySet = IntSet.fromList oldKeys -- remove duplicates, provide efficient lookup
      -- newRev' is all items that will remain the same
      -- newItm' is all (old) items that must be renamed
      ~(newItm', newRev') = IntMap.partitionWithKey (\k _ -> IntSet.member k oldKeySet) rev
      newItm :: IntSet.IntSet
      newItm = IntSet.unions (im : IntMap.elems newItm') -- all
      newRev = IntMap.insert newKey newItm newRev'
      newElems = IntMap.union (IntMap.fromSet (const newKey) newItm) allElems -- overwrites some of the allElems items with the new key

-- | Find all concepts equal to some concept-expression.
--   Equality is decided according to the concept-system given in the first argument.
--   If there are no concepts equal to the expression, the empty set is returned.
findExact :: (Ord a) => Op1EqualitySystem a -> FreeLattice a -> Set.Set a -- returns the empty set on a failure, returns all sets equivalent to "FreeLattice a" according to the equalitysystem
findExact = findWith lookupInRevMap (\x -> fromList [x])
-- | Find the least concepts that are greater or equal to some concept-equation.
findUpperbounds :: (Ord a) => Op1EqualitySystem a -> FreeLattice a -> [Set.Set a] -- returns a list of largest subsets
findUpperbounds = findWith findSubsetInRevMap (\x -> [fromList [x]])

findWith :: Ord a
  => ([Int] -> RevMap a -> b) -- Function that finds the normalized form
  -> (a -> b)                   -- Shorthand in case the FreeLattice does not need to go through the translation process
  -> Op1EqualitySystem a        -- system in which the FreeLattice elements can be found
  -> FreeLattice a              -- the FreeLattice that needs translation
  -> b
findWith f f2 es@(ES1 _ back _) trmUnsimplified
  = case trm of
     Atom x -> f2 x
     _ -> f (IntSet.toList (case trm' of
                       Just t -> intersections (map it t)
                       Nothing -> IntSet.empty
                     )
            ) back
  where it = simplifySet es
        intersections [] = IntSet.empty
        intersections (x:xs) = foldr IntSet.intersection x xs
        trm' = latticeToTranslatable es trm
        trm = simpl trmUnsimplified
        simpl (Meet a b)
          = case (simpl a, simpl b) of
             (Atom a', Atom b') | a'==b' -> Atom a'
             (a',b') -> Meet a' b'
        simpl (Join a b)
          = case (simpl a, simpl b) of
             (Atom a', Atom b') | a'==b' -> Atom a'
             (a',b') -> Join a' b'
        simpl (Atom x) = Atom x

simplifySet :: Op1EqualitySystem t -> IntSet.IntSet -> IntSet.IntSet
simplifySet (ES1 _ _ imap) x = imapTranslate imap x IntSet.empty

latticeToTranslatable :: Ord a => Op1EqualitySystem a -> FreeLattice a -> Maybe [IntSet.IntSet]
latticeToTranslatable (ES1 m _ _) = t
 where
   t (Atom a)   = do{r<-Map.lookup a m;return [r]}
   t (Meet a b) = do{a'<-t a;b'<- t b;return [IntSet.union ta tb | ta <- a', tb <- b']}
   t (Join a b) = do{a'<-t a;b'<- t b;return (a'++b')}

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
 where elimSubsets ((a,v):as) = v : elimSubsets (filter (\x -> not (IntSet.isSubsetOf (fst x) a)) as)
       elimSubsets [] = []

endPoints :: (Ord a, SetLike x) => RevMap a -> [(IntSet.IntSet,x a)]
endPoints (RevMap st im)
 = if IntMap.null im then (if slNull st then [] else [(IntSet.empty,fromSet st)]) else concatMap endPoints' (IntMap.toList im)
 where endPoints' (i,rm) = map addi (endPoints rm)
        where addi (lst,elm) = (IntSet.insert i lst,elm)

listAndRest :: [t] -> [(t, [t])]
listAndRest [] = []
listAndRest (a:rst) = (a,rst):listAndRest rst

data RevMap a
 = RevMap (Set.Set a) -- all elements equivalent to this point in the map
          (IntMap.IntMap (RevMap a)) -- recursive
          deriving Show

-- | An optimized structure that expressed equality in a finite semi-lattice
data Op1EqualitySystem a
 = ES1 (Map.Map a IntSet.IntSet)
       (RevMap a)
       (IntMap.IntMap  -- map for: whenever you encounter this element i in your set y
         [( IntSet.IntSet -- when you find this set (that is: if it is a subset of y)
          , IntSet.IntSet -- add this set
          )]
       )

-- TODO: this function can be optimised a lot
reverseMap :: (Ord a) => [(a,[Int])] -> RevMap a
reverseMap lst
 = RevMap (Set.fromList (map fst empties)) (buildMap rest)
 where
   (empties,rest) = L.partition (null . snd) lst
   buildMap [] = IntMap.empty
   buildMap o@((_,~(f:_)):_)
     = IntMap.insert f (reverseMap (map tail2 h)) (buildMap tl)
     where tail2 (a,b) = (a, tail b)
           (h,tl) = L.partition ((== f) . head . snd) o
           tail [] = fatal "tail called on empty list"
           tail (_:t) = t
           head [] = fatal "head used on empty list."
           head (x:_) = x
-- | Change the system into one with fast reverse lookups
optimize1 :: Ord a => EqualitySystem a -> Op1EqualitySystem a
optimize1 (ES oldmap oldimap)
 = ES1 newmap
       (reverseMap (Map.toList (Map.map IntSet.toList newmap)))
       (IntMap.mapMaybe maybeMapper     oldimap)
 where notEmpty [] = Nothing
       notEmpty a = Just a
       maybeMapper :: [(IntSet.IntSet,IntSet.IntSet)] -> Maybe [(IntSet.IntSet,IntSet.IntSet)]
       maybeMapper x = notEmpty [ (s1,imapTranslate oldimap s2 IntSet.empty)
                                | (s1,s2) <- x
                                , not (IntSet.null s1)
                                , not (IntSet.null s2)
                                ]
       newmap = Map.map (\x -> imapTranslate oldimap (IntSet.singleton x) IntSet.empty) oldmap

-- | Add an equality to a system of equalities.
addEquality :: (Ord a, SetLike x) => (x a, x a) -> EqualitySystem a -> EqualitySystem a
addEquality (set1, set2) eqSys0
 = addEquality' eqSys2 ns1 ns2
 where
   (eqSys1, ns1) = translateWith eqSys0 set1
   (eqSys2, ns2) = translateWith eqSys1 set2

-- Only adds forward arcs in the lattice-graph. Computing backward arcs is slow, so we do that in a single step.
addEquality' :: EqualitySystem a -> IntSet.IntSet -> IntSet.IntSet -> EqualitySystem a
addEquality' ~(ES nms imap) set1 set2
 = ES nms (addRule (addRule imap set1 set1 uni') set2 (IntSet.difference set2 set1) uni')
 where
   uni' = IntSet.union set1 set2
   addRule :: IntMap.IntMap [(IntSet.IntSet, IntSet.IntSet)] -> IntSet.IntSet -> IntSet.IntSet -> IntSet.IntSet -> IntMap.IntMap [(IntSet.IntSet, IntSet.IntSet)]
   addRule oldimap origSet triggers newSet
    = foldl' updateMapForTrigger oldimap (IntSet.toList triggers)
    where dif = IntSet.difference newSet origSet
          updateMapForTrigger :: IntMap.IntMap [(IntSet.IntSet, IntSet.IntSet)] -> Int -> IntMap.IntMap [(IntSet.IntSet, IntSet.IntSet)]
          updateMapForTrigger mp trigger
           = IntMap.insertWith (++) trigger [(IntSet.delete trigger origSet, dif)] mp

translateWith :: (Ord a, SetLike x) => EqualitySystem a -> x a -> (EqualitySystem a, IntSet.IntSet)
translateWith ~(ES nomenclature imap) inSet
 = ( ES newNomenclature imap
   , IntSet.fromList$ map (newNomenclature PARTIAL.!) (toList inSet)
   )
 where
  newNomenclature
   = foldr (\x y -> if Map.member x y then y else Map.insert x (Map.size y) y) nomenclature (toList inSet)

imapTranslate :: IntMap.IntMap [(IntSet.IntSet, IntSet.IntSet)] -> IntSet.IntSet -> IntSet.IntSet -> IntSet.IntSet
imapTranslate imap tds doneSet
 = case IntSet.minView tds of
    Nothing -> doneSet
    Just (todo,set') -> imapTranslate imap (newSet todo set') (IntSet.insert todo doneSet)
 where
  newSet todo set'
   = case IntMap.lookup todo imap of
       Nothing -> set'
       Just lst -> IntSet.unions (set':[IntSet.difference tl doneSet | (fl,tl) <- lst, IntSet.isSubsetOf fl doneSet])

-- | Data structure to capture an expression in a lattice
data FreeLattice a
 = Join (FreeLattice a) (FreeLattice a)
 | Meet (FreeLattice a) (FreeLattice a)
 | Atom a deriving Functor

instance SetLike [] where
  fromList = id
  fromSet = Set.toList
  toList = id
  slNull = null

instance SetLike Set.Set where
  slEmpty = Set.empty
  toList = Set.toList
  fromList = Set.fromList
  fromSet = id
  slNull = Set.null

-- | A single set of operations to use both for ordered lists and for sets
class SetLike x where -- I dislike having to put Ord everywhere, is there another way? (Without including a in the class)
  toList :: Ord a => x a -> [a]
  fromList :: Ord a => [a] -> x a
  fromSet :: Ord a => Set.Set a -> x a
  slEmpty :: Ord a => x a
  slEmpty = fromList []
  slNull :: Ord a => x a -> Bool
  slNull = null . toList


