{-# OPTIONS_GHC -fno-warn-orphans #-}
{- COPIED FROM http://hackage.haskell.org/package/altfloat-0.3.1 -}
{-
 - Copyright (C) 2009 Nick Bowler.
 -
 - License BSD2:  2-clause BSD license.  See LICENSE for full terms.
 - This is free software: you are free to change and redistribute it.
 - There is NO WARRANTY, to the extent permitted by law.
 -}

-- | Partially ordered data types.  The standard 'Prelude.Ord' class is for
-- total orders and therefore not suitable for floating point.  However, we can
-- still define meaningful 'max' and 'sortWith functions for these types.
--
-- We define our own 'Ord' class which is intended as a replacement for
-- 'Prelude.Ord'.  Should the user wish to take advantage of existing libraries
-- which use 'Prelude.Ord', just let Prelude.compare = (totalOrder .) . compare
module Database.Design.Ampersand.Core.Poset (
    Poset(..), Sortable(..), Ordering(..), Ord, comparableClass,greatest,least,maxima,minima,sortWith
) where
import qualified Prelude (Ordering(..))
import Prelude hiding (Ord(..), Ordering(..))
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.Poset.Instances() --required for instance Int of Poset only
import Database.Design.Ampersand.Core.Poset.Internal
import qualified Data.List as List


-- | makePartialOrder makes a partial order containing local partial orders, i.e. comparable classes.
--   it makes sense to sort comparable classes.
--   example: A and B are in a comparable class
--            A and B are not LT, not GT, not EQ => CP
--            if you sortBy comparableClass then A and B are considered EQ (comparableClass CP = Prelude.EQ)
--   when the comparable classes have a top, then join can be defined on them
--   when the comparable classes have a bottom, then meet can be defined on them
--
--   When A_Concept should be a collection of total orders change f a b guard (| or [ a `elem` cl && b `elem` cl | cl <- cls ] = NC)
--
--   examples on data X = A | B | C | D | E | F deriving (Eq,Show):
--   [bottom]       (makePartialOrder [(A,B),(C,D),(B,D),(A,C),(D,E),(D,F)]) :: (A <= B /\ C <= B \/ C <= D <= E /\ F <= E \/ F)
--   [ringish]      (makePartialOrder [(A,B),(C,D),(B,D),(A,C),(D,E),(D,F),(E,A),(F,A)]) _ _ = LT
--   [ringish]      (makePartialOrder [(A,B),(C,D),(B,D),(A,C),(D,E),(D,F),(E,A)])       F A = GT
--                  (makePartialOrder [(A,B),(C,D),(B,D),(A,C),(D,E),(D,F),(E,A)])       _ _ = LT
--   [bottom,total] (makePartialOrder [(A,B),(C,D),(B,D),(A,C),(E,F)]) :: ( A <= B /\ C <= B \/ C <= D , E <= F )
--   [2x total]     (makePartialOrder [(A,B),(B,C),(C,D),(E,F)]) :: ( A <= B <= C <= D , E <= F )
--   [total]        (makePartialOrder [(A,B),(B,C),(C,D),(D,E),(E,F)]) :: ( A <= B <= C <= D <= E <= F )
--   [3x total]     (makePartialOrder [(A,B),(B,C),(C,D)]) :: ( A <= B <= C <= D , E , F )
--   [partial]      (makePartialOrder [(A,B),(C,D),(B,D),(D,E),(D,F)]) :: ( (A <= B <= D <= E /\ F <= E \/ F) + (C <= D <= E /\ F <= E \/ F) )
--
--   a sorted list will have the x left of y for all x and y. x <= y
--   like x==y, the intraposition of x and y is without meaning for all x and y. x `compare` y = CP
--   for example given a (makePartialOrder [(A,B),(C,D),(B,D),(D,E),(F,C)]):
--    + sort  [F,E,D,C,B,A] = [F,C,A,B,D,E]
--    + sort  [F,E,D,B,A,C] = [F,A,B,C,D,E]
--    + sort  [B,F,E,C,D,A] = [A,B,F,C,D,E]

instance Poset a => Poset (Maybe a) where
    Just x  <= Just y = x <= y
    Nothing <= _      = True
    _       <= _      = False

instance Poset a => Poset [a] where
    compare = (mconcat .) . zipWith compare

{-
-- | Sort a list using the default comparison function.
sort :: Sortable a => [a] -> [a]
sort = sortBy compare

-- | Apply a function to values before comparing.
comparing :: Poset b => (a -> b) -> a -> a -> Ordering
comparing = on compare
-}

-- example where b=A_Concept: sortWith (snd . order , concs fSpec) idCpt (vIndices fSpec)
sortWith :: (Show b,Poset b) => (b -> [[b]], [b]) -> (a -> b) -> [a] -> [a]
sortWith _   _  [] = []
sortWith (tos,allb) f xs
 = let xtos = [ [x | x<-xs, elem (f x) to] --group xs such that each elem of (map f xtos) is a total order
              | to<-(tos . f . head) xs --non-trivial total orders
                    ++ [[b] | b<-allb, not( elem b (concat((tos . f . head) xs))) ] --trivial total orders
              ]
       sortwith = List.sortBy (\x y -> comparableClass(compare (f x) (f y))) --sortwith of Poset, which should be a total order
   in  concat(map sortwith xtos) --sortwith each total order and concat them

-- | Elements can be arranged into classes of comparable elements, not necessarily a total order
--   It makes sense to sort such a class.
--   Take for example instance Sortable A_Concept.
--   When A_Concept should be a collection of total orders: comparableClass CP = fatal 118 "Elements in totally ordered class, which are not LT, not GT and not EQ."
comparableClass :: Ordering -> Prelude.Ordering
comparableClass LT = Prelude.LT
comparableClass EQ = Prelude.EQ
comparableClass GT = Prelude.GT
comparableClass NC = fatal 123 "Uncomparable elements in comparable class."
comparableClass CP = Prelude.EQ --the position of two comparable concepts is equal

{-
-- | If elements are in a total order, then they can be sortedBy totalOrder using the Prelude.Ordering
--   When A_Concept should be in a total order with an Anything and Nothing: sortBy f = Data.List.sortBy ((totalOrder .) . f)
totalOrder :: Ordering -> Prelude.Ordering
totalOrder LT = Prelude.LT
totalOrder EQ = Prelude.EQ
totalOrder GT = Prelude.GT
totalOrder NC = fatal 132 "Uncomparable elements in total order."
totalOrder CP = fatal 133 "Uncomparable elements in total order."
-}

-- | takes the greatest a of comparables
greatest :: (Show a,Sortable a) => [a] -> a
greatest xs =
  case maxima (List.nub xs) of
    []  -> fatal 138 "there is no greatest"
    [x] -> x
    xs'  -> fatal 140 ("there is more than one greatest: "++ show (List.nub xs'))
-- | takes all a without anything larger
maxima :: Sortable a => [a] -> [a]
maxima [] = fatal 144 "the empty list has no maximum"
maxima xs = [x | x<-List.nub xs,not (or [x < y | y<-List.nub xs])]

-- | takes the least a of comparables if there is only one
least :: Sortable a => [a] -> a
least xs =
  case minima (List.nub xs) of
    []  -> fatal 150 "there is no least"
    [x] -> x
    _   -> fatal 150 "there is more than one least. "
-- | takes all a without anything less
minima :: Sortable a => [a] -> [a]
minima [] = fatal 156 "the empty list has no minimum"
minima xs = [x | x<-List.nub xs,not (or [y < x | y<-List.nub xs])]
