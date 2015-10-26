{- Based on http://hackage.haskell.org/package/altfloat-0.3.1  -}
{-
 - Copyright (C) 2009-2010 Nick Bowler.
 -
 - License BSD2:  2-clause BSD license.  See LICENSE for full terms.
 - This is free software: you are free to change and redistribute it.
 - There is NO WARRANTY, to the extent permitted by law.
 -}

{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Database.Design.Ampersand.Core.Poset.Internal where

import qualified Prelude
import Prelude hiding (Ordering(..), Ord(..))
import Database.Design.Ampersand.Basics (fatal)

data Ordering = LT | EQ | GT | CP | NC
    deriving (Eq, Show, Read, Bounded, Enum)

-- Lexicographic ordering.
instance Monoid Ordering where
    mempty = EQ
    mappend EQ x = x
    mappend NC _ = NC
    mappend LT _ = LT
    mappend GT _ = GT
    mappend CP _ = NC

-- | Internal-use function to convert our Ordering to the ordinary one.
totalOrder :: Ordering -> Prelude.Ordering
totalOrder LT = Prelude.LT
totalOrder EQ = Prelude.EQ
totalOrder GT = Prelude.GT
totalOrder NC = fatal 36 "Uncomparable elements in total order."
totalOrder CP = fatal 37 "Uncomparable elements in total order."

-- | Internal-use function to convert the ordinary Ordering to ours.
partialOrder :: Prelude.Ordering -> Ordering
partialOrder Prelude.LT = LT
partialOrder Prelude.EQ = EQ
partialOrder Prelude.GT = GT

-- | Class for partially ordered data types.  Instances should satisfy the
-- following laws for all values a, b and c:
--
-- * @a <= a@.
--
-- * @a <= b@ and @b <= a@ implies @a == b@.
--
-- * @a <= b@ and @b <= c@ implies @a <= c@.
--
-- But note that the floating point instances don't satisfy the first rule.
--
-- Minimal complete definition: 'compare' or '<='.
class Eq a => Poset a where
    compare :: a -> a -> Ordering
    -- | Is comparable to.
    (<==>) :: a -> a -> Bool
    -- | Is not comparable to.
    (</=>) :: a -> a -> Bool
    (<) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    (>) :: a -> a -> Bool

    a `compare` b
        | a == b = EQ
        | a <= b && b <= a = EQ
        | a <= b = LT
        | b <= a = GT
        | otherwise = NC

    a <    b = a `compare` b == LT
    a >    b = a `compare` b == GT
    a <==> b = a `compare` b /= NC
    a </=> b = a `compare` b == NC
    a <=   b = a < b || a `compare` b == EQ
    a >=   b = a > b || a `compare` b == EQ

-- | Class for partially ordered data types where sorting makes sense.
-- This includes all totally ordered sets and floating point types.  Instances
-- should satisfy the following laws:
--
-- * The set of elements for which 'isOrdered' returns true is totally ordered.
--
-- * The max (or min) of an insignificant element and a significant element
-- is the significant one.
--
-- * The result of sorting a list should contain only significant elements.
--
-- * @max a b@ = @max b a@
--
-- * @min a b@ = @min b a@
--
-- The idea comes from floating point types, where non-comparable elements
-- (NaNs) are the exception rather than the rule.  For these types, we can
-- define 'max', 'min' and 'sortBy' to ignore insignificant elements.  Thus, a
-- sort of floating point values will discard all NaNs and order the remaining
-- elements.
--
-- Minimal complete definition: 'isOrdered'
class Poset a => Sortable a where
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    join :: a -> a -> a
    meet :: a -> a -> a

-- | Class for totally ordered data types.  Instances should satisfy
-- @isOrdered a = True@ for all @a@.
class Poset a => Ord a
