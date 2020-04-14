{-# OPTIONS_GHC -Wall #-}
-- | A set of functions and data-types needed by the code generated through GRDT generators.
module GRDT.Common where
import GHC.Stack (HasCallStack)

-- | A relation is a set of pairs. This data-type does not quite respect that:
-- duplicate pairs are not ignored, and the order can be exposed by this implementation.
data Relation a b = Relation {getRelation::[(a,b)]} deriving (Show,Eq)

instance Semigroup (Relation a b) where
  Relation l <> Relation r = Relation (l <> r)
instance Monoid (Relation a b) where
  mempty = Relation mempty

-- | Relational converse.
converse :: Relation a b -> Relation b a
converse (Relation lst)= Relation (map (\(x,y)->(y,x)) lst)

-- | This version of head is identical to the one in the prelude,
--   except that it produces a stack trace.
--   It may be called when creating data types with total relations.
--   While the underlying problem might not be a bug in the GRDT code,
--    we would still like to hear about it if you run into a problem with this function.
head' :: HasCallStack => [a] -> a
head' [] = error "Empty list in call to GRDT.Common.head'"
head' (x:_) = x

-- | Relational lookup.
findIn :: Eq a => Relation a b -> a -> [b]
findIn xs p = [b | (a,b) <- getRelation xs, a == p]