module Ampersand.Basics.Collection
  (  Collection (..)
  )where
import           Ampersand.Basics.Prelude
import           Data.List(nub)
import qualified Data.Set as Set
----------------------------------------------
---- Collection of type a --------------------
----------------------------------------------
infixl 5  >-

class Collection a where      
 eleM :: (Ord b, Eq b) => b -> a b -> Bool
 uni, isc :: Ord b => a b -> a b -> a b
 (>-) :: Ord b => a b -> a b -> a b
 empty :: Eq b => a b
 elems :: Eq b => a b -> [b]
 singleton :: b -> a b

instance Collection [] where -- TODO Vervangen door 'Collection Set.Set' en fouten één voor één oplossen
 eleM         = elem
 xs `uni` ys  = nub xs++(ys>-xs)
 xs `isc` ys  = [y | y<-nub ys, y `Set.member` Set.fromList xs]
 xs >- ys     = [x | x<-nub xs, x `Set.notMember` Set.fromList ys]
 empty        = []
 elems        = nub
 singleton e  = [e]

instance Collection Set.Set where
 eleM      = Set.member
 uni       = Set.union
 isc       = Set.intersection
 (>-)      = Set.difference
 empty     = Set.empty
 elems     = Set.elems
 singleton = Set.singleton