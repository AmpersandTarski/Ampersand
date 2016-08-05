module Database.Design.Ampersand.Basics.Collection
  (  Collection ( eleM
                , uni
                , isc
                ,(>-)
                ,empty
                ,elems)
  )where
import qualified Data.Set as Set
----------------------------------------------
---- Collection of type a --------------------
----------------------------------------------
infixl 5  >-

class Collection a where      
 eleM :: Eq b => b -> a b -> Bool
 uni, isc :: Ord b => a b -> a b -> a b
 (>-) :: Ord b => a b -> a b -> a b
 empty :: Eq b => a b
 elems :: Eq b => a b -> [b]

instance Collection [] where -- TODO Vervangen door 'Collection Set.Set' en fouten één voor één oplossen
 eleM         = elem
 xs `uni` ys  = xs++(ys>-xs)
 xs `isc` ys  = [y | y<-ys, y `Set.member` Set.fromList xs]
 xs >- ys     = [x | x<-xs, x `Set.notMember` Set.fromList ys]
 empty        = []
 elems        = id
