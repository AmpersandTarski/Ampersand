module DatabaseDesign.Ampersand.ADL1.Lattices where
import qualified DatabaseDesign.Ampersand.Core.Poset as Poset (Ordering)

class Lattice a where
  join :: a -> a -> a
  meet :: a -> a -> a
  lCompare :: a -> a -> Poset.Ordering

