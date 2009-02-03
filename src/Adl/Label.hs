
module Adl.Label where
   import Adl.FilePos
   data Label = Lbl { lblnm   :: String
                    , lblpos  :: FilePos
                    , lblstrs :: [[String]]
                    }
   instance Eq Label where
    l==l' = lblnm l==lblnm l'

