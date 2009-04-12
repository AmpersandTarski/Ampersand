{-# OPTIONS_GHC -Wall #-}
module Adl.Label (Label(..))
where
   import Adl.FilePos  (FilePos)
   
   data Label = Lbl { lblnm   :: String
                    , lblpos  :: FilePos
                    , lblstrs :: [[String]]
                    }
   instance Eq Label where
    l==l' = lblnm l==lblnm l'

