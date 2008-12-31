{-# LINE 1 "Views.lhs" #-}
#line 1 "Views.lhs"
  module Views ( viewDataset )
  where

   import CommonClasses ( Identified(name))
   import Auxiliaries
          ( chain
          , haskellIdentifier
          )
   import ADLdef
   import ShowADL
   import Dataset
   import Data.Fspec

   viewDataset :: Context -> String
   viewDataset context
    = chain "\n\n" [showADL (makeView d)| d<-makeDatasets context]
      where
       makeView (DS c pths)
             = Obj (haskellIdentifier (name c))
                   posNone
                   (Tm (mIs c))
                   [ Obj (name m) posNone (Tm m) [] | m<-pths ]
       makeView (BR m) = Obj (name m) posNone (Tm m) []
