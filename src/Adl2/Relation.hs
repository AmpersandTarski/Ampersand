{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances#-}
module Adl2.Relation where
  import CommonClasses  ( Identified(..),showSign) 
  import Adl2.Concept
  
  data Relation c            -- ^The basic Relation
     = Rel { relnm :: String -- ^The name of the relation
           , relsrc :: c     -- ^Source concept
           , reltrg :: c     -- ^Target concept
           , relflp :: Bool  -- ^Whether this relation is flipped
           }
      | I  { reltyp :: c }   -- ^identity relation
      | V  { reltyp :: c }   -- ^full relation
        deriving Eq

  instance Identified (Relation c) where
    name r = case r of
               Rel{} -> relnm r
               I{}   -> "I"
               V{}   -> "V"
               
  instance Show (Relation c) where
    showsPrec _ r = showString (name r++
       case r of
        Rel{relflp = False} -> showSign [target r,source r]++"~"
        _                   -> showSign [source r,target r]
                               )
                               
  instance Association (Relation c) c where
    source r@Rel{} = relsrc r
    target r = reltrg r
    