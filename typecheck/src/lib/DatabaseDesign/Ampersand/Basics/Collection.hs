{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Basics.Collection
  (  Collection ( eleM
                , uni
                , isc
                ,(>-)
                ,empty
                ,elems)
  )where
   ----------------------------------------------
   ---- Collection of type a --------------------
   ----------------------------------------------
   infixl 5  >-

   class Collection a where      -- TODO Vervangen door efficient algorithme: Data.Set
    eleM :: Eq b => b -> a b -> Bool  
    uni, isc :: Eq b => a b -> a b -> a b  
    (>-) :: Eq b => a b -> a b -> a b
    empty :: Eq b => a b
    elems :: Eq b => a b -> [b]

   instance Collection [] where
    eleM         = elem
    xs `uni` ys  = xs++(ys>-xs)
    xs `isc` ys  = [y | y<-ys, y `elem` xs]
    xs >- ys     = [x | x<-xs, x `notElem` ys]
    empty        = []
    elems        = id
