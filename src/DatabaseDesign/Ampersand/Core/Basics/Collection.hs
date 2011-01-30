{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Core.Basics.Collection
  (  Collection ( eleM
                , uni
                , isc
                ,(>-)
                ,empty
                ,elems
                ,rd
                ,rd')
  )where
   ----------------------------------------------
   ---- Collection of type a --------------------
   ----------------------------------------------
   infixl 5  >-

   class Collection a where      -- TODO Vervangen door efficient algorithme: Data.Set
    eleM     :: Eq b => b -> a b -> Bool  
    uni, isc :: Eq b => a b -> a b -> a b  
    (>-)     :: Eq b => a b -> a b -> a b
    empty    :: Eq b => a b
    elems    :: Eq b => a b -> [b]
    rd       :: Eq b => a b -> a b
    rd'      :: Eq c => (b->c) -> a b -> a b    -- TODO Deze functie slaat nergens op. Verwijderen of toelichten. 

   instance Collection [] where
    eleM         = any . (==)
    xs `uni` ys  = xs++(ys>-xs)
    xs `isc` ys  = [y| y<-ys, y `elem` xs]
    xs >- ys     = [x| x<-xs, not (x `elem` ys)]
    empty        = []
    elems        = id
    rd []        = []
    rd (x:xs)    = x: rd [e|e<-xs, e/=x]
    rd' _ []     = []
    rd' f (x:xs) = x: rd' f [e|e<-xs, f e/=f x]
