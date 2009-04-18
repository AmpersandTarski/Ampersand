{-# OPTIONS_GHC -Wall #-}
module Collection
  (  Collection ( eleM
                , uni
                , isc
                ,(>-)
                ,empty
                ,elems
                ,rd)
  )where
   ----------------------------------------------
   ---- Collection of type a --------------------
   ----------------------------------------------
   infixl 5  >-

   class Collection a where
    eleM     :: Eq b => b -> a b -> Bool  
    uni, isc :: Eq b => a b -> a b -> a b  
    (>-)     :: Eq b => a b -> a b -> a b
    empty    :: Eq b => a b
    elems    :: Eq b => a b -> [b]
    rd       :: Eq b => a b -> a b

   instance Collection [] where
    eleM        = any . (==)
    xs `uni` ys = xs++(ys>-xs)
    xs `isc` ys = [y| y<-ys, y `elem` xs]
    xs >- ys    = [x| x<-xs, not (x `elem` ys)]
    empty       = []
    elems       = id
    rd []       = empty
    rd (x:xs)   = x: rd [e|e<-xs, e/=x]
