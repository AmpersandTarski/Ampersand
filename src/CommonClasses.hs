{-# OPTIONS_GHC -Wall #-}
  module CommonClasses
  (  Identified(name,typ) , showSign
   , ABoolAlg(glb,lub,order)
   , Explained(explain)
   , Conceptual(conts)
   , Morphics(anything)
   )

  where

   import Collection(rd)
   import Strings(chain)
   ----------------------------------------------
   class Identified a where
    name   :: a->String
    typ    :: a->String
 
   showSign :: Identified a => [a] -> String
   showSign cs = "["++(chain "*".rd.map name) cs++"]"

   instance Identified a => Identified [a] where
    name [] = ""
    name (i:_) = name i
    typ  [] = ""
    typ  (i:_) = typ i

   class (Show a,Ord a) => ABoolAlg a where
    glb,lub :: a -> a -> a
    order :: a -> a -> Bool
    glb a b | b <= a = b
            | a <= b = a
            | otherwise  = error "(module CommonClasses) glb undefined"
    lub a b | a <= b = b
            | b <= a = a
            | otherwise = error "(module CommonClasses) lub undefined"
    order a b | a <= b = True
              | b <= a = True
              | otherwise = False

   instance (ABoolAlg a,ABoolAlg b) => ABoolAlg (a,b) where
    glb a b | a <= b = a
            | b <= a = b
            | otherwise = error ("(module CommonClasses) Fatal: glb undefined: a="++show a++", b="++show b)
    lub a b | b <= a = a
            | a <= b = b
            | otherwise = error ("(module CommonClasses) Fatal: lub undefined: a="++show a++", b="++show b)

   class Explained a where
    explain :: a -> String
    explain _ = error "(module CommonClasses) explain undefined"

   class Conceptual a where
    conts      :: a -> [String]                   -- the set of all instances in a concept

   instance Conceptual a => Conceptual [a] where
    conts = rd . concat . map conts

   class Morphics a where
    anything       :: a -> Bool

   instance Morphics a => Morphics [a] where
    anything xs = and (map anything xs)
   instance (Morphics a,Morphics b) => Morphics (a,b) where
    anything (x,y) = anything x && anything y






