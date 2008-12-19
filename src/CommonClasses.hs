{-# LINE 1 "CommonClasses.lhs" #-}
#line 1 "CommonClasses.lhs"
  module CommonClasses
  (  Identified(name)
   , ABoolAlg(glb,lub,order)
   , Explained(explain)
   , Conceptual(conts)
   , Morphics(anything)
   )

  where
  -- import Strings()
   import Collection(rd)
   ----------------------------------------------
   class Identified a where
    name   :: a->String
    -- idName :: a->String

   instance Identified a => Identified [a] where
    name [] = ""
    name (i:is) = name i
    -- idName c = idNam (name c)

   class Ord a => ABoolAlg a where
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

   instance (Show a,Show b,ABoolAlg a,ABoolAlg b) => ABoolAlg (a,b) where
    glb a b | a <= b = a
            | b <= a = b
            | otherwise = error ("(module CommonClasses) Fatal: glb undefined: a="++show a++", b="++show b)
    lub a b | b <= a = a
            | a <= b = b
            | otherwise = error ("(module CommonClasses) Fatal: lub undefined: a="++show a++", b="++show b)

   class Explained a where
    explain :: a -> String
    explain a = error "(module CommonClasses) explain undefined"

   class Conceptual a where
    conts      :: a -> [String]                   -- the set of all instances in a concept

   instance Conceptual a => Conceptual [a] where
    conts                                         = rd . concat . map conts

   class Morphics a where
    anything       :: a -> Bool

   instance Morphics a => Morphics [a] where
    anything xs = and (map anything xs)
   instance (Morphics a,Morphics b) => Morphics (a,b) where
    anything (x,y) = anything x && anything y






