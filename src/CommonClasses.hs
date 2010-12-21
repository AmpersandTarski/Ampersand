{-# OPTIONS_GHC -Wall #-}
module CommonClasses
  (  Identified(name,rename),uniqueNames , showSign
   , ABoolAlg(glb,lub,order)
   , Conceptual(conts)
   , Morphics(anything)
   )

  where

   import Collection(rd)
   import Data.Maybe
   import Data.List
   import Auxiliaries    (eqCl)
   import Char (toLower)
   
   ----------------------------------------------
   class Identified a where
    name   :: a->String
    rename :: a->String->a
    rename x _ = error ("!Fatal (module CommonClasses 19): some Identified element named " ++ name x ++ " cannot be renamed.")

   --the function uniqueNames ensures case-insensitive unique names like sql plug names
   uniqueNames :: (Identified a) => [String]->[a]->[a]
   uniqueNames taken xs
    = [p | cl<-eqCl (map toLower.name) xs  -- each equivalence class cl contains (identified a) with the same map toLower (name p)
         , p <-if name (head cl) `elem` taken || length cl>1
               then [rename p (name p++show i)| (p,i)<-zip cl [(1::Int)..]]
               else cl
      ]

   showSign :: Identified a => [a] -> String
   showSign cs = "["++(intercalate "*".rd.map name) cs++"]"

   instance Identified a => Identified [a] where
    name [] = ""
    name (i:_) = name i

   class (Show a,Ord a) => ABoolAlg a where
    glb,lub :: a -> a -> a
    order :: a -> a -> Bool
    glb a b | b <= a = b
            | a <= b = a
            | otherwise  = error "!Fatal (module CommonClasses 34): glb undefined"
    lub a b | a <= b = b
            | b <= a = a
            | otherwise = error "!Fatal (module CommonClasses 37): lub undefined"
    order a b | a <= b = True
              | b <= a = True
              | otherwise = False

   instance (ABoolAlg a,ABoolAlg b) => ABoolAlg (a,b) where
    glb a b | a <= b = a
            | b <= a = b
            | otherwise = error ("!Fatal (module CommonClasses 45): glb undefined: a="++show a++", b="++show b)
    lub a b | b <= a = a
            | a <= b = b
            | otherwise = error ("!Fatal (module CommonClasses 48): lub undefined: a="++show a++", b="++show b)

   class Conceptual a where
    -- | the set of all instances in a concept (if we know it, Nothing otherwise)
    conts :: a -> Maybe [String] 

   instance Conceptual a => Conceptual [a] where
    conts as | elem Nothing c = Nothing
             | otherwise = Just$ rd (concat c')
          where c = map conts as
                c' = map fromJust c

   class Morphics a where
    anything :: a -> Bool

   instance Morphics a => Morphics [a] where
    anything xs = and (map anything xs)
   instance (Morphics a,Morphics b) => Morphics (a,b) where
    anything (x,y) = anything x && anything y






