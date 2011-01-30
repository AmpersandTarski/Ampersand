{-# OPTIONS_GHC -Wall -XFlexibleContexts -XUndecidableInstances #-}
module DatabaseDesign.Ampersand.ADL.Concept ( Concept(..),Conceptual(..)
                   , Sign,GenR, SpecHierarchy(..)
                   , Signaling(..)
                   , cptnew,cptAnything,cptS,cptos'
                   ) 
where
  import DatabaseDesign.Ampersand.Core.Basics       (Typologic)

-- The following definition of concept is used in the type checker only.
-- It is called Concept, meaning "type checking concept"
  data Concept
     = C   { cptnm :: String    -- ^The name of this Concept
           , cptgE :: GenR Concept
           , cptos :: Maybe [String]  -- ^Atoms
           }  -- ^C nm gE cs represents the set of instances cs by name nm.
     | S      -- ^The universal Singleton: 'I'['Anything'] = 'V'['Anything']
     | Anything -- ^Really Anything!
     | NOthing  -- ^Nothing at all

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Concept                     ***
-- \***********************************************************************

  instance Eq Concept where
   C a _ _ == C b _ _ = a==b
   S == S = True
   Anything == Anything = True
   NOthing == NOthing = True
   _ == _ = False

  instance Ord Concept where
   NOthing <= _  = False
   _ <= NOthing  = True
   Anything <= _ = True
   _ <= Anything = False
   a@(C _ gE _) <= b = a `gE` b
   a <= b = a==b

{- class SpecHierarchy was previously called ABoolAlg -}
-- | class SpecHierarchy supports generalisation and specialisation.
--   a <= b means that concept a is more generic than b and b is more specific than a. For instance 'Animal' <= 'Elephant'
--   The generalization relation <= between concepts is a partial order.
--   Partiality reflects the fact that not every pair of elements of a specification need be related.
--   A partial order is by definition reflexive, antisymmetric, and transitive)
--   For every concept a and b in Ampersand, the following rule holds: a<=b || b<=a || a\= b
  class Ord c => SpecHierarchy c where
   glb,lub    :: c -> c -> c
   comparable :: c -> c -> Bool
   order      :: c -> GenR c
   glb a b | b <= a = b
           | a <= b = a
           | otherwise  = error "!Fatal (module Concept 56): glb undefined"
   lub a b | a <= b = b
           | b <= a = a
           | otherwise = error "!Fatal (module Concept 59): lub undefined"
   comparable a b | a <= b = True
                  | b <= a = True
                  | otherwise = False
   order _ = (<=)

  instance Show Concept => SpecHierarchy Concept where
   glb a b | b <= a = b
           | a <= b = a
           | otherwise = error ("!Fatal (module Concept 68): glb undefined: a="++show a++", b="++show b)
   lub a b | a <= b = b
           | b <= a = a
           | otherwise = error ("!Fatal (module Concept 71): lub undefined: a="++show a++", b="++show b)

  instance (Show a, Show b, SpecHierarchy a, SpecHierarchy b) => SpecHierarchy (a,b) where
   glb a b | a <= b = a
           | b <= a = b
           | otherwise = error ("!Fatal (module Concept 76): glb undefined: a="++show a++", b="++show b)
   lub a b | b <= a = a
           | a <= b = b
           | otherwise = error ("!Fatal (module Concept 79): lub undefined: a="++show a++", b="++show b)

  class Eq c => Conceptual c where
   isSingleton :: c -> Bool

  instance Conceptual Concept where
   isSingleton S = True
   isSingleton _ = False

  instance Typologic Concept
  
  cptos' :: Concept -> [String]
  cptos' C{cptos=(Just x)} = x
  cptos' _ = []

  type Sign = (Concept,Concept)
  type GenR c = c->c->Bool

  cptS :: Concept
  cptS = S                    -- constructor
  cptAnything :: Concept
  cptAnything = Anything      -- constructor
  cptnew :: String -> Concept
  cptnew nm = C{ cptnm=nm, cptgE = (==), cptos = Nothing}

  class Signaling a where
   isSignal       :: a -> Bool  -- > tells whether the argument refers to a signal
    

