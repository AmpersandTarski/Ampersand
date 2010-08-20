{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -XTypeSynonymInstances #-}
module Adl.Concept ( Concept(..),Concepts
                   , Sign,GenR
                   , Association(..),Morphic(..),MorphicId(..)
                   , isSingleton
                   , cptnew,cptAnything,cptS,cptos'
                   ) 
where
   import Adl.Prop       ( Prop(..))
   import CommonClasses  ( Identified(..)
                         , ABoolAlg(..)
                         , Conceptual(..)
                         , Morphics(..)
                         )
   import Collection     ((>-))
   import Typology       (Typologic)
   import {-# SOURCE #-} Adl.Expression (Expression(..))
   
   type Concepts = [Concept]
   data Concept
      = C   { cptnm :: String    -- ^The name of this Concept
            , cptgE :: GenR 
            , cptos :: Maybe [String]  -- ^Atoms
            }  -- ^C nm gE cs represents the set of instances cs by name nm.
      | S  -- ^The universal Singleton: 'I'['Anything'] = 'V'['Anything']
      | DExp Expression -- ^A concept containing pairs representing the population in the expression. The letter D stands for derived
      | Anything -- ^Really Anything!
      | NOthing  -- ^Nothing at all

   instance Typologic Concept
   
   cptos' :: Concept -> [String]
   cptos' C{cptos=(Just x)} = x
   cptos' _ = []

   type Sign = (Concept,Concept) 
   type GenR = Concept->Concept->Bool

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Concept                       ***
-- \***********************************************************************
   instance Eq Concept where
    C a _ _ == C b _ _ = a==b
    S == S = True
    Anything == Anything = True
    NOthing == NOthing = True
    DExp a == DExp b = a==b
    _ == _ = False
   instance Show Concept where
    showsPrec _ c = showString (name c)
   instance Identified Concept where
    name (C {cptnm = nm}) = nm
    name S = "S"
    name Anything   = "Anything"
    name NOthing    = "NOthing"
    name (DExp _)   = error "Derived concepts have no name (on line 53 in Concept.hs)"

   instance Association Concept where
    source c = c
    target c = c
   instance Ord Concept where
    NOthing <= _  = False
    _ <= NOthing  = True
    Anything <= _ = True
    _ <= Anything = False
    a@(C _ gE _) <= b = a `gE` b
    (DExp _) <= _ = error "Derived concepts are not ordered (on line 64 in Concept.hs)"
    _ <= (DExp _) = error "Derived concepts are not ordered (on line 65 in Concept.hs)"
    a <= b = a==b

   instance ABoolAlg Concept where
    glb a b | b <= a = b
            | a <= b = a
            | otherwise = error ("!Fatal (module Concept 66): glb undefined: a="++show a++", b="++show b)
    lub a b | a <= b = b
            | b <= a = a
            | otherwise = error ("!Fatal (module Concept 69): lub undefined: a="++show a++", b="++show b)

   cptS :: Concept
   cptS = S                    -- constructor
   cptAnything :: Concept
   cptAnything = Anything      -- constructor
   cptnew :: String -> Concept
   cptnew nm = C{ cptnm=nm, cptgE = (==), cptos = Nothing}

   class Association a where
     source, target :: a -> Concept
     sign           :: a -> Sign
     sign x = (source x,target x) 
     homogeneous :: a -> Bool
     homogeneous s = source s == target s


   instance Association Sign where
     source (src, _ ) = src
     target (_ , tgt) = tgt
 
   class Association a => MorphicId a where
    isIdent        :: a -> Bool  -- > tells whether the argument is equivalent to I

   instance MorphicId Concept where
    isIdent _ = True    -- > tells whether the argument is equivalent to I
   
     
    
   class Association a => Morphic a where
    multiplicities :: a -> [Prop]
    multiplicities _ = []  --WAAROM? Stef, dit stond er eerst, maar ik geloof niet dat dat goed is. zelfs niet als default regel. Toch?
                           --DAAROM! Als default regel is er niets mis mee. Als je niets specificeert heeft het ding geen multipliciteitseigenschappen....
    flp            :: a -> a
--    isIdent        :: a -> Bool  -- > tells whether the argument is equivalent to I
    isProp         :: a -> Bool  -- > tells whether the argument is a property
    isNot          :: a -> Bool  -- > tells whether the argument is equivalent to I-
    isTrue         :: a -> Bool  -- > tells whether the argument is equivalent to V
    isFalse        :: a -> Bool  -- > tells whether the argument is equivalent to V-
    isSignal       :: a -> Bool  -- > tells whether the argument refers to a signal
    singleton      :: a -> Bool  -- > tells whether V=I
    singleton e     = isProp e && isTrue e
    equiv          :: a -> a -> Bool
    equiv m m' = source m==source m'&&target m==target m' || source m==target m'&&target m==source m'
    isFunction :: a -> Bool     
    isFunction m   = null ([Uni,Tot]>-multiplicities m)
    isFlpFunction :: a -> Bool
    isFlpFunction m = null ([Sur,Inj]>-multiplicities m)
    isTot :: a -> Bool  -- 
    isTot d = Tot `elem` multiplicities d
    isUni :: a -> Bool  -- 
    isUni d = Uni `elem` multiplicities d
    isSur :: a -> Bool  -- 
    isSur d = Sur `elem` multiplicities d
    isInj :: a -> Bool  -- 
    isInj d = Inj `elem` multiplicities d
    isRfx :: a -> Bool  -- 
    isRfx d = Rfx `elem` multiplicities d
    isTrn :: a -> Bool  -- 
    isTrn d = Trn `elem` multiplicities d
    isSym :: a -> Bool  -- 
    isSym d = Sym `elem` multiplicities d
    isAsy :: a -> Bool  -- 
    isAsy d = Asy `elem` multiplicities d

   instance Morphic Concept where
    multiplicities _ = [Uni,Tot,Sur,Inj,Sym,Trn,Rfx,Asy]
    flp c = c
--    isIdent c = True    -- > tells whether the argument is equivalent to I
    isProp _ = True    -- > tells whether the argument is equivalent to I
    isNot _   = False   -- > tells whether the argument is equivalent to I-
    isTrue c = singleton c
    isFalse _ = False
    isSignal _ = False
    singleton S = True
    singleton _ = False


   isSingleton :: Concept -> Bool
   isSingleton s = s == S
   
   instance Conceptual Concept where
    conts c@C{}    = cptos c
    conts S        = Nothing -- S has exactly one atom, but that atom may not be referred to
    conts Anything = Nothing
    conts NOthing  = Nothing

   instance Morphics Concept where
    anything c = c == Anything
   
