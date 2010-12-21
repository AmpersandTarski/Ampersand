{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -XTypeSynonymInstances -XMultiParamTypeClasses -XFlexibleInstances#-}
module Adl2.Concept  
where
  import Adl.Prop       ( Prop(..))
  import CommonClasses  ( Identified(..)
                        , ABoolAlg(..)
                        , Conceptual(..)
                        , Morphics(..)
                        )
  import Collection     ((>-))
  import Typology       (Typologic)
  
  data Concept -- ^The basic Concept
     = C   { cptnm :: String    -- ^The name of this Concept
           , cptgE :: GenR
           , cptos :: Maybe [String]  -- ^Atoms
           }
  instance Typologic Concept
  
  cptos' :: Concept -> [String]
  cptos' C{cptos=(Just x)} = x
  cptos' _ = []

  type Sign  = (Concept,Concept) 
  type GenR  = Concept->Concept->Bool

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Concept                       ***
-- \***********************************************************************
  instance Eq Concept where
   C a _ _ == C b _ _ = a==b

  instance Show Concept where
   showsPrec _ c = showString (name c)
 
  instance Identified Concept where
   name = cptnm

  instance Association Concept Concept where
   source x = x
   target x = x
  instance Ord Concept where
   a@(C _ gE _) <= b = a `gE` b

  instance ABoolAlg Concept where
   glb a b | b <= a = b
           | a <= b = a
           | otherwise = error ("!Fatal (module Concept 66): glb undefined: a="++show a++", b="++show b)
   lub a b | a <= b = b
           | b <= a = a
           | otherwise = error ("!Fatal (module Concept 69): lub undefined: a="++show a++", b="++show b)

  cptnew :: String -> Concept
  cptnew nm = C{ cptnm=nm, cptgE = (==), cptos = Nothing}

  class Association a c where
    source, target :: a -> c
    sign           :: a -> (c,c)
    sign x = (source x,target x) 
    swap           :: a -> (c,c)
    swap x = (target x,source x)
    homogeneous :: a -> Bool
    homogeneous s = source s == target s


--  instance Association (c,c) c where
--    source (src, _ ) = src
--    target (_ , tgt) = tgt

  class Association c a => MorphicId c a where
   isIdent        :: a -> Bool  -- > tells whether the argument is equivalent to I

--  instance MorphicId Concept where
--   isIdent _ = True    -- > tells whether the argument is equivalent to I
  
  class Association c a => Signaling c a where
   isSignal       :: a -> Bool  -- > tells whether the argument refers to a signal
    
   
--  class Association c a => Morphic c a where
--   multiplicities :: a -> [Prop]
--   multiplicities _ = []  --WAAROM? Stef, dit stond er eerst, maar ik geloof niet dat dat goed is. zelfs niet als default regel. Toch?
--                          --DAAROM! Als default regel is er niets mis mee. Als je niets specificeert heeft het ding geen multipliciteitseigenschappen....
--   flp            :: a -> a
----   isIdent        :: a -> Bool  -- > tells whether the argument is equivalent to I
--   isProp         :: a -> Bool  -- > tells whether the argument is a property
--   isNot          :: a -> Bool  -- > tells whether the argument is equivalent to I-
--   isTrue         :: a -> Bool  -- > tells whether the argument is equivalent to V
--   isFalse        :: a -> Bool  -- > tells whether the argument is equivalent to V-
-- --  isSignal       :: a -> Bool  -- > tells whether the argument refers to a signal
--   singleton      :: a -> Bool  -- > tells whether V=I
--   singleton e     = isProp e && isTrue e
--   equiv          :: a -> a -> Bool
--   equiv m m' = source m==source m'&&target m==target m' || source m==target m'&&target m==source m'
--   isFunction :: a -> Bool     
--   isFunction m   = null ([Uni,Tot]>-multiplicities m)
--   isFlpFunction :: a -> Bool
--   isFlpFunction m = null ([Sur,Inj]>-multiplicities m)
--   isTot :: a -> Bool  -- 
--   isTot d = Tot `elem` multiplicities d
--   isUni :: a -> Bool  -- 
--   isUni d = Uni `elem` multiplicities d
--   isSur :: a -> Bool  -- 
--   isSur d = Sur `elem` multiplicities d
--   isInj :: a -> Bool  -- 
--   isInj d = Inj `elem` multiplicities d
--   isRfx :: a -> Bool  -- 
--   isRfx d = Rfx `elem` multiplicities d
--   isTrn :: a -> Bool  -- 
--   isTrn d = Trn `elem` multiplicities d
--   isSym :: a -> Bool  -- 
--   isSym d = Sym `elem` multiplicities d
--   isAsy :: a -> Bool  -- 
--   isAsy d = Asy `elem` multiplicities d

--  instance Morphic Concept where
--   multiplicities _ = [Uni,Tot,Sur,Inj,Sym,Trn,Rfx,Asy]
--   flp c = c
--   isIdent c = True    -- > tells whether the argument is equivalent to I
--   isProp _ = True    -- > tells whether the argument is equivalent to I
--   isNot _   = False   -- > tells whether the argument is equivalent to I-
--   isTrue c = singleton c
--   isFalse _ = False
--   singleton S = True
--   singleton I1{} = True
--   singleton _ = False

--  instance Signaling Concept where
--   isSignal _ = False
   
--  isSingleton :: Concept -> Bool
--  isSingleton s = s == S
  
  instance Conceptual Concept where
   conts c@C{}    = cptos c

