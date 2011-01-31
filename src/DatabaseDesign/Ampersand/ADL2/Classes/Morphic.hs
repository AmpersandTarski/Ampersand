{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -XMultiParamTypeClasses  #-} -- to allow multi-parameter classes
-- | This module contains several classes with associated function
--   for several *Relation-like* constructs
module Data.Ampersand.Classes.Morphic (
   -- * Class definitions
   Morphic(..)
   )where
import Data.Ampersand.Definition

-----------------------------------------------------------
class Association r c a => Morphic r c  where
   multiplicities :: a -> [Prop]
   multiplicities _ = []  -- Default, when nothing has been specified, no properties at all...
   flp            :: a -> a
   isProp         :: a -> Bool  -- > tells whether the argument is a property
   isNot          :: a -> Bool  -- > tells whether the argument is equivalent to I-
   isTrue         :: a -> Bool  -- > tells whether the argument is equivalent to V
   isFalse        :: a -> Bool  -- > tells whether the argument is equivalent to V-
   singleton      :: a -> Bool  -- > tells whether V=I
   singleton e     = isProp e && isTrue e
   equiv          :: a -> a -> Bool
   equiv m m' = source m==source m'&&target m==target m' || source m==target m'&&target m==source m'
   isFunction :: a -> Bool     
   isFunction m    = not(isUni m) && not(isTot m)
   isFlpFunction :: a -> Bool
   isFlpFunction m = not(isSur m) && not(isInj m)
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

instance Morphic Declaration Concept where
 multiplicities d = case d of
        Sgn {}       -> decprps_calc d
        Isn{}        -> [Uni,Tot,Inj,Sym,Asy,Trn,Rfx]
                     ++ [Sur | degen d == despc d]
        Iscompl{}    -> [Sym]
        Vs{}         -> [Tot,Sur]
 flp d = case d of
        Sgn {}       -> d{ desrc   = detrg d
                         , detrg   = desrc d
                         , decprps = flipProps (decprps d)
                         , decprps_calc = flipProps (decprps_calc d)
                         , decprL  = ""
                         , decprM  = ""
                         , decprR  = ""
                         , decpopu = map flipPair (decpopu d)
                         }
        Isn{}        -> d
        Iscompl{}    -> d
        Vs{}         -> d
 isProp d = case d of         -- > tells whether the argument is a property.
        Sgn {}       -> not(isAsy d) && not(isSym d)
        Isn{}        -> True
        Iscompl{}    -> False
        Vs{}         -> (degen d == despc d)
 isNot d = case d of          -- > tells whether the argument is equivalent to I-
        Iscompl{}    -> True   
        _            -> False
 isTrue d = case d of 
        Vs{}         -> True
        _            -> False
 isFalse _ = False

   
-----------------------------------------------------------
class Association r c a => MorphicId r c where
 isIdent        :: a -> Bool  -- > tells whether the argument is equivalent to I
  
instance MorphicId Declaration Concept where 
 isIdent d = case d of
     Isn{} -> True   -- > tells whether the argument is equivalent to I
     _     -> False

-----------------------------------------------------------
class Association r c a => Signaling r c where
 isSignal       :: a -> Bool  -- > tells whether the argument refers to a signal
    
instance Signaling Declaration Concept where
 isSignal d = case d of
     Sgn {} -> deciss d
     _      -> False


------------------ *hidden auxilliary functions*
flipProps :: Props -> Props
flipProps ps = map flipProp ps 
  where
   flipProp :: Prop -> Prop
   flipProp Uni = Inj
   flipProp Tot = Sur
   flipProp Sur = Tot
   flipProp Inj = Uni
   flipProp x = x

