module Ampersand.Classes.Relational
   ( HasProps(..)
   , Relational(..)
   ) where

import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Core.ParseTree(Prop(..),Props)
import           Data.Maybe
import qualified Data.Set as Set

class HasProps r where
    properties :: r -> Props
class Relational r where
    isProp :: r -> Bool  -- > tells whether the argument is a property
    isImin :: r -> Bool  -- > tells whether the argument is equivalent to I-
    isTrue :: r -> Bool  -- > tells whether the argument is equivalent to V
    isFalse :: r -> Bool  -- > tells whether the argument is equivalent to V-
    isFunction :: r -> Bool
    isTot :: r -> Bool  --
    isUni :: r -> Bool  --
    isSur :: r -> Bool  --
    isInj :: r -> Bool  --
    isRfx :: r -> Bool  --
    isIrf :: r -> Bool  --
    isTrn :: r -> Bool  --
    isSym :: r -> Bool  --
    isAsy :: r -> Bool  --
    isIdent :: r -> Bool  -- > tells whether the argument is equivalent to I
    isEpsilon :: r -> Bool  -- > tells whether the argument is equivalent to I

instance HasProps Relation where
    properties d = fromMaybe (decprps d) (decprps_calc d)

isSingleton :: A_Concept -> Bool
isSingleton ONE = True
isSingleton _   = False

-- The function "properties" does not only provide the properties provided by the Ampersand user,
-- but tries to derive the most obvious multiplicity constraints as well. The more multiplicity constraints are known,
-- the better the data structure that is derived.
-- Not every constraint that can be proven is obtained by this function. This does not hurt Ampersand.
instance HasProps Expression where
    properties expr = case expr of
     EDcD dcl   -> properties dcl
     EDcI{}     -> Set.fromList [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
     EEps a sgn -> Set.fromList $ [Tot | a == source sgn]++[Sur | a == target sgn] ++ [Uni,Inj]
     EDcV sgn   -> Set.fromList $ 
                   [Tot]
                 ++[Sur]
                 ++[Inj | isSingleton (source sgn)]
                 ++[Uni | isSingleton (target sgn)]
                 ++[Asy | isEndo sgn, isSingleton (source sgn)]
                 ++[Sym | isEndo sgn]
                 ++[Rfx | isEndo sgn]
                 ++[Trn | isEndo sgn]
     EBrk f     -> properties f
     ECps (l,r) -> Set.fromList $ [m | m<-Set.elems (properties l `Set.intersection` properties r)
                                  , m `elem` [Uni,Tot,Inj,Sur]] -- endo properties can be used and deduced by and from rules: many rules are properties (TODO)
     EPrd (l,r) -> Set.fromList $ [Tot | isTot l]++[Sur | isSur r]++[Rfx | isRfx l&&isRfx r]++[Trn]
     EKl0 e'    -> Set.fromList [Rfx,Trn] `Set.union` (properties e' Set.\\ Set.fromList [Uni,Inj])
     EKl1 e'    -> Set.singleton Trn `Set.union` (properties e' Set.\\ Set.fromList [Uni,Inj])
     ECpl e'    -> Set.singleton Sym `Set.intersection` properties e'
     EFlp e'    -> Set.fromList [fromMaybe m $ lookup m [(Uni,Inj),(Inj,Uni),(Sur,Tot),(Tot,Sur)] | m <- Set.elems $ properties e'] -- switch Uni<->Inj and Sur<->Tot, keeping the others the same
     EMp1{}     -> Set.fromList [Uni,Inj,Sym,Asy,Trn]
     _          -> Set.empty

instance Relational Expression where        -- TODO: see if we can find more multiplicity constraints...
 -- |  isTrue e == True   means that e is true, i.e. the population of e is (source e * target e).
 --    isTrue e == False  does not mean anything.
 --    the function isTrue is meant to produce a quick answer, without any form of theorem proving.
 isTrue expr
  = case expr of
     EEqu (l,r) -> l == r
     EInc (l,_) -> isTrue l
     EIsc (l,r) -> isTrue l && isTrue r
     EUni (l,r) -> isTrue l || isTrue r
     EDif (l,r) -> isTrue l && isFalse r
     ECps (l,r) | Uni `elem` properties l && Tot `elem` properties l -> isTrue r
                | Sur `elem` properties r && Sur `elem` properties r -> isTrue l
                | otherwise                          -> isTrue l && isTrue r
     EPrd (l,r) -> isTrue l && isTrue r || isTot l && isSur r || isRfx l && isRfx r
     EKl0 e     -> isTrue e
     EKl1 e     -> isTrue e
     EFlp e     -> isTrue e
     ECpl e     -> isFalse e
     EDcD{}     -> False
     EDcI c     -> isSingleton c
     EEps i _   -> isSingleton i
     EDcV{}     -> True
     EBrk e     -> isTrue e
     _          -> False  -- TODO: find richer answers for ERrs, ELrs, EDia, ERad, and EMp1

 -- |  isFalse e == True   means that e is false, i.e. the population of e is empty.
 --    isFalse e == False  does not mean anything.
 --    the function isFalse is meant to produce a quick answer, without any form of theorem proving.
 isFalse expr
  = case expr of
     EEqu (l,r) -> l == notCpl r
     EInc (_,r) -> isFalse r
     EIsc (l,r) -> isFalse r || isFalse l
     EUni (l,r) -> isFalse r && isFalse l
     EDif (l,r) -> isFalse l || isTrue r
     ECps (l,r) -> isFalse r || isFalse l
     EPrd (l,r) -> isFalse r || isFalse l
     EKl0 e     -> isFalse e
     EKl1 e     -> isFalse e
     EFlp e     -> isFalse e
     ECpl e     -> isTrue e
     EDcD{}     -> False
     EDcI{}     -> False
     EEps{}     -> False
     EDcV{}     -> False
     EBrk e     -> isFalse e
     _          -> False  -- TODO: find richer answers for ERrs, ELrs, EDia, and ERad

 isProp expr = Asy `elem` properties expr && Sym `elem` properties expr

 -- |  The function isIdent tries to establish whether an expression is an identity relation.
 --    It does a little bit more than just test on ERel I _.
 --    If it returns False, this must be interpreted as: the expression is definitely not I, an may not be equal to I as far as the computer can tell on face value.
 isIdent expr = (\x -> if x && (source expr /= target expr) 
                       then fatal $ "Something wrong with isIdent." ++ show expr
                       else x
                ) $
   case expr of
     EEqu (l,r) -> isIdent (EIsc (EInc (l,r), EInc (r,l)))    -- TODO: maybe derive something better?
     EInc (l,r) -> isIdent (EUni (ECpl l, r))                 -- TODO: maybe derive something better?
     EIsc (l,r) -> isIdent l && isIdent r
     EUni (l,r) -> isIdent l && isIdent r
     EDif (l,r) -> isIdent l && isFalse r
     ECps (l,r) -> isIdent l && isIdent r
     EKl0 e     -> isIdent e || isFalse e
     EKl1 e     -> isIdent e
     ECpl e     -> isImin e
     EDcD{}     -> False
     EDcI{}     -> True
     EEps{}     -> False
     EDcV sgn   -> isEndo sgn && isSingleton (source sgn)
     EBrk f     -> isIdent f
     EFlp f     -> isIdent f
     _          -> False  -- TODO: find richer answers for ELrs, ERrs, EDia, EPrd, and ERad
 isEpsilon e = case e of
                EEps{} -> True
                _      -> False

 isImin expr' = case expr' of       -- > tells whether the argument is equivalent to I-
     EEqu (l,r) -> isImin (EIsc (EInc (l,r), EInc (r,l)))       -- TODO: maybe derive something better?
     EInc (l,r) -> isImin (EUni (ECpl l, r))                  -- TODO: maybe derive something better?
     EIsc (l,r) -> isImin l && isImin r
     EUni (l,r) -> isImin l && isImin r
     EDif (l,r) -> isImin l && isFalse r
     ECpl e     -> isIdent e
     EDcD{}     -> False
     EDcI{}     -> False
     EEps{}     -> False
     EDcV{}     -> False
     EBrk f     -> isImin f
     EFlp f     -> isImin f
     _          -> False  -- TODO: find richer answers for ELrs, ERrs, and EDia
 isFunction r   = isUni r && isTot r
                 
 isTot r = Tot `elem` properties r
-- isUni r = Uni `elem` properties r
 isUni = isUni' Uni
   where
     isUni' :: Prop -> Expression -> Bool 
     isUni' prop expr 
       = case expr of
            EEqu (_,_) -> False
            EInc (_,_) -> False
            EIsc (l,r) -> isUni' prop l || isUni' prop r
            EUni (_,_) -> z
            EDif (l,_) -> isUni' prop l
            ECps (l,r) -> isUni' prop l && isUni' prop r
            EPrd (_,_) -> z
            EKl0 e     -> isUni' prop e
            EKl1 e     -> isUni' prop e
            EFlp e     -> isUni' (flp prop) e
            ECpl _     -> z
            ELrs _     -> z
            ERrs _     -> z
            EDia _     -> z
            ERad _     -> z
            EDcD d     -> prop `elem` properties d
            EDcI{}     -> True
            EEps{}     -> True
            EDcV{}     -> z
            EBrk e     -> isUni' e
            EMp1{}     -> True
      where
        z = prop `elem` properties expr
 isSur r = Sur `elem` properties r
 isInj r = isUni (flp r)
 isRfx r = Rfx `elem` properties r
 isIrf r = Irf `elem` properties r
 isTrn r = Trn `elem` properties r
 isSym r = Sym `elem` properties r
 isAsy r = Asy `elem` properties r
