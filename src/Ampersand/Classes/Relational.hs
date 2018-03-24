module Ampersand.Classes.Relational
   (Relational(..)
   ) where

import           Ampersand.ADL1.Expression
import           Ampersand.Basics
import           Ampersand.Core.AbstractSyntaxTree
import           Ampersand.Core.ParseTree(Prop(..),Props)
import           Data.Maybe
import qualified Data.Set as Set

class Association r => Relational r where
    properties :: r -> Props
    isProp :: r -> Bool  -- > tells whether the argument is a property
    isImin :: r -> Bool  -- > tells whether the argument is equivalent to I-
    isTrue :: r -> Bool  -- > tells whether the argument is equivalent to V
    isFalse :: r -> Bool  -- > tells whether the argument is equivalent to V-
    isFunction :: r -> Bool
    isFunction r   = Uni `elem` properties r && 
                     Tot `elem` properties r 
    isTot :: r -> Bool  --
    isTot r = Tot `elem` properties r
    isUni :: r -> Bool  --
    isUni r = Uni `elem` properties r
    isSur :: r -> Bool  --
    isSur r = Sur `elem` properties r
    isInj :: r -> Bool  --
    isInj r = Inj `elem` properties r
    isRfx :: r -> Bool  --
    isRfx r = Rfx `elem` properties r
    isIrf :: r -> Bool  --
    isIrf r = Irf `elem` properties r
    isTrn :: r -> Bool  --
    isTrn r = Trn `elem` properties r
    isSym :: r -> Bool  --
    isSym r = Sym `elem` properties r
    isAsy :: r -> Bool  --
    isAsy r = Asy `elem` properties r
    isIdent :: r -> Bool  -- > tells whether the argument is equivalent to I
    isEpsilon :: r -> Bool  -- > tells whether the argument is equivalent to I

instance Relational Relation where
    properties d = fromMaybe (decprps d) (decprps_calc d)
    isProp d = Asy `elem` properties d && Sym `elem` properties d
    isImin _ = False  -- LET OP: Dit kan natuurlijk niet goed zijn, maar is gedetecteerd bij revision 913, toen straffeloos de Iscompl{} kon worden verwijderd.
    isTrue _ = False
    isFalse _ = False
    isIdent _ = False
    isEpsilon _ = False

isSingleton :: A_Concept -> Bool
isSingleton ONE = True
isSingleton _   = False

-- The function "properties" does not only provide the properties provided by the Ampersand user,
-- but tries to derive the most obvious multiplicity constraints as well. The more multiplicity constraints are known,
-- the better the data structure that is derived.
-- Not every constraint that can be proven is obtained by this function. This does not hurt Ampersand.
instance Relational Expression where        -- TODO: see if we can find more multiplicity constraints...
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
     EDcD dcl   -> isImin dcl
     EDcI{}     -> False
     EEps{}     -> False
     EDcV{}     -> False
     EBrk f     -> isImin f
     EFlp f     -> isImin f
     _          -> False  -- TODO: find richer answers for ELrs, ERrs, and EDia
