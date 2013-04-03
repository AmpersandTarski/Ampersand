{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.Relational (Relational(..)
                                  ,makeRelation
                                  ) where

import Data.Maybe
                                  
import Prelude hiding (Ord(..))
import DatabaseDesign.Ampersand.Core.Poset (Poset(..))
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Core.ParseTree       (Prop(..))
import DatabaseDesign.Ampersand.ADL1.Expression
import DatabaseDesign.Ampersand.Basics

class Association r => Relational r where
    multiplicities :: r -> [Prop]
    isProp :: r -> Bool  -- > tells whether the argument is a property
    isImin :: r -> Bool  -- > tells whether the argument is equivalent to I-
    isTrue :: r -> Bool  -- > tells whether the argument is equivalent to V
    isFalse :: r -> Bool  -- > tells whether the argument is equivalent to V-
    isFunction :: r -> Bool     
    isFunction r   = null ([Uni,Tot]>-multiplicities r)
    isTot :: r -> Bool  -- 
    isTot r = Tot `elem` multiplicities r
    isUni :: r -> Bool  -- 
    isUni r = Uni `elem` multiplicities r
    isSur :: r -> Bool  -- 
    isSur r = Sur `elem` multiplicities r
    isInj :: r -> Bool  -- 
    isInj r = Inj `elem` multiplicities r
    isRfx :: r -> Bool  -- 
    isRfx r = Rfx `elem` multiplicities r
    isIrf :: r -> Bool  -- 
    isIrf r = Irf `elem` multiplicities r
    isTrn :: r -> Bool  -- 
    isTrn r = Trn `elem` multiplicities r
    isSym :: r -> Bool  -- 
    isSym r = Sym `elem` multiplicities r
    isAsy :: r -> Bool  -- 
    isAsy r = Asy `elem` multiplicities r
    isIdent :: r -> Bool  -- > tells whether the argument is equivalent to I

instance Relational Relation where
    multiplicities rel 
      = case rel of
           Rel{}               -> multiplicities (reldcl rel)
           V {}                -> [Tot]
                                ++[Sur]
                                ++[Inj | isSingleton (source rel)]
                                ++[Uni | isSingleton (target rel)]
                                ++[Asy | isEndo rel, isSingleton (source rel)]
                                ++[Sym | isEndo rel]
                                ++[Rfx | isEndo rel]
                                ++[Trn | isEndo rel]
           I{}                 -> [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
    isProp rel = case rel of
           Rel{}               -> null ([Asy,Sym]>-multiplicities (reldcl rel))
           V{}                 -> isEndo rel && isSingleton (source rel)
           I{}                 -> True
    isImin rel  = isImin (makeDeclaration rel)   -- > tells whether the argument is equivalent to I-
    isTrue rel = case rel of
           Rel{}               -> False
           V{}                 -> True
           I{}                 -> False
    isFalse _   = False
    isIdent rel = case rel of       -- > tells whether the argument is equivalent to I
                   Rel{} -> False
                   V{}   -> isEndo rel && isSingleton (source rel)
                   I{}   -> True

instance Relational Declaration where
    multiplicities d = case d of
           Sgn {}       -> case decprps_calc d of
                             Nothing -> decprps d
                             Just ps -> ps 
           Isn{}        -> [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
           Vs{}         -> [Tot,Sur]
    isProp d = case d of         -- > tells whether the argument is a property.
           Sgn {}       -> null ([Asy,Sym]>-multiplicities d)
           Isn{}        -> True
           Vs{}         -> isEndo (sign d) && isSingleton (source d)
    isImin _ = False  -- LET OP: Dit kan natuurlijk niet goed zijn, maar is gedetecteerd bij revision 913, toen straffeloos de Iscompl{} kon worden verwijderd.
    isTrue d = case d of 
           Vs{}         -> True
           _            -> False
    isFalse _ = False
    isIdent d = case d of
                 Isn{} -> True   -- > tells whether the argument is equivalent to I
                 _     -> False

isSingleton :: A_Concept -> Bool
isSingleton ONE = True
isSingleton _   = False

makeRelation :: Declaration -> Relation
makeRelation d
    = Rel { relpos = origin d 
          , reldcl = d
          }

-- The function "multiplicities" does not only provide the multiplicities provided by the Ampersand user,
-- but tries to derive the most obvious multiplicity constraints as well. The more multiplicity constraints are known,
-- the better the data structure that is derived.
-- Not every constraint that can be proven is obtained by this function. This does not hurt Ampersand.
instance Relational Expression where        -- TODO: see if we can find more multiplicity constraints...
 multiplicities expr = case expr of
     EDcD dcl   _ -> multiplicities dcl
     EDcI       _ -> [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
     EDcV     sgn -> [Tot]
                   ++[Sur]
                   ++[Inj | isSingleton (source sgn)]
                   ++[Uni | isSingleton (target sgn)]
                   ++[Asy | isEndo sgn, isSingleton (source sgn)]
                   ++[Sym | isEndo sgn]
                   ++[Rfx | isEndo sgn]
                   ++[Trn | isEndo sgn]
     EBrk f       -> multiplicities f
     ECps (l,r) _ -> [m | m<-multiplicities l `isc` multiplicities r, m `elem` [Uni,Tot,Inj,Sur]] -- endo properties can be used and deduced by and from rules: many rules are multiplicities (TODO)
     EPrd (l,r) _ -> [Tot | isTot l]++[Sur | isSur r]++[Rfx | isRfx l&&isRfx r]++[Trn]
     EKl0 e'    _ -> [Rfx,Trn] `uni` (multiplicities e'>-[Uni,Inj])
     EKl1 e'    _ -> [    Trn] `uni` (multiplicities e'>-[Uni,Inj])
     ECpl e'    _ -> [p |p<-multiplicities e', p==Sym]
     ETyp e'    _ -> multiplicities e'
     EFlp e'    _ -> [fromMaybe m $ lookup m [(Uni,Inj),(Inj,Uni),(Sur,Tot),(Tot,Sur)] | m <- multiplicities e'] -- switch Uni<->Inj and Sur<->Tot, keeping the others the same
     EMp1{}       -> [Uni,Inj,Sym,Asy,Trn]
     _            -> []

 -- |  isTrue e == True   means that e is true, i.e. the population of e is (source e * target e).
 --    isTrue e == False  does not mean anything.
 --    the function isTrue is meant to produce a quick answer, without any form of theorem proving.
 isTrue expr
  = case expr of
     EEqu (l,r) _   -> l == r
     EImp (l,_) sgn -> isTrue (ETyp l sgn)
     EIsc (l,r) sgn -> isTrue (ETyp l sgn) && isTrue (ETyp r sgn)
     EUni (l,r) sgn -> isTrue (ETyp l sgn) || isTrue (ETyp r sgn)
     EDif (l,r) sgn -> isTrue (ETyp l sgn) && isFalse r
     ECps (l,r) _   | null ([Uni,Tot]>-multiplicities l) -> isTrue r
                    | null ([Sur,Inj]>-multiplicities r) -> isTrue l
                    | otherwise                          -> isTrue (ETyp l (Sign sl z)) && isTrue (ETyp r (Sign z tr))
                       where Sign sl tl = sign l
                             Sign sr tr = sign r
                             z = tl `meet` sr
     EPrd (l,r) sgn -> isTrue l' && isTrue r' || isTot l' && isSur r' || isRfx l' && isRfx r'
                       where l' = ETyp l sgn
                             r' = ETyp r sgn
     EKl0 e     _   -> isTrue e
     EKl1 e     _   -> isTrue e
     EFlp e     _   -> isTrue e
     ECpl e     _   -> isFalse e
     ETyp e     sgn -> isTrue e && sgn <= sign e  -- The operator (<=) comes from Core.Poset
     EDcD d       _ -> False
     EDcI         _ -> False
     EDcV         _ -> True
     EBrk e         -> isTrue e
     _              -> False  -- TODO: find richer answers for ERrs, ELrs, ERad, and EMp1

 -- |  isFalse e == True   means that e is false, i.e. the population of e is empty.
 --    isFalse e == False  does not mean anything.
 --    the function isFalse is meant to produce a quick answer, without any form of theorem proving.
 isFalse expr
  = case expr of
     EEqu (l,r) sgn -> l == notCpl sgn r
     EImp (_,r) _   -> isFalse r
     EIsc (l,r) _   -> isFalse r || isFalse l
     EUni (l,r) _   -> isFalse r && isFalse l
     EDif (l,r) sgn -> isFalse l || isTrue (ETyp r sgn)
     ECps (l,r) _   -> isFalse r || isFalse l
     EPrd (l,r) _   -> isFalse r || isFalse l
     EKl0 e     _   -> isFalse e
     EKl1 e     _   -> isFalse e
     EFlp e     _   -> isFalse e
     ECpl e     _   -> isTrue e
     ETyp e     _   -> isFalse e
     EDcD _     _   -> False
     EDcI       _   -> False
     EDcV       _   -> False
     EBrk e         -> isFalse e
     _              -> False  -- TODO: find richer answers for ERrs, ELrs, and ERad

 isProp expr = null ([Asy,Sym]>-multiplicities expr)

 -- |  The function isIdent tries to establish whether an expression is an identity relation.
 --    It does a little bit more than just test on ERel I _.
 --    If it returns False, this must be interpreted as: the expression is definitely not I, an may not be equal to I as far as the computer can tell on face value. 
 isIdent expr = case expr of
     EEqu (l,r) sgn -> isIdent (EIsc (EImp (l,r) sgn, EImp (r,l) sgn) sgn)    -- TODO: maybe derive something better?
     EImp (l,r) sgn -> isIdent (EUni (ECpl l sgn, r) sgn)                     -- TODO: maybe derive something better?
     EIsc (l,r) sgn -> isIdent (ETyp l sgn) && isIdent (ETyp r sgn)
     EUni (l,r) sgn -> isIdent (ETyp l sgn) && isIdent (ETyp r sgn)
     EDif (l,r) sgn -> isIdent (ETyp l sgn) && isFalse r
     ECps (l,r) _   -> isIdent (ETyp l (Sign sl z)) && isIdent (ETyp r (Sign z tr))
                       where Sign sl tl = sign l
                             Sign sr tr = sign r
                             z = tl `meet` sr
     EKl0 e     _   -> isIdent e || isFalse e
     EKl1 e     _   -> isIdent e
     ECpl e     sgn -> isImin (ETyp e sgn)
     ETyp e     sgn -> source sgn==target sgn && sgn<=sign e && isIdent e
     EDcD _     _   -> False
     EDcI       _   -> True
     EDcV       sgn -> isEndo sgn && isSingleton (source sgn)
     EBrk f         -> isIdent f
     EFlp f     _   -> isIdent f
     _              -> False  -- TODO: find richer answers for ERrs, ELrs, EPrd, and ERad

 isImin expr' = case expr' of       -- > tells whether the argument is equivalent to I-
     EEqu (l,r) sgn -> isImin (EIsc (EImp (l,r) sgn, EImp (r,l) sgn) sgn)       -- TODO: maybe derive something better?
     EImp (l,r) sgn -> isImin (EUni (ECpl l sgn, r) sgn)                  -- TODO: maybe derive something better?
     EIsc (l,r) sgn -> isImin (ETyp l sgn) && isImin (ETyp r sgn)
     EUni (l,r) sgn -> isImin (ETyp l sgn) && isImin (ETyp r sgn)
     EDif (l,r) sgn -> isImin (ETyp l sgn) && isFalse r
     ECpl e     sgn -> isIdent (ETyp e sgn)
     ETyp e sgn     -> source sgn==target sgn && sgn<=sign e && isImin e
     EDcD dcl sgn   -> sgn<=sign dcl && isImin dcl
     EDcI       _   -> False
     EDcV       _   -> False
     EBrk f         -> isImin f
     EFlp f     _   -> isImin f
     _              -> False  -- TODO: find richer answers for ERrs and ELrs
