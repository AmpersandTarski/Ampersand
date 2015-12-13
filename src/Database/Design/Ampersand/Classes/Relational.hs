module Database.Design.Ampersand.Classes.Relational
   (Relational(..)
   ) where

import Data.Maybe
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.ADL1.Expression
import Database.Design.Ampersand.Basics

class Association r => Relational r where
    properties :: r -> [Prop]
    isProp :: r -> Bool  -- > tells whether the argument is a property
    isImin :: r -> Bool  -- > tells whether the argument is equivalent to I-
    isTrue :: r -> Bool  -- > tells whether the argument is equivalent to V
    isFalse :: r -> Bool  -- > tells whether the argument is equivalent to V-
    isFunction :: r -> Bool
    isFunction r   = null ([Uni,Tot]>-properties r)
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

--instance Relational Relation where
--    properties rel
--      = case rel of
--           Rel{}               -> properties (reldcl rel)
--           V {}                -> [Tot]
--                                ++[Sur]
--                                ++[Inj | isSingleton (source rel)]
--                                ++[Uni | isSingleton (target rel)]
--                                ++[Asy | isEndo rel, isSingleton (source rel)]
--                                ++[Sym | isEndo rel]
--                                ++[Rfx | isEndo rel]
--                                ++[Trn | isEndo rel]
--           I{}                 -> [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
--    isProp rel = case rel of
--           Rel{}               -> null ([Asy,Sym]>-properties (reldcl rel))
--           V{}                 -> isEndo rel && isSingleton (source rel)
--           I{}                 -> True
--    isImin rel  = isImin (makeDeclaration rel)   -- > tells whether the argument is equivalent to I-
--    isTrue rel = case rel of
--           Rel{}               -> False
--           V{}                 -> True
--           I{}                 -> False
--    isFalse _   = False
--    isIdent rel = case rel of       -- > tells whether the argument is equivalent to I
--                   Rel{} -> False
--                   V{}   -> isEndo rel && isSingleton (source rel)
--                   I{}   -> True

instance Relational Declaration where
    properties d = case d of
           Sgn {}       -> case decprps_calc d of
                             Nothing -> decprps d
                             Just ps -> ps
           Isn{}        -> [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
           Vs{}         -> [Tot,Sur]
    isProp d = case d of         -- > tells whether the argument is a property.
           Sgn {}       -> null ([Asy,Sym]>-properties d)
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
     EDcI{}     -> [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
     EEps a sgn -> [Tot | a == source sgn]++[Sur | a == target sgn] ++ [Uni,Inj]
     EDcV sgn   -> [Tot]
                 ++[Sur]
                 ++[Inj | isSingleton (source sgn)]
                 ++[Uni | isSingleton (target sgn)]
                 ++[Asy | isEndo sgn, isSingleton (source sgn)]
                 ++[Sym | isEndo sgn]
                 ++[Rfx | isEndo sgn]
                 ++[Trn | isEndo sgn]
     EBrk f     -> properties f
     ECps (l,r) -> [m | m<-properties l `isc` properties r, m `elem` [Uni,Tot,Inj,Sur]] -- endo properties can be used and deduced by and from rules: many rules are properties (TODO)
     EPrd (l,r) -> [Tot | isTot l]++[Sur | isSur r]++[Rfx | isRfx l&&isRfx r]++[Trn]
     EKl0 e'    -> [Rfx,Trn] `uni` (properties e'>-[Uni,Inj])
     EKl1 e'    -> [    Trn] `uni` (properties e'>-[Uni,Inj])
     ECpl e'    -> [p |p<-properties e', p==Sym]
     EFlp e'    -> [fromMaybe m $ lookup m [(Uni,Inj),(Inj,Uni),(Sur,Tot),(Tot,Sur)] | m <- properties e'] -- switch Uni<->Inj and Sur<->Tot, keeping the others the same
     EMp1{}     -> [Uni,Inj,Sym,Asy,Trn]
     _          -> []

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
     ECps (l,r) | null ([Uni,Tot]>-properties l) -> isTrue r
                | null ([Sur,Inj]>-properties r) -> isTrue l
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

 isProp expr = null ([Asy,Sym]>-properties expr)

 -- |  The function isIdent tries to establish whether an expression is an identity relation.
 --    It does a little bit more than just test on ERel I _.
 --    If it returns False, this must be interpreted as: the expression is definitely not I, an may not be equal to I as far as the computer can tell on face value.
 isIdent expr = case expr of
     EEqu (l,r) -> isIdent (EIsc (EInc (l,r), EInc (r,l)))    -- TODO: maybe derive something better?
     EInc (l,r) -> isIdent (EUni (ECpl l, r))                     -- TODO: maybe derive something better?
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
