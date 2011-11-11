{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration (Relation(..),Association(..),flpDecl,Relational(..)
                                  ,Declaration(..)
                                  ,makeRelation
                                  ,isSgn
                                  ) where

import Data.Maybe
                                  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.ADL1.Concept      (Conceptual(..))
import DatabaseDesign.Ampersand.ADL1.Prop         (Prop(..),flipProps)
import DatabaseDesign.Ampersand.ADL1.Pair         (flipPair) 
import DatabaseDesign.Ampersand.ADL1.Expression
import DatabaseDesign.Ampersand.Basics

fatal :: Int -> String -> a
fatal = fatalMsg "ADL1.MorphismAndDeclaration"

class Association r => Relational r where
    multiplicities :: r -> [Prop]
    isProp         :: r -> Bool  -- > tells whether the argument is a property
    isImin         :: r -> Bool  -- > tells whether the argument is equivalent to I-
    isTrue         :: r -> Bool  -- > tells whether the argument is equivalent to V
    isFalse        :: r -> Bool  -- > tells whether the argument is equivalent to V-
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
           Mp1{}               -> [Uni,Inj,Sym,Asy,Trn]
    isProp rel = case rel of
           Rel{}               -> null ([Asy,Sym]>-multiplicities (reldcl rel))
           V{}                 -> isEndo rel && isSingleton (source rel)
           I{}                 -> True
           Mp1{}               -> True
    isImin rel  = isImin (makeDeclaration rel)   -- > tells whether the argument is equivalent to I-
    isTrue rel = case rel of
           Rel{}               -> False
           V{}                 -> True
           I{}                 -> False
           Mp1{}               -> False
    isFalse _   = False
    isIdent rel = case rel of
                   I{}   -> True       -- > tells whether the argument is equivalent to I
                   V{}   -> isEndo rel && isSingleton (source rel)
                   _     -> False
   

flpDecl :: Declaration -> Declaration
flpDecl d
    = case d of
           Sgn {} -> d{ decsgn  = flpSign (decsgn d)
                      , decprps = flipProps (decprps d)
                      , decprps_calc = flipProps (decprps_calc d)
                      , decprL  = ""
                      , decprM  = ""
                      , decprR  = ""
                      , decpopu = map flipPair (decpopu d)
                      }
           Vs {}  -> d{ decsgn  = flpSign (decsgn d) }
           _      -> d

flpSign :: Sign -> Sign
flpSign (Sign s t) = Sign t s
   


instance Relational Declaration where
    multiplicities d = case d of
           Sgn {}       -> decprps_calc d --according to comment in data Declaration, decprps_calc also contains user defined prps
           Isn{}        -> [Uni,Tot,Inj,Sym,Asy,Trn,Rfx,Sur]
           Iscompl{}    -> [Sym]
           Vs{}         -> [Tot,Sur]
    isProp d = case d of         -- > tells whether the argument is a property.
           Sgn {}       -> null ([Asy,Sym]>-multiplicities d)
           Isn{}        -> True
           Iscompl{}    -> False
           Vs{}         -> isEndo (sign d) && isSingleton (source d)
    isImin d = case d of          -- > tells whether the argument is equivalent to I-
           Iscompl{}    -> True   
           _            -> False
    isTrue d = case d of 
           Vs{}         -> True
           _            -> False
    isFalse _ = False
    isIdent d = case d of
                 Isn{} -> True   -- > tells whether the argument is equivalent to I
                 _     -> False

makeRelation :: Declaration -> Relation
makeRelation d
    = Rel { relnm  = name d
          , relpos = origin d 
          , relsgn = decsgn d
          , reldcl = d
          }

isSgn :: Declaration -> Bool
isSgn Sgn{} = True
isSgn  _    = False

   
-- The function "multiplicities" does not only provide the multiplicities provided by the Ampersand user,
-- but tries to derive the most obvious multiplicity constraints as well. The more multiplicity constraints are known,
-- the better the data structure that is derived.
-- Not every constraint that can be proven is obtained by this function. This does not hurt Ampersand.
instance Relational Expression where        -- TODO: see if we can find more multiplicity constraints...
 multiplicities expr = case expr of
     ERel rel -> multiplicities rel
     EBrk f   -> multiplicities f
     ECps []  -> fatal 142 "Illegal call to multiplicities (ECps [])"
     ECps [t] -> multiplicities t
     ECps ts  -> foldr (isc . multiplicities) [Uni, Tot, Sur, Inj] ts -- endo properties can be used and deduced by and from rules: many rules are multiplicities (TODO)
     ERad []  -> fatal 145 "Illegal call to multiplicities (ERad [])"
     ERad [t] -> multiplicities t
     ERad _   -> [] -- TODO:  many rules with ERad in it are multiplicities (TODO). Solve perhaps by defining relation a = (ERad ts)
     EPrd []  -> fatal 148 "Illegal call to multiplicities (EPrd [])"
     EPrd [t] -> multiplicities t `uni` [Trn]
     EPrd ts  -> [Tot | isTot (head ts)]++[Sur | isSur (last ts)]++[Rfx | isRfx (head ts)&&isRfx (last ts)]++[Trn]
     EUni []  -> fatal 151 "Illegal call to multiplicities (EUni [])"
     EUni [t] -> multiplicities t
     EUni ts  -> [Tot | or (map isTot gts)]++[Sur | or (map isSur gts)]++[Rfx | or (map isRfx gts)]
                 where mgs = sign(EUni ts)  -- the Most General Signature of expressions in ts
                       gts = [t | t<-ts, sign t==mgs]
     EIsc []  -> fatal 154 "Illegal call to multiplicities (EIsc [])"
     EIsc [t] -> multiplicities t
     EIsc ts  -> [Tot | and (map isTot gts)]++[Sur | and (map isSur gts)]++
                 [Uni | and (map isUni gts)]++[Inj | and (map isInj gts)]++[Asy | or (map isAsy [t | t<-ts, sign t==mgs])] -- TODO:  Is this correct if the elements of ts have different types? (i.e. where generalization and specialization kick in)
                 where mgs = sign(EIsc ts)  -- the Most General Signature of expressions in ts
                       gts = [t | t<-ts, sign t==mgs]
                  -- TODO:  expr /\ a is Asy if a is Asy, is Uni if a is Uni (TODO), is Tot if both a and expr are tot
     EKl0 e'  -> [Rfx,Trn] `uni` (multiplicities e'>-[Uni,Inj])
     EKl1 e'  -> [    Trn] `uni` (multiplicities e'>-[Uni,Inj])
     ECpl e'  -> [p |p<-multiplicities e', p==Sym]
     ETyp e' _ -> multiplicities e'
     EFlp e'  -> [fromMaybe m $ lookup m [(Uni,Inj),(Inj,Uni),(Sur,Tot),(Tot,Sur)] | m <- multiplicities e'] -- switch Uni<->Inj and Sur<->Tot, keeping the others the same
     EEqu _   -> []
     EImp _   -> []
     EDif _   -> []
     ELrs _   -> []
     ERrs _   -> []

 isTrue expr
  = case expr of
     EEqu (l,r)   -> isTrue l && isTrue r
     EImp (l,_)   -> isTrue l
     EIsc fs      -> and [isTrue f | f<-fs]
     EUni fs      -> or  [isTrue f | f<-fs] -- isImin \/ isIdent => isTrue ( => TODO)
     EDif (l,r)   -> isTrue l && isTrue r
     ERrs _       -> False      -- TODO
     ELrs _       -> False      -- TODO
     ECps []      -> False      -- TODO: incorrect for singleton-concepts?
     ECps [e]     -> isTrue e
     ECps es       | null ([Uni,Tot]>-multiplicities (head es)) -> (isTrue. ECps .tail) es
                 | null ([Sur,Inj]>-multiplicities (last es)) -> (isTrue. ECps .init) es
                 | otherwise               -> isTrue (head es) && isTrue (last es) &&
                                                 (not.isFalse. ECps .drop 1.init) es  -- not isFalse between head and last
     ERad []      -> False
     ERad es      -> isFalse (ECps (map notCpl es))
     EPrd []      -> fatal 185 "EPrd [] may not occur"
     EPrd es      -> isTrue (head es)&&isTrue (last es) || isTot (head es)&&isSur (last es) || isRfx (head es)&&isRfx (last es)
     EKl0 e       -> isTrue e
     EKl1 e       -> isTrue e
     EFlp e       -> isTrue e
     ECpl e       -> isFalse e
     ETyp e _     -> isTrue e
     ERel rel     -> isTrue rel
     EBrk e       -> isTrue e

 isFalse expr
  = case expr of
     EEqu (l,r)   -> isFalse l && isFalse r
     EImp (_,r)   -> isFalse r
     EIsc fs      -> or  [isFalse f | f<-fs] -- isImin /\ isIdent => isFalse ( => TODO)
     EUni fs      -> and [isFalse f | f<-fs]
     EDif (l,_)   -> isFalse l
     ERrs _       -> False      -- TODO
     ELrs _       -> False      -- TODO
     ECps []      -> False
     ECps ts      -> any isFalse ts -- ook True als twee concepten op de ; niet aansluiten: a[A*B];b[C*D] met B/\C=0 (Dit wordt echter door de typechecker uitgesloten)
     ERad []      -> False -- TODO: incorrect for singleton-concepts?
     ERad [e]     -> isFalse e
     ERad es      -> isTrue (ECps (map notCpl es)) --  TODO: make dual to the code of isTrue
     EPrd []      -> False
     EPrd [e]     -> isFalse e
     EPrd es      -> isFalse (head es)||isFalse (last es)
     EKl0 e       -> isFalse e
     EKl1 e       -> isFalse e
     EFlp e       -> isFalse e
     ECpl e       -> isTrue e
     ETyp e _     -> isFalse e
     ERel rel     -> isFalse rel
     EBrk e       -> isFalse e

 isProp expr = null ([Asy,Sym]>-multiplicities expr)

-- The function isIdent tries to establish whether an expression is an identity relation. It does a little bit more than just test on ERel I.
-- If it returns False, this must be interpreted as: the expression is not equal to I, as far as the computer can tell on face value. 
 isIdent expr = case expr of
     EEqu (l,r)   -> isIdent (EIsc [EImp (l,r), EImp (r,l)])    -- TODO: maybe derive something better?
     EImp (l,r)   -> isIdent (EUni [ECpl l, r])                 -- TODO: maybe derive something better?
     EIsc fs      -> and [isIdent f | f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
     EUni fs      -> and [isIdent f | f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
     ECps []      -> True
     ECps ts      -> and [isIdent t | t<-ts]   -- > a;a~ = I bij bepaalde multipliciteiten (TODO)
     ERad [e]     -> isIdent e
     EKl0 e       -> isIdent e || isFalse e
     EKl1 e       -> isIdent e
     ECpl e       -> isImin e
     ETyp e sgn   -> source sgn==target sgn && isIdent e
     ERel rel     -> isIdent rel
     EBrk f       -> isIdent f
     _            -> False  -- TODO: find richer answers for EDif, ERrs and ELrs

 isImin expr' = case expr' of       -- > tells whether the argument is equivalent to I-
     EEqu (l,r)   -> isImin (EIsc [EImp (l,r), EImp (r,l)])       -- TODO: maybe derive something better?
     EImp (l,r)   -> isImin (EUni [ECpl l, r])                  -- TODO: maybe derive something better?
     EIsc fs      -> and [isImin f | f<-fs] && not (null fs) -- > fout voor singletons (TODO)
     EUni fs      -> and [isImin f | f<-fs] && not (null fs) -- > fout voor singletons (TODO)
     ECps [e]     -> isImin e
     ERad []      -> True
     ERad es      -> and [isImin e | e<-es]
     ECpl e       -> isIdent e
     ETyp e sgn   -> source sgn==target sgn && isImin e
     ERel rel     -> isImin rel
     EBrk f       -> isImin f
     _           -> False  -- TODO: find richer answers for EDif, ERrs and ELrs


   
                        
