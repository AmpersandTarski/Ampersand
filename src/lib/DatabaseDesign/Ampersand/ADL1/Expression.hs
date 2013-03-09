{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
module DatabaseDesign.Ampersand.ADL1.Expression (
                       subst,subsi
                      ,foldlMapExpression,foldrMapExpression
                      ,isECps,isERad,isEPrd,isEIsc,isEUni
                      ,isPos,isNeg, deMorgan ,notCpl, isCpl)
where
import DatabaseDesign.Ampersand.Basics (fatalMsg)
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree

fatal :: Int -> String -> a
fatal = fatalMsg "ADL1.Expression"

subst :: (Relation,Expression) -> Expression -> Expression
subst (rel,f) = subs
     where
       subs (EEqu (l,r) sgn) = EEqu (subs l,subs r) sgn
       subs (EImp (l,r) sgn) = EImp (subs l,subs r) sgn
       subs (EIsc (l,r) sgn) = EIsc (subs l,subs r) sgn
       subs (EUni (l,r) sgn) = EUni (subs l,subs r) sgn
       subs (EDif (l,r) sgn) = EDif (subs l,subs r) sgn
       subs (ELrs (l,r) sgn) = ELrs (subs l,subs r) sgn
       subs (ERrs (l,r) sgn) = ERrs (subs l,subs r) sgn
       subs (ECps (l,r) sgn) = ECps (subs l,subs r) sgn
       subs (ERad (l,r) sgn) = ERad (subs l,subs r) sgn
       subs (EPrd (l,r) sgn) = EPrd (subs l,subs r) sgn
       subs (EKl0 e     sgn) = EKl0 (subs e)        sgn
       subs (EKl1 e     sgn) = EKl1 (subs e)        sgn
       subs (EFlp e     sgn) = EFlp (subs e)        sgn
       subs (ECpl e     sgn) = ECpl (subs e)        sgn
       subs (EBrk e)         = EBrk (subs e)
       subs (ETyp e     sgn) = ETyp (subs e)        sgn
       subs e@(ERel r   _  ) | rel==r    = f
                             | otherwise = e
       subs e@EMp1{}         = e

-- | This function is used to replace the n-th relation (counting from the left)
--   with an expression. The parameter f will therefore be applied to an
--   expression of the form Erel rel.
subsi :: Int -> (Expression -> Expression) -> Expression -> Expression
subsi n f expr = expr'
       where
         (expr',_) = subs 1 expr
         subs :: Int -> Expression -> (Expression, Int)
         subs i (EEqu (l,r) sgn) = (EEqu (l',r') sgn, i'')
                                   where (l',i')  = subs i l
                                         (r',i'') = subs i' r
         subs i (EImp (l,r) sgn) = (EImp (l',r') sgn, i'')
                                   where (l',i')  = subs i l
                                         (r',i'') = subs i' r
         subs i (EIsc (l,r) sgn) = (EIsc (l',r') sgn, i'')
                                   where (l',i')  = subs i l
                                         (r',i'') = subs i' r
         subs i (EUni (l,r) sgn) = (EUni (l',r') sgn, i'')
                                   where (l',i')  = subs i l
                                         (r',i'') = subs i' r
         subs i (EDif (l,r) sgn) = (EDif (l',r') sgn, i'')
                                   where (l',i')  = subs i l
                                         (r',i'') = subs i' r
         subs i (ELrs (l,r) sgn) = (ELrs (l',r') sgn, i'')
                                   where (l',i')  = subs i l
                                         (r',i'') = subs i' r
         subs i (ERrs (l,r) sgn) = (ERrs (l',r') sgn, i'')
                                   where (l',i')  = subs i l
                                         (r',i'') = subs i' r
         subs i (ECps (l,r) sgn) = (ECps (l',r') sgn, i'')
                                   where (l',i')  = subs i l
                                         (r',i'') = subs i' r
         subs i (ERad (l,r) sgn) = (ERad (l',r') sgn, i'')
                                   where (l',i')  = subs i l
                                         (r',i'') = subs i' r
         subs i (EPrd (l,r) sgn) = (EPrd (l',r') sgn, i'')
                                   where (l',i')  = subs i l
                                         (r',i'') = subs i' r
         subs i (EKl0 x sgn)     = (EKl0 x' sgn, i') where (x',i') = subs i x
         subs i (EKl1 x sgn)     = (EKl1 x' sgn, i') where (x',i') = subs i x 
         subs i (EFlp x sgn)     = (EFlp x' sgn, i') where (x',i') = subs i x 
         subs i (ECpl x sgn)     = (ECpl x' sgn, i') where (x',i') = subs i x 
         subs i (EBrk x)         = (EBrk x'    , i') where (x',i') = subs i x 
         subs i (ETyp x sgn)     = (ETyp x' sgn, i') where (x',i') = subs i x
         subs i x@ERel{} | i==n      = (f x, i+1)
                         | otherwise = (x, i+1)
         subs i x@EMp1{}         = (x,i)

foldlMapExpression :: (a -> r -> a) -> (Relation->r) -> a -> Expression -> a
foldlMapExpression f = foldrMapExpression f' where f' x y = f y x

foldrMapExpression :: (r -> a -> a) -> (Relation->r) -> a -> Expression -> a
foldrMapExpression f g a (EEqu (l,r) _)    = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EImp (l,r) _)    = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EIsc (l,r) _)    = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EUni (l,r) _)    = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EDif (l,r) _)    = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (ELrs (l,r) _)    = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (ERrs (l,r) _)    = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (ECps (l,r) _)    = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (ERad (l,r) _)    = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EPrd (l,r) _)    = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EKl0 e _)        = foldrMapExpression f g a                         e
foldrMapExpression f g a (EKl1 e _)        = foldrMapExpression f g a                         e
foldrMapExpression f g a (EFlp e _)        = foldrMapExpression f g a                         e
foldrMapExpression f g a (ECpl e _)        = foldrMapExpression f g a                         e
foldrMapExpression f g a (EBrk e)          = foldrMapExpression f g a                         e
foldrMapExpression f g a (ETyp e _)        = foldrMapExpression f g a e
foldrMapExpression f g a (ERel rel _)      = f (g rel) a
foldrMapExpression _ _ a  EMp1{}           = a

isEUni :: Expression -> Bool
isEUni EUni{}  = True
isEUni _       = False

isEIsc :: Expression -> Bool
isEIsc EIsc{}  = True
isEIsc _       = False

isECps :: Expression -> Bool
isECps ECps{}  = True
isECps _       = False

isERad :: Expression -> Bool
isERad ERad{}  = True
isERad _       = False

isEPrd :: Expression -> Bool
isEPrd EPrd{}  = True
isEPrd _       = False


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************

-- | The rule of De Morgan requires care with respect to the complement.
--   The following function provides a function to manipulate with De Morgan correctly.
deMorgan :: Sign -> Expression -> Expression
deMorgan sgn@(Sign s t) (ERad (l,r) _)
    = case (target l, source r) of
       (tl@C{},sr@C{}) -> let z = tl `meet` sr in notCpl sgn (notCpl (Sign s z) l .:. notCpl (Sign z t) r)
       _               -> fatal 137 "expression in wrong format (has never been signalled so far...)"
deMorgan sgn@(Sign s t) (ECps (l,r) _)
    = case (target l, source r) of
       (tl@C{},sr@C{}) -> let z = tl `join` sr in notCpl sgn (notCpl (Sign s z) l .!. notCpl (Sign z t) r)
       _               -> fatal 141 "expression in wrong format (has never been signalled so far...)"
       
deMorgan sgn (EUni (l,r) _)    = notCpl sgn (notCpl sgn l ./\. notCpl sgn r)
deMorgan sgn (EIsc (l,r) _)    = notCpl sgn (notCpl sgn l .\/. notCpl sgn r)
deMorgan _ e    = fatal 214 ("De Morgan is not applicable to "++show e)

-- The origin of an expression is the origin of one of its relations.
-- Let us just take the first one... 
-- WHY? (SJ) Why does this instance exist? Can we not do without?
-- instance Traced Expression where
--  origin e = origin (head (mors e))

notCpl :: Sign -> Expression -> Expression
notCpl _ (ECpl e' _) = e'
notCpl sgn e' = ECpl e' sgn

isCpl :: Expression -> Bool
isCpl (ECpl{}) = True
isCpl _ = False

isPos :: Expression -> Bool
isPos (ECpl{}) = False
isPos _ = True
isNeg :: Expression -> Bool
isNeg = not . isPos 

