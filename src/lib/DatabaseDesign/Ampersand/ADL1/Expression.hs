{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances , OverlappingInstances #-}
module DatabaseDesign.Ampersand.ADL1.Expression (
                       subst -- SJ 18 nov 2013: obsolete? ,subsi
                      ,foldlMapExpression,foldrMapExpression
                      ,primitives,isMp1
                      ,isPos,isNeg, deMorganERad, deMorganECps, deMorganEUni, deMorganEIsc, notCpl, isCpl)
where
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree

fatal :: Int -> String -> a
fatal = fatalMsg "ADL1.Expression"

-- | subst is used to replace each occurrence of a relation
--   with an expression. The parameter expr will therefore be applied to an
--   expression of the form Erel rel.
subst :: (Declaration,Expression) -> Expression -> Expression
subst (decl,expr) = subs
     where
       subs (EEqu (l,r)) = EEqu (subs l,subs r)
       subs (EImp (l,r)) = EImp (subs l,subs r)
       subs (EIsc (l,r)) = EIsc (subs l,subs r)
       subs (EUni (l,r)) = EUni (subs l,subs r)
       subs (EDif (l,r)) = EDif (subs l,subs r)
       subs (ELrs (l,r)) = ELrs (subs l,subs r)
       subs (ERrs (l,r)) = ERrs (subs l,subs r)
       subs (ECps (l,r)) = ECps (subs l,subs r)
       subs (ERad (l,r)) = ERad (subs l,subs r)
       subs (EPrd (l,r)) = EPrd (subs l,subs r)
       subs (EKl0 e    ) = EKl0 (subs e)
       subs (EKl1 e    ) = EKl1 (subs e)
       subs (EFlp e    ) = EFlp (subs e)
       subs (ECpl e    ) = ECpl (subs e)
       subs (EBrk e)     = EBrk (subs e)
       subs (ETyp e    ) = ETyp (subs e)
       subs e@(EDcD d  ) | d==decl   = expr
                         | otherwise = e
       subs e@EDcI{}     = e
       subs e@EEps{}     = e
       subs e@EDcV{}     = e
       subs e@EMp1{}     = e

-- | This function is used to replace the n-th occurrence of a relation
--   (counting from the left) with an expression.
--   The parameter f will therefore be applied to an
--   expression of the form Erel rel.
subsi :: Int -> (Expression -> Expression) -> Expression -> Expression
subsi n f expr = expr'
       where
         (expr',_) = subs 1 expr
         subs :: Int -> Expression -> (Expression, Int)
         subs i (EEqu (l,r)) = (EEqu (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (EImp (l,r)) = (EImp (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (EIsc (l,r)) = (EIsc (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (EUni (l,r)) = (EUni (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (EDif (l,r)) = (EDif (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (ELrs (l,r)) = (ELrs (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (ERrs (l,r)) = (ERrs (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (ECps (l,r)) = (ECps (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (ERad (l,r)) = (ERad (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (EPrd (l,r)) = (EPrd (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (EKl0 x)     = (EKl0 x', i') where (x',i') = subs i x
         subs i (EKl1 x)     = (EKl1 x', i') where (x',i') = subs i x 
         subs i (EFlp x)     = (EFlp x', i') where (x',i') = subs i x 
         subs i (ECpl x)     = (ECpl x', i') where (x',i') = subs i x 
         subs i (EBrk x)     = (EBrk x', i') where (x',i') = subs i x 
         subs i (ETyp x sgn) = (ETyp x', i') where (x',i') = subs i x
         subs i x@EDcD{} | i==n      = (f x, i+1)
                         | otherwise = (x, i+1)
         subs i x@EDcI{} | i==n      = (f x, i+1)
                         | otherwise = (x, i+1)
         subs i x@EEps{}             = (x, i)
         subs i x@EDcV{} | i==n      = (f x, i+1)
                         | otherwise = (x, i+1)
         subs i x@EMp1{}             = (x,i)

foldlMapExpression :: (a -> r -> a) -> (Declaration->r) -> a -> Expression -> a
foldlMapExpression f = foldrMapExpression f' where f' x y = f y x

foldrMapExpression :: (r -> a -> a) -> (Declaration->r) -> a -> Expression -> a
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
foldrMapExpression f g a (EDcD rel _)      = f (g rel) a
foldrMapExpression _ _ a  EDcI{}           = a
foldrMapExpression _ _ a  EEps{}           = a
foldrMapExpression _ _ a  EDcV{}           = a
foldrMapExpression _ _ a  EMp1{}           = a

primitives :: Expression -> [Expression]
primitives expr =
  case expr of
    (EEqu (l,r)) -> primitives l `uni` primitives r
    (EImp (l,r)) -> primitives l `uni` primitives r
    (EIsc (l,r)) -> primitives l `uni` primitives r
    (EUni (l,r)) -> primitives l `uni` primitives r
    (EDif (l,r)) -> primitives l `uni` primitives r
    (ELrs (l,r)) -> primitives l `uni` primitives r
    (ERrs (l,r)) -> primitives l `uni` primitives r
    (ECps (l,r)) -> primitives l `uni` primitives r
    (ERad (l,r)) -> primitives l `uni` primitives r
    (EPrd (l,r)) -> primitives l `uni` primitives r
    (EKl0 e)     -> primitives e
    (EKl1 e)     -> primitives e
    (EFlp e)     -> primitives e
    (ECpl e)     -> primitives e
    (EBrk e)     -> primitives e
    (ETyp e _)   -> primitives e
    EDcD{}       -> [expr]
    EDcI{}       -> [expr]
    EEps{}       -> []  -- Since EEps is inserted for typing reasons only, we do not consider it a primitive..
    EDcV{}       -> [expr]
    EMp1{}       -> [expr]

-- | The rule of De Morgan requires care with respect to the complement.
--   The following function provides a function to manipulate with De Morgan correctly.
deMorganERad :: Expression -> Expression
deMorganERad (ERad(L,R))
  = notCpl (notCpl (deMorganERad l) .;. notCpl (deMorganERad r))
deMorganERad e = e
deMorganECps :: Expression -> Expression
deMorganECps (ECps(L,R))
  = notCpl (notCpl (deMorganECps l) .!. notCpl (deMorganECps r))
deMorganECps e = e
deMorganEUni :: Expression -> Expression
deMorganEUni (EUni(L,R))
  = notCpl (notCpl (deMorganEUni l) ./\. notCpl (deMorganEUni r))
deMorganEUni e = e
deMorganEIsc :: Expression -> Expression
deMorganEIsc (EIsc(L,R))
  = notCpl (notCpl (deMorganEIsc l) .\/. notCpl (deMorganEIsc r))
deMorganEIsc e = e

notCpl :: Expression -> Expression
notCpl (ECpl e _) = e
notCpl e = ECpl e

isCpl :: Expression -> Bool
isCpl (ECpl{}) = True
isCpl _ = False

isPos :: Expression -> Bool
isPos (ECpl{}) = False
isPos _ = True
isNeg :: Expression -> Bool
isNeg = not . isPos 

isMp1 :: Expression -> Bool
isMp1 EMp1{} = True
isMp1 _ = False
