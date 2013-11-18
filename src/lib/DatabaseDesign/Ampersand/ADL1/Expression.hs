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
       subs e@(EDcD d   _  ) | d==decl   = expr
                             | otherwise = e
       subs e@EDcI{}         = e
       subs e@EEps{}         = e
       subs e@EDcV{}         = e
       subs e@EMp1{}         = e

-- | This function is used to replace the n-th occurrence of a relation
--   (counting from the left) with an expression.
--   The parameter f will therefore be applied to an
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
    (EEqu (l,r) _) -> primitives l `uni` primitives r
    (EImp (l,r) _) -> primitives l `uni` primitives r
    (EIsc (l,r) _) -> primitives l `uni` primitives r
    (EUni (l,r) _) -> primitives l `uni` primitives r
    (EDif (l,r) _) -> primitives l `uni` primitives r
    (ELrs (l,r) _) -> primitives l `uni` primitives r
    (ERrs (l,r) _) -> primitives l `uni` primitives r
    (ECps (l,r) _) -> primitives l `uni` primitives r
    (ERad (l,r) _) -> primitives l `uni` primitives r
    (EPrd (l,r) _) -> primitives l `uni` primitives r
    (EKl0 e _)     -> primitives e
    (EKl1 e _)     -> primitives e
    (EFlp e _)     -> primitives e
    (ECpl e _)     -> primitives e
    (EBrk e)       -> primitives e
    (ETyp e _)     -> primitives e
    EDcD{}         -> [expr]
    EDcI{}         -> [expr]
    EEps{}         -> []  -- Since EEps is inserted for typing reasons only, we do not consider it a primitive..
    EDcV{}         -> [expr]
    EMp1{}         -> [expr]

-- | The rule of De Morgan requires care with respect to the complement.
--   The following function provides a function to manipulate with De Morgan correctly.
deMorganERad :: Sign -> Expression -> Expression
deMorganERad sgn expr@(ERad{})
  = recur sgn expr
    where
     recur (Sign s t) (ERad (l,r) _)
       = case (target l, source r) of
          (PlainConcept{},PlainConcept{}) 
             -> notCpl sgn (notCpl (Sign s (target l)) (recur (Sign s (target l)) l) .:. notCpl (Sign (source r) t) (recur (Sign (source r) t) r))
          _  -> fatal 159 "expression in wrong format (has never been signalled so far...)"
     recur _ e = e
deMorganERad _ e    = fatal 161 ("De Morgan for relational addition is not applicable to "++show e)
deMorganECps :: Sign -> Expression -> Expression
deMorganECps sgn expr@(ECps{})
  = recur sgn expr
    where
     recur (Sign s t) (ECps (l,r) _)
       = case (target l, source r) of
          (PlainConcept{},PlainConcept{}) 
             -> notCpl sgn (notCpl (Sign s (target l)) (recur (Sign s (target l)) l) .!. notCpl (Sign (source r) t) (recur (Sign (source r) t) r))
          _  -> fatal 168 "expression in wrong format (has never been signalled so far...)"
     recur _ e = e
deMorganECps _ e    = fatal 172 ("De Morgan for composition is not applicable to "++show e)
deMorganEUni :: Sign -> Expression -> Expression
deMorganEUni sgn expr@(EUni{})
  = recur expr
    where
     recur (EUni (l,r) _)
       = notCpl sgn (notCpl sgn (recur l) ./\. notCpl sgn (recur r))
     recur e = e
deMorganEUni _ e    = fatal 180 ("De Morgan for union is not applicable to "++show e)
deMorganEIsc :: Sign -> Expression -> Expression
deMorganEIsc sgn expr@(EIsc{})
  = recur expr
    where
     recur (EIsc (l,r) _)
       = notCpl sgn (notCpl sgn (recur l) .\/. notCpl sgn (recur r))
     recur e = e
deMorganEIsc _ e    = fatal 188 ("De Morgan for intersection is not applicable to "++show e)

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

isMp1 :: Expression -> Bool
isMp1 EMp1{} = True
isMp1 _ = False
