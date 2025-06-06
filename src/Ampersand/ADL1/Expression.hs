{-# LANGUAGE TypeSynonymInstances #-}

module Ampersand.ADL1.Expression
  ( Expressions,
    subst,
    primitives,
    subExpressions,
    isMp1,
    isEEps,
    isEDcD,
    isPos,
    isNeg,
    deMorganERad,
    deMorganECps,
    deMorganEUni,
    deMorganEIsc,
    notCpl,
    isCpl,
    isFlipped,
    isFitForCrudC,
    isFitForCrudR,
    isFitForCrudU,
    isFitForCrudD,
    exprIsc2list,
    exprUni2list,
    exprCps2list,
    exprRad2list,
    exprPrd2list,
    insParentheses,
  )
where

import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

-- | subst is used to replace each occurrence of a relation
--   with an expression. The parameter expr will therefore be applied to a
--   term of the form Erel rel.
subst :: (Relation, Expression) -> Expression -> Expression
subst (decl, expr) = subs
  where
    subs (EEqu (l, r)) = EEqu (subs l, subs r)
    subs (EInc (l, r)) = EInc (subs l, subs r)
    subs (EIsc (l, r)) = EIsc (subs l, subs r)
    subs (EUni (l, r)) = EUni (subs l, subs r)
    subs (EDif (l, r)) = EDif (subs l, subs r)
    subs (ELrs (l, r)) = ELrs (subs l, subs r)
    subs (ERrs (l, r)) = ERrs (subs l, subs r)
    subs (EDia (l, r)) = EDia (subs l, subs r)
    subs (ECps (l, r)) = ECps (subs l, subs r)
    subs (ERad (l, r)) = ERad (subs l, subs r)
    subs (EPrd (l, r)) = EPrd (subs l, subs r)
    subs (EKl0 e) = EKl0 (subs e)
    subs (EKl1 e) = EKl1 (subs e)
    subs (EFlp e) = EFlp (subs e)
    subs (ECpl e) = ECpl (subs e)
    subs (EBrk e) = EBrk (subs e)
    subs e@(EDcD d)
      | d == decl = expr
      | otherwise = e
    subs e@EDcI {} = e
    subs e@EBin {} = e
    subs e@EEps {} = e
    subs e@EDcV {} = e
    subs e@EMp1 {} = e

type Expressions = Set.Set Expression

primitives :: Expression -> Expressions
primitives expr =
  case expr of
    (EEqu (l, r)) -> primitives l `Set.union` primitives r
    (EInc (l, r)) -> primitives l `Set.union` primitives r
    (EIsc (l, r)) -> primitives l `Set.union` primitives r
    (EUni (l, r)) -> primitives l `Set.union` primitives r
    (EDif (l, r)) -> primitives l `Set.union` primitives r
    (ELrs (l, r)) -> primitives l `Set.union` primitives r
    (ERrs (l, r)) -> primitives l `Set.union` primitives r
    (EDia (l, r)) -> primitives l `Set.union` primitives r
    (ECps (l, r)) -> primitives l `Set.union` primitives r
    (ERad (l, r)) -> primitives l `Set.union` primitives r
    (EPrd (l, r)) -> primitives l `Set.union` primitives r
    (EKl0 e) -> primitives e
    (EKl1 e) -> primitives e
    (EFlp e) -> primitives e
    (ECpl e) -> primitives e
    (EBrk e) -> primitives e
    EDcD {} -> Set.singleton expr
    EDcI {} -> Set.singleton expr
    EBin {} -> Set.singleton expr
    EEps {} -> Set.empty -- Since EEps is inserted for typing reasons only, we do not consider it a primitive..
    EDcV {} -> Set.singleton expr
    EMp1 {} -> Set.singleton expr

subExpressions :: Expression -> Expressions
subExpressions expr =
  case expr of
    (EEqu (l, r)) -> Set.singleton expr `Set.union` subExpressions l `Set.union` subExpressions r
    (EInc (l, r)) -> Set.singleton expr `Set.union` subExpressions l `Set.union` subExpressions r
    (EIsc (l, r)) -> Set.singleton expr `Set.union` subExpressions l `Set.union` subExpressions r
    (EUni (l, r)) -> Set.singleton expr `Set.union` subExpressions l `Set.union` subExpressions r
    (EDif (l, r)) -> Set.singleton expr `Set.union` subExpressions l `Set.union` subExpressions r
    (ELrs (l, r)) -> Set.singleton expr `Set.union` subExpressions l `Set.union` subExpressions r
    (ERrs (l, r)) -> Set.singleton expr `Set.union` subExpressions l `Set.union` subExpressions r
    (EDia (l, r)) -> Set.singleton expr `Set.union` subExpressions l `Set.union` subExpressions r
    (ECps (l, r)) -> Set.singleton expr `Set.union` subExpressions l `Set.union` subExpressions r
    (ERad (l, r)) -> Set.singleton expr `Set.union` subExpressions l `Set.union` subExpressions r
    (EPrd (l, r)) -> Set.singleton expr `Set.union` subExpressions l `Set.union` subExpressions r
    (EKl0 e) -> Set.singleton expr `Set.union` subExpressions e
    (EKl1 e) -> Set.singleton expr `Set.union` subExpressions e
    (EFlp e) -> Set.singleton expr `Set.union` subExpressions e
    (ECpl e) -> Set.singleton expr `Set.union` subExpressions e
    (EBrk e) -> Set.singleton expr `Set.union` subExpressions e
    EDcD {} -> Set.singleton expr
    EDcI {} -> Set.singleton expr
    EBin {} -> Set.singleton expr
    EEps {} -> Set.singleton expr
    EDcV {} -> Set.singleton expr
    EMp1 {} -> Set.singleton expr

-- | The rule of De Morgan requires care with respect to the complement.
--   The following function provides a function to manipulate with De Morgan correctly.
deMorganERad :: Expression -> Expression
deMorganERad (ECpl (ERad (l, r))) =
  notCpl (deMorganERad l) .:. notCpl (deMorganERad r)
deMorganERad (ERad (l, r)) =
  notCpl (notCpl (deMorganERad l) .:. notCpl (deMorganERad r))
deMorganERad e = e

deMorganECps :: Expression -> Expression
deMorganECps (ECpl (ECps (l, r))) =
  notCpl (deMorganECps l) .!. notCpl (deMorganECps r)
deMorganECps (ECps (l, r)) =
  notCpl (notCpl (deMorganECps l) .!. notCpl (deMorganECps r))
deMorganECps e = e

deMorganEUni :: Expression -> Expression
deMorganEUni (ECpl (EUni (l, r))) =
  notCpl (deMorganEUni l) ./\. notCpl (deMorganEUni r)
deMorganEUni (EUni (l, r)) =
  notCpl (notCpl (deMorganEUni l) ./\. notCpl (deMorganEUni r))
deMorganEUni e = e

deMorganEIsc :: Expression -> Expression
deMorganEIsc (ECpl (EIsc (l, r))) =
  notCpl (deMorganEIsc l) .\/. notCpl (deMorganEIsc r)
deMorganEIsc (EIsc (l, r)) =
  notCpl (notCpl (deMorganEIsc l) .\/. notCpl (deMorganEIsc r))
deMorganEIsc e = e

notCpl :: Expression -> Expression
notCpl (ECpl e) = e
notCpl e = ECpl e

isCpl :: Expression -> Bool
isCpl ECpl {} = True
isCpl _ = False

isPos :: Expression -> Bool
isPos = not . isNeg

isNeg :: Expression -> Bool
isNeg = isCpl

isMp1 :: Expression -> Bool
isMp1 EMp1 {} = True
isMp1 _ = False

isEEps :: Expression -> Bool
isEEps EEps {} = True
isEEps _ = False

isEDcD :: Expression -> Bool
isEDcD EDcD {} = True
isEDcD _ = False

isFlipped :: Expression -> Bool
isFlipped EFlp {} = True
isFlipped (EBrk e) = isFlipped e
isFlipped _ = False

-- | Function to determine that the term
--   could be used to create a new atom in its target concept
isFitForCrudC :: Expression -> Bool
isFitForCrudC expr =
  case expr of
    EFlp e -> isFitForCrudC e
    EBrk e -> isFitForCrudC e
    ECps (EEps _ _, e) -> isFitForCrudC e
    ECps (e, EEps _ _) -> isFitForCrudC e
    ECps (_, _) -> True
    EEps _ _ -> False
    EMp1 {} -> False
    _ -> True

-- EDcI{} -> True -- TODO: set to False when functionality of +menu is adapted from I[Cpt] to V[SESSION*Cpt] expressions (see Issue #884)

-- | Function to determine that the term
--   could be used to read the population of its target concept
isFitForCrudR :: Expression -> Bool
isFitForCrudR _ = True

-- | Function to determine that the term
--   could be used to insert or delete a pair in the population of a relation
isFitForCrudU :: Expression -> Bool
isFitForCrudU expr =
  case expr of
    EDcD {} -> True
    EFlp e -> isFitForCrudU e
    EBrk e -> isFitForCrudU e
    ECps (EEps _ _, e) -> isFitForCrudU e
    ECps (e, EEps _ _) -> isFitForCrudU e
    ECps (e, EDcI {}) -> isFitForCrudU e
    ECps (_, _) -> False
    _ -> False

-- | Function to determine that the term is simple, that it
--   could be used to update the population of a relation
isFitForCrudD :: Expression -> Bool
isFitForCrudD _ = True

exprIsc2list, exprUni2list, exprCps2list, exprRad2list, exprPrd2list :: Expression -> NE.NonEmpty Expression
exprIsc2list (EIsc (l, r)) = exprIsc2list l <> exprIsc2list r
exprIsc2list r = r NE.:| []
exprUni2list (EUni (l, r)) = exprUni2list l <> exprUni2list r
exprUni2list r = r NE.:| []
exprCps2list (ECps (l, r)) = exprCps2list l <> exprCps2list r
exprCps2list r = r NE.:| []
exprRad2list (ERad (l, r)) = exprRad2list l <> exprRad2list r
exprRad2list r = r NE.:| []
exprPrd2list (EPrd (l, r)) = exprPrd2list l <> exprPrd2list r
exprPrd2list r = r NE.:| []

insParentheses :: Expression -> Expression
insParentheses = insPar 0
  where
    wrap :: Integer -> Integer -> Expression -> Expression
    wrap i j e' = if i <= j then e' else EBrk (insPar 0 e')
    insPar :: Integer -> Expression -> Expression
    insPar i (EEqu (l, r)) = wrap i 0 (insPar 1 l .==. insPar 1 r)
    insPar i (EInc (l, r)) = wrap i 0 (insPar 1 l .|-. insPar 1 r)
    insPar i x@EIsc {} = wrap i 2 (foldr1 (./\.) (fmap (insPar 3) (exprIsc2list x)))
    insPar i x@EUni {} = wrap i 2 (foldr1 (.\/.) (fmap (insPar 3) (exprUni2list x)))
    insPar i (EDif (l, r)) = wrap i 4 (insPar 5 l .-. insPar 5 r)
    insPar i (ELrs (l, r)) = wrap i 6 (insPar 7 l ./. insPar 7 r)
    insPar i (ERrs (l, r)) = wrap i 6 (insPar 7 l .\. insPar 7 r)
    insPar i (EDia (l, r)) = wrap i 6 (insPar 7 l .<>. insPar 7 r)
    insPar i x@ECps {} = wrap i 8 (foldr1 (.:.) (fmap (insPar 9) (exprCps2list x)))
    insPar i x@ERad {} = wrap i 8 (foldr1 (.!.) (fmap (insPar 9) (exprRad2list x)))
    insPar i x@EPrd {} = wrap i 8 (foldr1 (.*.) (fmap (insPar 9) (exprPrd2list x)))
    insPar _ (EKl0 e) = EKl0 (insPar 10 e)
    insPar _ (EKl1 e) = EKl1 (insPar 10 e)
    insPar _ (EFlp e) = EFlp (insPar 10 e)
    insPar _ (ECpl e) = ECpl (insPar 10 e)
    insPar i (EBrk e) = insPar i e
    insPar _ x = x -- x@EDcD{} or EDcI{} or EEps{} or EDcV{} or EMp1{}
    foldr1 :: (Expression -> Expression -> Expression) -> NE.NonEmpty Expression -> Expression
    foldr1 fun nonempty = foldr fun (NE.last nonempty) (NE.init nonempty)
