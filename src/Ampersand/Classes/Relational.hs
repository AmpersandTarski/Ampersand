module Ampersand.Classes.Relational
  ( HasProps (..),
    Relational (..),
    isONE,
    isSESSION,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import qualified RIO.Set as Set

class HasProps r where
  properties :: r -> AProps

class Relational r where
  isProp :: r -> Bool -- > If True, then the argument is a property. Otherwise we don't know.
  isIdent :: r -> Bool -- > If True, then the argument is equivalent to I. Otherwise we don't know.
  isImin :: r -> Bool -- > If True, then the argument is equivalent to -I. Otherwise we don't know.
  isTrue :: r -> Bool -- > If True, then the argument is equivalent to V. Otherwise we don't know.
  isFalse :: r -> Bool -- > If True, then the argument is equivalent to -V. Otherwise we don't know.
  isFunction :: r -> Bool -- > If True, then the argument is a total function. Otherwise we don't know.
  isTot :: r -> Bool -- > If True, then the argument is total. Otherwise we don't know.
  isUni :: r -> Bool -- > If True, then the argument is univalent. Otherwise we don't know.
  isSur :: r -> Bool -- > If True, then the argument is surjective. Otherwise we don't know.
  isInj :: r -> Bool -- > If True, then the argument is injective. Otherwise we don't know.
  isRfx :: r -> Bool -- > If True, then the argument is reflexive. Otherwise we don't know.
  isIrf :: r -> Bool -- > If True, then the argument is irreflexive. Otherwise we don't know.
  isTrn :: r -> Bool -- > If True, then the argument is transitive. Otherwise we don't know.
  isSym :: r -> Bool -- > If True, then the argument is symmetric. Otherwise we don't know.
  isAsy :: r -> Bool -- > If True, then the argument is antisymmetric. Otherwise we don't know.

instance HasProps Relation where
  properties = decprps

-- | Is the concept the ONE and only? (universal singleton)
isONE :: A_Concept -> Bool
isONE ONE = True
isONE _ = False

isSESSION :: A_Concept -> Bool
isSESSION cpt =
  case cpt of
    PlainConcept {} -> toText1Unsafe "SESSION" `elem` (fullName1 . fst <$> aliases cpt)
    ONE -> False

-- The function "properties" does not only provide the properties provided by the Ampersand user,
-- but tries to derive the most obvious constraints as well. The more property constraints are known,
-- the better the data structure that is derived.
-- Not every constraint that can be proven is obtained by this function. This does not hurt Ampersand.
instance HasProps Expression where
  properties expr = case expr of
    EDcD dcl -> properties dcl
    EDcI {} -> Set.fromList [Uni, Tot, Inj, Sur, Sym, Asy, Trn, Rfx]
    EEps a sgn -> Set.fromList $ [Tot | a == source sgn] ++ [Sur | a == target sgn] ++ [Uni, Inj]
    EDcV sgn ->
      Set.fromList
        $
        -- NOT totaal
        -- NOT surjective
        [Inj | isONE (source sgn)]
        ++ [Uni | isONE (target sgn)]
        ++ [Asy | isEndo sgn, isONE (source sgn)]
        ++ [Sym | isEndo sgn]
        ++ [Rfx | isEndo sgn]
        ++ [Trn | isEndo sgn]
    EBrk f -> properties f
    ECps (l, r) -> Set.filter (\x -> x `elem` [Uni, Tot, Inj, Sur]) (properties l `Set.intersection` properties r)
    EPrd (l, r) -> Set.fromList $ [Tot | isTot l] ++ [Sur | isSur r] ++ [Rfx | isRfx l && isRfx r] ++ [Trn]
    EKl0 e' -> Set.fromList [Rfx, Trn] `Set.union` (properties e' Set.\\ Set.fromList [Uni, Inj])
    EKl1 e' -> Set.singleton Trn `Set.union` (properties e' Set.\\ Set.fromList [Uni, Inj])
    ECpl e' -> Set.singleton Sym `Set.intersection` properties e'
    EFlp e' -> Set.map flp $ properties e'
    EMp1 {} -> Set.fromList [Uni, Inj, Sym, Asy, Trn]
    _ -> Set.empty

instance Relational Expression where -- TODO: see if we can find more property constraints...
  isTrue expr =
    case expr of
      EEqu (l, r) -> l == r || isTrue l && isTrue r || isFalse r && isFalse l
      EInc (l, _) -> isTrue l
      EIsc (l, r) -> isTrue l && isTrue r
      EUni (l, r) -> isTrue l || isTrue r
      EDif (l, r) -> isTrue l && isFalse r
      ECps (l, r)
        | isUni l && isTot l -> isTrue r
        | isInj r && isSur r -> isTrue l -- HJO, 20180331: Disabled this statement, for it has probably been bitrotted???
        -- SJO, 20220603: Restored this statement because this is the symmetric version of the former
        | otherwise -> isTrue l && isTrue r
      EPrd (l, r) -> isTrue l && isTrue r -- SJ, 20220604: if you refine this, please consider issue #1293
      EKl0 e -> isTrue e
      EKl1 e -> isTrue e
      EFlp e -> isTrue e
      ECpl e -> isFalse e
      EDcI c -> isONE c
      EEps i _ -> isONE i
      EDcV {} -> True
      EBrk e -> isTrue e
      _ -> False -- TODO: find richer answers for ERrs, ELrs, EDia, ERad, and EMp1
  isFalse expr =
    case expr of
      EEqu (l, r) -> l == notCpl r
      EInc (_, r) -> isFalse r
      EIsc (l, r) -> isFalse r || isFalse l
      EUni (l, r) -> isFalse r && isFalse l
      EDif (l, r) -> isFalse l || isTrue r
      ECps (l, r) -> isFalse r || isFalse l
      EPrd (l, r) -> isFalse r || isFalse l
      EKl0 e -> isFalse e
      EKl1 e -> isFalse e
      EFlp e -> isFalse e
      ECpl e -> isTrue e
      EBrk e -> isFalse e
      _ -> False -- TODO: find richer answers for ERrs, ELrs, EDia, and ERad

  isProp expr = isAsy expr && isSym expr
  isIdent expr = ( \x ->
                     if x && (source expr /= target expr)
                       then fatal $ "Something wrong with isIdent." <> tshow expr
                       else x
                 )
    $ case expr of
      EEqu (l, r) -> isIdent (EIsc (EInc (l, r), EInc (r, l))) -- TODO: maybe derive something better?
      EInc (l, r) -> isIdent (EUni (ECpl l, r)) -- TODO: maybe derive something better?
      EIsc (l, r) -> isIdent l && isIdent r
      EUni (l, r) -> isIdent l && isIdent r
      EDif (l, r) -> isIdent l && isFalse r
      ECps (l, r) -> isIdent l && isIdent r
      EKl0 e -> isIdent e || isFalse e
      EKl1 e -> isIdent e
      ECpl e -> isImin e
      EDcD _ -> False -- was: name dcl == "="
      EDcI {} -> True
      EEps {} -> False
      EDcV sgn -> isEndo sgn && isONE (source sgn)
      EBrk f -> isIdent f
      EFlp f -> isIdent f
      _ -> False -- TODO: find richer answers for ELrs, ERrs, EDia, EPrd, and ERad

  isImin expr' = case expr' of -- > tells whether the argument is equivalent to I-
    EEqu (l, r) -> isImin (EIsc (EInc (l, r), EInc (r, l))) -- TODO: maybe derive something better?
    EInc (l, r) -> isImin (EUni (ECpl l, r)) -- TODO: maybe derive something better?
    EIsc (l, r) -> isImin l && isImin r
    EUni (l, r) -> isImin l && isImin r
    EDif (l, r) -> isImin l && isFalse r
    ECpl e -> isIdent e
    EDcD {} -> False
    EDcI {} -> False
    EEps {} -> False
    EDcV {} -> False
    EBrk f -> isImin f
    EFlp f -> isImin f
    _ -> False -- TODO: find richer answers for ELrs, ERrs, and EDia
  isFunction r = isUni r && isTot r

  isTot = isTotSur Tot
  isSur = isTotSur Sur

  isUni = isUniInj Uni
  isInj = isUniInj Inj

  isRfx r = Rfx `elem` properties r
  isIrf r = Irf `elem` properties r
  isTrn r = Trn `elem` properties r
  isSym r = Sym `elem` properties r
  isAsy r = Asy `elem` properties r

-- Not to be exported:
isTotSur :: AProp -> Expression -> Bool
isTotSur prop expr =
  case expr of
    EEqu (_, _) -> False
    EInc (_, _) -> False
    EIsc (l, r) -> isTotSur prop l || isTotSur prop r
    EUni (_, _) -> todo
    EDif (l, _) -> isTotSur prop l
    ECps (l, r) -> isTotSur prop l && isTotSur prop r
    EPrd (_, _) -> todo
    EKl0 e -> isTotSur prop e
    EKl1 e -> isTotSur prop e
    EFlp e -> isTotSur (flp prop) e
    ECpl _ -> todo
    ELrs _ -> todo
    ERrs _ -> todo
    EDia _ -> todo
    ERad _ -> todo
    EDcD d -> prop `elem` properties d
    EDcI {} -> True
    EBin {} -> todo
    EEps c sgn -> case prop of
      Tot -> c == source sgn
      Sur -> c == target sgn
      _ -> fatal $ "isTotSur must not be called with " <> tshow prop
    EDcV {} -> todo
    EBrk e -> isTotSur prop e
    EMp1 {} -> True
  where
    todo = prop `elem` properties expr

isUniInj :: AProp -> Expression -> Bool
isUniInj prop expr =
  case expr of
    EEqu (_, _) -> False
    EInc (_, _) -> False
    EIsc (l, r) -> isUniInj prop l || isUniInj prop r
    EUni (_, _) -> todo
    EDif (l, _) -> isUniInj prop l
    ECps (l, r) -> isUniInj prop l && isUniInj prop r
    EPrd (_, _) -> todo
    EKl0 e -> isUniInj prop e
    EKl1 e -> isUniInj prop e
    EFlp e -> isUniInj (flp prop) e
    ECpl _ -> todo
    ELrs _ -> todo
    ERrs _ -> todo
    EDia _ -> todo
    ERad _ -> todo
    EDcD d -> prop `elem` properties d
    EDcI {} -> True
    EBin {} -> todo
    EEps {} -> True
    EDcV {} -> todo
    EBrk e -> isUniInj prop e
    EMp1 {} -> True
  where
    todo = prop `elem` properties expr
