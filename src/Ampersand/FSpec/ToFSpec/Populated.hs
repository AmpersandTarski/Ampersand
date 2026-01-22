module Ampersand.FSpec.ToFSpec.Populated
  ( fullContents,
    atomValuesOf,
    genericAndSpecifics,
    safePSingleton2AAtomVal,
  )
where

{- This file contains all functions to compute populations.
   The implementation is done through Haskell's Map mechanism, for reasons of efficiency.
-}

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree ( smallerConcepts )
import Ampersand.Classes hiding (gens)
-- import Ampersand.Core.ShowAStruct (showA)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

genericAndSpecifics :: AClassify -> [(A_Concept, A_Concept)]
genericAndSpecifics gen = filter (uncurry (/=))
  $ case gen of -- make sure that no tuples where source and target are equal are returned.
    Isa {} -> [(genspc gen, gengen gen)]
    IsE {} -> [(genspc gen, g) | g <- NE.toList $ genrhs gen]

-- | This function returns the atoms of a concept (like fullContents does for relation-like things.)
atomValuesOf ::
  [Population] ->
  A_Concept -> -- the concept from which the population is requested
  AAtomValues -- the elements in the concept's set of atoms
atomValuesOf pt c =
  case c of
    ONE -> Set.singleton AtomValueOfONE
    PlainConcept {} ->
      let smallerconcs = c : smallerConcepts c
          result = Set.fromList
            $ [apLeft p | pop@ARelPopu {} <- pt, source (popdcl pop) `elem` smallerconcs, p <- toList $ popps pop]
            ++ [apRight p | pop@ARelPopu {} <- pt, target (popdcl pop) `elem` smallerconcs, p <- toList $ popps pop]
            ++ [a | pop@ACptPopu {} <- pt, popcpt pop `elem` smallerconcs, a <- popas pop]
       in -- trace ("TRACE atomValuesOf: concept=" <> tshow c <> ", smallerconcs=" <> tshow smallerconcs <> ", result size=" <> tshow (Set.size result) <> ", atoms=" <> tshow result)
          result
    DISJT cpts -> (L.foldl Set.intersection Set.empty . fmap (atomValuesOf pt) . Set.toList) cpts -- needs to be computed to check that it is empty.
    UNION cpts -> (L.foldl    Set.union     Set.empty . fmap (atomValuesOf pt) . Set.toList) cpts
    ISECT cpts -> (L.foldl Set.intersection Set.empty . fmap (atomValuesOf pt) . Set.toList) cpts

pairsOf :: [Population] -> Relation -> Map.Map AAtomValue AAtomValues
pairsOf ps dcl =
  Map.unionsWith
    Set.union
    [ Map.fromListWith Set.union [(apLeft p, Set.singleton $ apRight p) | p <- toList $ popps pop]
      | pop@ARelPopu {} <- ps,
        name dcl == name (popdcl pop),
        let s = source (popdcl pop) in s `elem` source dcl : smallerConcepts (source dcl),
        let t = target (popdcl pop) in t `elem` target dcl : smallerConcepts (target dcl)
    ]

fullContents :: ContextInfo -> [Population] -> Expression -> AAtomPairs
fullContents ci ps e = -- trace ("\n=== fullContents called for: " <> showA e <> " ===") $
                        Set.fromList result
  where
    result = -- trace ("  Result pairs: " <> tshow resultPairs)
             resultPairs
      where resultPairs = [mkAtomPair a b | let pairMap = contents e, (a, bs) <- Map.toList pairMap, b <- Set.toList bs]
    unions = Map.unionWith Set.union
    inters = Map.mergeWithKey (\_ l r -> Just (Set.intersection l r)) c c
      where
        c = const Map.empty
    differ = Map.differenceWith (\l r -> Just (Set.difference l r))
    contents :: Expression -> Map.Map AAtomValue AAtomValues
    contents expr =
      let aVals = atomValuesOf ps
          lkp :: (Ord k) => k -> Map.Map k (Set.Set a) -> Set.Set a
          lkp = Map.findWithDefault Set.empty
       in case expr of
            EEqu (l, r) -> contents ((l .|-. r) .\/. (r .|-. l))  -- compute the symmetric difference
            EInc (l, r) -> contents (notCpl l .\/. r)
            EUni (l, r) -> unions (contents l) (contents r)
            EIsc (l, r) -> inters (contents l) (contents r)
            EDif (l, r) -> 
              -- trace ("\n  EDif case: " <> showA l <> " - " <> showA r <>
              --        "\n    left contents: " <> tshow (Map.toList $ contents l) <>
              --        "\n    right contents: " <> tshow (Map.toList $ contents r) <>
              --        "\n    result: " <> tshow resultMap)
               resultMap
                where
                  resultMap = differ (contents l) (contents r)
            -- The left residual l/r is defined by: for all x,y:  x(l/r)y  <=>  for all z in X, y r z implies x l z.
            ELrs (l, r) ->
              Map.fromListWith
                Set.union
                [ (x, Set.singleton y) | x <- toList $ aVals (source l), y <- toList $ aVals (source r), null (lkp y (contents r) Set.\\ lkp x (contents l))
                ]
            -- The right residual l\r defined by: for all x,y:   x(l\r)y  <=>  for all z in X, z l x implies z r y.
            ERrs (l, r) ->
              Map.fromListWith
                Set.union
                [ (x, Set.singleton y) | x <- toList $ aVals (target l), y <- toList $ aVals (target r), null (lkp x (contents (EFlp l)) Set.\\ lkp y (contents (EFlp r)))
                ]
            EDia (l, r) ->
              Map.fromListWith
                Set.union
                [ (x, Set.singleton y) | x <- toList $ aVals (source l), y <- toList $ aVals (target r), null (lkp y (contents (EFlp r)) Set.\\ lkp x (contents l)), null (lkp x (contents l) Set.\\ lkp y (contents (EFlp r)))
                ]
            ERad (l, r) ->
              Map.fromListWith
                Set.union
                [ (x, Set.singleton y) | x <- toList $ aVals (source l), y <- toList $ aVals (target r), null (aVals (target l) Set.\\ (lkp x (contents l) `Set.union` lkp y (contents (EFlp r))))
                ]
            EPrd (l, r) ->
              Map.fromList
                [ (a, Set.fromList cod)
                  | let cod = toList $ aVals (target r),
                    not (null cod),
                    a <- toList $ aVals (source l)
                ]
            ECps (l, r) ->
              -- trace ("\n  ECps case: " <> showA l <> " ; " <> showA r <>
              --        "\n    left contents: " <> tshow (Map.toList $ contents l) <>
              --        "\n    right contents: " <> tshow (Map.toList $ contents r) <>
              --        "\n    result map: " <> tshow resultMap)
              resultMap
               where
                 flipr = contents (EFlp r)
                 resultMap = Map.fromListWith
                   Set.union
                   [ (x, Set.singleton y) | (x, xv) <- Map.toList (contents l), (y, yv) <- Map.toList flipr, (not . Set.null) (xv `Set.intersection` yv)
                   ]
            EKl0 x ->
              if source x == target x -- see #166
                then transClosureMap (Map.unionWith Set.union (contents x) (contents (EDcI (source x))))
                else fatal ("source and target of " <> tshow x <> tshow (sign x) <> " are not equal.")
            EKl1 x ->
              if source x == target x -- see #166
                then transClosureMap (contents x)
                else fatal ("source and target of " <> tshow x <> tshow (sign x) <> " are not equal.")
            EFlp x -> Map.fromListWith Set.union [(b, Set.singleton a) | (a, bs) <- Map.assocs (contents x), b <- Set.toList bs]
            ECpl x -> contents (EDcV (sign x) .-. x)
            EBrk x -> contents x
            EDcD dcl -> pairsOf ps dcl
            EDcI c -> Map.fromList [(a, Set.singleton a) | a <- toList $ aVals c]
            EBin oper sgn -> -- trace ("EBin "<>tshow oper<>" "<>tshow sgn<>" "<>tshow result) $
                           Map.fromList binOpPop
             where
               binOpPop =
                [ (s, Set.fromList cod)
                  | s <- toList $ aVals (source sgn),
                    let cod = filter (binaryFunction oper s) . toList $ aVals (target sgn),
                    not (null cod)
                ]
            EDcV sgn ->
              -- trace ("\n  EDcV case: " <> tshow sgn <>
              --        "\n    source atoms: " <> tshow (toList $ aVals (source sgn)) <>
              --        "\n    target atoms: " <> tshow (toList $ aVals (target sgn)) <>
              --        "\n    result map: " <> tshow resultMap)
              resultMap
               where
                 resultMap = Map.fromList
                   [ (s, Set.fromList cod)
                     | let cod = toList $ aVals (target sgn),
                       not (null cod),
                       s <- toList $ aVals (source sgn)
                   ]
            EMp1 val c ->
              -- trace ("\n  EMp1 case: atom " <> tshow val <> " in concept " <> tshow c <>
              --        "\n    result: " <> tshow resultMap)
              resultMap
               where
                 resultMap = if isSESSION c -- prevent populating SESSION with "_SESSION"
                               && tshow val == tshow ("_SESSION" :: Text)
                             then Map.empty
                             else Map.singleton av (Set.singleton av)
                 av = safePSingleton2AAtomVal ci c val
