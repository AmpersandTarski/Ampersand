module Ampersand.FSpec.ToFSpec.Populated 
    (fullContents,atomValuesOf
    , smallerConcepts, largerConcepts, sortSpecific2Generic
    , genericAndSpecifics, safePSingleton2AAtomVal
    ) 
where
{- This file contains all functions to compute populations.
   The implementation is done through Haskell's Map mechanism, as defined in Data.Map, for reasons of efficiency.
-}

import           Ampersand.ADL1
import           Ampersand.Basics
import qualified Data.Map as Map
   -- WHY: don't we use strict Maps? Since the sets of atoms and pairs are finite, we might want the efficiency of strictness.
import qualified Data.Set as Set
import           Data.List (nub,delete)
       
genericAndSpecifics :: A_Gen -> [(A_Concept,A_Concept)]
genericAndSpecifics gen = 
    case gen of
      Isa{} -> [(genspc gen, gengen gen)]
      IsE{} -> [(genspc gen, g ) | g<-genrhs gen]

-- | this function takes all generalisation relations from the context and a concept and delivers a list of all concepts that are more specific than the given concept.
--   If there are no cycles in the generalization graph,  cpt  cannot be an element of  smallerConcepts gens cpt.
smallerConcepts :: [A_Gen] -> A_Concept -> [A_Concept]
smallerConcepts gs cpt
  = nub$ oneSmaller ++ concatMap (smallerConcepts gs) oneSmaller
  where oneSmaller = delete cpt. nub $ [ genspc g | g@Isa{}<-gs, gengen g==cpt ]++[ genspc g | g@IsE{}<-gs, cpt `elem` genrhs g ]
-- | this function takes all generalisation relations from the context and a concept and delivers a list of all concepts that are more generic than the given concept.
largerConcepts :: [A_Gen] -> A_Concept -> [A_Concept]
largerConcepts gs cpt
 = nub$ oneLarger ++ concatMap (largerConcepts gs) oneLarger
  where oneLarger  = delete cpt. nub $[ gengen g | g@Isa{}<-gs, genspc g==cpt ]++[ c | g@IsE{}<-gs, genspc g==cpt, c<-genrhs g ]

-- | This function returns a list of the same concepts, but in an ordering such that if for any two elements a and b in the 
--   list, if a is more specific than b, a will be to the left of b in the resulting list.
sortSpecific2Generic :: [A_Gen] -> [A_Concept] -> [A_Concept]
sortSpecific2Generic gens = go []
  where go xs [] = xs
        go xs (y:ys) = case [y' | y'<-nub ys, y' `elem` (Set.fromList $ smallerConcepts gens y)] of
                          []  -> go (xs++[y]) ys
                          _:_ -> go xs (ys++[y])
-- | This function returns the atoms of a concept (like fullContents does for relation-like things.)
atomValuesOf :: ContextInfo -- the relevant info of the context
        -> [Population]
        -> A_Concept    -- the concept from which the population is requested
        -> AAtomValues     -- the elements in the concept's set of atoms
atomValuesOf ci pt c
 = case c of
     ONE -> Set.singleton AtomValueOfONE
     PlainConcept{}
         -> let smallerconcs = c:smallerConcepts (ctxiGens ci) c in
            Set.fromList $
                 [apLeft p  | pop@ARelPopu{} <- pt, source (popdcl pop) `elem` smallerconcs, p <- Set.elems $ popps pop]
               ++[apRight p | pop@ARelPopu{} <- pt, target (popdcl pop) `elem` smallerconcs, p <- Set.elems $ popps pop]
               ++[a         | pop@ACptPopu{} <- pt, popcpt pop `elem` smallerconcs, a <- popas pop]
pairsOf :: ContextInfo -> [Population] -> Relation -> Map.Map AAtomValue AAtomValues
pairsOf ci ps dcl
 = Map.unionsWith Set.union
     [ Map.fromListWith Set.union [ (apLeft p,Set.singleton $ apRight p) | p<-Set.elems $ popps pop]
     | pop@ARelPopu{} <- ps
     , name dcl==name (popdcl pop)
     , let s=source (popdcl pop) in s `elem` source dcl:smallerConcepts (ctxiGens ci) (source dcl)
     , let t=target (popdcl pop) in t `elem` target dcl:smallerConcepts (ctxiGens ci) (target dcl)
     ]

fullContents :: ContextInfo -> [Population] -> Expression -> AAtomPairs
fullContents ci ps e = Set.fromList [ mkAtomPair a b | let pairMap=contents e, (a,bs)<-Map.toList pairMap, b<-Set.toList bs ]
  where
   unions = Map.unionWith Set.union
   inters = Map.mergeWithKey (\_ l r -> Just (Set.intersection l r)) c c
                  where c=const Map.empty
   differ = Map.differenceWith (\l r->Just (Set.difference l r))
   contents :: Expression -> Map.Map AAtomValue AAtomValues
   contents expr
    = let aVals = atomValuesOf ci ps 
          lkp :: (Ord k) => k -> Map.Map k (Set.Set a) -> (Set.Set a)
          lkp x contMap = (Map.findWithDefault Set.empty x contMap) in
      case expr of
         EEqu (l,r) -> contents ((l .|-. r) ./\. (r .|-. l))
         EInc (l,r) -> contents (notCpl l .\/. r)
         EUni (l,r) -> unions (contents l) (contents r)
         EIsc (l,r) -> inters (contents l) (contents r)
         EDif (l,r) -> differ (contents l) (contents r)
         -- The left residual l/r is defined by: for all x,y:  x(l/r)y  <=>  for all z in X, y r z implies x l z.
         ELrs (l,r) -> Map.fromListWith Set.union
                       [(x,Set.singleton y) | x<-Set.elems $ aVals (source l), y<-Set.elems $ aVals (source r)
                                , null (lkp y (contents r) Set.\\ lkp x (contents l))
                                ]
         -- The right residual l\r defined by: for all x,y:   x(l\r)y  <=>  for all z in X, z l x implies z r y.
         ERrs (l,r) -> Map.fromListWith Set.union
                       [(x,Set.singleton y) | x<-Set.elems $ aVals (target l), y<-Set.elems $ aVals (target r)
                                , null (lkp x (contents (EFlp l)) Set.\\ lkp y (contents (EFlp r)))
                                ]
         EDia (l,r) -> Map.fromListWith Set.union
                       [(x,Set.singleton y) | x <-Set.elems $ aVals (source l), y <- Set.elems $ aVals (target r)
                                , null (lkp y (contents (EFlp r)) Set.\\ lkp x (contents l))
                                , null (lkp x (contents l) Set.\\ lkp y (contents (EFlp r)))
                                ]
         ERad (l,r) -> Map.fromListWith Set.union
                       [(x,Set.singleton y) | x<-Set.elems $ aVals (source l), y<-Set.elems $ aVals (target r)
                                , null (aVals (target l) Set.\\ (lkp x (contents l) `Set.union` lkp y (contents (EFlp r))))
                                ]
         EPrd (l,r) -> Map.fromList
                       [ (a,Set.fromList cod) | a <- Set.elems $ aVals (source l), let cod=Set.elems $ aVals (target r), not (null cod) ]
         ECps (l,r) -> Map.fromListWith Set.union
                       [(x,Set.singleton y) | (x,xv)<-Map.toList (contents l), (y,yv)<-Map.toList flipr
                                , (not. Set.null) (xv `Set.intersection` yv)
                                ] where flipr = contents (EFlp r)
         EKl0 x     -> if source x == target x --see #166
                       then transClosureMap (Map.unionWith Set.union (contents x) (contents (EDcI (source x))))
                       else fatal ("source and target of "++show x++show (sign x)++ " are not equal.")
         EKl1 x     -> if source x == target x --see #166
                       then transClosureMap (contents x)
                       else fatal ("source and target of "++show x++show (sign x)++ " are not equal.")
         EFlp x     -> Map.fromListWith Set.union [(b,Set.singleton a) | (a,bs)<-Map.assocs (contents x), b<-Set.toList bs]
         ECpl x     -> contents (EDcV (sign x) .-. x)
         EBrk x     -> contents x
         EDcD dcl   -> pairsOf ci ps dcl
         EDcI c     -> Map.fromList [(a, Set.singleton a) | a <- Set.elems $ aVals c]
         EEps i _   -> Map.fromList [(a, Set.singleton a) | a <- Set.elems $ aVals i]
         EDcV sgn   -> Map.fromList [(s, Set.fromList cod) | s <- Set.elems $ aVals (source sgn), let cod=Set.elems $ aVals (target sgn), not (null cod) ]
         EMp1 val c -> if name c == "SESSION" -- prevent populating SESSION with "_SESSION"
                          && show val == show "_SESSION"
                        then Map.empty
                        else Map.singleton av (Set.singleton av)
                         where 
                           av = safePSingleton2AAtomVal ci c val

    
