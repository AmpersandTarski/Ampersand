module Ampersand.FSpec.ToFSpec.Populated 
    (fullContents,atomValuesOf
    , smallerConcepts, largerConcepts, sortSpecific2Generic
    , genericAndSpecifics, safePSingleton2AAtomVal
    ) 
where
{- This file contains all functions to compute populations.
   The implementation is done through Haskell's Map mechanism, as defined in Data.Map, for reasons of efficiency.
-}

import Prelude hiding (lookup)
import Ampersand.ADL1.Expression                 (notCpl)
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Basics hiding (empty)
import Data.Map as Map hiding (null, unions,delete)
   -- WHY: don't we use strict Maps? Since the sets of atoms and pairs are finite, we might want the efficiency of strictness.
import qualified Data.Set as Set
import Data.List (nub,delete)
       
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
sortSpecific2Generic gens cps = go [] cps
  where go xs [] = xs
        go xs (y:ys)
          | null (smallerConcepts gens y `isc` ys) = go (xs++[y]) ys
          | otherwise                              = go xs (ys++[y])
-- | This function returns the atoms of a concept (like fullContents does for relation-like things.)
atomValuesOf :: ContextInfo -- the relevant info of the context
        -> [Population] 
        -> A_Concept    -- the concept from which the population is requested
        -> [AAtomValue]     -- the elements in the concept's set of atoms
atomValuesOf ci pt c
 = case c of
     ONE -> [AtomValueOfONE]
     PlainConcept{}
         -> let smallerconcs = c:smallerConcepts (ctxiGens ci) c in
            nub$ [apLeft p  | pop@ARelPopu{} <- pt, source (popdcl pop) `elem` smallerconcs, p <- popps pop]
               ++[apRight p | pop@ARelPopu{} <- pt, target (popdcl pop) `elem` smallerconcs, p <- popps pop]
               ++[a         | pop@ACptPopu{} <- pt, popcpt pop `elem` smallerconcs, a <- popas pop]
pairsOf :: ContextInfo -> [Population] -> Declaration -> Map AAtomValue (Set.Set AAtomValue)
pairsOf ci ps dcl
 = case dcl of
     Isn c  -> fromList [ (a,Set.singleton a )   | a  <-atomValuesOf ci ps c]
     Vs sgn -> fromList [ (sa, Set.fromList (atomValuesOf ci ps (target sgn))) | sa <-atomValuesOf ci ps (source sgn)]
     Sgn{}  -> unionsWith Set.union
                      [ fromListWith Set.union [ (apLeft p,Set.singleton $ apRight p) | p<-popps pop]
                      | pop@ARelPopu{} <- ps
                      , name dcl==name (popdcl pop)
                      , let s=source (popdcl pop) in s `elem` source dcl:smallerConcepts (ctxiGens ci) (source dcl)
                      , let t=target (popdcl pop) in t `elem` target dcl:smallerConcepts (ctxiGens ci) (target dcl)
                      ]

fullContents :: ContextInfo -> [Population] -> Expression -> [AAtomPair]
fullContents ci ps e = [ mkAtomPair a b | let pairMap=contents e, (a,bs)<-Map.toList pairMap, b<-Set.toList bs ]
  where
   unions t1 t2 = unionWith Set.union t1 t2
   inters t1 t2 = mergeWithKey (\_ l r -> Just (Set.intersection l r)) c c t1 t2
                  where c=const empty
   differ t1 t2 = differenceWith (\l r->Just (Set.difference l r)) t1 t2
   contents :: Expression -> Map AAtomValue (Set.Set AAtomValue)
   contents expr
    = let aVals = atomValuesOf ci ps 
          lkp :: (Ord k) => k -> Map k (Set.Set a) -> [a]
          lkp x contMap = Set.toList (Map.findWithDefault Set.empty x contMap) in
      case expr of
         EEqu (l,r) -> contents ((l .|-. r) ./\. (r .|-. l))
         EInc (l,r) -> contents (notCpl l .\/. r)
         EUni (l,r) -> unions (contents l) (contents r)
         EIsc (l,r) -> inters (contents l) (contents r)
         EDif (l,r) -> differ (contents l) (contents r)
         -- The left residual l/r is defined by: for all x,y:  x(l/r)y  <=>  for all z in X, y r z implies x l z.
         ELrs (l,r) -> fromListWith Set.union
                       [(x,Set.singleton y) | x<-aVals (source l), y<-aVals (source r)
                                , null (lkp y (contents r) >- lkp x (contents l))
                                ]
         -- The right residual l\r defined by: for all x,y:   x(l\r)y  <=>  for all z in X, z l x implies z r y.
         ERrs (l,r) -> fromListWith Set.union
                       [(x,Set.singleton y) | x<-aVals (target l), y<-aVals (target r)
                                , null (lkp x (contents (EFlp l)) >- lkp y (contents (EFlp r)))
                                ]
         EDia (l,r) -> fromListWith Set.union
                       [(x,Set.singleton y) | x<-aVals (source l), y<-aVals (source r)
                                , null (lkp y (contents r) >- lkp x (contents l))
                                , null (lkp y (contents l) >- lkp x (contents r))
                                ]
         ERad (l,r) -> fromListWith Set.union
                       [(x,Set.singleton y) | x<-aVals (source l), y<-aVals (target r)
                                , null (aVals (target l) >- (lkp x (contents l) `uni` lkp y (contents (EFlp r))))
                                ]
         EPrd (l,r) -> fromList $
                       [ (a,Set.fromList cod) | a <- aVals (source l), let cod=aVals (target r), not (null cod) ]
         ECps (l,r) -> fromListWith Set.union
                       [(x,Set.singleton y) | (x,xv)<-Map.toList (contents l), (y,yv)<-Map.toList flipr
                                , (not. Set.null) (xv `Set.intersection` yv)
                                ] where flipr = contents (EFlp r)
         EKl0 x     -> if source x == target x --see #166
                       then transClosureMap (unionWith Set.union (contents x) (contents (EDcI (source x))))
                       else fatal 87 ("source and target of "++show x++show (sign x)++ " are not equal.")
         EKl1 x     -> if source x == target x --see #166
                       then transClosureMap (contents x)
                       else fatal 90 ("source and target of "++show x++show (sign x)++ " are not equal.")
         EFlp x     -> fromListWith Set.union [(b,Set.singleton a) | (a,bs)<-assocs (contents x), b<-Set.toList bs]
         ECpl x     -> contents (EDcV (sign x) .-. x)
         EBrk x     -> contents x
         EDcD dcl   -> pairsOf ci ps dcl
         EDcI c     -> fromList [(a, Set.singleton a) | a <- aVals c]
         EEps i _   -> fromList [(a, Set.singleton a) | a <- aVals i]
         EDcV sgn   -> fromList [(s, Set.fromList cod) | s <- aVals (source sgn), let cod=aVals (target sgn), not (null cod) ]
         EMp1 val c -> if name c == "SESSION" -- prevent populating SESSION
                                  then Map.empty
                                  else Map.singleton av (Set.singleton av)
                         where 
                           av = safePSingleton2AAtomVal ci c val

    
