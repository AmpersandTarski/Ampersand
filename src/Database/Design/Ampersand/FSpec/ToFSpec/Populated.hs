module Database.Design.Ampersand.FSpec.ToFSpec.Populated 
    (fullContents,atomValuesOf
    , smallerConcepts, largerConcepts, rootConcepts, genericAndSpecifics, safePSingleton2AAtomVal, typologies
    ) 
where
{- This file contains all functions to compute populations.
   The implementation is done through Haskell's Map mechanism, as defined in Data.Map, for reasons of efficiency.
-}

import Prelude hiding (lookup)
import Database.Design.Ampersand.ADL1.Expression                 (notCpl)
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Basics hiding (empty)
import Data.Map hiding (null, unions,delete)
   -- WHY: don't we use strict Maps? Since the sets of atoms and pairs are finite, we might want the efficiency of strictness.
import Data.Maybe (maybeToList)
import Data.List (nub,delete)
import Database.Design.Ampersand.Classes.ConceptStructure
import Database.Design.Ampersand.Classes.ViewPoint
  
       
       
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

-- | this function returns the most generic concepts in the class of a given concept
rootConcepts :: [A_Gen]  -> [A_Concept] -> [A_Concept]
rootConcepts gs cpts = [ root | root<-nub $ [ c | cpt<-cpts, c<-largerConcepts gs cpt ] `uni` cpts
                                , root `notElem` [ genspc g | g@Isa{}<-gs]++[c | g@IsE{}<-gs, c<-genrhs g ]
                                ]


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
pairsOf :: ContextInfo -> [Population] -> Declaration -> Map AAtomValue [AAtomValue]
pairsOf ci ps dcl
 = case dcl of
     Isn c  -> fromList [ (a,[a] )   | a  <-atomValuesOf ci ps c]
     Vs sgn -> fromList [ (sa, atomValuesOf ci ps (target sgn)) | sa <-atomValuesOf ci ps (source sgn)]
     Sgn{}  -> unionsWith uni
                      [ fromListWith uni [ (apLeft p,[apRight p]) | p<-popps pop]
                      | pop@ARelPopu{} <- ps
                      , name dcl==name (popdcl pop)
                      , let s=source (popdcl pop) in s `elem` source dcl:smallerConcepts (ctxiGens ci) (source dcl)
                      , let t=target (popdcl pop) in t `elem` target dcl:smallerConcepts (ctxiGens ci) (target dcl)
                      ]

fullContents :: ContextInfo -> [Population] -> Expression -> [AAtomPair]
fullContents ci ps e = [ mkAtomPair a b | let pairMap=contents e, a<-keys pairMap, b<-pairMap ! a ]
  where
   unions t1 t2 = unionWith uni t1 t2
   inters t1 t2 = mergeWithKey (\_ l r ->case l `isc` r of [] -> Nothing; atoms -> Just atoms) c c t1 t2
                  where c=const empty
   differ t1 t2 = differenceWith (\l r->case l >- r of [] -> Nothing; atoms -> Just atoms) t1 t2
   contents :: Expression -> Map AAtomValue [AAtomValue]
   contents expr
    = let aVals = atomValuesOf ci ps 
          lkp x contMap = (concat.maybeToList.lookup x) contMap in  -- (!) may not be used, because we don't know whether x `elem` keys fmap
      case expr of
         EEqu (l,r) -> contents ((l .|-. r) ./\. (r .|-. l))
         EInc (l,r) -> contents (notCpl l .\/. r)
         EUni (l,r) -> unions (contents l) (contents r)
         EIsc (l,r) -> inters (contents l) (contents r)
         EDif (l,r) -> differ (contents l) (contents r)
         -- The left residual l/r is defined by: for all x,y:  x(l/r)y  <=>  for all z in X, y r z implies x l z.
         ELrs (l,r) -> fromListWith (++)
                       [(x,[y]) | x<-aVals (source l), y<-aVals (source r)
                                , null (lkp y (contents r) >- lkp x (contents l))
                                ]
         -- The right residual l\r defined by: for all x,y:   x(l\r)y  <=>  for all z in X, z l x implies z r y.
         ERrs (l,r) -> fromListWith uni
                       [(x,[y]) | x<-aVals (target l), y<-aVals (target r)
                                , null (lkp x (contents (EFlp l)) >- lkp y (contents (EFlp r)))
                                ]
         EDia (l,r) -> fromListWith (++)
                       [(x,[y]) | x<-aVals (source l), y<-aVals (source r)
                                , null (lkp y (contents r) >- lkp x (contents l))
                                , null (lkp y (contents l) >- lkp x (contents r))
                                ]
         ERad (l,r) -> fromListWith uni
                       [(x,[y]) | x<-aVals (source l), y<-aVals (target r)
                                , null (aVals (target l) >- (lkp x (contents l) `uni` lkp y (contents (EFlp r))))
                                ]
         EPrd (l,r) -> fromList $
                       [ (a,cod) | a <- aVals (source l), let cod=aVals (target r), not (null cod) ]
         ECps (l,r) -> fromListWith uni
                       [(x,[y]) | x<-keys (contents l), y<-keys flipr
                                , (not.null) ((contents l ! x ) `isc` (flipr ! y))
                                ] where flipr = contents (EFlp r)
         EKl0 x     -> if source x == target x --see #166
                       then transClosureMap (unionWith uni (contents x) (contents (EDcI (source x))))
                       else fatal 87 ("source and target of "++show x++show (sign x)++ " are not equal.")
         EKl1 x     -> if source x == target x --see #166
                       then transClosureMap (contents x)
                       else fatal 90 ("source and target of "++show x++show (sign x)++ " are not equal.")
         EFlp x     -> fromListWith uni [(b,[a]) | (a,bs)<-assocs (contents x), b<-bs]
         ECpl x     -> contents (EDcV (sign x) .-. x)
         EBrk x     -> contents x
         EDcD dcl   -> pairsOf ci ps dcl
         EDcI c     -> fromList [(a,[a]) | a <- aVals c]
         EEps i _   -> fromList [(a,[a]) | a <- aVals i]
         EDcV sgn   -> fromList [(s, cod) | s <- aVals (source sgn), let cod=aVals (target sgn), not (null cod) ]
         EMp1 val c -> fromList $ if name c == "SESSION" -- prevent populating SESSION
                                  then []
                                  else [(av,[av])]
                         where 
                           av = safePSingleton2AAtomVal ci c val

typologies :: A_Context -> [Typology]
typologies context = Prelude.map mkTypology (conceptSets (concs context))
  where
    allGens = gens context
    mkTypology :: [A_Concept] -> Typology
    mkTypology cs = Typology { tyroot = rootConcepts allGens cs
                             , tyCpts = cs
                             } 
    conceptSets :: [A_Concept] -> [[A_Concept]]
    conceptSets []     = []
    conceptSets (c:cs) = let theSet = [c] `uni` largerConcepts allGens c `uni` smallerConcepts allGens c 
                         in theSet : conceptSets (Prelude.filter (\x -> x `notElem` theSet) cs)
    
