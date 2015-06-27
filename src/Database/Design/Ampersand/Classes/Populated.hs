module Database.Design.Ampersand.Classes.Populated 
    (fullContents,atomsOf
    , smallerConcepts, largerConcepts, rootConcepts, genericAndSpecifics
    ) 
where
{- This file contains all functions to compute populations.
   The implementation is done through Haskell's Map mechanism, as defined in Data.Map, for reasons of efficiency.
-}

import Prelude hiding (lookup)
import Database.Design.Ampersand.ADL1.Pair
import Database.Design.Ampersand.ADL1.Expression                 (notCpl)
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Basics hiding (empty)
import Data.Map hiding (null, unions,delete)
   -- WHY: don't we use strict Maps? Since the sets of atoms and pairs are finite, we might want the efficiency of strictness.
import Data.Maybe (maybeToList)
import Data.List (nub,delete)
fatal :: Int -> String -> a
fatal = fatalMsg "Classes.Populated"

      
       
       
genericAndSpecifics :: A_Gen -> [(A_Concept,A_Concept)]
genericAndSpecifics gen = 
    case gen of
      Isa{} -> [(genspc gen, gengen gen)]
      IsE{} -> [(genspc gen, g ) | g<-genrhs gen]

-- | this function takes all generalisation relations from the context and a concept and delivers a list of all concepts that are more specific than the given concept.
--   If there are no cycles in the generalization graph,  cpt  cannot be an element of  smallerConcepts gens cpt.
smallerConcepts :: [A_Gen] -> A_Concept -> [A_Concept]
smallerConcepts gens cpt
  = nub$ oneSmaller ++ concatMap (smallerConcepts gens) oneSmaller
  where oneSmaller = delete cpt. nub $ [ genspc g | g@Isa{}<-gens, gengen g==cpt ]++[ genspc g | g@IsE{}<-gens, cpt `elem` genrhs g ]
-- | this function takes all generalisation relations from the context and a concept and delivers a list of all concepts that are more generic than the given concept.
largerConcepts :: [A_Gen] -> A_Concept -> [A_Concept]
largerConcepts gens cpt
 = nub$ oneLarger ++ concatMap (largerConcepts gens) oneLarger
  where oneLarger  = delete cpt. nub $[ gengen g | g@Isa{}<-gens, genspc g==cpt ]++[ c | g@IsE{}<-gens, genspc g==cpt, c<-genrhs g ]

-- | this function returns the most generic concepts in the class of a given concept
rootConcepts :: [A_Gen]  -> [A_Concept] -> [A_Concept]
rootConcepts gens cpts = [ root | root<-nub $ [ c | cpt<-cpts, c<-largerConcepts gens cpt ] `uni` cpts
                                , root `notElem` [ genspc g | g@Isa{}<-gens]++[c | g@IsE{}<-gens, c<-genrhs g ]
                                ]


-- | This function returns the atoms of a concept (like fullContents does for relation-like things.)
atomsOf :: [A_Gen]      -- the generalisation relations from the context
        -> [Population] -- the user defined populations in the context
        -> A_Concept    -- the concept from which the population is requested
        -> [String]     -- the elements in the concept's set of atoms
atomsOf gens pt c
 = case c of
     ONE -> ["1"] -- fatal 22 "Asking for the value of the universal singleton"
     PlainConcept{}
         -> let smallerconcs = c:smallerConcepts gens c in
            nub$ [srcPaire p | pop@PRelPopu{} <- pt, source (popdcl pop) `elem` smallerconcs, p <- popps pop]
               ++[trgPaire p | pop@PRelPopu{} <- pt, target (popdcl pop) `elem` smallerconcs, p <- popps pop]
               ++[a          | pop@PCptPopu{} <- pt, popcpt pop `elem` smallerconcs, a <- popas pop]

pairsOf :: [A_Gen] -> [Population] -> Declaration -> Map String [String]
pairsOf gens pt dcl
 = case dcl of
     Isn c  -> fromList [ (a,[a])   | a  <-atomsOf gens pt c]
     Vs sgn -> fromList [ (sa, atomsOf gens pt (target sgn)) | sa <-atomsOf gens pt (source sgn)]
     Sgn{}  -> unionsWith uni
                      [ fromListWith uni [ (srcPaire p,[trgPaire p]) | p<-popps pop]
                      | pop@PRelPopu{} <- pt
                      , name dcl==name (popdcl pop)
                      , let s=source (popdcl pop) in s `elem` source dcl:smallerConcepts gens (source dcl)
                      , let t=target (popdcl pop) in t `elem` target dcl:smallerConcepts gens (target dcl)
                      ]

fullContents :: [A_Gen] -> [Population] -> Expression -> Pairs
fullContents gens pt e = [ mkPair a b | let pairMap=contents e, a<-keys pairMap, b<-pairMap ! a ]
  where
   unions t1 t2 = unionWith uni t1 t2
   inters t1 t2 = mergeWithKey (\_ l r ->case l `isc` r of [] -> Nothing; atoms -> Just atoms) c c t1 t2
                  where c=const empty
   differ t1 t2 = differenceWith (\l r->case l >- r of [] -> Nothing; atoms -> Just atoms) t1 t2
   contents :: Expression -> Map String [String]
   contents expr
    = let lkp x contMap = (concat.maybeToList.lookup x) contMap in  -- (!) may not be used, because we don't know whether x `elem` keys fmap
      case expr of
         EEqu (l,r) -> contents ((l .|-. r) ./\. (r .|-. l))
         EImp (l,r) -> contents (notCpl l .\/. r)
         EUni (l,r) -> unions (contents l) (contents r)
         EIsc (l,r) -> inters (contents l) (contents r)
         EDif (l,r) -> differ (contents l) (contents r)
         -- The left residual l/r is defined by: for all x,y:  x(l/r)y  <=>  for all z in X, y r z implies x l z.
         ELrs (l,r) -> fromListWith (++)
                       [(x,[y]) | x<-atomsOf gens pt (source l), y<-atomsOf gens pt (source r)
                                , null (lkp y (contents r) >- lkp x (contents l))
                                ]
         -- The right residual l\r defined by: for all x,y:   x(l\r)y  <=>  for all z in X, z l x implies z r y.
         ERrs (l,r) -> fromListWith uni
                       [(x,[y]) | x<-atomsOf gens pt (target l), y<-atomsOf gens pt (target r)
                                , null (lkp x (contents (EFlp l)) >- lkp y (contents (EFlp r)))
                                ]
         EDia (l,r) -> fromListWith (++)
                       [(x,[y]) | x<-atomsOf gens pt (source l), y<-atomsOf gens pt (source r)
                                , null (lkp y (contents r) >- lkp x (contents l))
                                , null (lkp y (contents l) >- lkp x (contents r))
                                ]
         ERad (l,r) -> fromListWith uni
                       [(x,[y]) | x<-atomsOf gens pt (source l), y<-atomsOf gens pt (target r)
                                , null (atomsOf gens pt (target l) >- (lkp x (contents l) `uni` lkp y (contents (EFlp r))))
                                ]
         EPrd (l,r) -> fromList $
                       [ (a,cod) | a <- atomsOf gens pt (source l), let cod=atomsOf gens pt (target r), not (null cod) ]
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
         EDcD dcl   -> pairsOf gens pt dcl
         EDcI c     -> fromList [(a,[a]) | a <- atomsOf gens pt c]
         EEps i _   -> fromList [(a,[a]) | a <- atomsOf gens pt i]
         EDcV sgn   -> fromList [(s, cod) | s <- atomsOf gens pt (source sgn), let cod=atomsOf gens pt (target sgn), not (null cod) ]
         EMp1 a c   -> fromList [(a,[a]) | name c/="SESSION"] -- prevent populating SESSION
