{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.Populated                 (fullContents,atomsOf)
{- This file contains all functions to compute populations.
   The implementation is done through Haskell's Map mechanism, as defined in Data.Map, for reasons of efficiency.
-}
where
   import Prelude hiding (lookup)
   import DatabaseDesign.Ampersand.ADL1.Pair                       (srcPaire,trgPaire)
   import DatabaseDesign.Ampersand.ADL1.Expression                 (notCpl)
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Basics                          (Collection (uni,isc,(>-)),fatalMsg, Identified(..))  
   import Data.Map (Map, (!), lookup, keys, assocs, elems, fromList, fromListWith, unionWith, unionsWith, differenceWith, mergeWithKey, empty)
      -- WHY: don't we use strict Maps? Since the sets of atoms and pairs are finite, we might want the efficiency of strictness.
   import Data.Maybe (maybeToList)
   import Data.List (nub)
   fatal :: Int -> String -> a
   fatal = fatalMsg "Classes.Populated"

   
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
               nub$ [srcPaire p | pop@PRelPopu{} <- pt, source (popsgn pop) `elem` smallerconcs, p <- popps pop]
                  ++[trgPaire p | pop@PRelPopu{} <- pt, target (popsgn pop) `elem` smallerconcs, p <- popps pop]
                  ++[a          | pop@PCptPopu{} <- pt, popcpt pop `elem` smallerconcs, a <- popas pop]

   pairsOf :: [A_Gen] -> [Population] -> Declaration -> Map String [String]
   pairsOf gens pt dcl
    = case dcl of
        Isn c  -> fromList [ (a,[a])   | a  <-atomsOf gens pt c]
        Vs sgn -> fromList [ (sa, atomsOf gens pt (target sgn)) | sa <-atomsOf gens pt (source sgn)]
        Sgn{}  -> unionsWith uni
                         [ fromListWith uni [ (a,[b]) | (a,b)<-popps pop]
                         | pop@PRelPopu{} <- pt
                         , name dcl==name (popdcl pop)
                         , let s=source (popdcl pop) in s `elem` s:smallerConcepts gens (source dcl)
                         , let t=target (popdcl pop) in t `elem` t:smallerConcepts gens (target dcl)
                         ]

   fullContents :: [A_Gen] -> [Population] -> Expression -> Pairs
   fullContents gens pt e = [ (a,b) | let pairMap=contents e, a<-keys pairMap, b<-pairMap ! a ]
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
                          then closMap (unionWith uni (contents x) (contents (EDcI (source x))))
                          else fatal 87 ("source and target of "++show x++show (sign x)++ " are not equal.")
            EKl1 x     -> if source x == target x --see #166
                          then closMap (contents x)
                          else fatal 90 ("source and target of "++show x++show (sign x)++ " are not equal.")
            EFlp x     -> fromListWith uni [(b,[a]) | (a,bs)<-assocs (contents x), b<-bs]
            ECpl x     -> contents (EDcV (sign x) .-. x)
            EBrk x     -> contents x
            EDcD dcl   -> pairsOf gens pt dcl
            EDcI c     -> fromList [(a,[a]) | a <- atomsOf gens pt c]
            EEps i _   -> fromList [(a,[a]) | a <- atomsOf gens pt i]
            EDcV sgn   -> fromList [(s, cod) | s <- atomsOf gens pt (source sgn), let cod=atomsOf gens pt (target sgn), not (null cod) ]
            EMp1 a c   -> fromList [(a,[a]) | name c/="SESSION"] -- prevent populating SESSION

----------------------------------------------------
--  Warshall's transitive closure algorithm in Haskell:
----------------------------------------------------
   closMap :: (Eq a, Ord a) => Map a [a] -> Map a [a]
   closMap xs
     = foldl f xs (keys xs `isc` nub (concat (elems xs)))
       where
        f :: (Eq a, Ord a) => Map a [a] -> a -> Map a [a]   -- The type is given for documentation purposes only
        f q x = unionWith uni q (fromListWith uni [(a, q ! x) | (a, bs) <- assocs q, x `elem` bs])
{- this was inspired by the following pairwise definition of clos1
   clos1 :: (Eq a) => [(a,a)] -> [(a,a)]   
   clos1 xs
     = foldl f xs (nub (map fst xs) `isc` nub (map snd xs))
       where
        f q x = q `uni` [(a, b') | (a, b) <- q, b == x, (a', b') <- q, a' == x]

The following derivation motivates the code for the f in closMap, assuming that the code for clos1 is correct.
        q `uni` [(a, b') | (a, b) <- q, b == x, (a', b') <- q, a' == x]
=~    {transform to Map data structure}
        unionWith uni q (fromListWith uni [(a, [b']) | (a, bs) <- assocs q, b<-bs, b == x, (a', bs') <- assocs q, b'<-bs', a' == x])
=                                
        unionWith uni q (fromListWith uni [(a, [b']) | (a, bs) <- assocs q, x `elem` bs,   (a', bs') <- assocs q, a' == x, b'<-bs'])
=     { note that x `elem` keys q, so q ! x is defined. }
        unionWith uni q (fromListWith uni [(a, [b']) | (a, bs) <- assocs q, x `elem` bs,   let bs'=q ! x, b'<-bs'])
=                                
        unionWith uni q (fromListWith uni [(a, q ! x) | (a, bs) <- assocs q, x `elem` bs])
-}

{-
-- for future reference and possible experimentation, we have retained the same functions implemented by means of pairs.
-- We expect that to have inferior performance.
   import DatabaseDesign.Ampersand.ADL1.Pair                       (kleenejoin,mkPair,closPair,srcPaire,trgPaire)
   import DatabaseDesign.Ampersand.ADL1.Expression                 (notCpl)
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Classes.Relational
   import DatabaseDesign.Ampersand.Basics                     (Collection (..),fatalMsg, Identified(..))   
   import qualified Data.Map as Map
   import Data.List (nub)
   fatal :: Int -> String -> a
   fatal = fatalMsg "Classes.Populated"

   -- | This function returns the atoms of a concept (like fullContents does for relation-like things.)
   atomsOf :: [A_Gen]      -- the generalisation relations from the context
           -> [Population] -- the user defined populations in the context
           -> A_Concept    -- the concept from which the population is requested
           -> [String]     -- the elements in the concept's set of atoms
   atomsOf gens pt c =
    case c of
      ONE -> ["1"] -- fatal 126 "Asking for the value of the universal singleton"
      PlainConcept{}
          -> nub$ [srcPaire p | PRelPopu dcl ps   <- pt, p <- ps, source dcl `elem` c:smallerConcepts gens c]
                ++[trgPaire p | PRelPopu dcl ps   <- pt, p <- ps, target dcl `elem` c:smallerConcepts gens c]
                ++[a          | PCptPopu cpt atms <- pt, a <- atms, cpt      `elem` c:smallerConcepts gens c]

    fullContents :: [A_Gen] -> [Population] -> Expression -> Pairs
    fullContents gens pt = contents
     where
      contents expr
       = case expr of
            EEqu (l,r) -> contents ((l .|-. r) ./\. (r .|-. l))
            EImp (l,r) -> contents (notCpl l .\/. r)
            EUni (l,r) -> contents l `uni` contents r
            EIsc (l,r) -> contents l `isc` contents r
            EDif (l,r) -> contents l >- contents r
            -- The left residual l/r is defined by: for all x,y:  x(l/r)y  <=>  for all z in X, y r z implies x l z.
            ELrs (l,r) -> [(x,y) | let fL = Map.fromListWith (++) ([(a,[z]) | (a,z)<-contents l]++[(a,[]) | not (isTot l), a<-atomsOf gens pt (source l)])
                                 , let fR = Map.fromListWith (++) ([(b,[z]) | (b,z)<-contents r]++[(b,[]) | not (isTot r), b<-atomsOf gens pt (source r)])
                                 , x<-atomsOf gens pt (source l), y<-atomsOf gens pt (source r)
                                 , null (fR Map.! y >- fL Map.! x)
                                 ]
            -- The right residual l\r defined by: for all x,y:   x(l\r)y  <=>  for all z in X, z l x implies z r y.
            ERrs (l,r) -> [(x,y) | let fLflip = Map.fromListWith (++) ([(a,[z]) | (z,a)<-contents l]++[(a,[]) | not (isSur l), a<-atomsOf gens pt (target l)])
                                 , let fRflip = Map.fromListWith (++) ([(b,[z]) | (z,b)<-contents r]++[(b,[]) | not (isSur r), b<-atomsOf gens pt (target r)])
                                 , x<-atomsOf gens pt (target l), y<-atomsOf gens pt (target r)
                                 , null (fLflip Map.! x >- fRflip Map.! y)
                                 ]
            ERad (l,r) -> [(x,y) | x <- case source l of
                                          sl@PlainConcept{} -> atomsOf gens pt sl
                                          sl     -> fatal 97 ("source l should be PlainConcept instead of "++show sl++".")
                                 , y <- case target r of
                                          tr@PlainConcept{} -> atomsOf gens pt tr
                                          tr     -> fatal 100 ("target r should be PlainConcept instead of "++show tr++".")
                                 , and [(x,z) `elem` contents l || (z,y) `elem` contents r | z<- atomsOf gens pt (target l) `uni` atomsOf gens pt (source r)]
                                 ]
            EPrd (l,r) -> [ (a,b) | a <- atomsOf gens pt (source l), b <- atomsOf gens pt (target r) ]
            ECps (l,r) -> contents l `kleenejoin` contents r
            EKl0 e     -> if source e == target e --see #166
                          then closPair (contents e `uni` contents (EDcI (source e)))
                          else fatal 69 ("source and target of "++show e++show (sign e)++ " are not equal.")
            EKl1 e     -> closPair (contents e)
            EFlp e     -> [(b,a) | (a,b)<-contents e]
            ECpl e     -> [apair | apair <-[ mkPair x y | x<-atomsOf gens pt (source e), y<-atomsOf gens pt (target e)]
                                 , apair `notElem` contents e  ]
            EBrk e     -> contents e
            EDcD dcl@Sgn{}
                       -> concatMap popps (filter (isTheDecl dcl) pt)
                          where isTheDecl d pop =
                                  case pop of
                                    PRelPopu{}  -> d == popdcl pop
                                    PCptPopu{}  -> False
            EDcD e     -> fatal 93 ("fullContents must not be used on expression "++show e)
            EDcI c     -> [mkPair a a | a <- atomsOf gens pt c]
            EEps i _   -> [mkPair a a | a <- atomsOf gens pt i]
            EDcV sgn   -> [mkPair s t | s <- atomsOf gens pt (source sgn), t <- atomsOf gens pt (target sgn) ]
            EMp1 a c   -> [mkPair a a | name c/="SESSION"] -- prevent populating SESSION
-}

{- Derivation of contents (ERrs (l,r)):
The right residual l\r defined by: for all x,y:   x(l\r)y  <=>  for all z in X, z l x implies z r y.

Consider the following derivation in terms of list comprehension:
             and [    (z,x)    `elem` contents l -> (z,y) `elem` contents r  | z<-contents (source l)]
= 
             and [    (z,x) `notElem` contents l || (z,y) `elem` contents r  | z<-contents (source l)]
= 
        not ( or [not((z,x) `notElem` contents l || (z,y) `elem` contents r) | z<-contents (source l)])
= 
        not ( or [    (z,x)  `elem` contents l && (z,y) `notElem` contents r | z<-contents (source l)])
= 
        null [ () | z<-contents (source l), (z,x)  `elem` contents l && (z,y) `notElem` contents r]
= 
        null [ () | z<-contents (source l), (z,x)  `elem` contents l, (z,y) `notElem` contents r]
= 
        null [ () | (z,x') <- contents l, x==x', (z,y) `notElem` contents r ]
= 
        null [ () | (z,x') <- contents l, x==x' && (z,y) `notElem` contents r ]

Based on this derivation,
  contents (ERrs (l,r))
    = [(x,y) | x<-contents (target l), y<-contents (target r)
             , null [ () | (z,x') <- contents l, x==x', (z,y) `notElem` contents r ]
             ]

So we construct the PSEUDO-query:

SELECT x, y FROM CONTtgtL, CONTtgtR
WHERE NOT EXISTS
      (SELECT z, x'
       FROM L
       WHERE x'=x AND
             NOT EXISTS
             (SELECT c, d
              FROM R
              WHERE z=c AND y=d
             )
      )
-}
