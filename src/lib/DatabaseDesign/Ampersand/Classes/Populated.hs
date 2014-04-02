{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.Populated                 (fullContents,atomsOf)
where
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

{-
   instance Populated Declaration where
    fullContents gens pt dcl
      = case dcl of
         Isn c  -> [ mkPair a a   | a  <-atomsOf gens pt c]
         Vs sgn -> [ mkPair sa ta | sa <-atomsOf gens pt (source sgn)
                                  , ta <-atomsOf gens pt (target sgn)]
         Sgn{} -> concatMap  popps (filter (isTheDecl dcl) pt)
     where isTheDecl d pop =
             case pop of
               PRelPopu{}  -> d == popdcl pop
               PCptPopu{}  -> False
-}

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
