{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.Populated                 (Populated(..),atomsOf)
where
   import DatabaseDesign.Ampersand.ADL1.Pair                       (kleenejoin,mkPair,closPair,srcPaire,trgPaire)
   import DatabaseDesign.Ampersand.ADL1.Expression                 (notCpl)
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Basics                     (Collection (..),fatalMsg, Identified(..))   
   import Data.List (nub)
   fatal :: Int -> String -> a
   fatal = fatalMsg "Classes.Populated"

   
   class Populated a where
    -- | this function returns the pairs as content of a specific a, given a list of populations. 
    --   The list of populations should contain all user-defined populations. 
    fullContents :: [A_Gen] -> [Population] -> a -> Pairs
   
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
      
   instance Populated Declaration where
    fullContents gens pt dcl
      = case dcl of
         Isn c  -> [ mkPair a a   | a  <-atomsOf gens pt c]
         Vs sgn -> [ mkPair sa ta | sa <-atomsOf gens pt (source sgn)
                                  , ta <-atomsOf gens pt (target sgn)]
         Sgn{} -> if decISA dcl
                  then [ mkPair a a | a <-atomsOf gens pt (source dcl)] 
                  else concatMap  popps (filter (isTheDecl dcl) pt)
     where isTheDecl d pop =
             case pop of
               PRelPopu{}  -> d == popdcl pop
               PCptPopu{}  -> False


   instance Populated Expression where
    fullContents gens pt = contents
     where
      contents expr
       = case expr of
            EEqu (l,r) -> contents ((l .|-. r) ./\. (r .|-. l))
            EImp (l,r) -> contents (notCpl l .\/. r)
            EUni (l,r) -> contents l `uni` contents r
            EIsc (l,r) -> contents l `isc` contents r
            EDif (l,r) -> contents l >- contents r
            -- The left residual l/r is defined by: for all x,y:  y(l/r)x  <=>  for all z in X, x l z implies y r z.
            ELrs (l,r) -> [(y,x) | x <- case source l of
                                          sl@PlainConcept{} -> atomsOf gens pt sl
                                          sl     -> fatal 68 ("source l should be PlainConcept instead of "++show sl++".")
                            --   Derivation:
                            --      y <- atomsOf gens pt (source r), and      [(y,z) `elem` contents r    -> (x,z) `elem` contents l       | z<- atomsOf gens pt (target l `join` target r)]
                            --   = { implication  }
                            --      y <- atomsOf gens pt (source r), and      [(y,z) `notElem` contents r || (x,z) `elem` contents l       | z<- atomsOf gens pt (target l `join` target r)]
                            --   = { De Morgan }
                            --      y <- atomsOf gens pt (source r), (not.or) [not ((y,z) `notElem` contents r || (x,z) `elem` contents l) | z<- atomsOf gens pt (target l `join` target r)]
                            --   = { De Morgan }
                            --      y <- atomsOf gens pt (source r), (not.or) [     (y,z) `elem` contents r && (x,z) `notElem` contents l  | z<- atomsOf gens pt (target l `join` target r)]
                            --   = { LET P(z)=(y,z) `elem` contents r && (x,z) `notElem` contents l, and use:  or [P(z) | z<-xs ] = null [() | z<-xs, P(z)]  }
                            --      y <- atomsOf gens pt (source r), (not.null) [ () | z<- atomsOf gens pt (target l `join` target r), (y,z) `elem` contents r && (x,z) `notElem` contents l]
                            --   =
                            --      y <- atomsOf gens pt (source r), (not.null) [ () | z<- atomsOf gens pt (target l `join` target r), (y,z) `elem` contents r, (x,z) `notElem` contents l]
                            --   = { since map snd (contents r) is a subset of atomsOf gens pt (source l `join` source r) }
                            --      y <- atomsOf gens pt (source r), (not.null) [ () | (y,z)<-contents r, (x,z) `notElem` contents l]
                            --   =
                                 ,  (y,z)<-contents r, (x,z) `notElem` contents l ]
            -- The right residual l\r defined by: for all x,y:   x(l\r)y  <=>  for all z in X, z l x implies z r y.
            ERrs (l,r) -> [(x,y) | y <- case target r of
                                          tr@PlainConcept{} -> atomsOf gens pt tr
                                          tr     -> fatal 86 ("target r should be PlainConcept instead of "++show tr++".")
                            --   Derivation:
                            --        x<-atomsOf gens pt (target l), and      [(z,x) `elem` contents l    -> (z,y) `elem` contents r       | z<- atomsOf gens pt (source l `join` source r)]
                            --   = { implication  }
                            --        x<-atomsOf gens pt (target l), and      [(z,x) `notElem` contents l || (z,y) `elem` contents r       | z<- atomsOf gens pt (source l `join` source r)]
                            --   = { De Morgan }
                            --        x<-atomsOf gens pt (target l), (not.or) [not ((z,x) `notElem` contents l || (z,y) `elem` contents r) | z<- atomsOf gens pt (source l `join` source r)]
                            --   = { De Morgan }
                            --        x<-atomsOf gens pt (target l), (not.or) [     (z,x) `elem` contents l && (z,y) `notElem` contents r  | z<- atomsOf gens pt (source l `join` source r)]
                            --   = { LET P(z)=(z,x) `elem` contents l && (z,y) `notElem` contents r, and use:  or [P(z) | z<-xs ] = null [() | z<-xs, P(z)]  }
                            --        x<-atomsOf gens pt (target l), (not.null) [ () | z<- atomsOf gens pt (source l `join` source r), (z,x) `elem` contents l && (z,y) `notElem` contents r]
                            --   =
                            --        x<-atomsOf gens pt (target l), (not.null) [ () | z<- atomsOf gens pt (source l `join` source r), (z,x) `elem` contents l, (z,y) `notElem` contents r]
                            --   = { since map fst (contents l) is a subset of atomsOf gens pt (source l `join` source r) }
                            --        x<-atomsOf gens pt (target l), (not.null) [ () | (z,x)<-contents l, (z,y) `notElem` contents r]
                            --   =
                                 ,    (z,x)<-contents l, (z,y) `notElem` contents r] 
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
            EDcD dcl   -> fullContents gens pt dcl
            EDcI c     -> [mkPair a a | a <- atomsOf gens pt c]
            EEps i _   -> [mkPair a a | a <- atomsOf gens pt i]
            EDcV sgn   -> [mkPair s t | s <- atomsOf gens pt (source sgn), t <- atomsOf gens pt (target sgn) ]
            EMp1 a c   -> [mkPair a a | name c/="SESSION"] -- prevent populating SESSION

{- Derivation of contents (ERrs (l,r)):
Let cL = contents l
    cR = contents r
  contents (ERrs (l,r))
= [(x,y) | x<-contents (target l), y<-contents (target r)
         ,      and [    (z,x) `notElem` cL || (z,y) `elem` cR  | z<-contents (source l)] ]
= [(x,y) | x<-contents (target l), y<-contents (target r)
         , not ( or [not((z,x) `notElem` cL || (z,y) `elem` cR) | z<-contents (source l)])]
= [(x,y) | x<-contents (target l), y<-contents (target r)
         , not ( or [    (z,x)  `elem` cL && (z,y) `notElem` cR | z<-contents (source l)])]
= [(x,y) | x<-contents (target l), y<-contents (target r)
         , null [ () | z<-contents (source l), (z,x)  `elem` cL && (z,y) `notElem` cR]]
= [(x,y) | x<-contents (target l), y<-contents (target r)
         , null [ () | (z,x') <- cL, x==x', (z,y) `notElem` cR ]]
-}
