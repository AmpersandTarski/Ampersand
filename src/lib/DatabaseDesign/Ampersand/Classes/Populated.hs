{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.Populated                 (Populated(..),atomsOf)
where
   import DatabaseDesign.Ampersand.ADL1.Pair                       (kleenejoin,mkPair,closPair,srcPaire,trgPaire)
   import DatabaseDesign.Ampersand.ADL1.Expression                 (notCpl)
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Basics                     (Collection (..),fatalMsg, Identified(..))   
   import qualified DatabaseDesign.Ampersand.Core.Poset
   import Data.List (nub)
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Populated.hs"

   
   class Populated a where
    -- | this function returns the pairs as content of a specific a, given a list of populations. 
    --   The list of populations should contain all user-defined populations. 
    fullContents :: [UserDefPop] -> a -> Pairs
   
   -- | This function returns the atoms of a concept (like fullContents does for relation-like things.)
   atomsOf :: [UserDefPop] -> A_Concept -> [String] 
   atomsOf _ ONE  = ["1(ONE)"] -- fatal 126 "Asking for the value of the universal singleton"
   atomsOf pt c@C{}
     = nub$[srcPaire p | PRelPopu dcl ps   <- pt, p <- ps, (source dcl) DatabaseDesign.Ampersand.Core.Poset.<= c]
         ++[trgPaire p | PRelPopu dcl ps   <- pt, p <- ps, (target dcl) DatabaseDesign.Ampersand.Core.Poset.<= c]
         ++[a          | PCptPopu cpt atms <- pt, a <- atms, cpt        DatabaseDesign.Ampersand.Core.Poset.<= c]

   instance Populated Declaration where
    fullContents pt dcl
      = case filter (isTheDecl dcl) pt of
         []    -> []
         [pop] -> popps pop
         _     -> fatal 31 $ "Multiple entries in lookup table found for declaration."
     where isTheDecl d pop =
             case pop of
               PRelPopu{}  -> d == popdcl pop
               PCptPopu{}  -> False

{-
   instance Populated Relation where
{- The atoms in a relation are accessible as follows:
   Atoms in a Rel{} are found through the declaration (via decpopu.reldcl).
   Atoms in a I{} and V{} are found in the concept (via rel1typ and reltyp).
-}    fullContents pt rel
       = case rel of
           Rel{}     -> fullContents pt (reldcl rel)
           I  {}     -> [mkPair a a | a <- atomsOf pt (rel1typ rel)]
           V  {}     -> [mkPair s t | s <- atomsOf pt (source rel)
                                    , t <- atomsOf pt (target rel) ]
-}

   instance Populated Expression where
    fullContents pt = contents
     where
      contents expr
       = case expr of
            EEqu (l,r) _ -> contents ((l .|-. r) ./\. (r .|-. l))
            EImp (l,r) sgn -> contents (notCpl sgn l .\/. r)
            EUni (l,r) _ -> contents l `uni` contents r
            EIsc (l,r) _ -> contents l `isc` contents r
            EDif (l,r) _ -> contents l >- contents r
            -- The left residual l/r is defined by: for all x,y:  y(l/r)x  <=>  for all z in X, x l z implies y r z.
            ELrs (l,r) _ -> [(y,x) | x <- case source l of
                                            sl@C{} -> atomsOf pt sl
                                            sl     -> fatal 68 ("source l should be C instead of "++show sl++".")
                                   , y <- case source r of
                                            sr@C{} -> atomsOf pt sr
                                            sr     -> fatal 71 ("source r should be C instead of "++show sr++".")
                              --   Derivation:
                              --   , and      [(x,z) `elem` contents l <- (y,z) `elem` contents r          |z<- atomsOf pt (target l `join` target r)]
                              --   , and      [(x,z) `elem` contents l || (y,z) `notElem` contents r       |z<- atomsOf pt (target l `join` target r)]
                              --   , (not.or) [not ((x,z) `elem` contents l || (y,z) `notElem` contents r) |z<- atomsOf pt (target l `join` target r)]
                              --   , (not.or) [     (x,z) `notElem` contents l && (y,z) `elem` contents r  |z<- atomsOf pt (target l `join` target r)]
                              --   , (not.null) [ () |z<- atomsOf pt (target l `join` target r), (x,z) `notElem` contents l, (y,z) `elem` contents r]
                                   , (not.null) [ () |z<- atomsOf pt (target r), (x,z) `notElem` contents l, (y,z) `elem` contents r]
                                   ]   -- equals contents (ERrs (flp r, flp l))
            -- The right residual l\r defined by: for all x,y:   x(l\r)y  <=>  for all z in X, z l x implies z r y.
            ERrs (l,r) _ -> [(x,y) | x <- case target l of
                                            tl@C{} -> atomsOf pt tl
                                            tl     -> fatal 83 ("target l should be C instead of "++show tl++".")
                                   , y <- case target r of
                                            tr@C{} -> atomsOf pt tr
                                            tr     -> fatal 86 ("target r should be C instead of "++show tr++".")
                              --   Derivation:
                              --     and      [(z,x) `elem` contents l    -> (z,y) `elem` contents r       |z<- atomsOf pt (source l `join` source r)]
                              --     and      [(z,x) `notElem` contents l || (z,y) `elem` contents r       |z<- atomsOf pt (source l `join` source r)]
                              --     (not.or) [not ((z,x) `notElem` contents l || (z,y) `elem` contents r) |z<- atomsOf pt (source l `join` source r)]
                              --     (not.or) [     (z,x) `elem` contents l && (z,y) `notElem` contents r  |z<- atomsOf pt (source l `join` source r)]
                              --     (not.null) [ () |z<- atomsOf pt (source l `join` source r), (z,x) `elem` contents l, (z,y) `notElem` contents r]
                                   , (not.null) [ () |z<- atomsOf pt (source l), (z,x) `elem` contents l, (z,y) `notElem` contents r]
                                   ]   -- equals contents (ELrs (flp r, flp l))
            ERad (l,r) _ -> [(x,y) | x <- case target l of
                                            tl@C{} -> atomsOf pt tl
                                            tl     -> fatal 97 ("target l should be C instead of "++show tl++".")
                                   , y <- case source r of
                                            sr@C{} -> atomsOf pt sr
                                            sr     -> fatal 100 ("source r should be C instead of "++show sr++".")
                                   , and [(x,z) `elem` contents l || (z,y) `elem` contents r |z<- atomsOf pt (target l `join` source r)]
                                   ]
            EPrd (l,r) _ -> [ (a,b) | a <- atomsOf pt (source l), b <- atomsOf pt (target r) ]
            ECps (l,r) _ -> contents l `kleenejoin` contents r
            EKl0 e     _ -> if source e == target e --see #166
                            then closPair (contents e `uni` contents (iExpr (source e)))
                            else fatal 69 ("source and target of "++show e++show (sign e)++ " are not equal.")
            EKl1 e     _ -> closPair (contents e)
            EFlp e     _ -> [(b,a) | (a,b)<-contents e]
            ECpl e     _ -> [apair | apair <-[ mkPair x y | x<-atomsOf pt (source e), y<-atomsOf pt (target e)]
                                   , apair `notElem` contents e  ]
            EBrk e       -> contents e
            ETyp e   sgn -> if sign e==sgn then contents e else [(a,b) | (a,b) <-contents e
                                                                       , a `elem` atomsOf pt (source sgn)
                                                                       , b `elem` atomsOf pt (target sgn)]
            EDcD dcl   _ -> fullContents pt dcl
            EDcI     sgn -> [mkPair a a | a <- atomsOf pt (source sgn)]
            EDcV     sgn -> [mkPair s t | s <- atomsOf pt (source sgn)
                                            , t <- atomsOf pt (target sgn) ]
            EMp1 atom       sgn -> if name (source sgn)=="SESSION" then [] else [mkPair atom atom] -- prevent populating SESSION

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
