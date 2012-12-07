{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.Populated                 (Populated(..),atomsOf)
where
   import DatabaseDesign.Ampersand.ADL1.Pair                       (kleenejoin,mkPair,closPair,srcPaire,trgPaire)
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Basics                     (Collection (..),fatalMsg)   
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

   instance Populated Relation where
{- The atoms in a relation are accessible as follows:
   Atoms in a Rel{} are found through the declaration (via decpopu.reldcl).
   Atoms in a I{} and V{} are found in the concept (via rel1typ and reltyp).
   Mp1{} has precisely one atom, which must be an element of its type, i.e. relatom m `elem` atoms (rel1typ c)
-}    fullContents pt rel
       = case rel of
           Rel{}     -> fullContents pt (reldcl rel)
           I  {}     -> [mkPair a a | a <- atomsOf pt (rel1typ rel)]
           V  {}     -> [mkPair s t | s <- atomsOf pt (source rel)
                                    , t <- atomsOf pt (target rel) ]
           (Mp1 _ (C {cptnm="SESSION"})) -> [] -- TODO: HACK to prevent populating SESSION
           (Mp1 x _) -> [mkPair x x]


   instance Populated Expression where
    fullContents pt expr = contents expr
     where
      contents expr
       = case expr of
            EEqu (l,r) -> contents (EIsc [EImp (l,r),EImp (r,l)])
            EImp (l,r) -> contents (EUni [ECpl l,r])
            EUni es    -> foldr (uni . contents) [] es
            EIsc []    -> fatal 47 "Cannot compute contents of EIsc []"
            EIsc es    -> foldr1 isc (map (contents) es)
            EDif (l,r) -> contents l >- contents r
            -- The left residual lRel/rRel is defined by y(left/rRel)x if and only if for all z in X, x rRel z implies y lRel z.
            ELrs (l,r) -> [(y,x) | x <- atomsOf pt (source l)
                                 , y <- atomsOf pt (source r)
                                 , null [z |z<- atomsOf pt (target r), (y,z) `elem` contents r, (x,z) `notElem` contents l]]   -- equals contents (ERrs (flp r, flp l))
            -- The right residual lRel\rRel is defined by x(lRel\rRel)y if and only if for all z in X, z lRel x implies z rRel y.
            ERrs (l,r) -> [(x,y) | x <- atomsOf pt (target l)
                                 , y <- atomsOf pt (target r)
                                 , null [z |z<- atomsOf pt (source l), (z,x) `elem` contents l, (z,y) `notElem` contents r]]   -- equals contents (ELrs (flp r, flp l))
--            ERad es     -> fatal 104 "Relative addition needs rework. Sorry. "
            ERad es    -> if null es 
                          then fatal 55 "Cannot compute contents of ERad []"
                          else let (dx,_,_,_)
                                    = foldr1' 59 dagg [ (ct,compl ct (atomsInSource t) (atomsInTarget t),atomsInSource t,atomsInTarget t)
                                                      | t<-es, ct<-[contents t]]
                                            where 
                                              atomsInSource t = atomsOf pt (source t)
                                              atomsInTarget t = atomsOf pt (target t) 
                               in dx
            EPrd es    -> if null es 
                          then fatal 63 "Cannot compute contents of EPrd []"
                          else [ (a,b)
                               | a <- atomsOf pt (source (head es))
                               , b <- atomsOf pt (target (last es)) ]
            ECps es    -> if null es 
                          then []
                          else foldr1 kleenejoin (map (contents) es)
            EKl0 e     -> if source e == target e --see #166
                          then closPair (contents e `uni` contents (ERel (I (source e))))
                          else fatal 69 ("source and target of "++show e++show (sign e)++ " are not equal.")
            EKl1 e     -> closPair (contents e)
            EFlp e     -> [(b,a) | (a,b)<-contents e]
            ECpl e     -> [apair | apair <-cartesianProduct (atomsOf pt (source e)) (atomsOf pt (target e))
                                   , apair `notElem` contents e  ]
            EBrk e     -> contents e
            ETyp e sgn -> if sign e==sgn then contents e else [(a,b) | (a,b) <-contents e
                                                                     , a `elem` atomsOf pt (source sgn)
                                                                     , b `elem` atomsOf pt (target sgn)]
            ERel rel   -> fullContents pt rel

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


         where
          -- dagg is de tegenhanger van kleenejoin. Hij krijgt systematisch viertallen mee: een rij tupels (a),
          -- het complement van a (ca), de source van a (sa), en de target van a (ta).
          -- TODO: dagg is razend inefficient. Daar kunnen we nog last van krijgen....
          -- Aanpak: op basis van redeneren de hele expressie optimaliseren, en vervolgens een aantal varianten van dagg maken
          -- die gebuik maken van de efficientere implementatie van -r!s en r!-s.
          -- dagg (a,ca,sa,ta) (b,cb,sb,tb)
             dagg (_,ca,sa,_)  (_,cb,_ ,tb)
               = ([mkPair x y | x<-sa, y<-tb, mkPair x y `notElem` jnab], [mkPair x y | x<-sa, y<-tb, mkPair x y `elem` jnab], sa, tb)
                 where jnab = kleenejoin ca cb
             compl (a) (sa) (ta) = [mkPair x y |x<-sa, y<-ta, mkPair x y `notElem` a]  -- complement van a
             cartesianProduct :: [String] -> [String] -> Pairs
             xs `cartesianProduct` ys = [ mkPair x y | x<-xs,y<-ys] 
             foldr1' :: Int -> (a -> a -> a) -> [a] -> a
             foldr1' rowNr _ [] = fatal rowNr "Call to foldr1 with empty list! (see Ticket #71 ) "
             foldr1' _ f lst = foldr1 f lst

  
