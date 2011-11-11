{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.Populated                 (Populated(..))
where
   import DatabaseDesign.Ampersand.ADL1.Concept                    (cptos')
   import DatabaseDesign.Ampersand.ADL1.Pair                       (kleenejoin,mkPair,closPair)
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration     (Relation(..))
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Basics                     (Collection (..),fatalMsg)   
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Populated.hs"

   
   foldr1' :: Int -> (a -> a -> a) -> [a] -> a
   foldr1' rowNr _ [] = fatal rowNr "Call to foldr1 with empty list! (see Ticket #71 ) "
   foldr1' _ f lst = foldr1 f lst
   
   class Populated a where
    contents  :: a -> Pairs
   
   instance Populated A_Concept where
    contents c
       = [mkPair s s |s<-cptos' c]

   instance Populated Sign where
    contents (Sign s t)
       = [mkPair a b |a<-cptos' s, b<-cptos' t]

   instance Populated Declaration where
    contents d 
       = case d of
           Sgn{}     -> decpopu d
           Isn{}     -> contents (detyp d)
           Iscompl{} -> [mkPair a b |(a,_)<-contents (detyp d),(_,b)<-contents (detyp d),a/=b]
           Vs{}      -> [mkPair a b |a<-cptos' (source d),b<-cptos' (target d)]

   instance Populated Relation where
    contents (Mp1{relval=x}) = [mkPair x x]
    contents rel = contents (makeDeclaration rel)

   instance Populated Expression where
    contents expr  
       = case expr of
            EEqu (l,r) -> contents (EIsc [EImp (l,r),EImp (r,l)])
            EImp (l,r) -> contents (EUni [ECpl l,r])
            EUni es    -> foldr (uni . contents) [] es
            EIsc []    -> fatal 47 "Cannot compute contents of EIsc []"
            EIsc es    -> foldr1 isc (map contents es)
            EDif (l,r) -> contents l >- contents r
            ELrs (l,r) -> [(y,x) | x<-cptos' (source l), y <-cptos' (source r)
                                 , null [z |z<-cptos' (target r), (y,z) `elem` contents r, (x,z) `notElem` contents l]]   -- equals contents (ERrs (flp r, flp l))
            ERrs (l,r) -> [(x,y) | x<-cptos' (target l), y <-cptos' (target r)
                                 , null [z |z<-cptos' (source l), (z,x) `elem` contents l, (z,y) `notElem` contents r]]   -- equals contents (ELrs (flp r, flp l))
            ERad es    -> if null es 
                          then fatal 55 "Cannot compute contents of ERad []"
                          else let (dx,_,_,_)
                                    = foldr1' 59 dagg [ (ct,compl ct st tt,map fst st,map fst tt)
                                                      | t<-es, ct<-[contents t]
                                                      , st<-[contents (source t)]
                                                      , tt<-[contents (target t)] ]
                               in dx
            EPrd es    -> if null es 
                          then fatal 63 "Cannot compute contents of EPrd []"
                          else [ (a,b)
                               | (a,_)<-contents (source (head es))
                               , (b,_)<-contents (target (last es)) ]
            ECps es    -> if null es 
                          then []
                          else foldr1 kleenejoin (map contents es)
            EKl0 e     -> if source e == target e --see #166
                          then closPair (contents e `uni` contents (source e))
                          else fatal 69 ("source and target of "++show e++show (sign e)++ " are not equal.")
            EKl1 e     -> closPair (contents e)
            EFlp e     -> [(b,a) | (a,b)<-contents e]
            ECpl e     -> [apair | apair <-cartesianProduct (contents (source e)) (contents (target e))
                                   , apair `notElem` contents e  ]
            EBrk e     -> contents e
            ETyp e sgn -> if sign e==sgn then contents e else [x | x<-contents e, x `elem` contents sgn]
            ERel rel   -> contents rel

{- Derivation of contents (ERrs (l,r)):
Let cartP = contents (target l, target r)
    cL = contents l
    cR = contents r
  contents (ERrs (l,r))
= [(x,y) | (x,y)<-cartP,      and [    (z,x) `notElem` cL || (z,y) `elem` cR  |z<-contents (source l)] ]
= [(x,y) | (x,y)<-cartP, not ( or [not((z,x) `notElem` cL || (z,y) `elem` cR) |z<-contents (source l)])]
= [(x,y) | (x,y)<-cartP, not ( or [    (z,x)  `elem` cL && (z,y) `notElem` cR |z<-contents (source l)])]
= [(x,y) | (x,y)<-cartP, null [z |z<-contents (source l), (z,x)  `elem` cL && (z,y) `notElem` cR]]
= 
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
             compl (a) (sa) (ta) = [mkPair (fst x) (fst y) |x<-sa, y<-ta, mkPair (fst x) (fst y) `notElem` a]  -- complement van a
             cartesianProduct :: Pairs -> Pairs -> Pairs
             xs `cartesianProduct` ys = [ mkPair (fst x) (fst y) | x<-xs,y<-ys] 
