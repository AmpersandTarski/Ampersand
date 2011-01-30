{-# OPTIONS_GHC -Wall -XFlexibleInstances -XFlexibleContexts -XUndecidableInstances #-}
module Classes.Populated                 (Populated(contents,contents'))
where
   import DatabaseDesign.Ampersand.ADL.Concept                    (Concept(..), SpecHierarchy(..))
   import DatabaseDesign.Ampersand.ADL.Pair                       (Pairs,join,flipPair,mkPair,closPair)
   import DatabaseDesign.Ampersand.ADL.Expression                 (Expression(..))
   import DatabaseDesign.Ampersand.ADL.MorphismAndDeclaration     (Relation(..),Declaration(..),Association(..)
                                         ,makeDeclaration,inline)
   import DatabaseDesign.Ampersand.Core.Basics                     (Collection (uni,isc))   
   import Data.Maybe                     (fromJust)
   
   class Populated a where
    contents  :: a -> Maybe Pairs
    contents' :: a -> Pairs
    contents' a = case contents a of 
                     Nothing->[]
                     (Just x)->x
   
   instance Populated Concept where
    contents c
       = case cptos c of
           Nothing->Nothing
           Just c'-> Just [mkPair s s|s<-c']

   instance Populated (Declaration Concept) where
    contents d 
       = case d of
           Sgn{}     -> if(decplug d) then Nothing else Just$decpopu d
           Isn{}     -> case contents (despc d) of
                               Nothing -> Nothing
                               (Just as) -> Just as
           Iscompl{} -> case contents (despc d) of
                               Nothing -> Nothing
                               (Just as) -> Just$[mkPair a a' |(a,_)<-as,(_,a')<-as,a/=a']
           Vs{}      -> case (cptos (desrc d), cptos (detrg d)) of
                               (Just as, Just as') -> Just$[mkPair a a' |a<-as,a'<-as']
                               _ -> Nothing

   instance Populated (Relation Concept) where
    contents (Mp1{rel1val=x}) = Just$[mkPair x x]
    contents rel | inline rel = contents (makeDeclaration rel)
    contents rel | otherwise = case (contents (makeDeclaration rel)) of
                                 (Just ps) -> Just$ map flipPair ps
                                 Nothing -> Nothing

--   instance (Populated c,Identified c,SpecHierarchy c,Populated rel) => Populated (Expression rel) where
   instance (Populated c, SpecHierarchy c, Association rel c, Show c, Show rel, Populated rel)
              => Populated (Expression rel) where
    contents expr  
       = case expr of
            (Tm x _)  -> contents x
            (Tc x)  -> contents x
            (F  x)  -> if null x 
                         then Nothing -- no terms, cannot return I of generic type
                         else foldr1 join [contents t| t<-x ]
            (Fdx x)  -> if null x 
                         then Nothing -- no terms, cannot return -I of generic type
                         else let (dx,_,_,_)
                                   = foldr1 dagg [(ct,compl ct st tt,Just$map fst st,Just$map fst tt)
                                                 | t<-x, ct<-[contents t]
                                                 , Just st<-[contents (source t)], Just tt<-[contents (target t)] ]
                              in dx
            (Fux x)  -> if (elem Nothing (map contents x)) then Nothing else Just$ foldr uni [] [fromJust$ contents f| f<-x ]
            (Fix x)  -> if null x 
                         then Nothing -- no factors: cannot generate V of generic type
                         else if (elem Nothing (map contents x)) then Nothing else
                          Just$ foldr1 isc [fromJust$ contents f| f<-x ]
            (K0x x)  -> closPair (contents x) `join` contents (source x `lub` target x)
            (K1x x)  -> closPair (contents x)
            (Cpx x)  -> case contents x of
                          Nothing->Nothing
                          (Just c)
                           ->(case cartesianProduct (contents (source x)) (contents (target x)) of
                                Nothing -> Nothing
                                (Just cp) -> Just$[apair | apair <-cp, not (apair `elem` c)  ])
         where
          -- dagg is de tegenhanger van join. Hij krijgt systematisch viertallen mee: een rij tupels (a),
          -- het complement van a (ca), de source van a (sa), en de target van a (ta).
          -- TODO: dagg is razend inefficient. Daar kunnen we nog last van krijgen....
          -- Aanpak: op basis van redeneren de hele expressie optimaliseren, en vervolgens een aantal varianten van dagg maken
          -- die gebuik maken van de efficientere implementatie van -r!s en r!-s.
          -- dagg (a,ca,sa,ta) (b,cb,sb,tb)
             dagg (_,Just ca,Just sa,_)  (_,Just cb,_ ,Just tb)
               = (Just [mkPair x y| x<-sa, y<-tb, not (mkPair x y `elem` jnab)], Just [mkPair x y| x<-sa, y<-tb, mkPair x y `elem` jnab], Just sa, Just tb)
                 where jnab = join ca cb
             dagg (_, _, _, _) (_, _, _, _) = (Nothing,Nothing,Nothing,Nothing)
             compl (Just a) (sa) (ta) = Just$ [mkPair (fst x) (fst y)|x<-sa, y<-ta, not (mkPair (fst x) (fst y) `elem` a)]  -- complement van a
             compl _ _ _ = Nothing
             cartesianProduct :: Maybe Pairs -> Maybe Pairs -> Maybe Pairs
             (Just xs) `cartesianProduct` (Just ys) = Just$[ mkPair (fst x) (fst y) | x<-xs,y<-ys] 
             _ `cartesianProduct` _ = Nothing


             
