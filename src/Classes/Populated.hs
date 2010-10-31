{-# OPTIONS_GHC -Wall #-}
module Classes.Populated                 (Populated(contents,contents'))
where
   import Adl.Concept                    (Association(..),Concept(..))
   import Adl.Pair                       (Pairs,join,flipPair,mkPair,closPair)
   import Adl.Expression                 (Expression(..))
   import Adl.MorphismAndDeclaration     (Morphism(..),Declaration(..)
                                         ,makeDeclaration,makeInline,inline)
   import CommonClasses                  (Conceptual(conts),lub)    
   import Collection                     (Collection (uni,isc))   
   import Data.Maybe                     (fromJust)
   
   class Populated a where
    contents  :: a -> Maybe Pairs
    contents' :: a -> Pairs
    contents' a = case contents a of 
                     Nothing->[]
                     (Just x)->x
   
   instance Populated Concept where
    contents c 
       = case conts c of
           Nothing->Nothing
           Just c'-> Just [mkPair s s|s<-c']

   instance Populated Declaration where
    contents d 
       = case d of
           Sgn{}     -> if(decplug d) then Nothing else Just$decpopu d
           Isn{}     -> (case conts (despc d) of
                               Nothing -> Nothing
                               (Just as) -> Just$[mkPair a a |a <-as])
           Iscompl{} -> (case conts (despc d) of
                               Nothing -> Nothing
                               (Just as) -> Just$[mkPair a a' |a<-as,a'<-as,a/=a'])
           Vs{}      -> (case conts (despc d) of
                               Nothing -> Nothing
                               (Just as) -> Just$[mkPair a a' |a<-as,a'<-as])

   instance Populated Morphism where
    contents (Mp1{mph1val=x}) = Just$[mkPair x x]
    contents mph | inline mph = contents (makeDeclaration mph)
    contents mph | otherwise = case (contents (makeDeclaration (makeInline mph))) of
                                 (Just ps) -> Just$ map flipPair ps
                                 Nothing -> Nothing

   instance Populated Expression where
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
                                   = foldr1 dagg [(ct,compl ct st tt,st,tt)
                                                 | t<-x, ct<-[contents t]
                                                 , st<-[conts (source t)], tt<-[conts (target t)] ]
                              in dx
            (Fux x)  -> if (elem Nothing (map contents x)) then Nothing else Just$ foldr uni [] [fromJust$ contents f| f<-x ]
            (Fix x)  -> if null x 
                         then Nothing -- no factors: cannot generate V of generic type
                         else if (elem Nothing (map contents x)) then Nothing else
                          Just$ foldr1 isc [fromJust$ contents f| f<-x ]
            (K0x x)  -> closPair (contents x) `join`
                         (case conts (source x `lub` target x) of
                               Nothing -> Nothing
                               (Just as) -> Just$[mkPair a a |a <-as])
            (K1x x)  -> closPair (contents x)
            (Cpx x)  -> case contents x of
                          Nothing->Nothing
                          (Just c)
                           ->(case cartesianProduct (conts (source x)) (conts (target x)) of
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
             compl (Just a) (Just sa) (Just ta) = Just$ [mkPair x y|x<-sa, y<-ta, not (mkPair x y `elem` a)]  -- complement van a
             compl _ _ _ = Nothing
             cartesianProduct :: Maybe [String] -> Maybe [String] -> Maybe Pairs
             (Just xs) `cartesianProduct` (Just ys) = Just$[ mkPair x y | x<-xs,y<-ys] 
             _ `cartesianProduct` _ = Nothing


             
