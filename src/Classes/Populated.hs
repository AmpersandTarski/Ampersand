{-# OPTIONS_GHC -Wall #-}
module Classes.Populated                 (Populated(contents))
where
   import Adl.Concept                    (Association(..),Concept(..))
   import Adl.Pair                       (Pairs,join,flipPair,mkPair,closPair)
   import Adl.Expression                 (Expression(..))
   import Adl.MorphismAndDeclaration     (Morphism(..),Declaration(..)
                                         ,makeDeclaration,makeInline,inline)
   import CommonClasses                  (Conceptual(conts),lub)    
   import Collection                     (Collection (uni,isc))   

   class Populated a where
    contents  :: a -> Pairs
   
   instance Populated Concept where
    contents c 
       = case c of
           C {}     -> [mkPair s s|s<-cptos c]
           S        -> error ("!Fatal (module Populated 19): Cannot refer to the contents of S")
           Anything -> error ("!Fatal (module Populated 20): Cannot refer to the contents of Anything")
           NOthing  -> error ("!Fatal (module Populated 21): Cannot refer to the contents of Nothing")

   instance Populated Declaration where
    contents d 
       = case d of
           Sgn{}     -> decpopu d
           Isn{}     -> [mkPair o o | o<-conts (despc d)]
           Iscompl{} -> [mkPair o o'| o<-conts (despc d), o'<-conts (despc d), o/=o']
           Vs{}      -> [mkPair o o'| o<-conts (degen d), o'<-conts (despc d)]

   instance Populated Morphism where
    contents (Mp1{mph1val=x}) = [mkPair x x]
    contents mph | inline mph = contents (makeDeclaration mph)
    contents mph | otherwise = map flipPair (contents (makeDeclaration (makeInline mph)))

   instance Populated Expression where
    contents expr  
       = case expr of
            (Tm x _)  -> contents x
            (Tc x)  -> contents x
            (F  x)  -> if null x 
                         then error ("!Fatal (module Populated 41): no terms in contents ("++show expr++")") 
                         else foldr1 join [contents t| t<-x ]
            (Fdx x)  -> if null x 
                         then error ("!Fatal (module Populated 44): no terms in contents ("++show expr++")") 
                         else let (dx,_,_,_)
                                   = foldr1 dagg [(ct,compl ct st tt,st,tt)
                                                 | t<-x, ct<-[contents t]
                                                 , st<-[conts (source t)], tt<-[conts (target t)] ]
                              in dx
            (Fux x)  -> foldr uni [] [contents f| f<-x ]
            (Fix x)  -> if null x 
                         then error ("!Fatal (module Populated 52): no factors in contents ("++show expr++")") 
                         else foldr1 isc [contents f| f<-x ]
            (K0x x)  -> closPair (contents x) `join` [mkPair a a |a <-conts (source x `lub` target x)]
            (K1x x)  -> closPair (contents x)
            (Cpx x)  -> [apair | apair <-cartesianProduct (conts (source x)) (conts (target x))
                              , not (apair `elem` contents x)  ]
         where
          -- dagg is de tegenhanger van join. Hij krijgt systematisch viertallen mee: een rij tupels (a),
          -- het complement van a (ca), de source van a (sa), en de target van a (ta).
          -- TODO: dagg is razend inefficient. Daar kunnen we nog last van krijgen....
          -- Aanpak: op basis van redeneren de hele expressie optimaliseren, en vervolgens een aantal varianten van dagg maken
          -- die gebuik maken van de efficientere implementatie van -r!s en r!-s.
          -- dagg (a,ca,sa,ta) (b,cb,sb,tb)
             dagg (_,ca,sa,_)  (_,cb,_ ,tb)
               = ([mkPair x y| x<-sa, y<-tb, not (mkPair x y `elem` jnab)], [mkPair x y| x<-sa, y<-tb, mkPair x y `elem` jnab], sa, tb)
                 where jnab = join ca cb
             compl a sa ta = [mkPair x y|x<-sa, y<-ta, not (mkPair x y `elem` a)]  -- complement van a
             cartesianProduct :: [String] -> [String] -> Pairs
             xs `cartesianProduct` ys = [ mkPair x y | x<-xs,y<-ys] 


             
