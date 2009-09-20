{-# OPTIONS_GHC -Wall #-}
module Classes.Populated                 (Populated(contents))
where
   import Adl.Concept                    (Association(..),Concept(..))
   import Adl.Pair                       (Pairs,join,flipPair,aPair)
   import Adl.Expression                 (Expression(..))
   import Adl.MorphismAndDeclaration     (Morphism(..),Declaration(..)
                                         ,makeDeclaration,makeInline,inline)
   import CommonClasses                  (Conceptual(conts),lub)    
   import Collection                     (Collection (uni,isc))   
   import Auxiliaries                    (clos1,diag) 

   class Populated a where
    contents  :: a -> Pairs

   instance Populated Concept where
    contents c 
       = case c of
           C {}     -> [aPair s s|s<-cptos c]
           S        -> error ("(Module Populated) Cannot refer to the contents of the universal singleton")
           Anything -> error ("(Module Populated) Cannot refer to the contents of Anything")
           NOthing  -> error ("(Module Populated) Cannot refer to the contents of Nothing")

   instance Populated Declaration where
    contents d 
       = case d of
           Sgn{}     -> decpopu d
           Isn{}     -> [aPair o o | o<-conts (despc d)]
           Iscompl{} -> [aPair o o'| o<-conts (despc d), o'<-conts (despc d), o/=o']
           Vs{}      -> [aPair o o'| o<-conts (despc d), o'<-conts (despc d)]

   instance Populated Morphism where
    contents mph | inline mph = contents (makeDeclaration mph)
    contents mph | otherwise = map flipPair (contents (makeDeclaration (makeInline mph)))

   instance Populated Expression where
    contents expr  
       = case expr of
            (Tm x)  -> contents x
            (Tc x)  -> contents x
            (F  x)  -> if null x 
                         then error ("(module Populated) Fatal: no terms in contents ("++show expr++")") 
                         else foldr1 join [contents t| t<-x ]
            (Fd x)  -> if null x 
                         then error ("(module Populated) Fatal: no terms in contents ("++show expr++")") 
                         else let (dx,_,_,_)
                                   = foldr1 dagg [(ct,compl ct st tt,st,tt)
                                                 | t<-x, ct<-[contents t]
                                                 , st<-[conts (source t)], tt<-[conts (target t)] ]
                              in dx
            (Fu x)  -> foldr uni [] [contents f| f<-x ]
            (Fi x)  -> if null x 
                         then error ("(module Populated) Fatal: no factors in contents ("++show expr++")") 
                         else foldr1 isc [contents f| f<-x ]
            (K0 x)  -> clos1 (contents x) `uni` [aPair a a |a <-conts (source x `lub` target x)]
            (K1 x)  -> clos1 (contents x)
            (Cp x)  -> [[a,b]| [a,b]<-diag [] (conts (source x)) [] (conts (target x)), not ([a,b] `elem` contents x)]
         where
          -- dagg is de tegenhanger van join. Hij krijgt systematisch viertallen mee: een rij tupels (a),
          -- het complement van a (ca), de source van a (sa), en de target van a (ta).
          -- TODO: dagg is razend inefficient. Daar kunnen we nog last van krijgen....
          -- Aanpak: op basis van redeneren de hele expressie optimaliseren, en vervolgens een aantal varianten van dagg maken
          -- die gebuik maken van de efficientere implementatie van -r!s en r!-s.
          -- dagg (a,ca,sa,ta) (b,cb,sb,tb)
             dagg (_,ca,sa,_)  (_,cb,_ ,tb)
               = ([aPair x y| x<-sa, y<-tb, not (aPair x y `elem` jnab)], [aPair x y| x<-sa, y<-tb, aPair x y `elem` jnab], sa, tb)
                 where jnab = join ca cb
             compl a sa ta = [aPair x y|x<-sa, y<-ta, not (aPair x y `elem` a)]  -- complement van a