
module Classes.Populated where
   import Adl.Concept
   import Adl.Pair
   import Adl.Expression
   import Adl.MorphismAndDeclaration
   import CommonClasses--(Conceptual(conts))    
   import Classes.Morphical
   import Collection                     (Collection (uni,(>-),isc))   
   import Auxiliaries                    (clos1,diag,eqCl) 

   class Populated a where
    contents  :: a -> Pairs

   instance Populated Morphism where
    contents m = contents (makeDeclaration (makeInline m))
-- Dit was in het verleden: (maar flp is hier niet bekend!)
--    contents m@(Mph _ _ _ _ False _) = map reverse (contents (flp m))
--    contents m = contents (makeDeclaration m)

   instance Populated Declaration where
    contents d 
       = case d of
           Sgn{}     -> decpopu d
           Isn{}     -> [[o,o] | o<-conts (despc d)]
           Iscompl{} -> [[o,o']| o <-conts (despc d),o'<-conts (despc d),o/=o']
           Vs{}      -> [[o,o']| o <-conts (despc d),o'<-conts (despc d)]

   instance Populated Expression where
    contents (Tm m)        = contents m
    contents (Tc f)        = contents f
    contents f@(F ts)
     | idsOnly ts
        = if not (source f `order` target f) then error ("(module CC_aux) Fatal: no order in "++show f) else
                             [[e,e]|e<-os]
     | otherwise
        = if null css then error ("(module CC_aux) Fatal: no terms in F "++show ts) else
                             foldr1 join css
                             where os = conts (source f `lub` target f)
                                   css = [contents t|t<-ts, not (idsOnly t)]
    contents f@(Fd ts)
      = if null ts then error ("(module CC_aux) Fatal: no terms in Fd "++show ts) else joinD ts
    contents (Fu fs) = if null fs then [] else
                       (foldr1 uni .map contents) fs
    contents (Fi fs) = if null fs then [] else
                       (foldr1 isc .map contents) fs
    contents (K0 e)  = clos1 (contents e) `uni` [[c,c]|c <-conts (source e `lub` target e)]
    contents (K1 e)  = clos1 (contents e)
    contents (Cp (Cp e)) = contents e
    contents (Cp e)  = [[a,b]| [a,b]<-diag [] (conts (source e)) [] (conts (target e)), not ([a,b] `elem` contents e)]


    
   joinD :: [Expression] -> [Paire]
   joinD [s]      = contents s
   joinD (r:s:ts) = [ [head (head rc),last (head sc)]
                    | rc<-eqCl head (contents r)
                    , sc<-eqCl last (joinD (s:ts))
                    , null (conts (target r `glb` source s) >-(map last rc `uni` map head sc))
                    ]

    