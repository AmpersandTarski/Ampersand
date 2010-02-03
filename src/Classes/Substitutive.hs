{-# OPTIONS_GHC -Wall #-}
module Classes.Substitutive  (Substitutive(subst))
where
   import Adl.Expression                   (Expression(..))
   import Adl.MorphismAndDeclaration       (Morphism)
   import Adl.Concept                      (Morphic(..),sign)
   import Adl.Rule                         (Rule(..),RuleType(Truth))
   import CommonClasses                    (order,lub)
   
   class Substitutive a where
 -- precondition: sign f `order` sign m
    subst :: (Morphism,Expression) -> a -> a
 --   subst (_,_) _ = error "!Fatal (module Classes.Substitutive 13): Unable to substitute"

   instance (Morphic a,Substitutive a) => Substitutive [a] where
    subst (mph,f) xs = map (subst (mph,f)) xs

   instance Substitutive Expression where
    subst (mph,f) t@(Tm mph' _) |     mph==mph' = f
                              | flp mph==mph' = flp f 
                              | otherwise = t
    subst (mph,f) f'          = subs f'
     where
       subs (F ts)     = F  (subst (mph,f) ts)
       subs (Fd ts)    = Fd (subst (mph,f) ts)
       subs (Fu fs)    = Fu (subst (mph,f) fs)
       subs (Fi fs)    = Fi (subst (mph,f) fs)
       subs (K0 e')    = K0 (subst (mph,f) e')
       subs (K1 e')    = K1 (subst (mph,f) e')
       subs (Cp e')    = Cp (subst (mph,f) e')
       subs (Tc f'')   = Tc (subst (mph,f) f'')
       subs e'         = subst (mph,f) e'

   instance Substitutive Rule where
    subst (m',f) r
     = case r of
         Ru{rrsrt = Truth} -> r{rrant = error ("!Fatal (module Classes.Substitutive 37): illegal call to antecedent in subst ("++show m'++","++show f++") ("++show r++")")
                               ,rrcon = cons
                               ,rrtyp = sign cons
                               }
             where cons = subst (m',f) (rrcon r)
         Ru{}              -> if sign antc `order` sign cons
                              then r{rrant = antc
                                    ,rrcon = cons
                                    ,rrtyp = sign antc `lub` sign cons
                                    }
                              else r
             where antc = subst (m',f) (rrant r)
                   cons = subst (m',f) (rrcon r)
