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
       subs (Fdx ts)    = Fdx (subst (mph,f) ts)
       subs (Fux fs)    = Fux (subst (mph,f) fs)
       subs (Fix fs)    = Fix (subst (mph,f) fs)
       subs (K0x e')    = K0x (subst (mph,f) e')
       subs (K1x e')    = K1x (subst (mph,f) e')
       subs (Cpx e')    = Cpx (subst (mph,f) e')
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
