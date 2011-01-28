{-# OPTIONS_GHC -Wall -XFunctionalDependencies -XMultiParamTypeClasses -XUndecidableInstances -XFlexibleContexts -XFlexibleInstances #-}
module Classes.Substitutive  (Substitutive(subst))
where
   import DatabaseDesign.Ampersand.ADL.Expression                   (Expression(..))
   import DatabaseDesign.Ampersand.ADL.Concept                      (Concept(..), SpecHierarchy(..))
   import DatabaseDesign.Ampersand.ADL.MorphismAndDeclaration       (Relational(..), Association(..))
   import DatabaseDesign.Ampersand.ADL.Rule                         (Rule(..),RuleType(Truth))
   
   class Substitutive exp rel where
 -- precondition: sign f `comparable` sign m
    subst :: (rel,Expression rel) -> exp -> exp
 --   subst (_,_) _ = error "!Fatal (module Classes.Substitutive 11): Unable to substitute"

   instance Substitutive e r => Substitutive [e] r where
    subst (rel,f) xs = map (subst (rel,f)) xs

   instance (Eq r, Show c, Show r, SpecHierarchy c, Relational r c) => Substitutive (Expression r) r where
    subst (rel,f) t@(Tm rel' _) |     rel==rel' = f
                              | flp rel==rel' = flp f 
                              | otherwise = t
    subst (rel,f) f'          = subs f'
     where
       subs (F ts)     = F  (subst (rel,f) ts)
       subs (Fdx ts)   = Fdx (subst (rel,f) ts)
       subs (Fux fs)   = Fux (subst (rel,f) fs)
       subs (Fix fs)   = Fix (subst (rel,f) fs)
       subs (K0x e')   = K0x (subst (rel,f) e')
       subs (K1x e')   = K1x (subst (rel,f) e')
       subs (Cpx e')   = Cpx (subst (rel,f) e')
       subs (Tc f'')   = Tc (subst (rel,f) f'')
       subs e'         = subst (rel,f) e'

   instance (Show r, Association r Concept, Substitutive (Expression r) r) => Substitutive (Rule r) r where
    subst (m',f) r
     = case r of
         Ru{rrsrt = Truth} -> r{rrant = error ("!Fatal (module Classes.Substitutive 35): illegal call to antecedent in subst ("++show m'++","++show f++") ("++show r++")")
                               ,rrcon = cons
                               ,rrtyp = sign cons
                               }
             where cons = subst (m',f) (rrcon r)
         Ru{}              -> if sign antc `comparable` sign cons
                              then r{rrant = antc
                                    ,rrcon = cons
                                    ,rrtyp = sign antc `lub` sign cons
                                    }
                              else r
             where antc = subst (m',f) (rrant r)
                   cons = subst (m',f) (rrcon r)
