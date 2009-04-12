{-# OPTIONS_GHC -Wall #-}
module Classes.Substitutive  (Substitutive(subst))
where
   import Adl.Expression     (Expression(..))
   import Adl.Concept        (Morphic(..),sign)
   import Adl.Rule           (Rule(..),RuleType(Truth))
   import CommonClasses      (order,lub)
   
   class Substitutive a where
 -- precondition: sign f `order` sign m
    subst :: (Expression,Expression) -> a -> a
 --   subst (_,_) _ = error "(module Classes.Substitutive) Unable to substitute"

   instance (Morphic a,Substitutive a) => Substitutive [a] where
    subst (mph,f) xs = map (subst (mph,f)) xs

   instance Substitutive Expression where
    subst (Tm mph,f) t@(Tm mph') | mph==mph' = f
                                 | flp mph==mph' = flp f 
                                 | otherwise = t
    subst (_,_) t@(Tm _)     = t
    subst (mph,f) f'         = if mph `match` f'
                               then (if mph==f' then f else if flp mph==f' then flp f else subs f')
                               else subs f'
     where
       subs (Tc f'')   = Tc (subst (mph,f) f'')
       subs (F ts)     = F  (subst (mph,f) ts)
       subs (Fd ts)    = Fd (subst (mph,f) ts)
       subs (Fu fs)    = Fu (subst (mph,f) fs)
       subs (Fi fs)    = Fi (subst (mph,f) fs)
       subs (K0 e')    = K0 (subst (mph,f) e')
       subs (K1 e')    = K1 (subst (mph,f) e')
       subs (Cp e')    = Cp (subst (mph,f) e')
       subs e'         = subst (mph,f) e'
       Tm _  `match` Tm _   = True
       Tm _  `match` _      = False
       Tc _  `match` Tc _   = True
       Tc _  `match` _      = False
       F  _  `match` F  _   = True
       F  _  `match` _      = False
       Fd _  `match` Fd _   = True
       Fd _  `match` _      = False
       Fu _  `match` Fu _   = True
       Fu _  `match` _      = False
       Fi _  `match` Fi _   = True
       Fi _  `match` _      = False
       K0 _  `match` K0 _   = True
       K0 _  `match` _      = False
       K1 _  `match` K1 _   = True
       K1 _  `match` _      = False
       Cp _  `match` Cp _   = True
       Cp _  `match` _      = False

   instance Substitutive Rule where
    subst (m',f) r
     = case r of
         Ru{rrsrt = Truth} -> r{rrant = error ("(Module Classes.Substitutive:) illegal call to antecedent in subst ("++show m'++","++show f++") ("++show r++")")
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
         Sg{}             -> r{srsig = rule
                              ,srtyp = sign rule
                              }
             where rule = subst (m',f) (srsig r)
         Gc{}             -> r{grgen = expr
                              ,grtyp = sign expr
                              }                     
             where expr = subst (m',f) (grgen r)
         Fr{}             -> undefined
--    subst (m,f) r@(Ru Truth antc pos cons cpu expla sgn nr pn)
--     = Ru Truth (error ("(Module CC_aux:) illegal call to antecedent in subst ("++show m++","++show f++") ("++show r++")"))
--                                 pos cons' cpu expla (sign cons') nr pn
--       where cons' = subst (m,f) cons
--    subst (m,f) r@(Ru c antc pos cons cpu expla sgn nr pn)
--     = if sign antc' `order` sign cons'
--       then Ru c antc' pos cons' cpu expla (sign antc' `lub` sign cons') nr pn
--       else r -- error ("(module CC_aux) Fatal: cannot execute:   subst (m,f) r\nwith m="++show m++"\n     f="++show f++"\nand  r="++show r++"\n"++showHS "" r++"\nbecause "++show (sign antc')++" `order` "++show (sign cons')++" is False.\n"++gEtabG gEq [c| (a,b)<-[sign antc',sign cons'], c<-[a,b]])
--       where antc' = subst (m,f) antc
--             cons' = subst (m,f) cons
--    subst (m,f) (Sg p rule expla sgn nr pn signal)
--     = Sg p r' expla (sign r') nr pn signal
--       where r'= subst (m,f) rule
--    subst (m,f) (Gc pos m' expr cpu sgn nr pn)
--     = Gc pos m' expr' cpu (sign expr') nr pn
--       where expr' = subst (m,f) expr

