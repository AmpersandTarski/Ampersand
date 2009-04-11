module Classes.Substitutive where
   import Adl.Expression
   import Adl.Concept
   import Adl.Rule
   import CommonClasses
   import Classes.Morphic
   
   class Substitutive a where  --WAAROM? Stef, hier hoort een vette uitleg bij...
 -- precondition: sign f `order` sign m
    subst :: (Expression,Expression) -> a -> a
 --   subst (_,_) _ = error "(module CC_aux) Unable to substitute"

   instance (Morphic a,Substitutive a) => Substitutive [a] where
    subst (m',f) xs = map (subst (m',f)) xs

   instance Substitutive Expression where
    subst (Tm mph,f) t@(Tm mph') | mph==mph' = f
                                 | flp mph==mph' = flp f 
                                 | otherwise = t
    subst (_,_) t@(Tm _)     = t
    subst (m,f) f'           = if m `match` f'
                               then (if m==f' then f else if flp m==f' then flp f else subs f')
                               else subs f'
     where
       subs (Tc f')    = Tc (subst (m,f) f')
       subs (F ts)     = F  (subst (m,f) ts)
       subs (Fd ts)    = Fd (subst (m,f) ts)
       subs (Fu fs)    = Fu (subst (m,f) fs)
       subs (Fi fs)    = Fi (subst (m,f) fs)
       subs (K0 e)     = K0 (subst (m,f) e)
       subs (K1 e)     = K1 (subst (m,f) e)
       subs (Cp e)     = Cp (subst (m,f) e)
       subs e          = subst (m,f) e
       Tm m  `match` Tm m'   = True
       Tc f  `match` Tc f'   = True
       F ts  `match` F  ts'  = True
       Fd ts `match` Fd ts'  = True
       Fu fs `match` Fu fs'  = True
       Fi fs `match` Fi fs'  = True
       K0 e  `match` K0 e'   = True
       K1 e  `match` K1 e'   = True
       Cp e  `match` Cp e'   = True
       _     `match` _       = False

   instance Substitutive Rule where
    subst (m,f) r@(Ru Truth antc pos cons cpu expla sgn nr pn)
     = Ru Truth (error ("(Module CC_aux:) illegal call to antecedent in subst ("++show m++","++show f++") ("++show r++")")) pos cons' cpu expla (sign cons') nr pn
       where cons' = subst (m,f) cons
    subst (m,f) r@(Ru c antc pos cons cpu expla sgn nr pn)
     = if sign antc' `order` sign cons'
       then Ru c antc' pos cons' cpu expla (sign antc' `lub` sign cons') nr pn
       else r -- error ("(module CC_aux) Fatal: cannot execute:   subst (m,f) r\nwith m="++show m++"\n     f="++show f++"\nand  r="++show r++"\n"++showHS "" r++"\nbecause "++show (sign antc')++" `order` "++show (sign cons')++" is False.\n"++gEtabG gEq [c| (a,b)<-[sign antc',sign cons'], c<-[a,b]])
       where antc' = subst (m,f) antc
             cons' = subst (m,f) cons
    subst (m,f) (Sg p rule expla sgn nr pn signal)
     = Sg p r' expla (sign r') nr pn signal
       where r'= subst (m,f) rule
    subst (m,f) (Gc pos m' expr cpu sgn nr pn)
     = Gc pos m' expr' cpu (sign expr') nr pn
       where expr' = subst (m,f) expr

