{-# OPTIONS_GHC -Wall #-}
module Adl.Expression where
   import Adl.MorphismAndDeclaration
   import Adl.FilePos
   import Adl.Concept
   import Collection (Collection (rd))
   import Strings(chain)
   import CommonClasses(Identified(name), ABoolAlg(lub,order))
   import Auxiliaries (eqClass)

   type Expressions = [Expression]
   data Expression  = Tm { m :: Morphism}     -- ^ simple morphism, possibly conversed     ~
                    | Tc { e :: Expression}   -- ^ bracketed expression                 ( ... )
                    | F  { es :: Expressions} -- ^ composition                             ;
                    | Fd { es :: Expressions} -- ^ relative addition                       !
                    | Fi { es :: Expressions} -- ^ intersection                            /\
                    | Fu { es :: Expressions} -- ^ union                                   \/
                    | K0 { e :: Expression}   -- ^ Reflexive and transitive closure        *
                    | K1 { e :: Expression}   -- ^ Transitive closure                      +
                    | Cp { e :: Expression}   -- ^ Complement                              -

                      
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************
   instance Eq Expression where
    F  ts == F  ts' = ts==ts'
    Fd ts == Fd ts' = ts==ts'
    Fu fs == Fu fs' = rd fs==rd fs'
    Fi fs == Fi fs' = rd fs==rd fs'
    Cp expr  == Cp expr'  = expr==expr'
    K0 expr  == K0 expr'  = expr==expr'
    K1 expr  == K1 expr'  = expr==expr'
    Tm mph  == Tm mph'  = mph==mph'
    Tc expr  == expr'     = expr==expr'
    expr     == Tc expr'  = expr==expr'
    _     == _      = False

   instance Show Expression where
    showsPrec _ expr  = showString (showExpr ("\\/", "/\\", "!", ";", "*", "+", "-", "(", ")") expr)
      where
       showExpr (union,inter,rAdd,rMul,clos0,clos1,compl,lpar,rpar) expr' = showchar (insParentheses expr')
         where
      --    wrap i j str = if i<=j then str else lpar++str++rpar
          showchar (Tm mph)  = name mph++if inline mph then "" else "~"
          showchar (Fu []) = "-V"
          showchar (Fu fs) = chain union [showchar f| f<-fs]
          showchar (Fi []) = "V"
          showchar (Fi fs) = chain inter [showchar f| f<-fs]
          showchar (Fd []) = "-I"
          showchar (Fd ts) = chain rAdd [showchar t| t<-ts]
          showchar (F [])  = "I"
          showchar (F ts)  = chain rMul [showchar t| t<-ts]
          showchar (K0 e') = showchar e'++clos0
          showchar (K1 e') = showchar e'++clos1
          showchar (Cp e') = compl++showchar e'
          showchar (Tc f)  = lpar++showchar f++rpar
    
   insParentheses :: Expression -> Expression
   insParentheses expr = insPar 0 expr
         where
          wrap :: Integer -> Integer -> Expression -> Expression
          wrap i j e' = if i<=j then e' else Tc e'
          insPar :: Integer -> Expression -> Expression
          insPar _ (Tm mph) = Tm mph
          insPar i (Fu fs)  = wrap i 4 (Fu [insPar 4 f| f<-fs])
          insPar i (Fi fs)  = wrap i 5 (Fi [insPar 5 f| f<-fs])
          insPar i (Fd ts)  = wrap i 6 (Fd [insPar 6 t| t<-ts])
          insPar i (F ts)   = wrap i 7 (F  [insPar 7 t| t<-ts])
          insPar _ (K0 e')  = K0 (insPar 8 e')
          insPar _ (K1 e')  = K1 (insPar 8 e')
          insPar _ (Cp e')  = Cp (insPar 8 e')
          insPar i (Tc f)   = insPar i f

   instance Association Expression where

    source (Tm mph)        = source mph
    source (Tc f)          = source f
    source (F  [])         = Anything -- error ("(module CC_aux) Fatal: source (F [])")
    source (F  ts)         = source (head ts)
    source (Fd [])         = Anything -- error ("(module CC_aux) Fatal: source (Fd [])")
    source (Fd ts)         = source (head ts)
    source (Fu fs)         = if length (eqClass order (map source fs))==1 then minimum (map source fs)
                             else Anything -- error ("(module CC_aux) Fatal: source ("++showHS "" (Fu fs)++")")
    source (Fi fs)         = if length (eqClass order (map source fs))==1 then maximum (map source fs)
                             else Anything -- error ("(module CC_aux) Fatal: source ("++showHS "" (Fi fs)++")")
    source (K0 e')         = source e'
    source (K1 e')         = source e'
    source (Cp e')         = source e'

    target (Tm mph)        = target mph
    target (Tc f)          = target f
    target (F  [])         = Anything -- error ("(module CC_aux) Fatal: target (F [])")
    target (F  ts)         = target (last ts)
    target (Fd [])         = Anything -- error ("(module CC_aux) Fatal: target (Fd [])")
    target (Fd ts)         = target (last ts)
    target (Fu fs)         = if length (eqClass order (map target fs))==1 then minimum (map target fs)
                             else Anything
    target (Fi fs)         = if length (eqClass order (map target fs))==1 then maximum (map target fs)
                             else Anything
    target (K0 e')         = target e'
    target (K1 e')         = target e'
    target (Cp e')         = target e'

    sign (Tm mph)          = sign mph
    sign (Tc f)            = sign f
    sign (F ts)            = if null ts 
                              then error ("(module CC_aux) Fatal: no terms in sign (F "++show ts++")")
                              else foldr1 jnSign (map sign ts)
                              where (s , _ ) `jnSign` ( _ ,t') = (s,t')
    sign (Fd ts)           = if null ts 
                              then error ("(module CC_aux) Fatal: no terms in sign (Fd "++show ts++")")
                              else foldr1 jnSign (map sign ts)
                              where (s , _ ) `jnSign` ( _ ,t') = (s,t')
    sign (Fu fs)           = if length (eqClass order (map sign fs))>1 then error ("(module CC_aux) Fatal: sign (Fu fs) not defined\nwith map sign fs="++show (map sign fs)) else
                             if null fs then (cptAnything, cptAnything) else
                             foldr1 lub (map sign fs)
    sign (Fi fs)           = if length (eqClass order (map sign fs))>1 then error ("(module CC_aux) Fatal: sign (Fi fs) not defined\nwith map sign fs="++show (map sign fs)) else
                             if null fs then (cptAnything, cptAnything) else
                             foldr1 lub (map sign fs)
    sign (K0 e')           = sign e'
    sign (K1 e')           = sign e'
    sign (Cp e')           = sign e'

   instance Numbered Expression where
    pos (Tm mph)  = pos mph
    pos (Tc f)  = pos f
    pos (F ts)  = if not (null ts) then pos (head ts) else error "(module Expression) !!Software error 813. Please submit a complete bug report to your dealer"
    pos (Fd ts) = if not (null ts) then pos (head ts) else error "(module Expression) !!Software error 814. Please submit a complete bug report to your dealer"
    pos (Fu fs) = if not (null fs) then pos (head fs) else error "(module Expression) !!Software error 815. Please submit a complete bug report to your dealer"
    pos (Fi fs) = if not (null fs) then pos (head fs) else error "(module Expression) !!Software error 816. Please submit a complete bug report to your dealer"
    pos (K0 e')  = pos e'
    pos (K1 e')  = pos e'
    pos (Cp e')  = pos e'

   v :: Sign -> Expression
   v (a,b) = Tm (V [] (a,b))

   notCp :: Expression -> Expression
   notCp (Cp e') = e'
   notCp e' = Cp e'

   isPos :: Expression -> Bool
   isPos (Cp _) = False
   isPos _ = True
   isNeg :: Expression -> Bool
   isNeg e' = not (isPos e')


                      