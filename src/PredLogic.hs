  module PredLogic 
              ( normRule
              , normRL
              , assemble
              , PredLogic(Forall, Exists, Implies, Equiv, Conj, Disj, Not, Pred, Rel, Funs)
              , predLshow
              , mathVars
              , lang
              , objOrShow
              , explainArt
              )
   where

   import CommonClasses ( Identified(name))
   import Collection (Collection((>-)))
   import Auxiliaries (chain, eqCl)

   import Adl
   import ShowADL
   import ShowHS
   import CC_aux ( applyM 
                 , lub, sur, inj, fun, tot 
                 , mkVar
                 , order
            )
   import Languages (Lang(English,Dutch), plural)
 --  import Calc






   data PredLogic       
    = Forall [(String,Concept)] PredLogic   |
      Exists [(String,Concept)] PredLogic   |
      Implies PredLogic PredLogic           |
      Equiv PredLogic PredLogic             |
      Conj [PredLogic]                      |
      Disj [PredLogic]                      |
      Not PredLogic                         |
      Pred String String                    |  -- Pred nm v, with v::type   is equiv. to Mph nm posNone [] (type,type) True (Sgn (showADL e) type type [] "" "" "" [Asy,Sym] posNone 0 False)
      Rel PredLogic
          Morphism
          PredLogic                         |
      Funs String [Morphism]

   predLshow (forall, exists, implies, equiv, equal, nequal, or, and, not, rel, fun, showVars, break, space) e = charshow 0 e
     where
      wrap i j str = if i<=j then str else "("++str++")"
      charshow i (Forall vars restr)
       = wrap i 1 (showVars forall vars ++ charshow 1 restr)
      charshow i (Exists vars restr)
       = wrap i 1 (showVars exists vars  ++ charshow 1 restr)
      charshow i (Implies antc conseq)
       = wrap i 2 (break++implies (charshow 2 antc) (charshow 2 conseq))
      charshow i (Equiv lhs rhs)
       = wrap i 2 (break++charshow 2 lhs++space++equiv++space++ charshow 2 rhs)
      charshow i (Disj rs)
       = if null rs then "" else
         wrap i 3 (chain (space++or++space) (map (charshow 3) rs))
      charshow i (Conj rs)
       = if null rs then "" else
         wrap i 4 (chain (space++and++space) (map (charshow 4) rs))
      charshow i (Rel lhs m rhs)
       = wrap i 5 (if inline m
                   then rel m (charshow 5 lhs) (charshow 5 rhs)
                   else rel m (charshow 5 rhs) (charshow 5 lhs))
      charshow i (Funs x [])     = x
      charshow i (Funs x (m:ms)) = if isIdent m then charshow i (Funs x ms) else charshow i (Funs (fun m x) ms)
      charshow i (Not rs)        = wrap i 6 (space++not++charshow 6 rs)
      charshow i (Pred nm v)     = nm++"{"++v++"}"
      ishow e | idsOnly e = equal
              | isNot e   = nequal
              | otherwise = showADL e

   instance Show PredLogic where
    showsPrec p x = showString (predLshow ("For all", "Exists", implies, "<=>", "=", "\\=", "||", "&", "not", rel, fun, mathVars, "", " ") x)
                    where rel m lhs rhs = lhs++" "++name m++" "++rhs
                          fun m x = name m++"("++x++")"
                          implies antc cons = antc++" ==> "++cons

   mathVars :: String -> [(String,Concept)] -> String
   mathVars q vs
    = if null vs then "" else
      q++" "++chain "; " [chain ", " vs++"::"++dType | (vs,dType)<-vss]++": "
      where
       vss = [(map fst vs,show(snd (head vs))) |vs<-eqCl snd vs]

   explainArt :: Context -> Lang -> Rule -> String
   explainArt thisCtx l r
    = if null (explain r)
      then (if l==English then "Artificial explanation: " else
            if l==Dutch   then "Kunstmatige uitleg: " else
            error("Module PredLogic: unsupported language"))++(lang l .assemble.normRule) r
      else explain r

   lang :: Lang -> PredLogic -> String
   lang English x = predLshow ("For all", "There exists", implies, "is equivalent to", "equals", "is unequal to", "or", "and", "not", rel, fun, langVars English, "\n  ", " ") x
                    where rel m lhs rhs = applyM (makeDeclaration m) lhs rhs
                          fun m x = name m++"("++x++")"
                          implies antc cons = "If "++antc++", then "++cons
   lang Dutch   x = predLshow ("Voor elke", "Er is een", implies, "is equivalent met", "gelijk aan", "is ongelijk aan", "of", "en", "niet", rel, fun, langVars Dutch, "\n  ", " ") x
                    where rel m lhs rhs = applyM (makeDeclaration m) lhs rhs
                          fun m x = name m++"("++x++")"
                          implies antc cons = "Als "++antc++", dan "++cons

   langVars English q vs
    = if null vs then "" else
      if q=="Exists"
      then chain " and " ["there exist"++(if length vs==1 then "s a "++dType else " "++plural English dType)++" called "++chain ", " vs | (vs,dType)<-vss]
      else "If "++langVars English "Exists" vs++", "
      where
       vss = [(map fst vs,show(snd (head vs))) |vs<-eqCl snd vs]
   langVars Dutch q vs
    = if null vs then "" else
      if q=="Er is"
      then chain " en " ["er "++(if length vs==1 then "is een "++dType else "zijn "++plural Dutch dType)++" genaamd "++chain ", " vs | (vs,dType)<-vss]
      else "Als "++langVars Dutch "Er is" vs++", "
      where
       vss = [(map fst vs,show(snd (head vs))) |vs<-eqCl snd vs]



   objOrShow :: PredLogic -> String
   objOrShow = predLshow ("For all", "Exists", implies, " = ", " = ", "<>", "OR", "AND", "NOT", rel, fun, langVars English, "\n", " ")
               where rel m lhs rhs = applyM (makeDeclaration m) lhs rhs
                     fun m x = x++"."++name m
                     implies antc cons = "IF "++antc++" THEN "++cons
























   normRL r = normRule r -- try this (not quite OK, though...): head [ (makeRule r) c| c <- (simplify . shiftL . normExpr) r]

   ruleToPL :: Rule -> PredLogic
   ruleToPL = assemble . normRule

   normRule :: Rule -> Rule
   normRule r@(Ru Truth _ pos cons cpu expla sgn nr pn)
    = Ru Truth (error ("(Module PredLogic:) illegal reference to antecedent in normRule ("++showADL r++")")) pos (cons) cpu expla sgn nr pn
   normRule (Ru Implication a@(F ants) pos c@(F cons) cpu expla sgn nr pn)
    | idsOnly ants = Ru Implication (F [Tm (mIs idA)]) pos (F cons) cpu expla (idC,idC) nr pn
    | otherwise    = Ru Implication (F as) pos (F cs) cpu expla (sac,tac) nr pn
    where
     idC = source c `lub` target c `lub` idA
     idA = foldr lub (target (last ants)) (map source ants)
     (as,cs) = move ants cons
     (sac,tac) = (source (head as) `lub` source (head cs), target (last as) `lub` target (last cs))
     move [] cs = ([(Tm . mIs . source . head) cs],cs)
     move as cs
      | sur (multiplicities h) && inj (multiplicities h) = move (tail as) ([flp h]++cs)
      | fun (multiplicities l) && tot (multiplicities l) = move (init as) (cs++[flp l])
      | otherwise      = (as,cs)
      where h=head as; l=last as
   normRule (Sg p rule expla sgn nr pn signal)
     = Sg p r' expla (sign r') nr pn signal
       where r'= normRule rule
   normRule r = r

   assemble :: Rule -> PredLogic
   assemble r
    | ruleType r==Equivalence = Forall [(s,source(r)),(t,target(r))] (Equiv ra rc)
    | ruleType r==Implication = if idsOnly (antecedent r)
                        then Forall [(s,source(r))] rb
                        else transform (Forall [(s,source(r)),(t,target(r))] (Implies ra rc))
    | ruleType r==Truth = Forall [(s,source(r)),(t,target(r))] rc
    where
      [s,t] = mkVar [] [source(r), target(r)]
      transform (Forall vs (Implies (Exists es antc) cons)) = Forall (vs++es) (Implies antc cons)
      transform expr = expr
      (ra,avars) | ruleType r==Equivalence = assembleF [s,t] (antecedent r) s t
                 | ruleType r==Implication = assembleF [s,t] (antecedent r) s t
                 | ruleType r==Truth = assembleF [s,t] (consequent r) s t
      (rb,bvars) | ruleType r==Implication = assembleF [s]   (consequent r) s s
      (rc,cvars) | ruleType r==Equivalence = assembleF avars (consequent r) s t
                 | ruleType r==Implication = assembleF avars (consequent r) s t
                 | ruleType r==Truth = assembleF [s,t] (consequent r) s t

   assembleF :: [String] -> Expression -> String -> String -> (PredLogic,[String])
   assembleF exclVars (F ts) s t
    = {- debug 
      if error("Debug: assembleF exclVars (F ts) s t\nwith "++
               "exclVars = "++show exclVars++"\n          "++
               "ics = "++show ics++"\n          "++
               "ivs = "++show ivs++"\n         "++
               "(F ts) = "++showHS "" (F ts)++"\n        "++
               "yields: "++show (Exists (zip ivs ics) (Conj (frels "alpha" "omega"))::PredLogic)) then error ("") else -}
      if null ics then (head (frels s t), exclVars) else
      (Exists (zip ivs ics) (Conj (frels s t)), ivs++exclVars)
      where
       res       = pars3 exclVars (split ts)  -- levert drietallen (r,s,t)
       frels s t = [r v w|((r,_,_),v,w)<-zip3 res ([s]++ivs) (ivs++[t]) ]
       ics       = [if v `order` w then v `lub` w else error("Fatal: assembleF")
                   |(v,w)<-zip [s|(_,s,_)<-tail res] [t|(_,_,t)<-init res]]
       ivs       = mkVar exclVars ics




   assembleF exclVars (Fd ts) s t
    = {- debug 
      if error("Debug: assembleFd exclVars (Fd ts)\nwith "++
               "exclVars = "++show exclVars++"\n          "++
               (if null antc then "" else "ics antc = "++show icsantc++"\n          ")++
               "ics cons = "++show icscons++"\n          "++
               (if null antc then "" else "ivsantc = "++show ivsantc++"\n         ")++
               "ivscons = "++show ivscons++"\n         "++
               "(Fd ts) = "++showHS "" (Fd ts)++"\n        "++
               "yields: "++show res) then error ("") else -}
      res
      where
       res = if null antc
             then (if null icscons
                   then (head (frels cons ivscons s t), exclVars)
                   else (Forall (zip ivscons icscons) (Disj (frels cons ivscons s t)), ivscons++exclVars)
                  )
             else (Forall (zip ivs (ci: icsantc++icscons))
                      (Implies (if null (tail ivsantc) then head (frels antc ivsantc s (head ivsantc)) else Conj (frels antc (tail ivsantc) s (head ivsantc)) )
                               (if null ivscons then head (frels cons ivscons (head ivsantc) t) else Disj (frels cons ivscons (head ivsantc) t))
                      )
                  , ivsantc++ivscons++exclVars)
       lnegs = takeWhile isNeg ts; rpos = dropWhile isNeg ts
       rnegs = (reverse . takeWhile isNeg . reverse) ts; lpos = (reverse . dropWhile isNeg . reverse) ts
       (antc,cons,ca,cc) | not (null lnegs) && not (null rpos) = ((pars3 exclVars.split.map notCp) lnegs,pars3 exclVars (split rpos),target (last lnegs),source (head rpos))
                         | not (null rnegs) && not (null lpos) = ((pars3 exclVars.split.map notCp) rnegs,pars3 exclVars (split lpos),source (head rnegs),target (last lpos))
                         | otherwise = ([],pars3 exclVars (split ts),cptAnything,cptAnything)
       frels res ivs s t = [r v w|((r,_,_),v,w)<-zip3 res ([s]++ivs) (ivs++[t]) ]
       ics res   = if null res then [] else
                   [unite v w |(v,w)<-zip [s|(_,s,_)<-tail res] [t|(_,_,t)<-init res]]
       icscons   = ics cons
       icsantc   = ics antc
       ivsantc   = mkVar exclVars (ci:icsantc)
       ivscons   = mkVar exclVars icscons
       unite v w = if v `order` w then v `lub` w else error("Fatal: assembleFd")
       ci        = ca `unite` cc
       ivs       = ivsantc++ivscons
   assembleF exclVars (Fu fs) s t
    = (Disj [fst (assembleF exclVars f s t)|f<-fs], exclVars)
   assembleF exclVars (Fi fs) s t         
    = (Conj [fst (assembleF exclVars f s t)|f<-fs], exclVars)
   assembleF exclVars (Cp e) s t          
    = (Not  (fst (assembleF exclVars e s t)), exclVars)
   assembleF exclVars e s t
    = if length (morlist e)==1 then (res, exclVars) else
      error ("Non-exhaustive patterns in function assembleF "++show exclVars++" ("++showHS "" e++")")
      where
       res | denote e==Flr = Rel (Funs s (mors e)) (mIs (target e)) (Funs t [])
           | denote e==Frl = Rel (Funs s []) (mIs (source e)) (Funs t (map flp (mors e)))
           | denote e==Rn  = if inline  m
                             then Rel (Funs s []) m (Funs t [])
                             else Rel (Funs t []) (flp m) (Funs s [])
                             where m = head (mors e)

   data Notation = Flr | Frl | Rn | Wrap deriving Eq   -- yields notations y=r(x)  |  x=r(y)  |  x r y  | exists ... respectively.

   relFun :: [String] -> [Expression] -> Expression -> [Expression] -> (String->String->PredLogic)
   relFun exclVars lhs (Tm m) rhs
     = (\s->(\t->if inline m
                 then Rel (Funs s [m| t<-lhs, m<-mors t]) m (Funs t [m| t<-reverse rhs, m<-mors t])
                 else Rel (Funs t [m| t<-reverse rhs, m<-mors t]) (flp m) (Funs s [m| t<-lhs, m<-mors t])))
   relFun exclVars lhs e rhs
     = (\s->(\t->let (pl,exclVars') = assembleF (exclVars++[s,t]) e s t in pl))

   pars3 exclVars (lhs: [e]: rhs: ts)
    | denotes lhs==Flr && denote e==Rn && denotes rhs==Frl
       = ( relFun exclVars lhs e rhs, source (head lhs), target (last rhs)): pars3 exclVars ts
    | otherwise = pars2 exclVars (lhs:[e]:rhs:ts)
   pars3 exclVars ts = pars2 exclVars ts -- for lists shorter than 3
   pars2 exclVars (lhs: [e]: ts)
    | denotes lhs==Flr && denote e==Rn
                = (relFun exclVars lhs e [], source (head lhs), target e): pars3 exclVars ts
    | denotes lhs==Flr && denote e==Frl
                = (relFun exclVars lhs (Tm (mIs (source e))) [e], source (head lhs), target e): pars3 exclVars ts
    | otherwise = pars1 exclVars (lhs:[e]:ts)
   pars2 exclVars ([e]: rhs: ts)
    | denotes rhs==Frl && denote e==Rn
                = (relFun exclVars [] e rhs, source e, target (last rhs)): pars3 exclVars ts
    | denote e==Flr && denotes rhs==Frl
                = (relFun exclVars [e] (Tm (mIs (source e))) rhs, source e, target (last rhs)): pars3 exclVars ts
    | otherwise = pars1 exclVars ([e]:rhs:ts)
   pars2 exclVars (lhs: rhs: ts)
    | denotes lhs==Flr && denotes rhs==Frl
                = (relFun exclVars lhs (Tm (mIs (source (head rhs)))) rhs, source (head lhs), target (last rhs)): pars3 exclVars ts
    | otherwise = pars1 exclVars (lhs:rhs:ts)
   pars2 exclVars ts = pars1 exclVars ts -- for lists shorter than 2
   pars1 exclVars (lhs: ts)
    = (pars0 exclVars lhs, source (head lhs), target (last lhs)): pars3 exclVars ts
   pars1 exclVars [] = []
   pars0 exclVars lhs
    | denotes lhs==Flr = (relFun exclVars lhs (Tm (mIs (target (last lhs)))) [])
    | denotes lhs==Frl = (relFun exclVars [] (Tm (mIs (target (last lhs)))) lhs)
    | otherwise        = (relFun exclVars [] (let [r]=lhs in r) [])

   denote :: Expression -> Notation
   denote (Tm m)
    | null([Uni,Inj,Tot,Sur] >- ps)  = Rn
    | Uni `elem` ps && Tot `elem` ps = Flr
    | Inj `elem` ps && Sur `elem` ps = Frl
    | otherwise                      = Rn
    where ps = multiplicities m
   denote e = Rn
   denotes = denote . head



   split []  = []
   split [t] = [[t]]
   split (t:t':ts)
    = --if denote t `eq` Wrap      then (t:spl):spls else
      if denote t `eq` denote t' then (t:spl):spls else
                                      [t]:spl:spls
      where 
        spl:spls = split (t':ts)
        Flr `eq` Flr = True
        Frl `eq` Frl = True
        x `eq` y     = False
