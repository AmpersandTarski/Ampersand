{-# OPTIONS_GHC -Wall -XFlexibleInstances #-} --TODO verder opschonen van deze module
module PredLogic
           ( PredLogicShow(..), showLatex
           ) 
   where

   import Collection (Collection((>-)))
   import Data.List
   import Auxiliaries (eqCl)
   import Ampersand
   import ShowADL
   import Languages (Lang(English,Dutch), plural)
   import Options
   import Char (toLower)
   import Rendering.PandocAux 
 
 --  data PredVar = PV String     -- TODO Bedoeld om predicaten inzichtelijk te maken. Er bestaan namelijk nu verschillende manieren om hier mee om te gaan (zie ook AdlExplanation. HJO. 
   data PredLogic       
    = Forall [(String,Concept)] PredLogic   |
      Exists [(String,Concept)] PredLogic   |
      Implies PredLogic PredLogic           |
      Equiv PredLogic PredLogic             |
      Conj [PredLogic]                      |
      Disj [PredLogic]                      |
      Not PredLogic                         |
      Pred String String                    |  -- Pred nm v, with v::type   is equiv. to Rel nm Nowhere [] (type,type) True (Sgn (showADL e) type type [] "" "" "" [Asy,Sym] Nowhere 0 False)
      R PredLogic
        (Relation Concept)
        PredLogic                         |
      Funs String [Relation Concept]

--   predKeyWords flags = 
--     case language flags of
--        English  -> 

   class PredLogicShow a where
     showPredLogic :: Options -> a -> String
     showPredLogic flags r =  
       predLshow (natLangOps flags) (toPredLogic r)
     toPredLogic :: a -> PredLogic

   instance PredLogicShow (Rule (Relation Concept)) where
    toPredLogic ru = assemble (normRule ru)
     where 
   -- normRule is bedoeld om een regel in de juiste vorm te brengen voor assemblage....
      normRule :: Rule (Relation Concept) -> Rule (Relation Concept)
      normRule r@(Ru{rrsrt=Truth})
       = r{rrant=error ("!Fatal (module PredLogic 48): illegal reference to antecedent in normRule ("++showADL r++")")}
      normRule r@(Ru{rrsrt=Implication,rrant=(F ants),rrcon=c@(F cons)})
       | and (map idsOnly ants) = r{rrant=F [Tm (mIs idA) (-1)],rrtyp=(idC,idC)}
       | otherwise              = r{rrant=F as,rrcon=F cs,rrtyp=(sac,tac)}
       where
        idC = source c `lub` target c `lub` idA
        idA = foldr lub (target (last ants)) (map source ants)
        (as,cs) = move ants cons
        (sac,tac) = (source (head as) `lub` source (head cs), target (last as) `lub` target (last cs))
        move [] cs' = ([(Tm . mIs . source . head) cs' (-1)],cs')
        move as' cs'
         | isSur h && isInj h = move (tail as') ([flp h]++cs')
         | isUni l && isTot l = move (init as') (cs'++[flp l])
         | otherwise      = (as',cs')
         where h=head as'; l=last as'
      normRule r = r

   instance PredLogicShow (Expression (Relation Concept)) where
     toPredLogic e
       = case (source(e), target(e)) of
           (S, S) -> rc
           (_, S) -> Forall [(s,source(e))] rc
           (S, _) -> Forall [(s,target(e))] rc
           (_, _) -> Forall [(s,source(e)),(t,target(e))] rc
      where
       [s,t] = mkVar [] [source e, target e]
       (rc,_) = assembleF [s,t] e s t


-- showLatex ought to produce PandDoc mathematics instead of LaTeX source code.
-- PanDoc, however, does not support mathematics sufficiently, as to date. For this reason we have showLatex.
-- It circumvents the PanDoc structure and goes straight to LaTeX source code.
   showLatex :: PredLogic -> String
   showLatex x
    = predLshow ("\\forall", "\\exists", implies, "\\Leftrightarrow", "\\vee", "\\wedge", "\\neg", rel, fun, mathVars, "", " ", apply) x
      where rel m lhs rhs  -- TODO: the stuff below is very sloppy. This ought to be derived from the stucture, instead of by this naming convention.
              = if isIdent m then lhs++"\\ =\\ "++rhs else
                case name m of
                 "lt"     -> lhs++"\\ <\\ "++rhs
                 "gt"     -> lhs++"\\ >\\ "++rhs
                 "le"     -> lhs++"\\ \\leq\\ "++rhs
                 "leq"    -> lhs++"\\ \\leq\\ "++rhs
                 "ge"     -> lhs++"\\ \\geq\\ "++rhs
                 "geq"    -> lhs++"\\ \\geq\\ "++rhs
                 _        -> lhs++"\\ "++texOnly_Id (name m)++"\\ "++rhs
            fun m e = texOnly_Id (name m)++"("++e++")"
            implies antc cons = antc++" \\Rightarrow "++cons
            apply :: Declaration c -> String -> String -> String    --TODO language afhankelijk maken. 
            apply decl d c =
               case decl of
                 Sgn{}     -> d++"\\ "++texOnly_Id (name decl)++"\\ "++c
                 Isn{}     -> d++"\\ =\\ "++c
                 Iscompl{} -> d++"\\ \not =\\ "++c
                 Vs{}      -> "V"
            mathVars :: String -> [(String,Concept)] -> String
            mathVars q vs
             = if null vs then "" else
               q++" "++intercalate "; " [intercalate ", " var++"::"++dType | (var,dType)<-vss]++": "
               where
                vss = [(map fst varCl,show(snd (head varCl))) |varCl<-eqCl snd vs]

-- natLangOps exists for the purpose of translating a predicate logic expression to natural language.
-- It yields a vector of mostly strings, which are used to assemble a natural language text in one of the natural languages supported by Ampersand.
   natLangOps :: Identified a => Options -> ([Char],
                                            [Char],
                                            [Char] -> [Char] -> [Char],
                                            [Char],
                                            [Char],
                                            [Char],
                                            [Char],
                                            Relation Concept -> String -> String -> String,
                                            a -> [Char] -> [Char],
                                            String -> [(String, Concept)] -> String,
                                            [Char],
                                            [Char],
                                            Declaration Concept -> String -> String -> String)
   natLangOps flags
            = case language flags of
-- parameternamen:         (forallP,     existsP,        impliesP, equivP,             orP,  andP,  notP,  relP, funP, showVarsP, breakP, spaceP)
                English -> ("For each",  "There exists", implies, "is equivalent to",  "or", "and", "not",  rel, fun,  langVars , "\n  ", " ", apply)
                Dutch   -> ("Voor elke", "Er is een",    implies, "is equivalent met", "of", "en",  "niet", rel, fun,  langVars , "\n  ", " ", apply)
               where
                  rel m lhs rhs = apply (makeDeclaration m) lhs rhs
                  fun m x' = name m++"("++x'++")"
                  implies antc cons = case language flags of 
                                        English  -> "If "++antc++", then "++cons
                                        Dutch    -> "Als "++antc++", dan "++cons
                  apply :: Declaration Concept -> String -> String -> String    --TODO language afhankelijk maken. 
                  apply decl d c =
                     case decl of
                       Sgn{}     -> if null (prL++prM++prR) 
                                      then d++" "++decnm decl++" "++c 
                                      else prL++d++prM++c++prR
                          where prL = decprL decl
                                prM = decprM decl
                                prR = decprR decl
                       Isn{}     -> case language flags of 
                                        English  -> d++" equals "++c
                                        Dutch    -> d++" is gelijk aan "++c
                       Iscompl{} -> case language flags of 
                                        English  -> d++" differs from "++c
                                        Dutch    -> d++" verschilt van "++c
                       Vs{}      -> case language flags of 
                                        English  -> show True
                                        Dutch    -> "Waar"
                  langVars :: String -> [(String, Concept)] -> String
                  langVars q vs
                      = case language flags of
                         English -> if null vs then "" else
                                    if q=="Exists"
                                    then intercalate " and " ["there exist"++(if length vs'==1 then "s a "++dType else " "++plural English dType)++" called "++intercalate ", " vs' | (vs',dType)<-vss]
                                    else "If "++langVars "Exists" vs++", "
                                    where
                                     vss = [(map fst vs',show(snd (head vs'))) |vs'<-eqCl snd vs]
                         Dutch   -> if null vs then "" else
                                    if q=="Er is"
                                    then intercalate " en " ["er "++(if length vs'==1 then "is een "++dType else "zijn "++plural Dutch dType)++" genaamd "++intercalate ", " vs' | (vs',dType)<-vss]
                                    else "Als "++langVars "Er is" vs++", "
                                    where
                                     vss = [(map fst vs',show(snd (head vs'))) |vs'<-eqCl snd vs]

-- predLshow exists for the purpose of translating a predicate logic expression to natural language.
-- It uses a vector of operators (mostly strings) in order to produce text. This vector can be produced by, for example, natLangOps.
-- example:  'predLshow (natLangOps flags) e' translates expression 'e'
-- into a string that contains a natural language representation of 'e'.
   predLshow :: ( String                                    -- forallP
                , String                                    -- existsP
                , String -> String -> String                -- impliesP
                , String                                    -- equivP
                , String                                    -- orP
                , String                                    -- andP
                , String                                    -- notP
                , Relation Concept -> String -> String -> String    -- relP
                , Relation Concept -> String -> String              -- funP
                , String -> [(String, Concept)] -> String   -- showVarsP
                , String                                    -- breakP
                , String                                    -- spaceP
                , Declaration Concept -> String -> String -> String -- apply
                ) -> PredLogic -> String
   predLshow (forallP, existsP, impliesP, equivP, orP, andP, notP, relP, funP, showVarsP, breakP, spaceP, apply) e
    = charshow 0 e
        where
         wrap i j str = if i<=j then str else "("++str++")"
         charshow :: Integer -> PredLogic -> String
         charshow i predexpr
          = case predexpr of
                  Forall vars restr   -> wrap i 1 (showVarsP forallP vars ++ charshow 1 restr)
                  Exists vars restr   -> wrap i 1 (showVarsP existsP vars  ++ charshow 1 restr)
                  Implies antc conseq -> wrap i 2 (breakP++impliesP (charshow 2 antc) (charshow 2 conseq))
                  Equiv lhs rhs       -> wrap i 2 (breakP++charshow 2 lhs++spaceP++equivP++spaceP++ charshow 2 rhs)
                  Disj rs             -> if null rs 
                                         then "" 
                                         else wrap i 3 (intercalate (spaceP++orP ++spaceP) (map (charshow 3) rs))
                  Conj rs             -> if null rs 
                                         then "" 
                                         else wrap i 4 (intercalate (spaceP++andP++spaceP) (map (charshow 4) rs))
                  Funs x ls           -> case ls of
                                            []    -> x
                                            m:ms  -> if isIdent m then charshow i (Funs x ms) else charshow i (Funs (funP m x) ms)
                  R pexpr m pexpr'   -> case (pexpr,pexpr') of
                                            (Funs l [] , Funs r [])  -> wrap i 5 (apply (makeDeclaration m) l r)
                                            (Funs x [l], Funs r [])  -> wrap i 5 (if isIdent m
                                                                                  then apply (makeDeclaration l) x r
                                                                                  else apply (makeDeclaration m) x r)
                                            (Funs l [] , Funs y [r]) -> wrap i 5 (if isIdent m
                                                                                  then apply (makeDeclaration r) l y
                                                                                  else apply (makeDeclaration m) l y)
                                            (lhs,rhs)                -> wrap i 5 (if inline m
                                                                                  then relP m (charshow 5 lhs) (charshow 5 rhs)
                                                                                  else relP m (charshow 5 rhs) (charshow 5 lhs))
                  Not rs              -> wrap i 6 (spaceP++notP++charshow 6 rs)
                  Pred nm v'          -> nm++"{"++v'++"}"


--objOrShow :: Options -> PredLogic -> String
--objOrShow flags = predLshow ("For all", "Exists", implies, " = ", " = ", "<>", "OR", "AND", "NOT", rel, fun, langVars flags, "\n", " ")
--               where rel m lhs rhs = applyM (makeDeclaration m) lhs rhs
--                     fun m x = x++"."++name m
--                     implies antc cons = "IF "++antc++" THEN "++cons

   assemble :: Rule (Relation Concept) -> PredLogic
   assemble r
      = case ruleType r of
          Equivalence ->  Forall [(s,source(r)),(t,target(r))] (Equiv ra rc)
          Implication ->  if idsOnly (antecedent r)
                          then Forall [(s,source(r))] rb
                          else transform (Forall [(s,source(r)),(t,target(r))] (Implies ra rc))
          Truth       ->  Forall [(s,source(r)),(t,target(r))] rc
          Generalization -> error ("!Fatal (module PredLogic 236): assemble not defined for Generalization. ("++showADL r++")")
    where
      [s,t] = mkVar [] [source(r), target(r)]
      transform (Forall vs (Implies (Exists es antc) cons)) = Forall (vs++es) (Implies antc cons)
      transform plExpr = plExpr
      (ra,avars) =
         case ruleType r of
           Equivalence -> assembleF [s,t] (antecedent r) s t
           Implication -> assembleF [s,t] (antecedent r) s t
           Truth       -> assembleF [s,t] (consequent r) s t
           Generalization -> error ("!Fatal (module PredLogic 246): (ra,avars) not defined for Generalization. ("++showADL r++")")
      (rb,_    ) = if ruleType r==Implication 
                   then assembleF [s]   (consequent r) s s
                   else error ("!Fatal (module PredLogic 249): (rb,bvars) not defined. ("++showADL r++")")
      (rc,_    ) =
         case ruleType r of
           Equivalence -> assembleF avars (consequent r) s t
           Implication -> assembleF avars (consequent r) s t
           Truth       -> assembleF [s,t] (consequent r) s t
           Generalization -> error ("!Fatal (module PredLogic 255): (rc,cvars) not defined for Generalization. ("++showADL r++")")

   assembleF :: [String] -> (Expression (Relation Concept)) -> String -> String -> (PredLogic,[String])
   assembleF exclVars (F ts) s t
    = {- debug 
      if error("!Debug (module PredLogic 260): assembleF exclVars (F ts) s t\nwith "++
               "exclVars = "++show exclVars++"\n          "++
               "ics = "++show ics++"\n          "++
               "ivs = "++show ivs++"\n         "++
               "(F ts) = "++showHS options "" (F ts)++"\n        "++
               "yields: "++show (Exists (zip ivs ics) (Conj (frels "alpha" "omega"))::PredLogic)) then error ("") else -}
      if null ics then (head (frels s t), exclVars) else
      (Exists (zip ivs ics) (Conj (frels s t)), ivs++exclVars)
      where
       res       = pars3 exclVars (split ts)  -- levert drietallen (r,s,t)
       frels s' t' = [r v' w|((r,_,_),v',w)<-zip3 res ([s']++ivs) (ivs++[t']) ]
       ics       = [if v' `comparable` w then v' `lub` w else error("!Fatal (module PredLogic 271): assembleF")
                   |(v',w)<-zip [s'|(_,s',_)<-tail res] [t'|(_,_,t')<-init res]]
       ivs       = mkVar exclVars ics

   assembleF exclVars (Fdx ts) s t
    = {- debug 
      if error("!Debug (module PredLogic 277): assembleFd exclVars (Fd ts)\nwith "++
               "exclVars = "++show exclVars++"\n          "++
               (if null antc then "" else "ics antc = "++show icsantc++"\n          ")++
               "ics cons = "++show icscons++"\n          "++
               (if null antc then "" else "ivsantc = "++show ivsantc++"\n         ")++
               "ivscons = "++show ivscons++"\n         "++
               "(Fd ts) = "++showHS options "" (Fd ts)++"\n        "++
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
       frels res' ivs' s' t' = [r v' w|((r,_,_),v',w)<-zip3 res' ([s']++ivs') (ivs'++[t']) ]
       ics res'   = if null res' then [] else
                   [unite v' w |(v',w)<-zip [s'|(_,s',_)<-tail res'] [t'|(_,_,t')<-init res']]
       icscons   = ics cons
       icsantc   = ics antc
       ivsantc   = mkVar exclVars (ci:icsantc)
       ivscons   = mkVar exclVars icscons
       unite v' w = if v' `comparable` w then v' `lub` w else error("!Fatal (module PredLogic 309): assembleFd")
       ci        = ca `unite` cc
       ivs       = ivsantc++ivscons
   assembleF exclVars (Fux fs) s t
    = (Disj [fst (assembleF exclVars f s t)|f<-fs], exclVars)
   assembleF exclVars (Fix fs) s t         
    = (Conj [fst (assembleF exclVars f s t)|f<-fs], exclVars)
   assembleF exclVars (Cpx e) s t          
    = (Not  (fst (assembleF exclVars e s t)), exclVars)
   assembleF exclVars e s t
    = if length (morlist e)==1 then (res, exclVars) else
      error ("!Fatal (module PredLogic 320): Non-exhaustive patterns in function assembleF "++show exclVars++" ("++showADL e++")")
      where
       res :: PredLogic
       res = case denote e of
               Flr  -> R (Funs s (mors e)) (mIs (target e)) (Funs t [])
               Frl  -> R (Funs s []) (mIs (source e)) (Funs t (map flp (mors e)))
               Rn   -> if inline  m
                       then R (Funs s []) m (Funs t [])
                       else R (Funs t []) (flp m) (Funs s [])
               Wrap -> error ("!Fatal (module PredLogic 329): function res not defined when denote e == Wrap. ")
              where m = head (mors e)





   data Notation = Flr | Frl | Rn | Wrap deriving Eq   -- yields notations y=r(x)  |  x=r(y)  |  x r y  | exists ... respectively.

   relFun :: [String] -> [Expression (Relation Concept)] -> Expression (Relation Concept) -> [Expression (Relation Concept)] -> (String->String->PredLogic)
   relFun exclVars lhs e rhs
     = case e of
         (Tm rel _) -> (\s->(\t->if inline rel
                               then R (Funs s [m'| t'<-lhs, m'<-mors t']) rel (Funs t [m'| t'<-reverse rhs, m'<-mors t'])
                               else R (Funs t [m'| t'<-reverse rhs, m'<-mors t']) (flp rel) (Funs s [m'| t'<-lhs, m'<-mors t'])))
         _        -> (\s->(\t->let (pl,_) = assembleF (exclVars++[s,t]) e s t in pl))       

   pars3 :: [String] -> [[Expression (Relation Concept)]] -> [(String -> String -> PredLogic, Concept, Concept)] 
   pars3 exclVars (lhs: [e]: rhs: ts)
    | denotes lhs==Flr && denote e==Rn && denotes rhs==Frl
       = ( relFun exclVars lhs e rhs, source (head lhs), target (last rhs)): pars3 exclVars ts
    | otherwise = pars2 exclVars (lhs:[e]:rhs:ts)
   pars3 exclVars ts = pars2 exclVars ts -- for lists shorter than 3

   pars2 :: [String] -> [[Expression (Relation Concept)]]-> [(String -> String -> PredLogic, Concept, Concept)]
   pars2 exclVars (lhs: [e]: ts)
    | denotes lhs==Flr && denote e==Rn
                = (relFun exclVars lhs e [], source (head lhs), target e): pars3 exclVars ts
    | denotes lhs==Flr && denote e==Frl
                = (relFun exclVars lhs (Tm (mIs (source e))(-1)) [e], source (head lhs), target e): pars3 exclVars ts
    | otherwise = pars1 exclVars (lhs:[e]:ts)
   pars2 exclVars ([e]: rhs: ts)
    | denotes rhs==Frl && denote e==Rn
                = (relFun exclVars [] e rhs, source e, target (last rhs)): pars3 exclVars ts
    | denote e==Flr && denotes rhs==Frl
                = (relFun exclVars [e] (Tm (mIs (source e))(-1)) rhs, source e, target (last rhs)): pars3 exclVars ts
    | otherwise = pars1 exclVars ([e]:rhs:ts)
   pars2 exclVars (lhs: rhs: ts)
    | denotes lhs==Flr && denotes rhs==Frl
                = (relFun exclVars lhs (Tm (mIs (source (head rhs)))(-1)) rhs, source (head lhs), target (last rhs)): pars3 exclVars ts
    | otherwise = pars1 exclVars (lhs:rhs:ts)
   pars2 exclVars ts = pars1 exclVars ts -- for lists shorter than 2
   
   pars1 :: [String] -> [[Expression (Relation Concept)]] -> [(String -> String -> PredLogic, Concept, Concept)]
   pars1 exclVars expressions
     = case expressions of
         []        -> []
         (lhs: ts) -> (pars0 exclVars lhs, source (head lhs), target (last lhs)): pars3 exclVars ts

   pars0 :: [String] -> [Expression (Relation Concept)] -> String -> String -> PredLogic
   pars0 exclVars lhs
    | denotes lhs==Flr = (relFun exclVars lhs (Tm (mIs (target (last lhs)))(-1)) [])
    | denotes lhs==Frl = (relFun exclVars [] (Tm (mIs (target (last lhs)))(-1)) lhs)
    | otherwise        = (relFun exclVars [] (let [r]=lhs in r) [])

   denote :: (Expression (Relation Concept)) -> Notation
   denote e = case e of
      (Tm m _)
        | null([Uni,Inj,Tot,Sur] >- multiplicities m)  -> Rn
        | (isUni m) && (isTot m)                       -> Flr
        | (isInj m) && (isSur m)                       -> Frl
        | otherwise                                    -> Rn 
      _                                                -> Rn
   denotes :: [Expression (Relation Concept)] -> Notation
   denotes = denote . head



   split :: [Expression (Relation Concept)] -> [[Expression (Relation Concept)]]
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
        _ `eq` _     = False

        
-- mkVar is bedoeld om nieuwe variabelen te genereren, gegeven een set (ex) van reeds vergeven variabelen.
-- mkVar garandeert dat het resultaat niet in ex voorkomt, dus postconditie:   not (mkVar ex cs `elem` ex)
-- Dat gebeurt door het toevoegen van apostofes.
   mkVar :: (Identified a) => [String] -> [a] -> [String]  
   mkVar ex cs = mknew ex [[(toLower.head.(++"x").name) c]|c<-cs]
    where
     mknew _ [] = []
     mknew ex' (x:xs) | x `elem` ex' = mknew ex' ((x++"'"):xs)
                      | otherwise = x: mknew (ex'++[x]) xs

        
 
