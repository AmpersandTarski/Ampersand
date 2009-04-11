{-# OPTIONS_GHC -Wall #-}
module ComputeRule (ComputeRule(..)
                   ,triggers
                   ,conjuncts
                   ,allClauses
                   ,hornCs)
where
   import Adl 
--   import Char ( isSpace )
   import Collection (Collection (rd))
   import Auxiliaries(sort',eqClass)
--   import Classification
   import FspecDef
   import ShowADL
   import ShowHS
   import CC_aux
   import NormalForms(simplify,conjNF,disjNF)
   
   data ComputeRule = CR { crOps :: [(String,Declaration)]
                         , crexp :: Expression
                         , crbOp :: String
                         , crto  :: Expression
                         , crfrm :: Expression
                         , crule :: Rule} deriving (Eq)
   instance Show ComputeRule where
    showsPrec _ cr 
     = showString ("("++show (crOps cr)++", "
                      ++show (crexp cr)++", "
                      ++show (crbOp cr)++", "
                      ++show (crto cr) ++", "
                      ++show (crfrm cr)++", "
                      ++show (crule cr)++")")

   hornCs :: Rule -> Expression -> [ComputeRule]
   hornCs rule f@(Fu fus)
    = 
  -- the following inserts new atoms in positive terms. Deletion of new atoms from negative terms cannot occur,
  -- because an atom which is new in V is not in t in the first place (t-:V)
      [ CR { crOps = [("INSERT INTO", Vs (source t) (target t))]
           , crexp = v (sign t)
           , crbOp = "INSERT INTO"
           , crto  = simplify t
           , crfrm = v (sign t)
           , crule = rule}
      | and (map isPos fus), t<-fus, not (isIdent t)]++  -- (ignore generating to I, because both I and V are derived from C-tables.)
      [ CR { crOps = [("DELETE FROM",hdl l)]
           , crexp = if isPos t' then t' else notCp t'
           , crbOp = "DELETE FROM"
           , crto  = (conjNF.Cp) t
           , crfrm = (disjNF.Cp) (Fu (rest t))
           , crule = rule}
      | t<-fus, t'<-rest t, l<-leaves t', isPos l, isNeg t]++
      [ CR { crOps = [("INSERT INTO",hdl l)]
           , crexp = if isPos t' then t' else notCp t'
           , crbOp = "DELETE FROM"
           , crto  = (conjNF.Cp) t
           , crfrm = (disjNF.Cp) (Fu (rest t))
           , crule = rule}
      | t<-fus, t'<-rest t, l<-leaves t', isNeg l, isNeg t]++
      [ CR { crOps = [("DELETE FROM",hdl l)]
           , crexp = if isPos t' then t' else notCp t'
           , crbOp = "INSERT INTO"
           , crto  = conjNF t
           , crfrm = (disjNF.Cp) (Fu (rest t))
           , crule = rule}
      | t<-fus, t'<-rest t, l<-leaves t', isPos l, isPos t]++
      [ CR { crOps = [("INSERT INTO",hdl l)]
           , crexp = if isPos t' then t' else notCp t'
           , crbOp = "INSERT INTO"
           , crto  = conjNF t
           , crfrm = (disjNF.Cp) (Fu (rest t))
           , crule = rule}
      | t<-fus, t'<-rest t, l<-leaves t', isNeg l, isPos t]
      where rest t = if length [e'|e'<-fus, t /= e'] == length fus-1 then [e'|e'<-fus, t /= e'] else
                     error ("(module ComputeRule) Failure in hornCs rule f@(Fu fus) with\n"++
                            "Rule : "++showADL rule++"\n"++
                            "t    : "++showADL t++"\n"++
                            "t    : "++showHS "" t++"\n"++
                            "f    : "++showADL f++"\n"++
                            "f    : "++showHS "" f++"\n"
                           )
            hdl l | null (declarations l) = error("Module ComputeRule: empty list of declarations in hornCs")
                  | otherwise             = head (declarations l)
   hornCs rule e' = error("Module ComputeRule: erroneous call of hornCs ("++showHS "" e'++") in rule "++show (runum rule)++":\n  "++showADL rule)

   leaves :: Expression -> [Expression]
   leaves e' = rd (lvs e')
    where
     lvs (F fs)  = (concat.map lvs) fs
     lvs (Fd fs) = (concat.map lvs) fs
     lvs (Fu fs) = (concat.map lvs) fs
     lvs (Fi fs) = (concat.map lvs) fs
     lvs (Cp e'')  = [notCp l|l<-lvs e'' ]
     lvs (Tm r)  = [Tm r]
     lvs (K0 e'')  = lvs e''
     lvs (K1 e'')  = lvs e''
     lvs e'' = error("module ComputeRule: illegal pattern in leaves ("++showADL e''++")\ne = "++showHS "" e'')



   triggers :: Rule -> [ComputeRule]
   triggers rule
    = (concat.map (sort' crbOp).eqClass eq2expr)          --  < ---  bij gelijke targets: eerst DELETE dan INSERT
      [ hc ::ComputeRule
      | conjunct<-conjuncts rule, clause<-allClauses conjunct
      , hcID<-(map collect.eqClass eqHC.hornCs rule) clause  --  < ---  alle gelijke horn clauses op hoopjes vegen.
      , hc<-splitInsDel hcID
      , computing rule hc]
      where
   -- eerst alle gelijke horn clauses op hoopjes vegen.
       eqHC :: ComputeRule -> ComputeRule -> Bool
       cr `eqHC` cr' = and [ crbOp cr == crbOp cr'
                           , crto  cr == crto  cr'
                           , crfrm cr == crfrm cr']

       collect :: [ComputeRule] -> ComputeRule
       collect [] = undefined
       collect (x:xs) = x {crOps = (rd.concat)[crOps cr| cr <-x:xs]
                        ,crexp = simplify (Fu [crexp cr| cr <-x:xs])
                           }
       splitInsDel :: ComputeRule -> [ComputeRule]
       splitInsDel cr = [ cr{crOps = [fOp]} | fOp@("DELETE FROM",_)<-crOps cr]
                      ++[ cr{crOps = [fOp]} | fOp@("INSERT INTO",_)<-crOps cr]
                         
   -- volgorde aanbrengen in hornclauses met gelijke toExpr: eerst DELETE dan INSERT
       eq2expr :: ComputeRule -> ComputeRule -> Bool
       cr `eq2expr` cr' = (crto cr) == (crto cr')
   -- alleen "COMPUTING" termen opleveren
       computing :: Rule -> ComputeRule -> Bool
       computing r hc = crto hc `elem` map simplify (r_cpu r)
   -- debug:    computing rule e = error ("(module ComputeRule diagnostic) rule: "++showADL rule++"\e: "++show e++"\nr_cpu rule : "++ show (e `elem` r_cpu rule))

   conjuncts :: Rule -> [Expression]
   conjuncts = fiRule.conjNF.normExpr
    where fiRule (Fi fis) = {- map disjuncts -} fis
          fiRule r        = [ {- disjuncts -} r]

   disjuncts :: Expression -> Expression
   disjuncts = fuRule
    where fuRule (Fu cps) = (Fu . rd . map cpRule) cps
          fuRule r        = Fu [cpRule r]
          cpRule (Cp r)   = Cp (fRule r)
          cpRule r        = fRule r
          fRule (F ts)    = F ts
          fRule  r        = F [r]





   allClauses :: Expression -> [Expression]
   allClauses cl = rd [simplify e'|e'<-shiftL cl++shiftR cl, not (isTrue e')]
   shiftL :: Expression -> [Expression]
   shiftL r
    | length antss+length conss /= length fus = error ("(module ComputeRule) shiftL will not handle argument of the form "++showHS "" r)
    | null antss || null conss                = [disjuncts r|not (null fs)] --  shiftL doesn't work here.
    | idsOnly antss                           = [Fu ([Cp (F [Tm (mIs srcA)])]++map F conss)]
    | otherwise                               = [Fu ([ Cp (F (if null ts then id' css else ts))
                                                     | ts<-ass++if null ass then [id' css] else []]++
                                                     [ F (if null ts then id' ass else ts)
                                                     | ts<-css++if null css then [id' ass] else []])
                                                | (ass,css)<-rd(move antss conss)
                                                , if null css then error "Null css" else True
                                                , if null ass then error "Null ass" else True
                                                ]
    where
     Fu fs = disjuncts r
     fus = filter (not.isIdent) fs
     antss = [ts | Cp (F ts)<-fus]
     conss = [ts | F ts<-fus]
     srcA = -- if null antss  then error ("(module ComputeRule) empty antecedent in shiftL ("++showHS "" r++")") else
            if length (eqClass order [ source (head ants) | ants<-antss])>1 then error ("(module ComputeRule) shiftL ("++showHS "" r++")\n"++showADL r++"\nin calculation of srcA\n"++show (eqClass order [ source (head ants) | ants<-antss])) else
            foldr1 lub [ source (head ants) | ants<-antss]
     id' ass = [Tm (mIs c)]
      where a = (source.head.head) ass
            c = if not (a `order` b) then error ("(module ComputeRule) shiftL ("++showHS "" r++")\n"++showADL r++"\nass: "++show ass++"\nin calculation of c = a `lub` b with a="++show a++" and b="++show b) else
                a `lub` b
            b = (target.last.last) ass
   -- It is imperative that both ass and css are not empty.
     move :: [Expressions] -> [Expressions] -> [([Expressions],[Expressions])]
     move ass [] = [(ass,[])]
     move ass css
      = (ass,css):
        if and ([not (idsOnly (F cs))| cs<-css]) -- idsOnly (F [])=True, so:  and [not (null cs)| cs<-css]
        then [ts| length (eqClass (==) (map head css)) == 1
                , fun (multiplicities h)
                , ts<-move [[flp h]++as|as<-ass] (map tail css)]++
             [ts| length (eqClass (==) (map last css)) == 1
                , inj (multiplicities l)
                , ts<-move [as++[flp l]|as<-ass] (map init css)]
        else []
        where h=head (map head css); l=head (map last css)

   shiftR :: Expression -> [Expression]
   shiftR r
    | length antss+length conss /= length fus = error ("(module ComputeRule) shiftR will not handle argument of the form "++showHS "" r)
    | null antss || null conss                = [disjuncts r|not (null fs)] --  shiftR doesn't work here.
    | idsOnly conss                           = [Fu ([Cp (F [Tm (mIs srcA)])]++map F antss)]
    | otherwise                               = [Fu ([ Cp (F (if null ts then id' css else ts))
                                                     | ts<-ass++if null ass then [id' css] else []]++
                                                     [ F (if null ts then id' ass else ts)
                                                     | ts<-css++if null css then [id' ass] else []])
                                                | (ass,css)<-rd(move antss conss)]
    where
     Fu fs = disjuncts r
     fus = filter (not.isIdent) fs
     antss = [ts | Cp (F ts)<-fus]
     conss = [ts | F ts<-fus]
     srcA = if null conss then error ("(module ComputeRule) empty consequent in shiftR ("++showHS "" r++")") else
            if length (eqClass order [ source (head cons) | cons<-conss])>1 then error ("(module ComputeRule) shiftR ("++showHS "" r++")\n"++showADL r++"\nin calculation of srcA\n"++show (eqClass order [ source (head cons) | cons<-conss])) else
            foldr1 lub [ source (head cons) | cons<-conss]
     id' css = [Tm (mIs c)]
      where a = (source.head.head) css
            c = if not (a `order` b) then error ("(module ComputeRule) shiftR ("++showHS "" r++")\n"++showADL r++"\nass: "++show css++"\nin calculation of c = a `lub` b with a="++show a++" and b="++show b) else
                a `lub` b
            b = (target.last.last) css
     move :: [Expressions] -> [Expressions] -> [([Expressions],[Expressions])]
     move [] css = [([],css)]
     move ass css
      = (ass,css):
        if and [not (null as)| as<-ass]
        then [ts| length (eqClass (==) (map head ass)) == 1
                , sur (multiplicities h)
                , ts<-move (map tail ass) [[flp h]++cs|cs<-css]]++
             [ts| length (eqClass (==) (map last ass)) == 1
                , tot (multiplicities l)
                , ts<-move (map init ass) [cs++[flp l]|cs<-css]]
        else []
        where h=head (map head ass); l=head (map last ass)

   