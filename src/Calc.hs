{-# OPTIONS_GHC -Wall -XFlexibleContexts #-}
module Calc ( deriveProofs
            , reprAsRule
            , lambda
            , checkMono
            , showProof
            , positiveIn) 
  where

   import Collection         (Collection (isc,rd,rd'))
   import Auxiliaries        (sort',eqCl)
   import Strings            (commaEng)
   import Data.List
   import ADL
   import Data.Fspec
   import ADL2Fspec          (actSem, delta, allClauses, conjuncts, assembleECAs, preEmpt, doCode, editable, editMph)
   import ShowECA
   import ShowHS
   import ShowADL            (ShowADL(..))
   import NormalForms        (conjNF,disjNF,nfProof,cfProof,dfProof,simplify, normPA) --,proofPA) -- proofPA may be used to test derivations of PAclauses.
   import Options            (Options(..),defaultFlags)
   import Data.Explain
   import Languages          (Lang(..))
   showClause  :: Fspc -> Clauses -> String
   showClause fSpec cl
    = "\nRule: "++showADLcode fSpec (cl_rule cl) ++concat
       [if null shifts then "\nNo clauses" else
        "\nConjunct: "++showADLcode fSpec conj++
        concat ["\n   Clause: "++showADLcode fSpec clause| clause<-shifts]
       | (conj, shifts)<-cl_conjNF cl]

-- testService :: Fspc -> ObjectDef -> String
-- Deze functie is bedoeld om te bedenken hoe services moeten worden afgeleid uit een vers vertaalde ObjectDef.
-- Nadat deze goed werkt kunnen de bewijsgenerator en de codegenerator worden gemaakt.
   testService :: Fspc -> ObjectDef -> String
   testService fSpec object
    = "\nService "++ objnm object++"("++intercalate ", " [showADL m++":"++name (target m)| m<-rels]++")\n"++
      " - The parameters correspond to editable fields in a UI-service.\n   "++
      showADLcode fSpec object++"\n"++
      " - Invariants:\n   "++intercalate "\n   " [showADLcode fSpec rule    | rule<-invariants]++"\n"++
      " - Derivation of clauses for ECA-rules:"   ++
      concat [showClause fSpec (allClauses rule) | rule<-invariants]++"\n"++
{-
      " - ECA rules:"++concat  [ "\n\n   "++showECA fSpec "\n>     "  (eca{ecaAction=normPA (ecaAction eca)})
                                 ++"\n------ Derivation ----->"++showProof (showECA fSpec "\n>     ") (proofPA (ecaAction eca))++"\n<------End Derivation --"
                               | eca<-ecaRs]++"\n\n"++
-}
      " - Visible relations:\n   "++intercalate "\n   " (spread 80 ", " [showADLcode fSpec m  | m<-vis])++"\n"
    where
--        showQ i (m, shs,conj,r)
--         = "\nQuad "++show i++":\nmorphism: "++showADLcode fSpec m++":\nshifts: "++concat ["\n"++showADLcode fSpec s|s<-shs]++"\nconjunct: "++showADLcode fSpec conj++"\nrule: "++showADLcode fSpec r++""
--TODO: Deze code komt ook voor in ADL2Fspec.hs. Dat lijkt dubbelop, en derhalve niet goed.
        rels = rd (recur object)
         where recur obj = [editMph (objctx o)| o<-objats obj, editable (objctx o)]++[m| o<-objats obj, m<-recur o]
        vis        = rd (map makeInline rels++map (mIs.target) rels)
--        visible m  = makeInline m `elem` vis
        invariants = [rule| rule<-rules fSpec, not (null (map makeInline (mors rule) `isc` vis))]
--        qs         = vquads fSpec
--        ecaRs      = assembleECAs visible qs
--        editable (Tm Mph{} _)  = True    --WHY?? Stef, welke functie is de juiste?? TODO deze functie staat ook in ADL2Fspec.hs, maar is daar ANDERS(!)...
--        editable _             = False
--        editMph (Tm m@Mph{} _) = m       --WHY?? Stef, welke functie is de juiste?? TODO deze functie staat ook in ADL2Fspec.hs, maar is daar ANDERS(!)...
--        editMph e              = error("!Fatal (module Calc 63): cannot determine an editable declaration in a composite expression: "++show e)
        -- De functie spread verspreidt strings over kolommen met een breedte van n.
        -- Deze functie garandeert dat alle strings worden afgedrukt in de aangegeven volgorde.
        -- Hij probeert daarbij zo weinig mogelijk regels te gebruiken,
        -- en alleen de grens van n te overschrijden als een string zelf langer is dan n.
        spread :: Int -> String -> [String] -> [String]
        spread n str = f ""
         where f stored []       = [stored| not (null stored)]
               f [] (cs:css)     = f cs css
               f stored (cs:css) = if length stored > n then stored: f cs css else
                                   if length new <= n then f new css else stored: f cs css
                                   where new = stored++str++cs




   deriveProofs :: Options -> Fspc -> String
   deriveProofs flags fSpec
    = --"\nSignals for "++name fSpec++"\n--------------\n"++
      --proof (signals fSpec)++
      "\nTransformation of user specified rules into ECA rules for "++name fSpec++"\n--------------\n"++
      "\nFirst step: determine quads\n--------------\n"++
      intercalate "\n--------------\n"
      [   "relation m:               "++showADLcode fSpec m++
        "\nHorn clause:              "++showADLcode fSpec hc++
        "\nis derived from conjunct: "++showADLcode fSpec conj++
        "\nis derived from rule:     "++showADLcode fSpec r
      | Quad m ccrs<-qs, let r=cl_rule ccrs, (conj,hcs)<-cl_conjNF ccrs, hc<-hcs ]++
      "\n\nSecond step: collect Horn clauses\n--------------\n"++
      intercalate "\n--------------\n"
      [ "Horn clause "++showADLcode fSpec hc++"\nis derived from rule "++showADLcode fSpec r++
        case length ms of
         0 -> "no relations affect this clause"
         1 -> "\nIt can be called when relation " ++commaEng "and" [showADLcode fSpec m| m<-ms]++" is affected."
         _ -> "\nIt can be called when relations "++commaEng "and" [showADLcode fSpec m| m<-ms]++" are affected."
      | (ms,hc,r)<-
          [ (rd[ makeInline m|(m,_,_)<-cl],hc,r)
          | cl<-eqCl (\(_,_,hc)->hc) [(m,hc,r)|Quad m ccrs<-qs, let r=cl_rule ccrs, (_,hcs)<-cl_conjNF ccrs, hc<-hcs]
          , let (_,hc,r) = head cl
          ]
      ]++
      "\n\nThird step: determine ECA rules\n--------------\n"++
      intercalate "\n--------------\n"
      [ "ECA Rule "++showECA fSpec "\n  " ecarule{ecaAction=normPA (ecaAction ecarule)} ++
        if verboseP flags
        then let ds = [paDelta e| e@Do{}<-[ecaAction ecarule]] in
             "\nnormalized action\n "++showECA fSpec "\n  " (normPA (ecaAction ecarule))++
             (if null ds then "" else 
              "\ndelta expression\n "++intercalate "\n " [showADLcode fSpec d| d<-ds]++
              "\nderivation:\n "++showProof (showADLcode fSpec) (nfProof (showADLcode fSpec) (head ds))++
              "\ndisjunctly normalized delta expression\n "++showADLcode fSpec (disjNF (head ds)))
        else ""
      | (er,i) <- zip ecas [(1::Int)..], let ecarule = er i]++
      "\n\nFourth step: cascade blocking rules\n--------------\n"++
      intercalate "\n--------------\n"
      [ "ECA Rule "++showECA fSpec "\n  " er
      | er<-preEmpt ecaRs]++
      "\nFinal step: derivations --------------\n"++

      intercalate "\n--------------\n"
      [ intercalate "\n" ["   "++stmt++if null comment then "" else "\n<=> { "++comment++". }"
                   | (stmt,comment)<-cleanup (derivation rule)]
      | rule<-rules fSpec]++
      "\n--------------\n"++ -- TODO: make an ontological analysis, which explains the delete behaviour.
      "Ontological analysis: \n  "++
      intercalate "\n\n  " [name o++"("++intercalate ", " [name a++"["++(name.target.ctx) a++"]"|a<-attributes o]++"):\n  "
                     | o<-serviceS fSpec]++
      "\n--------------\n"++
      "Analyzing services: \n     "++
      intercalate "\n     " [testService fSpec o| o<-take 1 (serviceG fSpec)]++
      "\n--------------\n"
      where 
       qs = vquads fSpec  -- the quads that are derived for this fSpec specify horn clauses, meant to maintain rule r, to be called when morphism m is affected (m is in r).
          --   assembleECAs :: (Relation Concept->Bool) -> [Quad] -> [ECArule]
       ecaRs = assembleECAs (\_->True) qs  -- the raw (unnormalized) ECA rules.
       mphEqCls = eqCl fst4 [(m,shifts,conj,cl_rule ccrs)| Quad m ccrs<-qs, (conj,shifts)<-cl_conjNF ccrs]
       visible _ = True -- for computing totality, we take all quads into account.
       ecas
        = [ ECA (On ev m) delt act
          | mphEq <- mphEqCls
          , let (m,_,_,_) = head mphEq
          , let Tm delt _ = delta (sign m)
          , ev<-[Ins,Del]
          , let act = All [ Chc [ (if isTrue  clause'   then Nop else
                                   if isTrue  step      then Nop else
                                   if isFalse clause'   then Blk else
--                                 if not (visible m) then Blk else
                                   doCode visible ev toExpr viols)
                                   [(conj,causes)]  -- the motivation for these actions
                                | clause@(Fux fus) <- shifts
                                , let clause' = conjNF (subst (m, actSem Ins m (delta (sign m))) clause)
                                , let step    = conjNF (Fux[Cpx clause,clause'])
                                , let viols   = conjNF (notCp clause')
                                , let negs    = Fux [f| f<-fus, isNeg f]
                                , let poss    = Fux [f| f<-fus, isPos f]
                                , let frExpr  = if ev==Ins
                                                then conjNF negs
                                                else conjNF poss
                                , m `elem` map makeInline (mors frExpr)
                                , let toExpr = if ev==Ins
                                               then conjNF poss
                                               else conjNF (notCp negs)
                                ]
                                [(conj,causes)]  -- to supply motivations on runtime
                          | conjEq <- eqCl snd3 [(shifts,conj,rule)| (_,shifts,conj,rule)<-mphEq]
                          , let causes          = rd' nr (map thd3 conjEq)
                          , let (shifts,conj,_) = head conjEq
                          ]
                          [(conj,rd' nr [r|(_,_,_,r)<-cl])| cl<-eqCl thd4 mphEq, let (_,_,conj,_) = head cl]  -- to supply motivations on runtime
          ]
       fst4 (w,_,_,_) = w
       snd3 (_,y,_) = y
       thd3 (_,_,z) = z
       thd4 (_,_,z,_) = z
       derivation rule 
         = [ (showADL rule, if e'==e then "Translates directly to conjunctive normal form" else "Convert into conjunctive normal form\ne:  "++showHS flags "\n    " e++"\ne': "++showHS flags "\n    " e')
           , (showProof (showADLcode fSpec) prf  , "")
           ]++
           if rrsrt rule==Truth then [] else
              [ ("\nViolations are computed by (conjNF . Cp . normexpr) rule:\n     "++
                 (conjProof. Cpx . normExpr) rule++"\n"
                , "")
              , ("\nConjuncts:\n     "++
                 intercalate "\n     " (rd[ showADL conjunct
                                    | conjunct<-conjuncts rule])
                , "")
              , ("\n"++showClause fSpec (allClauses rule)
                , "")
              , ("\nAvailable code fragments on rule "++show (nr rule)++":\n     "++
                   intercalate "\n     " [showADL (reprAsRule rule r)++ " yields\n"++intercalate "\n\n"
                                   [ "event = "++show ev++" "++showADL m++"\n"++
                                     showADL r++"["++showADL m++":="++showADL (actSem ev m (delta (sign m)))++"] = r'\n"++
                                     "r'    = "++conjProof r'++"\n"++
                                     "viols = r'-"++disjProof (Cpx r')++"\n"++
                                     "violations, considering that the valuation of "++showADL m++" has just been changed to "++showADL (actSem ev m (delta (sign m)))++
                                     "            "++conjProof (Cpx r) ++"\n"++
                                     "reaction? evaluate r |- r' ("++(showADL.conjNF) (Fux[Cpx r,r'])++")"++
                                        conjProof (Fux[Cpx r,r'])++"\n"++
                                     "delta: r-/\\r' = "++conjProof (Fix[notCp r,r'])++
                                     "\nNow compute a reaction\n(isTrue.conjNF) (Fu[Cp r,r']) = "++show ((isTrue.conjNF) (Fux[Cpx r,r']))++"\n"++
                                     (if null (lambda ev (Tm m (-1)) r)
                                      then "lambda "++showADL m++" ("++showADL r++") = empty\n"
                                      else -- for debug purposes:
                                           -- "lambda "++show ev++" "++showADL m++" ("++showADL r++") = \n"++(intercalate "\n\n".map showPr.lambda ev (Tm m)) r++"\n"++
                                           -- "derivMono ("++showADL r++") "++show ev++" "++showADL m++"\n = "++({-intercalate "\n". map -}showPr.derivMono r ev) m++"\n"++
                                           -- "\nNow compute checkMono r ev m = \n"++show (checkMono r ev m)++"\n"++
                                           if (isTrue.conjNF) (Fux[Cpx r,r'])
                                           then "A reaction is not required, because  r |- r'. Proof:"++conjProof (Fux[Cpx r,r'])++"\n"
                                           else if checkMono r ev m
                                           then "A reaction is not required, because  r |- r'. Proof:"{-++(showPr.derivMono r ev) m-}++"NIET TYPECORRECT: (showPr.derivMono r ev) m"++"\n"  --WHY? Stef, gaarne herstellen...Deze fout vond ik nadat ik het type van showProof had opgegeven.
                                           else let Tm _ _ = delta (sign m) in
                                                "An appropriate reaction on this event is required."
                                           --     showECA fSpec "\n  " (ECA (On ev m) delt (doCode visible Ins r viols conj [rule]) 0)
                                     )
                                   | m<-rd [m'|x<-mors r, m'<-[x,flp x], inline m', not (isIdent m')] -- TODO: include proofs that allow: isIdent m'
                                   , ev<-[Ins,Del]
                                   , r'<-[subst (m, actSem ev m (delta (sign m))) r]
                        --        , viols<-[conjNF (Cp r')]
                                   , True ]  -- (isTrue.conjNF) (Fu[Cp r,r'])
                                  | r<-[hc| cs<-[allClauses rule], (_,hcs)<-cl_conjNF cs, hc<-hcs]
                                  ]
                , "")
              ] where e = normExpr rule
                      prf = cfProof (showADLcode fSpec) e
                      (e',_,_) = last prf
                      conjProof = showProof (showADLcode fSpec) . cfProof (showADLcode fSpec)
                      disjProof = showProof (showADLcode fSpec) . dfProof (showADLcode fSpec)
              --      showPr = showProof (showADLcode fSpec)  -- hoort bij de uitgecommentaarde code hierboven...
       cleanup :: [(String,String)] -> [(String,String)]
       cleanup [x] = [x]
       cleanup ((x,c):(x',c'):xs) = if x==x' then rest else (x,c): rest where rest = cleanup ((x',c'):xs)
       cleanup [] = []

-- Stel we voeren een actie a uit, die ��n van de volgende twee is:
--        {r} INS m INTO expr {r'}       ofwel
--        {r} DEL m FROM expr {r'}
-- Dan toetst checkMono of r|-r' waar is op grond van de afleiding uit derivMono.
-- Als dat waar is, betekent dat dat invariant r waar blijft wanneer actie a wordt uitgevoerd.
   checkMono :: Expression (Relation Concept)
             -> InsDel
             -> Relation Concept
             -> Bool
   checkMono expr ev m = simplify expr == simplify (antecedent conclusion) &&
                         simplify (subst (m, actSem ev m (delta (sign m))) expr) ==
                         simplify (consequent conclusion)
     where (conclusion,_,_) = last (derivMono expr ev m)


-- The function reprAsRule is used in show-functions, whenever an expression that represents a rule has to be shown to look like a rule.
   reprAsRule :: Rule (Relation Concept) -> Expression (Relation Concept) -> Rule (Relation Concept)
   reprAsRule r expr = r'{r_usr=False}
    where
     r' = case expr of
           (Fux []) -> error ("!Fatal (module Calc 256): erroneous call to function reprAsRule r ("++showADL expr++").")
           (Fux ts) -> if or [isNeg t|t<-ts] 
                      then r { rrsrt = Implication
                             , rrant = Fix [notCp t|t<-ts,isNeg t]
                             , rrcon = Fux [t|t<-ts,isPos t]
                             }
                      else r { rrsrt = Truth
                             , rrant = error ("!Fatal (module Calc 263): erroneous call to antecedent of r "++showADL expr)
                             , rrcon = Fux ts
                             }  
           _ -> case disjNF expr of
                      e@(Fux{}) -> reprAsRule r e
                      _    -> r { rrsrt = Truth
                                , rrant = (error ("!Fatal (module Calc 269): erroneous call to antecedent of r "++showADL expr))
                                , rrcon = expr
                                }
                        

   type Proof expr = [(expr,[String],String)]
   reversePrf :: Proof e -> Proof e
   reversePrf [] = []
   reversePrf [s] = [s]
   reversePrf ((r,cs,e'):prf@((r',_ ,_):_)) = init rp++[(r',cs,rev e'),(r,[],"")]
     where rp = reversePrf prf
           rev "==>" = "<=="
           rev "<==" = "==>"
           rev "-->" = "<--"
           rev "<--" = "-->"
           rev x = x

   showProof :: (expr->String) -> Proof expr -> String
   showProof shw [(expr,_,_)]        = "\n      "++shw expr++"\n"
   showProof shw ((expr,ss,equ):prf) = "\n      "++shw expr++
                                       "\n"++(if null ss then "   "++equ else if null equ then intercalate " " ss else "   "++equ++" { "++intercalate " and " ss++" }")++
                                       showProof shw prf
                                       --where e'= if null prf then "" else let (expr,_,_):_ = prf in showHS options "" expr 
   showProof _  []                   = ""

-- De volgende functie levert een afleiding, die bedoeld is om aan te tonen dat een preconditie r deelverzameling is van postconditie r'.
-- Hiermee hoop je aan te tonen dat een actie {expr} a {expr'} de invariant behoudt, ofwel dat expr|-expr', op basis van monotonie eigenschappen.
-- Derivmono geeft alleen de afleiding.
   derivMono :: Expression (Relation Concept) -> InsDel -> Relation Concept -> [(Rule (Relation Concept), [String], String)]
   derivMono expr -- preconditie van actie a
             tOp  -- de actie (Ins of Del)
             m'   -- het morfisme, zodat de actie bestaat uit INSERT m' INTO expr of DELETE m' FROM expr
    = f (head (lambda tOp (Tm m'(-1)) expr++[[]])) (start tOp)
    where
     f :: [(Expression (Relation Concept), [String], whatever)] -> (Expression (Relation Concept), Expression (Relation Concept)) -> [(Rule (Relation Concept), [String], String)]
     f [] (_,_) = []
     f [(e',_,_)] (neg',pos')
      = [(rule (subst (m',neg') e') (subst (m',pos') e'),[],"")]
     f ((e',["omkeren"],_): prf@((_,_,_):_)) (neg',pos')
      = (rule (subst (m',neg') e') (subst (m',pos') e'),["r |- s  <=>  s- |- r-"],"<=>"):
         f prf (pos',neg')
     f ((e1,_,_): prf@((e2,_,_):_)) (neg',pos')
      = (rule (subst (m',neg') e1) (subst (m',pos') e1),["Monotony of "++showOp e2],"==>"):
         f prf (neg',pos')
         
     start Ins  = (Tm m'(-1),Fux [Tm m'(-1),delta (sign m')])
     start Del  = (Fix [Tm m'(-1),Cpx (delta (sign m'))],Tm m'(-1))
     rule :: Expression (Relation Concept) -> Expression (Relation Concept) -> Rule (Relation Concept)
     rule neg' pos' | isTrue neg' = Ru { rrsrt = Truth
                                       , rrant = error ("!Fatal (module Calc 318): illegal reference to antecedent in rule ("++showADL neg'++") |- ("++showADL pos'++")")
                                       , rrfps = Nowhere
                                       , rrcon = pos'
                                       , rrxpl = [string2AutoExplain (defaultFlags {language=Dutch}) ("Waarom wordt deze regel hier aangemaakt? (In Calc.hs, regel 306)")]
                                              ++ [string2AutoExplain (defaultFlags {language=English}) ("Why is this rule created? (In Calc.hs, line 307)")]  --TODO Stef, gaarne de explanations aanvullen/verwijderen. Dank! Han.
                                       , rrtyp = sign neg' {- (neg `lub` pos) -}
                                       , rrtyp_proof = Nothing
                                       , rrdcl = Nothing
                                       , runum = 0
                                       , r_pat = ""
                                       , r_usr = False
                                       , r_sgl = error ("!Fatal (module Calc 329): illegal reference to r_sgl in rule ("++showADL neg'++") |- ("++showADL pos'++")")
                                       , srrel = error ("!Fatal (module Calc 330): illegal reference to srrel in rule ("++showADL neg'++") |- ("++showADL pos'++")")
                                       }
                    | otherwise   = Ru { rrsrt = Implication
                                       , rrant = neg'
                                       , rrfps = Nowhere
                                       , rrcon = pos'
                                       , rrxpl = [string2AutoExplain (defaultFlags {language=Dutch}) ("Waarom wordt deze regel hier aangemaakt? (In Calc.hs, regel 321)")]
                                              ++ [string2AutoExplain (defaultFlags {language=English}) ("Why is this rule created? (In Calc.hs, line 322)")]  --TODO Stef, gaarne de explanations aanvullen/verwijderen. Dank! Han.
                                       , rrtyp = sign neg' {- (neg `lub` pos) -}
                                       , rrtyp_proof = Nothing
                                       , rrdcl = Nothing
                                       , runum = 0
                                       , r_pat = ""
                                       , r_usr = False
                                       , r_sgl = error ("!Fatal (module Calc 344): illegal reference to r_sgl in rule ("++showADL neg'++") |- ("++showADL pos'++")")
                                       , srrel = error ("!Fatal (module Calc 345): illegal reference to srrel in rule ("++showADL neg'++") |- ("++showADL pos'++")")
                                       }
     showOp expr' = case expr' of
                     F{}      -> ";"
                     Fdx{}     -> "!"
                     Fux{}     -> "\\/"
                     Fix{}     -> "/\\"
                     Cpx{}     -> "-"
                     K0x{}     -> "*"
                     K1x{}     -> "+"
                     Tm mph _   -> if inline mph then "" else "~"
                     Tc{}     -> error("!Fatal (module Calc 356): call to showOp (Tc x) in module Calc.hs")

   positiveIn :: Expression (Relation Concept) -> Relation Concept -> Maybe Bool
   positiveIn expr m | and result           = Just True   -- all are True, so an insert in m means an insert in expr
                     | and (map not result) = Just False  -- all are False, so a delete from m means an insert in expr
                     | otherwise            = Nothing     -- inconclusive
    where
     result = f expr
     f (F fus)  = concat (map f fus)
     f (Fdx fus) = concat (map f fus)
     f (Fix fus) = concat (map f fus)
     f (Fux fus) = concat (map f fus)
     f (Tm mph _) = [ True | makeInline mph==makeInline m ]
     f (Cpx e)   = [ not b| b<- f e]
     f (K0x e)   = f e
     f (K1x e)   = f e
     f (Tc e)   = f e

   lambda :: InsDel -> Expression (Relation Concept) -> Expression (Relation Concept) -> [Proof (Expression (Relation Concept))]
   lambda tOp' e' expr' = [reversePrf[(e'',text,op)
                          | (e'',_,text,op)<-prf]
                          | prf<-lam tOp' e' expr' ]
    where
     lam :: InsDel -> Expression (Relation Concept) -> Expression (Relation Concept) ->
            [[(Expression (Relation Concept),Expression (Relation Concept) -> Expression (Relation Concept),[String],String)]]
     lam tOp e3 expr =
            case expr of
                (F [f])   -> lam tOp e3 f
                (F fs) | e3==expr             -> [[(e3,(\x->x),[],"")]]
                       | and [isNeg f|f<-fs] -> [(expr,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg expr) expr],"==")
                                                 :prf
                                                | prf<-lam tOp e3 (deMrg expr)
                                                ] -- isNeg is nog niet helemaal correct.
                       | or[null p|p<-fPrfs (F fs) ] -> []
                       | otherwise           -> [(expr,(\_->expr),   [derivtext tOp "mono" (first (lc (F fs))) expr],"<--"): (lc (F fs))]        
                (Fux [f])  -> lam tOp e3 f
                (Fux fs)| e3==expr             -> [[(e3,(\x->x),[],"")]]
                       | length (const' (Fux fs))>0     -> [(expr,(\_->expr),   [derivtext tOp "mono" (inter' expr) expr],"<--")
                                                          :prf
                                                         | prf<-lam tOp e3 (inter' expr)
                                                         ]
                       | and [isNeg f|f<-fs] -> [(expr,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg expr) expr],"==") :prf| prf<-lam tOp e3 (deMrg expr)]
                       | or[null p|p<-fPrfs (Fux fs)] -> []
                       | otherwise           -> [(expr,(\_->expr),      [derivtext tOp "mono" (first (lc (Fux fs))) expr],"<--") : (lc (Fux fs))]
                (Fdx [f])  -> lam tOp e3 f
                (Fdx fs)| e3==expr             -> [[(e3,(\x->x),[],"")]]
                       | and [isNeg f|f<-fs] -> [(expr,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg expr) expr],"==") :prf| prf<-lam tOp e3 (deMrg expr)] -- isNeg is nog niet helemaal correct.
                       | or[null p|p<-fPrfs (Fdx fs)] -> []
                       | otherwise           -> [(expr,(\_->expr),[derivtext tOp "mono" (first (lc (Fdx fs))) expr],"<--"): (lc (Fdx fs))]
                (Fix [f])  -> lam tOp e3 f
                (Fix fs)| e3==expr             -> [[(e3,(\x->x),[],"")]]
                       | length (const' (Fix fs))>0     -> [(expr,(\_->expr),      [derivtext tOp "mono" (inter' expr) expr],"<--")
                                                          :prf
                                                         | prf<-lam tOp e3 (inter' expr)
                                                         ]
                       | and [isNeg f|f<-fs] -> [(expr,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg expr) expr],"==") :prf| prf<-lam tOp e3 (deMrg expr)]
                       | or[null p|p<-fPrfs (Fix fs)] -> []
                       | otherwise           -> [(expr,(\_->expr),      [derivtext tOp "mono" (first (lc expr)) expr],"<--") : (lc expr)]
                (K0x x)   -> [(expr,(\x'->K0x x'),[derivtext tOp "mono" x expr],"<--") :prf   | prf<-lam tOp e3 x]
                (K1x x)   -> [(expr,(\x'->K1x x'),[derivtext tOp "mono" x expr],"<--") :prf   | prf<-lam tOp e3 x]
                (Cpx x)   -> [(expr,(\x'->Cpx x'),["omkeren"],"<--") :prf| prf<-lam (inv tOp) e3 x]
                (Tc x)   -> lam tOp e3 x
                (Tm _ _)  ->  [[(e3,(\x->x),[],"")]]
  
             where
               deMrg expr'' = case expr'' of
                                (F fs)  -> notCp (Fdx [notCp f| f<-fs])
                                (Fux fs) -> notCp (Fix [notCp f| f<-fs])
                                (Fdx fs) -> notCp (F  [notCp f| f<-fs])
                                (Fix fs) -> notCp (Fux [notCp f| f<-fs])
                                Tm{} -> error ("!Fatal (module Calc 426). deMrg Tm{} is not defined.Consult your dealer!")
                                Tc{} -> error ("!Fatal (module Calc 427). deMrg Tc{} is not defined.Consult your dealer!")
                                K0x{} -> error ("!Fatal (module Calc 428). deMrg K0{} is not defined.Consult your dealer!")
                                K1x{} -> error ("!Fatal (module Calc 429). deMrg K1{} is not defined.Consult your dealer!")
                                Cpx{} -> error ("!Fatal (module Calc 430). deMrg Cp{} is not defined.Consult your dealer!")
               fPrfs expr'' = case expr'' of
                                (F fs)  -> xs fs
                                (Fux fs) -> xs fs
                                (Fdx fs) -> xs fs
                                (Fix fs) -> xs fs
                                Tm{} -> error ("!Fatal (module Calc 436). fPrfs Tm{} is not defined.Consult your dealer!")
                                Tc{} -> error ("!Fatal (module Calc 437). fPrfs Tc{} is not defined.Consult your dealer!")
                                K0x{} -> error ("!Fatal (module Calc 438). fPrfs K0{} is not defined.Consult your dealer!")
                                K1x{} -> error ("!Fatal (module Calc 439). fPrfs K1{} is not defined.Consult your dealer!")
                                Cpx{} -> error ("!Fatal (module Calc 440). fPrfs Cp{} is not defined.Consult your dealer!")
                       where
                          xs fs = [lam tOp e3 f|f<-fs, isVar f e3]                       
               lc expr'' = longstcomn (vars expr'')++concat (drop (length (rc expr'')-1) (sort' length (rc expr'')))
               rc expr'' = remainders (vars expr'') (vars expr'')
               vars expr'' = map head (fPrfs expr'')
               const' (Fux fs) = [f|f<-fs, isConst f e3]
               const' (Fix fs) = [f|f<-fs, isConst f e3]
               const' expr'' = error ("!Fatal (module Calc 448). 'const'("++ show expr''++")' is not defined.Consult your dealer!")
               inter' (Fux fs) = Fux [f|f<-fs, isVar f e3]
               inter' (Fix fs) = Fix [f|f<-fs, isVar f e3]
               inter' expr'' = error ("!Fatal (module Calc 451). 'inter'("++ show expr''++")' is not defined.Consult your dealer!")
 --      lam tOp e f       = []

  -- longstcomn determines the longest prefix common to all xs in xss.
     longstcomn :: (Eq a) => [[(a, b, c, d)]] -> [(a, b, c, d)]
     longstcomn xss | or [null xs| xs<-xss]      = []
                    | length (eqCl first xss)==1 = head [head prf| prf<-xss]: longstcomn [tail prf| prf<-xss]
                    | otherwise                  = []
    -- remainders determines the remainders.
     remainders :: (Eq a) => [[(a, b, c, d)]] -> [[(a, b, c, d)]] -> [[(a, b, c, d)]]
     remainders _ xss | or [null xs| xs<-xss]      = xss
                      | length (eqCl first xss)==1 = remainders xss [tail prf| prf<-xss]
                      | otherwise                  = xss
     isConst :: (ConceptStructure a c, ConceptStructure b c) => a->b->Bool
     isConst e f = null (mors e `isc` mors f)
     isVar :: (ConceptStructure a c, ConceptStructure b c) => a->b->Bool
     isVar e f   = not (isConst e f)
     derivtext :: InsDel -> String -> Expression (Relation Concept) -> Expression (Relation Concept) -> String
     derivtext tOp "omkeren" e'' expr = sh tOp++showADL e''++" means "++sh (inv tOp)++showADL expr++"."
     derivtext tOp "mono"    e'' expr = "("++showADL e''++"->"++showADL expr++") is monotonous, so "++sh tOp++showADL e''++" means "++sh tOp++showADL expr++"."
     derivtext _ str _ _ = str
     sh :: InsDel -> String
     sh Ins  = "insert into "
     sh Del  = "delete from "
     inv :: InsDel -> InsDel
     inv Ins = Del
     inv Del = Ins
     first :: [(a,b,c,d)] -> a
     first ((e'',_,_,_):_) = e''
     first _ = error "!Fatal (module Calc 480): wrong pattern in first"
