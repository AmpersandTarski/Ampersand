{-# OPTIONS_GHC -Wall #-}
module Calc (   deriveProofs
              , computeOrder
              , conjuncts
              , simplify
              , lClause
              , rClause
              , makeRule
              , doClause
              , filterPA
              , simplPAclause
              , assembleClauses
              , informalRule ) 
  where 

   import Collection         (Collection (uni,isc,rd,(>-)))
   import Auxiliaries        (sort',eqCl,commaEng,elem')
   import Adl
   import Data.Fspec
   import FspecDef           (Fspc,vrules,chain,serviceS,showL)
   import Adl.ECArule        (InsDel(..),ECArule(..),Event(..),PAclause(..))
   import ShowADL            (showADL,showADLcode)
   import ShowHS             (showHS)
   import ShowSQL            (showSQL)
   import CommonClasses      (ABoolAlg(..))
   import ComputeRule        (ComputeRule(..),triggers,conjuncts,allClauses,hornCs)
   import NormalForms        (conjNF,disjNF,nfProof,nfPr,simplify)
--   multiplicityAnalysis context 
--    = let fnm = "MULT"++name context in
--      putStr ("\n--------------\n"++
--              "Multiplicity Analysis:\n")>>
--      writeFile (fnm++".dot") (dotGraph context "anyStyle" fnm (multDerivations context)) >>
--      putStr ("\nMultiplicity propagation graph "++fnm++".dot written... ") >>
--      processDotgraphFile fnm >>
--      writeFile ("CF"++fnm++".dot") (dotGraph context "anyStyle" fnm codeFragments) >>
--      putStr ("\nCode graph CF"++fnm++".dot written... ") >>
--      processDotgraphFile ("CF"++fnm)
--      where
--       codeFragments :: [ECArule]
--       codeFragments = zipWith f [ eca | rule<-declaredRules context, clause<-conjuncts rule, eca<-doClause (simplify clause) ] [0..]
--                       where f eca i = eca i

   deriveProofs :: Fspc -> String
   deriveProofs fSpec
    = --"\nSignals for "++name fSpec++"\n--------------\n"++
      --proof (signals fSpec)++
      "\nRules for "++name fSpec++"\n--------------\n"++
      proof [r| r<-vrules fSpec]++
      "\n--------------\n"++
      "Summarizing all compute rules: \n  "++
      chain "\n  " [ informalRule {-(declarations frExpr)-} hc | rule<-vrules fSpec, hc<-triggers rule]++
      "\n--------------\n"++ -- TODO: make an ontological analysis, which explains the delete behaviour.
      "Ontological analysis: \n  "++
      chain "\n\n  " [name o++"("++chain ", " [name a++"["++(name.target.ctx) a++"]"|a<-attributes o]++"):\n  "
                     | o<-serviceS fSpec]++
      "\n--------------\n"++
      "Triggers from services: \n     "++
      chain "\n     " [name c++"("++chain ", "[name a++"["++(name.target.ctx) a++"]"|a<-attributes o]++"):"++
                       condNull ("\n  Rules for Insert transactions\n    ") (chain "\n    ") informalRule 
                        (computeOrder hcs "INSERT INTO" (Isn c c:map makeDeclaration (mors o)))++          -- taken from phpCodeEntCreate
                       condNull ("\n  Rules for Update transactions\n    ") (chain "\n    ") informalRule 
                        (computeOrder hcs "UPDATE"              (map makeDeclaration (mors o)))++          -- taken from phpCodeEntUpdate
                       condNull ("\n  Rules for Delete transactions\n    ") (chain "\n    ") informalRule 
                        (computeOrder hcs "DELETE FROM" (Isn c c:map makeDeclaration (mors o)))++          -- taken from phpCodeEntDelete
                       "\n"
                      | o<-serviceS fSpec, c<-[concept o]]++
      "\n--------------\n"++
      "Analyzing services: \n     "++
      chain "\n     " [analyseService fSpec o| o<-serviceG fSpec]++
      "\n--------------\n"
      where
       hcs = [hc| rule<-vrules fSpec, hc<-triggers rule ]
       condNull header fold f xs = if null xs then "" else header++fold (map f xs)


-- assembleClauses :: [Expression] -> [Expression]
-- Deze functie neemt verschillende clauses samen met het oog op het genereren van code.
-- Hierdoor kunnen grotere brokken procesalgebra worden gegenereerd.
-- assembleClauses levert altijd een disjunctieve normaalvorm op.
   assembleClauses :: [Expression] -> [Expression]
   assembleClauses clauses
    = rd ([ disjNF (Fu[antecedent (head cs), Fi (map consequent cs)]) | cs<-antcEq, length cs>1] ++
          [ disjNF (Fu[Fi (map antecedent cs), consequent (head cs)]) | cs<-cseqEq, length cs>1]) ++
      [ c| [c]<-antcEq, [c] `elem` cseqEq]
      where
       antcEq = eqCl (antecedent) clauses
       cseqEq = eqCl (consequent) clauses
       antecedent (Fu fs) = Fu (sort' (showHS "") [f| f<-fs, isNeg f])
       antecedent e = antecedent (disjNF e)
       consequent (Fu fs) = Fu (sort' (showHS "") [f| f<-fs, isPos f])
       consequent e = consequent (disjNF e)

-- analyseService :: Fspc -> ObjectDef -> String
-- Deze functie is bedoeld om te bedenken hoe services moeten worden afgeleid uit een vers vertaalde ObjectDef.
-- Nadat deze goed werkt kunnen de bewijsgenerator en de codegenerator worden gemaakt.
   analyseService :: Fspc -> ObjectDef -> String
   analyseService fSpec obj
    = "\nService "++ objnm obj++"("++chain ", " [showADL m++":"++name (target m)| m<-rels]++")\n"++
      " - The parameters correspond to editable fields in a UI-service.\n   "++
      showADLcode fSpec obj++"\n"++
      "\n Various events in this service trigger the following behaviour:\n"++
      chain "\n" [ showSQL ( ECA (On ev m) action 0)
                 | m<-rd (map makeInline rels)
                 , ev<-[Ins,Del]
                 , action<-[ All [ doCode visible Ins r viols
                                 | r<-assembleClauses [clause| rule<-invariants     
                                                             , conjunct<-conjuncts rule     
                                                             , makeInline m `elem` map makeInline (mors conjunct)     
                                                             , clause<-allClauses conjunct    
                                                      ]     
                                 , (not.null.lambda ev (Tm m)) r     
                                 , not (checkMono r ev m)     
                                 , r'<-[subst (Tm m,actSem ev (Tm m) (delta (sign m))) r]     
                                 , (not.isTrue.conjNF) (Fu[Cp r,r']) -- the system must act to restore invariance     
                                 , viols<-[conjNF (Cp r')]     
                                 , True ]     -- (isTrue.conjNF) (Fu[Cp r,r'])     
                           ]
                 , action/=All [], action/=Choice []
                 ]++"\n"++
      " - Invariants:\n   "++chain "\n   " [showADLcode fSpec rule| rule<-invariants]++"\n"++
      " - Clauses:\n   "++
      (chain "\n   " . map (showADLcode fSpec) . assembleClauses)
        [clause| rule<-invariants, conjunct<-conjuncts rule, clause<-allClauses conjunct]++"\n"
    where
        visible = rd (map makeInline rels++map (mIs.target) rels)
        rels = rd (recur obj)
         where recur obj = [editMph (objctx o)| o<-objats obj, editable (objctx o)]++[m| o<-objats obj, m<-recur o]
        invariants = [rule| rule<-vrules fSpec, not (null (map makeInline (mors rule) `isc` map makeInline (rels)))]
        fields     = map fld (objats obj)
        fld obj
         = Att { fld_name     = objnm obj
               , fld_expr     = objctx obj
               , fld_mph      = if editable (objctx obj)
                                then editMph (objctx obj)
                                else error("Fatal (module Calc): cannot edit a composite expression: "++show (objctx obj))
               , fld_editable = editable (objctx obj)                          -- can this field be changed by the user of this service?
               , fld_list     = not (Uni `elem` multiplicities (objctx obj))   -- can there be multiple values in this field?
               , fld_must     = Tot `elem` multiplicities (objctx obj)         -- is this field obligatory?
               , fld_new      = True                                           -- can new elements be filled in? (if no, only existing elements can be selected)
               , fld_fields   = map fld (objats obj)
               }
        editable (Tm m@Mph{}) = True
        editable _            = False
        editMph (Tm m@Mph{}) = m
        editMph e            = error("Fatal (module Calc): cannot determine an editable declaration in a composite expression: "++show e)
     

   proof :: [Rule] -> String
   proof rs
    = chain "\n--------------\n"
      [ (if isSignal rule then "SIGNAL\n" else "")++
        chain "\n" ["   "++stmt++if null comment then "" else "\n<=> { "++comment++". }"
                   | (stmt,comment)<-cleanup (derivation rule)]
      | rule<-rs]
      where 
        derivation rule = 
         case rule of
          Ru{} -> [ (showADL rule          , if null prf then "Translates directly to conjunctive normal form" else "Convert into conjunctive normal form")
                  , (showProof prf         , "")
                  ]++
                  if (rrsrt rule)==Truth then [] else
                     [ ("\nViolations are computed by (conjNF . Cp . normexpr) rule:\n     "++
                        (showProof.cfProof. Cp . normExpr) rule++"\n"
                       , "")
                     , ("\nConjuncts:\n     "++
                        chain "\n     " (rd[showADL conjunct
                                           |conjunct<-conjuncts rule])
                       , "")
                     , ("\nClauses:\n     "++
                        chain "\n     " (rd[showADL (makeRule rule clause)
                                           |conjunct<-conjuncts rule, clause<-allClauses conjunct])
                       , "")
                     , ("\nniClauses:\n     "++
                        chain "\n     " (map (showADL) (rd [clause|conjunct<-conjuncts rule, clause<-niClauses conjunct]))
                       , "")
                     , ( "\nAvailable Triggers on rule "++show (nr rule)++":\n     "++
                         chain "\n     " [showADL (makeRule rule clause)++ " yields"++concat
                                          [ "\n        "++informalRule {-(declarations conjunct)-} hc
                                          | hc<-hornCs rule clause
                                          ]
                                         |conjunct<-conjuncts rule, clause<-allClauses conjunct]++
                         "\nAvailable code fragments on rule "++show (nr rule)++":\n     "++
                         chain "\n     " [showADL (makeRule rule r)++ " yields\n"++chain "\n\n"
                                          [ "event = "++show ev++" "++showADL m'++"\n"++
                                            showADL r++"["++showADL m'++":="++showADL (actSem ev (Tm m') (delta (sign m')))++"] = r'\n"++
                                            "r'    = "++(showProof.cfProof) r'++"\n"++
                                            "viols = r'-"++(showProof.cfProof) (Cp r')++"\n"++
                                            "violations, considering that the valuation of "++showADL m'++" has just been changed to "++showADL (actSem ev (Tm m') (delta (sign m')))++
                                            "            "++(showProof.cfProof) (Cp r)++"\n"++
                                      --      "reaction? evaluate r -: r' ("++(showADL.conjNF) (Fu[Cp r,r'])++")"++
                                      --         (showProof.cfProof) (Fu[Cp r,r'])++"\n"++
                                      --      "delta: r-/\\r' = "++(showProof.cfProof) (Fi[notCp r,r'])++
                                      --      "\nNow compute a reaction\n(isTrue.conjNF) (Fu[Cp r,r']) = "++show ((isTrue.conjNF) (Fu[Cp r,r']))++"\n"++
                                            (if null (lambda ev (Tm m') r)
                                             then "lambda "++showADL m'++" ("++showADL r++") = empty\n"
                                             else {- for debug purposes:
                                                     "lambda "++show ev++" "++showADL m'++" ("++showADL r++") = \n"++(chain "\n\n".map showProof.lambda ev (Tm m')) r++"\n"++
                                                     "derivMono ("++showADL r++") "++show ev++" "++showADL m'++"\n = "++({-chain "\n". map -}showProof.derivMono r ev) m'++"\n"++
                                                     "\nNow compute checkMono r ev m' = \n"++show (checkMono r ev m')++"\n"++ -}
                                                  if (isTrue.conjNF) (Fu[Cp r,r'])
                                                  then "A reaction is not required, because  r -: r'. Proof:"++(showProof.cfProof) (Fu[Cp r,r'])++"\n"
                                                  else if checkMono r ev m'
                                                  then "A reaction is not required, because  r -: r'. Proof:"{-++(showProof.derivMono r ev) m'-}++"NIET TYPECORRECT: (showProof.derivMono r ev) m'"++"\n"  --WAAROM? Stef, gaarne herstellen...Deze fout vond ik nadat ik het type van showProof had opgegeven.
                                                  else "An appropriate reaction on this event is\n"++showSQL (ECA (On ev m') (doCode (rd (map makeInline (mors r))) Ins r viols) 0)
                                            )
                                          | m'<-rd [m'|x<-mors r, m'<-[x,flp x], inline m', not (isIdent m')] -- TODO: include proofs that allow: isIdent m'
                                          , ev<-[Ins,Del]
                                          , r'<-[subst (Tm m',actSem ev (Tm m') (delta (sign m'))) r]
                                          , nr'<-[conjNF r']
                                          , viols<-[conjNF (Cp r')]
                                          , True ]  -- (isTrue.conjNF) (Fu[Cp r,r'])
                                         |conjunct<-conjuncts rule, r<-allClauses conjunct]
                       , "")
                     , ("\nGenerated Triggers for: "++showADL rule++" (rule "++show (nr rule)++")\n     "++
                         chain "\n     " ([ informalRule {-(declarations frExpr)-} hc | hc<-triggers rule]), "")
                     ] where prf = nfProof (normExpr rule)
                             cfProof expr = nfPr True False (simplify expr)
          Sg{} ->    [ (showADL rule          , if null prf then "Translates directly to conjunctive normal form" else "Convert into conjunctive normal form")
                     , (showProof prf      , "")
                     ]++
                     [ ("\nConjuncts:\n     "++
                        chain "\n     " (rd[showADL conjunct
                                           |conjunct<-conjuncts (srsig rule)])
                       , "")
                     , ("\nClauses:\n     "++
                        chain "\n     " (rd[showADL (makeRule (srsig rule) clause)
                                           |conjunct<-conjuncts (srsig rule), clause<-allClauses conjunct])
                       , "")
                     ] where prf = nfProof (normExpr (srsig rule))
          Gc{} -> undefined
          Fr{} -> undefined 
 --         dummy = Fu [Cp (F [Tm (Mph "q" Nowhere [] (C "A" (==) [],C "C" (==) []) True dec) ])
 --                    , F [Tm (Mph "r" Nowhere [] (C "A" (==) [],C "B" (==) []) True dec)
 --                        ,Tm (Mph "s" Nowhere [] (C "B" (==) [],C "C" (==) []) True dec)
 --                        ]
 --                    ]
 --                 where dec = error ("(Module Calc) Declaration error")
        cleanup :: [(String,String)] -> [(String,String)]
        cleanup [x] = [x]
        cleanup ((x,c):(x',c'):xs) = if x==x' then rest else (x,c): rest where rest = cleanup ((x',c'):xs)
        cleanup [] = []
--        showcomp rule clause -- laat de functionaliteit van 'computing' in de functie 'triggers' zien (nuttig voor testen).
--         = [ "\ntoExpr                  : "++showADL toExpr++
--             "\nr_cpu rule              : "++show (r_cpu rule)++
--             "\ntoExpr `elem` r_cpu rule: "++show ((map name.morlist) toExpr `elem` map (map name.morlist) (r_cpu rule))
--           | (CR (_, _, _, toExpr, _, _))<-hornCs rule clause ]

   checkMono expr ev m' = simplify expr == simplify (antecedent conclusion) &&
                          simplify (subst (Tm m',actSem ev (Tm m') (delta (sign m'))) expr) ==
                          simplify (consequent conclusion)
     where (conclusion,_,_) = last (derivMono expr ev m')

-- doClause produces the ECA rules for a given expression, restricted to the ones that change a predefined set of relations (the editable morphisms)
   doClause :: Morphisms -> Expression -> [Int->ECArule]
   doClause editableMorphisms r@(Fu fs)
    = {- if error("Diagnostic: \n"++
               showADL r++"\n"++
               showHS "   " r++"\n"++
               "mors r = "++show (mors r)++"\n"++
               "subst r =\n   "++chain "\n   " ([ "m="++showADL m++"\nsubst ("++showADL m++","++showADL (Fu[m,delta (sign m)])++") ("++showADL r++") = "++showADL (subst (m,Fu[m,delta (sign m)]) r)++" <=> (r') "++showADL r'++"\n phi: "++showADL phi++" <=> "++(showADL.conjNF) phi++"\n (Fu[Cp r,r']): "++showADL (Fu[Cp r,r'])++" <=> "++disjNF (Fu[Cp r,r'])
                                                | m <-rd [Tm m|x<-mors r, m<-[x,flp x], inline m]
                                                , r'<-[(conjNF.subst (m,Fu[m,delta (sign m)])) r]
                                                , phi<-[Fi[Cp r',r]]
                                                ])++"\n"
              ) then res else -} res
    where
     res = [ ECA (On ev m') (doCode editableMorphisms Ins r phi)
           | m'<-rd [makeInline m|m<-editableMorphisms]
           , ev<-[Ins,Del]
           , case ev of
              Ins -> let antc = Fu [f| f<-fs, isNeg f] in
                     disjNF antc == Tm m' || disjNF antc == flp (Tm m')
              Del -> let cons = Fu [f| f<-fs, isPos f] in
                     disjNF cons == Tm m' || disjNF cons == flp (Tm m')
           , r'<-[(subst (Tm m',actSem ev (Tm m') (delta (sign m'))) ) r]
           , phi<-[Fi [Cp r,r']]
        {- , (not.isTrue) r' -} ]
   doClause editableMorphisms arg = error ("Fatal (module Calc): function doClause must be called in disjunctive normal form. However, it was called with "++showADL arg)

   actSem :: InsDel -> Expression -> Expression -> Expression
   actSem Ins e' delta' = Fu[e',delta']
   actSem Del e' delta' = Fi[e',Cp delta']

--   makeTm :: String -> Expression
--   makeTm name' = Tm (makeMph (Sgn { decnm   = name'
--                                   , desrc   = cptAnything
--                                   , detgt   = cptAnything
--                                   , decprps = []
--                                   , decprL  = ""
--                                   , decprM  = ""
--                                   , decprR  = ""
--                                   , decpopu = []
--                                   , decexpl = ""
--                                   , decfpos = Nowhere
--                                   , decid   = 0
--                                   , deciss  = True
--                                   }))
   delta :: (Concept, Concept) -> Expression
   delta (a,b)  = Tm (makeMph (Sgn { decnm   = "Delta"
                                   , desrc   = a
                                   , detgt   = b
                                   , decprps = []
                                   , decprL  = ""
                                   , decprM  = ""
                                   , decprR  = ""
                                   , decpopu = []
                                   , decexpl = ""
                                   , decfpos = Nowhere
                                   , decid   = 0
                                   , deciss  = True
                                   }))

-- Retain only clauses that change relations that are editable within this service
   filterPA :: [Morphism]  -- the list of editable relations
            -> PAclause    -- the clause
            -> PAclause    -- the same clause, but with only those actions that change relations that are editable within this service
   filterPA ms = simplifyPA (\e->null ((rd.map makeInline.mors.paTo) e >- ms))

   simplPAclause :: PAclause -> PAclause
   simplPAclause = simplifyPA (\e->True)

   simplifyPA :: (PAclause->Bool) -> PAclause -> PAclause
   simplifyPA leafcondition c -- = let sc = fPA c in head ([x| e@(All [x])<-[sc]]++[x| e@(Choice [x])<-[sc]]++[sc])
    = case fPA c of
       e@(All [])       -> NoOp
       e@(All [x])      -> x
       e@(Choice [])    -> NoOp
       e@(Choice [x])   -> x
       e                -> e
    where
      fPA (Choice [])  = NoOp
      fPA (Choice [c]) = fPA c
      fPA (Choice cs)  = Choice [e| c<-cs
                                  , e<-case fPA c of
                                        Choice xs  -> xs
                                        All [x]    -> [x]
                                        e@(All xs) -> [e]
                                        e@Do{}     -> [e| leafcondition e]
                                        NoOp       -> []
                                  , e/=NoOp
                                ]
      fPA (All [])     = NoOp
      fPA (All [c])    = fPA c
      fPA (All cs)     = All    [e|c<-cs, sc<-[fPA c], sc/=NoOp
                                  , e<-[x| e@(Choice [x])<-[sc]]++
                                       [e| e@(Choice xs) <-[sc], length xs>1]++
                                       [x| e@(All xs)    <-[sc], x<-xs]++
                                       [e| e@Do{}<-[sc], leafcondition e]
                                ]
      fPA  c           = c


   -- | de functie doCode beschrijft de voornaamste mogelijkheden om een expressie delta' te verwerken in expr (met tOp'==Ins of tOp==Del)
   doCode :: Morphisms    -- the morphisms that may be changed
          -> InsDel
          -> Expression
          -> Expression
          -> PAclause
   doCode editables tOp' expr1 delta1 = simplPAclause (doCod delta1 tOp' expr1)
    where
      doCod deltaX tOp exprX =
        case (tOp, exprX) of
          (_ ,  Fu [])   -> error ("!Fail (Module Calc): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (Fu [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showHS "" expr1++") ("++showADL delta1++").")
          (_ ,  Fi [])   -> error ("!Fail (Module Calc): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (Fi [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showHS "" expr1++") ("++showADL delta1++").")
          (_ ,  F [])    -> error ("!Fail (Module Calc): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (F [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showHS "" expr1++") ("++showADL delta1++").")
          (_ ,  Fd [])   -> error ("!Fail (Module Calc): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (Fd [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showHS "" expr1++") ("++showADL delta1++").")
          (_ ,  Fu [t])  -> doCod deltaX tOp t
          (_ ,  Fi [t])  -> doCod deltaX tOp t
          (_ ,  F [t])   -> doCod deltaX tOp t
          (_ ,  Fd [t])  -> doCod deltaX tOp t
          (Ins, Cp x)    -> doCod deltaX Del x
          (Ins, Fu fs)   -> Choice [ doCod deltaX Ins f | f<-fs, not (f==expr1 && Ins/=tOp') ] -- the filter prevents self compensating PA-clauses.
          (Ins, Fi fs)   -> All    [ doCod deltaX Ins f | f<-fs ]
          (Ins, F ts)    -> All [ c | (l,r)<-chop ts 
                                    , one1 <- [Tm (Mph "One" Nowhere [] (source (F r),target (F l)) True             -- Stef, WAAROM? gebruik je hier niet Mp1{}?
                                                      (Sgn "One" (target (F l)) (source (F r)) [Sym,Asy,Trn] "" "" "" [] "" Nowhere 0 True))]
                                    , c<-[ Do Ins (F l) (simplify (Fi [F[delta3,v (target deltaX,target one1)],F[v(source deltaX,source one1),one1]]))
                                         , Do Ins (F r) (simplify (Fi [F[one1,v(target one1,target deltaX)],F[v (source one1,source deltaX),delta3]]))
                                         ]
                                ]
          (Del, Cp x)    -> Choice [ doCod deltaX Ins x
                                   , doCod (F [deltaX, v (target x,source x)]) Del (Tm (mIs (source x)))
                                   , doCod (F [v (source x,target x), flp deltaX]) Del (Tm (mIs (target x)))
                                   ]
          (Del, Fu fs)   -> All    [ doCod deltaX Del f | f<-fs ]
          (Del, Fi fs)   -> Choice [ doCod deltaX Del f | f<-fs ]
          (Del, F ts)    -> Choice [ All [ Do Del (F l) (Fd [ F[deltaX, Cp (flp (F r))]])
                                         , Do Del (F r) (Fd [ F[Cp (flp (F l)), deltaX]])
                                         ]
                                   | (l,r)<-chop ts ]
          (_  , Fd ts)   -> doCod deltaX tOp (Cp (F (map Cp ts)))
          (_  , K0 x)    -> doCod (deltaK0 deltaX tOp x) tOp x
          (_  , K1 x)    -> doCod (deltaK1 deltaX tOp x) tOp x
          (_  , Tm m)    -> if m `elem` editables then Do tOp exprX deltaX else NoOp
          (_ , _)        -> error ("!Fail (Module Calc): Non-exhaustive patterns in the recursive call doCod ("++showADL deltaX++") "++show tOp++" ("++showHS "" exprX++"),\n"++
                                   "within function doCode "++show tOp'++" ("++showHS "" exprX++") ("++showADL delta1++").")
      delta3 = Fi[delta1,Cp expr1]

                                             


   chop :: [t] -> [([t], [t])]
   chop []     = []
   chop [_]    = []
   chop (x:xs) = ([x],xs): [(x:l, r)| (l,r)<-chop xs]

   deltaK0 :: t -> InsDel -> t1 -> t
   deltaK0 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta -: x*)
   deltaK0 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x* leeg is)
   deltaK1 :: t -> InsDel -> t1 -> t
   deltaK1 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta -: x+)
   deltaK1 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x+ leeg is)

--   dos (Choice ds) = concat (map dos ds)
--   dos (All ds) = concat (map dos ds)
--   dos x = [x]

--   fragmentProof clause (ECA (On fOp frm) (Do dOp toExpr phi))
--    = (clause',[motivate fOp (delta (sign frm)) frm],""):
--      (clause'',[motivate dOp ((conjNF. Cp) clause) toExpr],""):
--      (definiens,["Now prove that precondition -: postcondition = V"],""):
--      nfp'
--      where
--        nfp' = -- if (not.isTrue.last) [c|(c,m,e)<-nfp]
--               -- then error("Clause: "++showADL clause++"\nThe derivation of \"precondition -: postcondition\" should yield V,\n but instead:"++showProof nfp) else
--               nfp
--        nfp  = nfProof definiens
--        clause'  = subst (Tm frm, actSem fOp (Tm frm) (delta (sign frm))) clause
--        clause'' = subst (toExpr, actSem dOp toExpr phi) clause'
--        definiens = Fu [Cp clause, clause'']
--        motivate Ins delta' m' = "INSERT "++showADL delta'++" INTO ("++showADL m'++");"
--        motivate Del delta' m' = "DELETE "++showADL delta'++" FROM ("++showADL m'++");"
--

--   ilClauses :: Expression -> [Expression]
--   ilClauses cl'  = [ hc | hc@(Fu fus)<-allClauses cl'
--                         , and [idsOnly e'| (Cp e')<-fus]
--                   ]
--   irClauses :: Expression -> [Expression]
--   irClauses cl'  = [ hc | hc@(Fu fus)<-allClauses cl'
--                         , and [idsOnly t| t@(F _)<-fus]
--                   ]
--   iClauses :: Expression -> [Expression]
--   iClauses  cl'  = [ hc | hc@(Fu fus)<-allClauses cl'
--                        , and [idsOnly e'| (Cp e')<-fus] || and [idsOnly t| t@(F _)<-fus]
--                   ]
   niClauses :: Expression -> [Expression]
   niClauses cl'  = [ hc | hc@(Fu fus)<-allClauses cl'
                        , not (and [idsOnly e'| (Cp e')<-fus] || and [idsOnly t| t@(F _)<-fus])
                   ]
   lClause :: Expression -> Expression
   lClause cl'    = head (niClauses cl'++[cl'])
   rClause :: Expression -> Expression
   rClause cl'    = last ([cl']++niClauses cl')







--   multDerivations :: Language pat => pat -> Declarations
--   multDerivations context
--    = rd ([d| d<-declarations context, not (null ([Tot,Sur] `isc` multiplicities d))]++
--          [ d
--          | rule<-declaredRules context, conjunct<-conjuncts rule, clause<-ilClauses conjunct
--          , Fi fs<-[conjNF clause], Fu fus<-fs
--          , if and [isIdent c| c<-fus, isNeg c] then True else error (" in module Calc, multDerivations "++showADL clause)
--          , F ts<-fus -- t<-fus, isPos t
--          , if not (null ts) then True else error (" in module Calc, multDerivations: null ts ")
--          , d<-(map (addProp Tot) . declarations . head) ts++(map (addProp Sur) . declarations . last) ts
--          ])
--       where addProp p decl = decl {decprps = rd (p:decprps decl)}  
   closRule ::(Eq a, Show a) => [[a]] -> [[a]]
   closRule [] = error ("Module Calc: empty argument in closRule.")
   closRule xxs
     = if or [null x| x<-xxs] then error ("Module Calc: empty list in closRule "++show xxs) else
       f xxs (rd (map head xxs) `isc` rd (map last xxs))
       where
        f q (x:xs) = f (q `uni` [[head ls,last rs]| ls<-q,last ls==x ,rs<-q,head rs==x]) xs
        f q []     = q
















   computeOrder :: [ComputeRule] -> String -> [Declaration] ->[ComputeRule]
   computeOrder hcs         -- hcs is the entire set of compute rules
                onOperation -- "INSERT INTO", "DELETE FROM", or "UPDATE".
                ss          -- the set of declarations at the beginning of the compute-chain.
    = if True then paths else -- False maken voor diagnose...
      error ( "(diagnostic in module Calc)\ncomputeOrder hcs ["++chain "," (map showADL ss)++"] = \n"++
               "(hcs: \n  "++chain "\n  " [informalRule {-(declarations frExpr)-} hc++"\n  "++chain "\n  " (map showADL (declarations (crfrm hc)))
                                          | hc<-hcs]++"\n\nsel: "++
               "\n  "++chain "\n  " [informalRule {-(declarations frExpr)-} hc| hc<-sel]++"\n)\n\npaths:"++
               "\n  "++chain "\n  " [informalRule {-(declarations frExpr)-} hc| hc<-paths] )
      where
       before :: ComputeRule -> ComputeRule -> Bool
       hc `before` hc'
        = (not.null) [(fOp',r)|(fOp',r)<-(crOps hc'), (crbOp hc)==fOp',r `elem` declarations (crto hc)]
--       (CR (_, _, bOp, toExpr, _, _)) `before` (CR (fOps', _, _, _, _, _))
--        -- = not (null (declarations toExpr `isc` declarations frExpr')) && bOp==fOp'
--        = (not.null) [(fOp',r) |(fOp',r)<-fOps', bOp==fOp', r `elem` declarations toExpr]
       sel
        = combineTriggers  -- WAAROM? Stef, kan je uitleggen waarom onderstaande 3 verzamelingen disjuct zijn? (anders kan een kandidaatregel meerdere keren worden opgenomen, hetgeen me niet de bedoeling lijkt...)
            ([ hc{crOps = fOps'} |hc <-hcs     -- men neme een kandidaatregel, hc
             , isTrue (crfrm hc)               -- waarvan de antecedent persé waar is
             , fOps'<-[[ on |on@(_,s)<-crOps hc
                       , elem' typeEq s ss]
                      ]
             , not (null fOps')                -- en die wordt aangetrapt
             , if onOperation `elem` ["INSERT INTO", "DELETE FROM"] then onOperation `elem` map fst (crOps hc) else True
             ]++
             [ if True {- debug: nr rule/=12 -} then hc{crOps = fOps'} else
               error ( "(diagnostic in module Calc)" ++
                       "\nfrExpr (showADL)    = "++showADL (crfrm hc) ++
                       "\nfrExpr (showHS)     = "++showHS "" (crfrm hc) ++
                       "\ndeclarations frExpr = "++showHS "" (declarations (crfrm hc)) ++
                       "\nss                  = "++showHS "" ss  ++
                       "\nnot (null (ss `isc` declarations frExpr)) = "++show(not (null (ss `isc` declarations (crfrm hc))))
                     )
             | hc <-hcs                        -- men neme een kandidaatregel, hc
             , not (isTrue (crfrm hc))         -- die een antecedent heeft die niet persé waar is
             , fOps'<-[[ on |on@(_,s)<-crOps hc
                       , s `elem ` ss]
                      ]
             , not (null fOps')                -- en die wordt aangetrapt
             , if onOperation `elem` ["INSERT INTO", "DELETE FROM"] then onOperation `elem` map fst (crOps hc) else True
             , not (null (ss `isc` declarations (crfrm hc))) 
             ]++
             [ hc{crOps = fOps'} |hc <-hcs     -- men neme een kandidaatregel, hc
             , isIdent (crfrm hc)              -- met de identiteit als antecedent
             , fOps'<-[[ on |on@(_,s)<-crOps hc
                       , s `elem ` ss]
                      ]
             , not (null fOps')                -- en die wordt aangetrapt
             , if onOperation `elem` ["INSERT INTO", "DELETE FROM"] then onOperation `elem` map fst (crOps hc) else True
             ])         


       typeEq a b | isIdent a && isIdent b = source a `order` source b
                  | otherwise              = a==b
    -- define tuples, which are paths of length 1
       rrs = [[x,x']| x<-clossel, x'<-clossel, x `before` x']
    -- taking the transitive closure means to compute paths of arbitrary length. rs is such a path
       paths    = sort isin clossel
       isin a b = if null rrs then False else [a,b] `elem` closRule rrs
       sort _ [] = []
       sort f (x:xs) = sort f [e'|e'<-xs, f e' x] ++ [x] ++ sort f [e'|e'<-xs, not (f e' x)]
       clossel = f [] sel [hc| hc<-hcs, not (identifies hc `elem` map identifies sel)]
        where f sel' [] _ = sel'
              f sel' new rs = f (sel'++new) [r|r<-rs, or [n `before` r| n<-new], not (r `elem` new)] [r|r<-rs, and [not (n `before` r)| n<-new]]

   identifies :: ComputeRule -> (String,Expression,Expression)
   identifies hc = ((crbOp hc),(crto hc),(crfrm hc))
   combineTriggers :: [ComputeRule] -> [ComputeRule]
   combineTriggers hcs -- Triggers die toevallig semantisch identiek zijn worden op één hoop geveegd.
    = [ if length cl'==1 then head cl' else
        headHc { crOps = rd [fOp|hc<-cl', fOp<-crOps hc]
               , crule = Ru { rrsrt = Implication
                            , rrant = crfrm headHc
                            , rrfps = Nowhere
                            , rrcon = crto headHc
                            , r_cpu = []
                            , rrxpl = chain "; " (rd[explain (crule hc)++" ("++show (pos (crule hc))++")"|hc<-cl', (not.null.explain) (crule hc)])
                            , rrtyp = sign (crfrm headHc) `lub` sign (crto headHc)
                            , runum = 0
                            , r_pat = ""
                            }
               }
      |cl'<-eqCl identifies hcs, headHc <-[head cl']  ]

   informalRule :: ComputeRule -> String
   informalRule hc --(CR (fOps, e, bOp, toExpr, frExpr, rule))
    = "ON "++commaEng "OR" [fOp++" "++if isSgn r then name r else showADL r|(fOp,r)<-crOps hc] ++" DO "++crbOp hc++" "++showADL (crto hc)++" SELECTFROM "++sh (crfrm hc)
      where sh x = if isTrue x then "V["++(chain ",".rd.map name) [source x,target x]++"]" else showADL x


--   recalc :: Context -> Context
--   recalc context = update (foldr subst' (declarations context) calcrules) context
--    where
----     calcrules = computeOrder [hc| rule<-rules context, hc@ (CR (fOps, e, "INSERT INTO", toExpr, frExpr, rule))<-triggers rule, "INSERT INTO" `elem` [fOp|(fOp,r)<-fOps] ] "UPDATE" (declarations context)
--     calcrules = computeOrder [hc | rule <- rules context
--                                  , hc <- triggers rule
--                                  , crbOp hc == "INSERT INTO"
--                                  , "INSERT INTO" `elem` [fOp|(fOp,_)<-crOps hc]
--                              ] "UPDATE" (declarations context)    
--     subst' :: ComputeRule -> [Declaration] -> [Declaration]
--     subst' hc ss
--      = [ if [s]==declarations (crto hc) then insert (calc (crfrm hc) ss) s else s
--        | s<-ss ]
--     insert :: Pairs -> Declaration -> Declaration
--     insert pairs decl 
--         = case decl of
--              Sgn{} -> decl {decpopu = pairs `uni` (decpopu decl) }
--              _     -> decl 
--                       




--   norm1Rule :: Rule -> Expression
--   norm1Rule = conjNF.normExpr
   makeRule :: Rule -> Expression -> Rule
   makeRule r expr =
     case expr of
           (Fu []) -> error ("(module Calc:) erroneous call to function makeRule r ("++showADL expr++").")
           (Fu ts) -> if or [isNeg t|t<-ts] 
                      then r { rrsrt = Implication
                             , rrant = Fi [notCp t|t<-ts,isNeg t]
                             , rrcon = Fu [t|t<-ts,isPos t]
                             , r_cpu = []}
                      else r { rrsrt = Truth
                             , rrant = error ("(Module Calc: ) erroneous call to antecedent of r "++showADL expr)
                             , rrcon = error ("Module Calc: Loop found!! (hier stond eerst \"r\").")    -- WAAROM?  Stef, hier zat volgens mij de bug. Kan jij dit herstellen?
                             , r_cpu = []
                             }  
--   Was ooit: 
--   makeRule r (Fu ts)
--    | or [isNeg t|t<-ts] = r { rrsrt = Implication
--                             , rrant = Fi [notCp t|t<-ts,isNeg t]
--                             , rrcon = Fu [t|t<-ts,isPos t]
--                             , r_cpu = []}
--    | otherwise          = Ru Truth (error ("(Module Calc: ) erroneous call to antecedent of r "++showADL (Fu ts))) (rrfps r) (Fu ts) [] (rrxpl r) (rrtyp r) (runum r) (r_pat r)
           _ -> case disjNF expr of
                      Fu{} -> makeRule r (disjNF expr)
                      _    -> r { rrsrt = Truth
                                , rrant = (error ("(Module Calc: ) erroneous call to antecedent of r "++showADL expr))
                                , rrcon = expr
                                , r_cpu = []}
                        




   type Proof = [(Expression,[String],String)]
   reversePrf :: Proof -> Proof
   reversePrf [] = []
   reversePrf [s] = [s]
   reversePrf ((r,cs,e'):prf@((r',_ ,_):_)) = init rp++[(r',cs,rev e'),(r,[],"")]
     where rp = reversePrf prf
           rev "==>" = "<=="
           rev "<==" = "==>"
           rev "-->" = "<--"
           rev "<--" = "-->"
           rev x = x

   showProof :: Proof -> String
   showProof [(expr,_,_)]     = "\n      "++showADL expr++"\n"
   showProof ((expr,ss,equ):prf) = "\n      "++showADL expr++
                                "\n"++(if null ss then "\n   "++equ else if null equ then chain " " ss else "   "++equ++" { "++chain "; " ss++" }")++
                                showProof prf
                                --where e'= if null prf then "" else let (expr,_,_):_ = prf in showHS "" expr 
   showProof []               = ""















   derivMono :: Expression -> InsDel -> Morphism -> [(Rule, [String], String)]
   derivMono expr tOp m' = f (head (lambda tOp (Tm m') expr++[[]])) (start tOp)
    where
     f:: [(Expression, [String], whatever)] -> (Expression, Expression) -> [(Rule, [String], String)]   -- WAAROM?? Stef, graag enige uitleg van wat hier gebeurt...
     f [] (_,_) = []
     f [(e',_,_)] (neg',pos')
      = [(rule (subst (Tm m',neg') e') (subst (Tm m',pos') e'),[],"")]
     f ((e',["omkeren"],_): prf@((_,_,_):_)) (neg',pos')
      = (rule (subst (Tm m',neg') e') (subst (Tm m',pos') e'),["r -: s  <=>  s- -: r-"],"<=>"):
         f prf (pos',neg')
     f ((e1,_,_): prf@((e2,_,_):_)) (neg',pos')
      = (rule (subst (Tm m',neg') e1) (subst (Tm m',pos') e1),["Monotony of "++showOp e2],"==>"):
         f prf (neg',pos')
         
     start Ins  = (Tm m',Fu [Tm m',delta (sign m')])
     start Del  = (Fi [Tm m',Cp (delta (sign m'))],Tm m')
     rule :: Expression -> Expression -> Rule
     rule neg' pos' | isTrue neg' = Ru { rrsrt = Truth
                                       , rrant = error ("(Module Calc:) illegal reference to antecedent in rule ("++showADL neg'++") ("++showADL pos'++")")
                                       , rrfps = Nowhere
                                       , rrcon = pos'
                                       , r_cpu = []
                                       , rrxpl = ""
                                       , rrtyp = sign neg' {- (neg `lub` pos) -}
                                       , runum = 0
                                       , r_pat = ""}
                    | otherwise   = Ru { rrsrt = Implication
                                       , rrant = neg'
                                       , rrfps = Nowhere
                                       , rrcon = pos'
                                       , r_cpu = []
                                       , rrxpl = ""
                                       , rrtyp = sign neg' {- (neg `lub` pos) -}
                                       , runum = 0
                                       , r_pat = ""}
     showOp expr' = case expr' of
                     F{}      -> ";"
                     Fd{}     -> "!"
                     Fu{}     -> "\\/"
                     Fi{}     -> "/\\"
                     Cp{}     -> "-"
                     K0{}     -> "*"
                     K1{}     -> "+"
                     Tm mph   -> if inline mph then "" else "~"
                     Tc{}     -> error("Fatal: call to showOp (Tc x) in module Calc.hs")



   lambda :: InsDel -> Expression -> Expression -> [Proof]
   lambda tOp' e' expr' = [reversePrf[(e'',text,op)
                          | (e'',_,text,op)<-prf]
                          | prf<-lam tOp' e' expr' ]
    where
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
              (Fu [f])  -> lam tOp e3 f
              (Fu fs)| e3==expr             -> [[(e3,(\x->x),[],"")]]
                     | length (const' (Fu fs))>0     -> [(expr,(\_->expr),   [derivtext tOp "mono" (inter' expr) expr],"<--")
                                                        :prf
                                                       | prf<-lam tOp e3 (inter' expr)
                                                       ]
                     | and [isNeg f|f<-fs] -> [(expr,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg expr) expr],"==") :prf| prf<-lam tOp e3 (deMrg expr)]
                     | or[null p|p<-fPrfs (Fu fs)] -> []
                     | otherwise           -> [(expr,(\_->expr),      [derivtext tOp "mono" (first (lc (Fu fs))) expr],"<--") : (lc (Fu fs))]
              (Fd [f])  -> lam tOp e3 f
              (Fd fs)| e3==expr             -> [[(e3,(\x->x),[],"")]]
                     | and [isNeg f|f<-fs] -> [(expr,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg expr) expr],"==") :prf| prf<-lam tOp e3 (deMrg expr)] -- isNeg is nog niet helemaal correct.
                     | or[null p|p<-fPrfs (Fd fs)] -> []
                     | otherwise           -> [(expr,(\_->expr),[derivtext tOp "mono" (first (lc (Fd fs))) expr],"<--"): (lc (Fd fs))]
              (Fi [f])  -> lam tOp e3 f
              (Fi fs)| e3==expr             -> [[(e3,(\x->x),[],"")]]
                     | length (const' (Fi fs))>0     -> [(expr,(\_->expr),      [derivtext tOp "mono" (inter' expr) expr],"<--")
                                                        :prf
                                                       | prf<-lam tOp e3 (inter' expr)
                                                       ]
                     | and [isNeg f|f<-fs] -> [(expr,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg expr) expr],"==") :prf| prf<-lam tOp e3 (deMrg expr)]
                     | or[null p|p<-fPrfs (Fi fs)] -> []
                     | otherwise           -> [(expr,(\_->expr),      [derivtext tOp "mono" (first (lc expr)) expr],"<--") : (lc expr)]
              (K0 x)   -> [(expr,(\x'->K0 x'),[derivtext tOp "mono" x expr],"<--") :prf   | prf<-lam tOp e3 x]
              (K1 x)   -> [(expr,(\x'->K1 x'),[derivtext tOp "mono" x expr],"<--") :prf   | prf<-lam tOp e3 x]
              (Cp x)   -> [(expr,(\x'->Cp x'),["omkeren"],"<--") :prf| prf<-lam (inv tOp) e3 x]
              (Tc x)   -> lam tOp e3 x
--              (Tm _)   -> error("WAAROM?? Stef, na het opschonen van deze code zag ik ook nog een stukje uitgecomentariseerde code, die hier eventueel op zijn plaats zou zijn. Dit laat ik graag aan jou deskundigheid over....")
              (Tm _)  ->  [[(e3,(\x->x),[],"")]]

           where
             deMrg expr'' = case expr'' of
                              (F fs)  -> notCp (Fd [notCp f| f<-fs])
                              (Fu fs) -> notCp (Fi [notCp f| f<-fs])
                              (Fd fs) -> notCp (F  [notCp f| f<-fs])
                              (Fi fs) -> notCp (Fu [notCp f| f<-fs])
                              Tm{} -> undefined
                              Tc{} -> undefined
                              K0{} -> undefined
                              K1{} -> undefined
                              Cp{} -> undefined
             fPrfs expr'' = case expr'' of
                              (F fs)  -> xs fs
                              (Fu fs) -> xs fs
                              (Fd fs) -> xs fs
                              (Fi fs) -> xs fs
                              Tm{} -> undefined
                              Tc{} -> undefined
                              K0{} -> undefined
                              K1{} -> undefined
                              Cp{} -> undefined
                     where
                        xs fs = [lam tOp e3 f|f<-fs, isVar f e3]                       
             lc expr'' = longstcomn (vars expr'')++concat (drop (length (rc expr'')-1) (sort' length (rc expr'')))
             rc expr'' = remainders (vars expr'') (vars expr'')
             vars expr'' = map head (fPrfs expr'')
             const' (Fu fs) = [f|f<-fs, isConst f e3]
             const' (Fi fs) = [f|f<-fs, isConst f e3]
             const' _ = undefined
             inter' (Fu fs) = Fu [f|f<-fs, isVar f e3]
             inter' (Fi fs) = Fi [f|f<-fs, isVar f e3]
             inter' _ = undefined

             


 --      lam tOp e f       = []
  -- longstcomn determines the longest prefix common to all xs in xss.
       longstcomn xss | or [null xs| xs<-xss]      = []
                      | length (eqCl first xss)==1 = head [head prf| prf<-xss]: longstcomn [tail prf| prf<-xss]
                      | otherwise                  = []
  -- remainders determines the remainders.
       remainders _ xss | or [null xs| xs<-xss]      = xss
                        | length (eqCl first xss)==1 = remainders xss [tail prf| prf<-xss]
                        | otherwise                  = xss
       isConst e'' f = null (declarations e'' `isc` declarations f)
       isVar e'' f   = not (isConst e'' f)
       derivtext tOp "omkeren" e'' expr = sh tOp++showADL e''++" means "++sh (inv tOp)++showADL expr++"."
       derivtext tOp "mono"    e'' expr = "("++showADL e''++"->"++showADL expr++") is monotonous, so "++sh tOp++showADL e''++" means "++sh tOp++showADL expr++"."
       derivtext _ str _ _ = str
       sh Ins  = "insert into "
       sh Del  = "delete from "
       inv Ins = Del
       inv Del = Ins
       first ((e'',_,_,_):_) = e''
       first _ = error "Module Calc: wrong pattern in first"

