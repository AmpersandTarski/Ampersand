{-# OPTIONS_GHC -Wall #-}
module Calc (   deriveProofs
              , computeOrder
              , lClause
              , rClause
              , makeRule
              , doClause
              , simplPAclause
              , informalRule ) 
  where 

   import Collection         (Collection (uni,isc,rd))
   import Auxiliaries        (sort',eqCl,commaEng,elem')
   import Adl
   import FspecDef           (Fspc,vrules,chain,serviceS,ECArule(..),Event(..),InsDel(..),PAclause(..),showL)
   import ShowADL            (showADL)
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
--       codeFragments = [ eca | rule<-declaredRules context, clause<-conjuncts rule, eca<-doClause (simplify clause) ]

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
      "\n--------------\n"
      where
       hcs = [hc| rule<-vrules fSpec, hc<-triggers rule ]
       condNull header fold f xs = if null xs then "" else header++fold (map f xs)




--   delMors :: Context -> Concept -> [Morphism]
--   delMors context e = [m| m<-rd (ms++[ m| m<-rd (ms'++map flp ms'), sur (multiplicities m)]), source m == e]
--    where ms' = mors (rules context)
--          ms = rd [ if null (morlist term) then error "Module Calc: head error 1" else
--                    head (morlist term)
--                  | rule<-rules context
--                  , conjunct<-conjuncts rule
--                  , Fu terms<-ilClauses conjunct
--                  , F ts<-terms, ts'<-[[t| t<-ts, length (morlist t)==1]]
--                  , if null ts' then error(" module Calc "++show (nr rule)++" ("++show (pos rule)++") in "++showADL rule++"\nterms = "++showHS "" (Fu terms)++"\nts = "++showHS "" (F ts)) else True
--                  , term<-[flp (head ts'), last ts']--, term `elem` r_cpu rule
--                  ]

--   delFrs :: Context -> Concept -> [Rule]
--   delFrs context e
--        = [ makeRule rule (Fu terms)
--          | rule<-rules context
--          , conjunct<-conjuncts rule
--          , clause@(Fu terms)<-allClauses conjunct
--          , and [idsOnly t| Cp t<-terms], source clause==e]

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
                                                  else "The correct reaction on this event is\n"++showSQL (ECA (On ev m') (doCode viols Ins r'))++"\n"++
                                                       "\ndoClause :\n"++showSQL (ECA (On ev m') (doCode (Fi [Cp r,nr']) Ins r))
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
                             checkMono expr ev m' = simplify expr == simplify (antecedent conclusion) &&
                                                    simplify (subst (Tm m',actSem ev (Tm m') (delta (sign m'))) expr) == simplify (consequent conclusion)
                               where (conclusion,_,_) = last (derivMono expr ev m')
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

   doClause :: Expression -> [ECArule]
   doClause r
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
     res = -- [ ECA (On Del (V [] (sign to))) (doCode (Cp r) Ins to) | null [t| t<-ts, isNeg t],  to<-[t| t<-ts, isPos t]]++
           [ ECA (On ev m') (doCode phi Ins r)
           | m'<-rd [m'|x<-mors r, m'<-[x,flp x], inline m']
           , ev<-[Ins,Del]
           , r'<-[(conjNF.subst (Tm m',actSem ev (Tm m') (delta (sign m'))) ) r]
           , phi<-[(Fi [Cp r,r'])]
        {- , (not.isTrue) r' -} ]

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

-- TODO: De volgende code voor simplPAclause stinkt. Opzoeken: procesalgebra herschrijfregels.

   simplPAclause :: PAclause -> PAclause
   simplPAclause (Choice [c])             = simplPAclause c
   simplPAclause (Choice [])              = Choice []
-- ?   simplPAclause (Choice (All []:cs))  = simplPAclause (Choice cs) -- TODO: wat moet er in dit geval gebeuren?
   simplPAclause (Choice (Choice cs:cs')) = simplPAclause (Choice (cs++cs'))
   simplPAclause (Choice (c:cs))          = f (simplPAclause (Choice cs))
                                            where f (Choice [])  = Choice []
                                                  f (Choice cs') = Choice (simplPAclause c:cs')
                                                  f _            = error ("Module Calc: something funny in simplPAclause (Choice ("++showL(map showSQL (c:cs))++"))")
   simplPAclause (All [c])                = simplPAclause c
   simplPAclause (All [])                 = All []
   simplPAclause (All (Choice []:_ ))     = Choice []
   simplPAclause (All (All cs:cs'))       = simplPAclause (All (cs++cs'))
   simplPAclause (All (c:cs))             = f (simplPAclause (All cs))
                                            where f (Choice []) = Choice []
                                                  f (All cs')   = All (c:cs')
                                                  f _           = error ("Module Calc: something funny in simplPAclause (All ("++showL(map showSQL (c:cs))++"))")
   simplPAclause c                        = c

   -- | de functie doCode beschrijft de voornaamste mogelijkheden om een expressie delta' te verwerken in expr (met tOp'==Ins of tOp==Del)
   doCode :: Expression -> InsDel -> Expression -> PAclause
   doCode delta1 tOp' expr1 = {- simplPAclause -} (doCod delta1 tOp' expr1)
    where
      doCod deltaX tOp exprX =
        case (tOp, exprX) of
          (_ ,  Fu [])   -> error ("!Fail (Module Calc): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (Fu [])++",\n"++
                                     "within function doCode ("++showADL delta1++") "++show tOp'++" ("++showHS "" (F [])++").")
          (_ ,  Fi [])   -> error ("!Fail (Module Calc): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (Fi [])++",\n"++
                                     "within function doCode ("++showADL delta1++") "++show tOp'++" ("++showHS "" (F [])++").")
          (_ ,  F [])    -> error ("!Fail (Module Calc): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (F [])++",\n"++
                                     "within function doCode ("++showADL delta1++") "++show tOp'++" ("++showHS "" (F [])++").")
          (_ ,  F [t])   -> doCod deltaX Ins t
          (Ins, Cp x)    -> doCod deltaX Del x
          (Ins, Fu fs)   -> Choice [ doCod deltaX Ins f | f<-fs ]
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
          (_  , Tm _)    -> Do tOp exprX deltaX
--          (_  , Tm m')    -> if name m'=="Delta" then Choice [] else
--                                if name m'=="One"
--                                then New (source m')
--                                else if tOp==Ins 
--                                     then Do Ins (Tm m') (f Ins (conjNF (Fi [Cp (Tm m'),deltaX])))
--                                     else Do Del (Tm m') (f Del (conjNF (Fi [    Tm m' ,deltaX])))
--                                where -- De functie f versimpelt de uitdrukking (en dus de SELECT expressie), maar nu moet wel INSERT IGNORE gebruikt worden (DELETE is al IGNORE)
--                                  f Ins (Fi fs) = simplify (Fi [f'| f'<-fs, not (isNeg f' && notCp f'==Tm m')])
--                                  f Del (Fi fs) = simplify (Fi [f'| f'<-fs, not (isPos f' &&      (f'==Tm m' || (isIdent f' && isIdent m')) )])
--                                  f _ e' = e'
          (_ , Tc _)     -> error ("!Fail (Module Calc): Non-exhaustive patterns in the recursive call doCod ("++showADL deltaX++") "++show tOp++" ("++showHS "" exprX++"),\n"++
                                   "within function doCode ("++showADL delta1++") "++show tOp'++" ("++showHS "" exprX++").")
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

             




--       lam tOp e (F [f])   = lam tOp e f
--       lam tOp e f@(F fs)  | e==f                = [[(e,(\x->x),[],"")]]
--                           | and [isNeg f|f<-fs] = [(f,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg f) f],"==") :prf| prf<-lam tOp e (deMrg f)] -- isNeg is nog niet helemaal correct.
--                           | or[null p|p<-fPrfs] = []
--                           | otherwise           = [(f,(\x->f),[derivtext tOp "mono" (first lc) f],"<--"): lc]
--                             where fPrfs = [lam tOp e f|f<-fs, isVar f e]
--                                   vars  = map head fPrfs -- wordt niet aangeroepen als er een lege afleiding in fPrfs zit
--                                   deMrg (F fs) = notCp (Fd [notCp f| f<-fs])
--                                   lc    = longstcomn vars++concat (drop (length rc-1) (sort' length rc))
--                                   rc    = remainders vars vars
--       lam tOp e  (Fu [f]) = lam tOp e f
--       lam tOp e f@(Fu fs) | e==f                = [[(e,(\x->x),[],"")]]
--                           | length const>0      = [(f,(\x->f),      [derivtext tOp "mono" inter f],"<--") :prf     | prf<-lam tOp e inter]
--                           | and [isNeg f|f<-fs] = [(f,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg f) f],"==") :prf| prf<-lam tOp e (deMrg f)]
--                           | or[null p|p<-fPrfs] = []
--                           | otherwise           = [(f,(\x->f),      [derivtext tOp "mono" (first lc) f],"<--") : lc]
--                             where fPrfs = [lam tOp e f|f<-fs, isVar f e]
--                                   vars  = map head fPrfs
--                                   const = [f|f<-fs, isConst f e]
--                                   inter = Fu [f|f<-fs, isVar f e]
--                                   deMrg (Fu fs) = notCp (Fi [notCp f| f<-fs])
--                                   lc    = longstcomn vars++concat (drop (length rc-1) (sort' length rc))
--                                   rc    = remainders vars vars
--
--
--
--
--       lam tOp e (Fd[f])   = lam tOp e f
--       lam tOp e f@(Fd fs) | e==f                = [[(e,(\x->x),[],"")]]
--                           | and [isNeg f|f<-fs] = [(f,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg f) f],"==") :prf| prf<-lam tOp e (deMrg f)] -- isNeg is nog niet helemaal correct.
--                           | or[null p|p<-fPrfs] = []
--                           | otherwise           = [(f,(\x->f),[derivtext tOp "mono" (first lc) f],"<--"): lc]
--                             where fPrfs = [lam tOp e f|f<-fs, isVar f e]
--
--
--
--                                   vars  = map head fPrfs -- wordt niet aangeroepen als er een lege afleiding in fPrfs zit
--                                   deMrg (Fd fs) = notCp (F [notCp f| f<-fs])
--                                   lc    = longstcomn vars++concat (drop (length rc-1) (sort' length rc))
--                                   rc    = remainders vars vars
--       lam tOp e  (Fi [f]) = lam tOp e f
--       lam tOp e f@(Fi fs) | e==f                = [[(e,(\x->x),[],"")]]
--                           | length const>0      = [(f,(\x->f),      [derivtext tOp "mono" inter f],"<--") :prf     | prf<-lam tOp e inter]
--                           | and [isNeg f|f<-fs] = [(f,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg f) f],"==") :prf| prf<-lam tOp e (deMrg f)]
--                           | or[null p|p<-fPrfs] = []
--                           | otherwise           = [(f,(\x->f),      [derivtext tOp "mono" (first lc) f],"<--") : lc]
--                             where fPrfs = [lam tOp e f|f<-fs, isVar f e]
--                                   vars  = map head fPrfs
--                                   const = [f|f<-fs, isConst f e]
--                                   inter = Fi [f|f<-fs, isVar f e]
--                                   deMrg (Fi fs) = notCp (Fu [notCp f| f<-fs])
--                                   lc    = longstcomn vars++concat (drop (length rc-1) (sort' length rc))
--                                   rc    = remainders vars vars
--       lam tOp e f@(K0 x)  = [(f,(\x->K0 x),[derivtext tOp "mono" x f],"<--") :prf   | prf<-lam tOp e x]
--       lam tOp e f@(K1 x)  = [(f,(\x->K1 x),[derivtext tOp "mono" x f],"<--") :prf   | prf<-lam tOp e x]
--       lam tOp e f@(Cp x)  = [(f,(\x->Cp x),["omkeren"],"<--") :prf| prf<-lam (inv tOp) e x]
--       lam tOp e   (Tc x)  = lam tOp e x
--
--
--
--
--














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



--   normR r@(Ru Truth antc pos cons cpu expla sgn nr pn) = Ru Truth err pos (conjNF cons) (r_cpu r) expla sgn nr (r_pat r)
--    where err = error ("Module Calc: erroneous reference to antc of rule "++showADL r)
--   normR r@(Ru c antc pos cons cpu expla sgn nr pn)   = Ru c (disjNF antc) pos (conjNF cons) (r_cpu r) expla sgn nr (r_pat r)
--   normR r@(Sg p rule expla sgn nr pn signal)         = Sg p (normR rule) expla sgn nr (r_pat r) signal
--   normR r@(Gc pos m expr cpu sgn nr pn)              = error "Calc.lhs: normR op glue regel. Gc pos m (conjNF expr) (r_cpu r) sgn nr (r_pat r)"
--



--   unVee (Fi es) | or [ x==Cp y | x<-es, y<-es ] = Cp (v (sign (Fi es)))
--                 | or [ isFalse x | x<-es ]      = Cp (v (sign (Fi es)))
--                 | otherwise                     = Fi [e| e<-es, not (isTrue e)]
--   unVee (Fu es) | or [ x==Cp y | x<-es, y<-es ] = v (sign (Fi es))
--                 | or [ isTrue x | x<-es ]       = v (sign (Fi es))
--                 | otherwise                     = Fu [e| e<-es, not (isFalse e)]
--   unVee x = x





--   cl::Expression -> Expression
--   cl (Fu [F [t]]) = t
--   cl (Fu rs)
--    | length (eqCl headEq [ts| F ts<-rs])>1 = cr (Fu rs)
--    | otherwise = Fu ([ F ([prefix]++[expr])
--                      | tss  <- eqCl headEq [ts| F ts<-rs], prefix<-take 1 [head ts| ts<-tss, not (null (tail ts))]
--                      , expr <- [cl (Fu [F (tail ts)| ts<-tss, not (null (tail ts))])]
--                      ] ++
--                      [ expr
--                      | tss<-eqCl headEq [ts| F ts<-rs], expr<-take 1 [head ts| ts<-tss, null (tail ts)] ]
--                     )
--                  where headEq x = (showADL (head x),sign (head x))
--   cl (Fi [F [t]]) = t
--   cl (Fi rs)
--    | length (eqCl headEq [ts| F ts<-rs])>1 = cr (Fi rs)
--    | otherwise = Fi ([ F ([prefix]++[expr])
--                      | tss  <- eqCl headEq [ts| F ts<-rs], prefix<-take 1 [head ts| ts<-tss, not (null (tail ts))]
--                      , expr <- [cl (Fi [F (tail ts)| ts<-tss, not (null (tail ts))])]
--                      ] ++
--                      [ expr
--                      | tss<-eqCl headEq [ts| F ts<-rs], expr<-take 1 [head ts| ts<-tss, null (tail ts)] ]
--                     )
--                  where headEq x = (showADL (head x),sign (head x))

--   cr::Expression -> Expression
--   cr (Fu [F [t]]) = t
--   cr (Fu rs)
--     = Fu ([ F ([expr]++[postfix])
--           | tss  <- eqCl lastEq [ts| F ts<-rs], postfix<-take 1 [last ts| ts<-tss, not (null (init ts))]
--           , expr <- [cr (Fu [F (init ts)| ts<-tss, not (null (init ts))])]
--           ] ++
--           [ expr
--           | tss<-eqCl lastEq [ts| F ts<-rs], expr<-take 1 [last ts| ts<-tss, null (init ts)] ]
--          )
--       where lastEq x = (showADL (last x),sign (last x))
--   cr (Fi [F [t]]) = t
--   cr (Fi rs)
--     = Fi ([ F ([expr]++[postfix])
--           | tss  <- eqCl lastEq [ts| F ts<-rs], postfix<-take 1 [last ts| ts<-tss, not (null (init ts))]
--           , expr <- [cr (Fi [F (init ts)| ts<-tss, not (null (init ts))])]
--           ] ++
--           [ expr
--           | tss<-eqCl lastEq [ts| F ts<-rs], expr<-take 1 [last ts| ts<-tss, null (init ts)] ]
--          )
--       where lastEq x = (showADL (last x),sign (last x))


  {- Wat gebeurt hier precies???

   data Deduction = Deduce Expression DedNode DedNode
                  | DedAnd Expression DedNode DedNode DedNode
                  | DedOr  Expression DedNode DedNode DedNode
                  | Ded0              DedNode DedNode
                  | Deriv  [Deduction] [DedNode] [DedNode]
                    deriving Eq
   data DedNode   = Dn Prop Expression
   instance Eq DedNode where
    n==n'= showNode n == showNode n'
   mults pat = [Dn p (Tm (makeMph d))|d<-declarations pat, p<-multiplicities d]

   showClause :: Expression -> String
   showClause (Fu terms)
    = sh "/\\" antc++" -: "++sh "\\/" cons
      where
       antc = [t|t<-terms, isNeg t]
       cons = [t|t<-terms, isPos t]
       sh str es = chain str (map showADL es)
   showClause e = error ("(module Calc: function showClause) '"++showADL e++"' has the wrong form and should never occur\nshowHS: "++showHS "" e)

   showMLink (Deduce clause (Dn pl l) (Dn pr r))           = "Deduce ("++showADL clause++")  "++showNode (Dn pl l)++" implies "++showNode (Dn pr r)
   showMLink (DedAnd clause (Dn pl l) (Dn pr r) (Dn pc c)) = "Deduce ("++showADL clause++")  "++showNode (Dn pl l)++" and "++showNode (Dn pr r)++" implies "++showNode (Dn pc c)
   showMLink (DedOr  clause (Dn pl l) (Dn pr r) (Dn pc c)) = "Deduce ("++showADL clause++")  "++showNode (Dn pl l)++" or " ++showNode (Dn pr r)++" implies "++showNode (Dn pc c)
   showMLink (Ded0          (Dn pl l) (Dn pc c))           = "Ded0 "++showNode (Dn pl l)++" implies "++showNode (Dn pc c)
   showMLink (Deriv clauses fs ts)
    = "Derivation\n     "++showNode (head (froms (head clauses)))++chain "" [showDer c| c<-clauses]
      where
       showDer (Deduce clause (Dn pl l) (Dn pr r))           = "\n  =>  { "++showClause clause++" }\n     "++showNode (Dn pr r)
       showDer (DedAnd clause (Dn pl l) (Dn pr r) (Dn pc c)) = "\n  =>  { "++showClause clause++" }\n     "++showNode (Dn pc c)
       showDer (DedOr  clause (Dn pl l) (Dn pr r) (Dn pc c)) = "\n  =>  { "++showClause clause++" }\n     "++showNode (Dn pc c)
       showDer (Ded0          (Dn pl l) (Dn pc c))           = "\n  =>\n     "++showNode (Dn pc c)

   froms (Deduce clause (Dn pl l) (Dn pr r))           = [Dn pl l]
   froms (DedAnd clause (Dn pl l) (Dn pr r) (Dn pc c)) = [Dn pl l,Dn pr r]
   froms (DedOr  clause (Dn pl l) (Dn pr r) (Dn pc c)) = [Dn pl l,Dn pr r]
   froms (Ded0          (Dn pl l) (Dn pc c))           = [Dn pl l]
   froms (Deriv clauses fs ts)                         = fs
   tos (Deduce clause (Dn pl l) (Dn pr r))             = [Dn pr r]
   tos (DedAnd clause (Dn pl l) (Dn pr r) (Dn pc c))   = [Dn pc c]
   tos (DedOr  clause (Dn pl l) (Dn pr r) (Dn pc c))   = [Dn pc c]
   tos (Ded0          (Dn pl l) (Dn pc c))             = [Dn pc c]
   tos (Deriv clauses fs ts)                           = ts
   derivLength (Deriv clauses fs ts)                   = sum (map derivLength clauses)
   derivLength _                                       = 1

   showNode (Dn Uni e) = "univalent("++showADL e++")"
   showNode (Dn Tot e) = "total("++showADL e++")"
   showNode (Dn Inj e) = "injective("++showADL e++")"
   showNode (Dn Sur e) = "surjective("++showADL e++")"
   showNode (Dn x e) = show x++"("++showADL e++")"



   deduce :: [Deduction] -> [Deduction]
   deduce xs
    = f xs' (rd (concat (map froms xs)) `isc` rd (concat (map tos xs)))
       where
        xs' = [ Deriv [d] (froms d) (tos d) | d<-sort' sortCrit xs]
        f q (n:nodes) = f (q `uni` sort' sortCrit [ Deriv [ded,ded'] [a] [b']
                                                  | ded <-q, a <-froms ded,  b <-tos ded,  b==n
                                                  , ded'<-q, a'<-froms ded', b'<-tos ded', a'==n, a/=b']) nodes
        f q []        = q
        sortcrit (Dn p e) = (length (morlist e),show p, showADL e)
        sortCrit d = map sortcrit (tos d++froms d)
        [] `uni` ds' = ds'
        ds `uni` []  = ds
        (d:ds) `uni` (d':ds')
         | sortCrit d<sortCrit d' = d: ds `uni` (d':ds')
         | sortCrit d>sortCrit d' = d': (d:ds) `uni` ds'
         | derivLength d<derivLength d' = d: ds `uni` ds'
         | otherwise = d': ds `uni` ds'

   type DeduceG = [Deduction]
   multDerivations :: Language pat => pat -> DeduceG
   multDerivations pat
    = [ Deriv (flatten deriv) fs ts
      | deriv@(Deriv clauses fs ts)<-(deduce.rd.filter uneq) (fliplinks ++ frs)
      , and[oneMorphism e&&inline m|Dn p e<-fs++ts, m<-morlist e]
      ]
      where
       flatten (Deriv clauses froms tos) = concat (map flatten clauses)
       flatten d = [d]
       frs = concat
             ( [ if idsOnly antc then (if length cons==1 then crule clause (head cons) else []) else
                 if idsOnly cons then (if length antc==1 then arule clause (head antc) else []) else
                 [ Deduce clause (Dn Inj (Fu cons)) (Dn Inj (Fu antc))
                 , Deduce clause (Dn Uni (Fu cons)) (Dn Uni (Fu antc))
                 , Deduce clause (Dn Tot (Fu antc)) (Dn Tot (Fu cons))
                 , Deduce clause (Dn Sur (Fu antc)) (Dn Sur (Fu cons))]
               | rule<-rules pat, conjunct<-conjuncts rule, clause@(Fu terms)<-allClauses conjunct
               , antc<-[[t|t<-terms, isNeg t]]
               , cons<-[[t|t<-terms, isPos t]]
               ])
       crule clause (F ts) = concat [ [Deduce clause (Dn Inj s) (Dn Tot r), Deduce clause (Dn Uni r) (Dn Sur s)] | (s,r)<-split ts ]
       arule clause (F ts) = concat [ [Deduce clause (Dn Tot s) (Dn Inj r), Deduce clause (Dn Sur r) (Dn Uni s)] | (s,r)<-split ts ]
       arule clause x = error ("in module Calc, arule: unexpected pattern: "++showADL x) 
       split []     = []
       split [r]    = []
       split [r,s]  = [(F [r],F [s])]
       split (r:rs) = (F [r],F rs): [(F (r:r'), F (r:s')) | (F r',F s')<-split rs]
 --      uneq (Deduce clause (Dn pa antc) (Dn pc cons)) = pa/=pc || showADL antc/=showADL cons
       uneq ded = null (map showNode (froms ded) `isc` map showNode (tos ded))
       harmonize deduction@(Deduce clause (Dn pa antc) (Dn pc cons))
        = Deduce clause (Dn pa' antc') (Dn pc' cons')
          where
           Dn pa' antc' = if length [a| a<-mors antc, not (inline a)] > length [a| a<-mors antc] `div` 2 
                          then flip (Dn pa antc)
                          else Dn pa antc
           Dn pc' cons' = if length [c| c<-mors cons, not (inline c)] > length [c| c<-mors cons] `div` 2 
                          then flip (Dn pc cons)
                          else Dn pc cons
       flip (Dn p e) = Dn (flipProp p) (flp e)
       fliplinks :: [Deduction]
       fliplinks
        = [ Deduce (Fu [Cp cons, antc']) (Dn pc cons) (Dn pa' antc')
          | Deduce clause (Dn pa antc) (Dn pc cons)<-frs
          , Deduce clause' (Dn pa' antc') (Dn pc' cons')<-frs, pc==flipProp pa'&& cons==flp antc'] ++
          [ Ded0 (flip (Dn pa antc)) (Dn pa antc)
          | Deduce clause (Dn pa antc) (Dn pc cons)<-frs
          , length [a| a<-mors antc, not (inline a)] > length [a| a<-mors antc] `div` 2] ++
          [ Ded0 (Dn pc cons) (flip (Dn pc cons))
          | Deduce clause (Dn pa antc) (Dn pc cons)<-frs
          , length [c| c<-mors cons, not (inline c)] > length [c| c<-mors cons] `div` 2]
  
   instance Graphic DeduceG where
    dotGraph context style nm links
     = "digraph "++show [x|x<-nm,not(isSpace x)]++introG++newline
             ++ chain newline
                ( [ line "" (Dn pa antc) (Dn pc cons) | link@(Ded0 (Dn pa antc) (Dn pc cons))<-links]++
                  [ line (showADL clause) (Dn pa antc) (Dn pc cons) | link@(Deduce clause (Dn pa antc) (Dn pc cons))<-links])
             ++ "\n   }"
       where
         introG = "\n   { bgcolor=transparent"++newline
               ++ " { node [shape=box,fontsize=18,font=helvetica] "++chain "; " [quote(showNode (Dn p e))| (Dn p e)<-nodes links, oneMorphism e, m<-mors e, inline m, p `elem` multiplicities (makeDeclaration m)]++" }"++newline
               ++ "node [shape=plaintext,fontsize=18,font=helvetica]"++newline
               ++ "edge [fontsize=12,arrowsize=0.8]"
         nodes links = rd ([ Dn pa antc | Deduce clause (Dn pa antc) (Dn pc cons)<-links]++
                           [ Dn pc cons | Deduce clause (Dn pa antc) (Dn pc cons)<-links])
         line s p1 p2 = quote (showNode p1) ++ edgearrow ++ quote (showNode p2) ++if null s then "" else " [label="++quote s++"]"
         --- Nog wat hulpfuncties. Die horen overigens waarschijnlijk niet hier...
         edgearrow = " -> "
         newline = "\n   ; "
         quote s = "\"" ++ s ++ "\" "
  -}
