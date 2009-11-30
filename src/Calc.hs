 {-# OPTIONS_GHC -Wall #-}
module Calc ( deriveProofs
            , conjuncts
            , simplify
            , allClauses
            , makeRule
            , lambda
            , checkMono
            , actSem
            , assembleECAs
            , delta ) 
  where

   import Collection         (Collection (isc,rd,rd'))
   import Auxiliaries        (sort',eqCl,eqClass)
   import Strings            (spread)
   import Adl
   import Data.Fspec
   import FspecDef           (Fspc,chain,serviceS)
   import Adl.ECArule        (InsDel(..),ECArule(..),Event(..),PAclause(..))
   import ShowADL            (showADL,showADLcode)
   import ShowECA            (showECA)
   import CommonClasses      (ABoolAlg(..))
   import NormalForms        (conjNF,disjNF,normECA,nfProof,proofPA,nfPr,simplify)

   conjuncts :: Rule -> [Expression]
   conjuncts = fiRule.conjNF.normExpr
    where fiRule (Fi fis) = {- map disjuncts -} fis
          fiRule r        = [ {- disjuncts -} r]

-- The function disjuncts yields an expression which has constructor Fu in every case.
   disjuncts :: Expression -> Expression
   disjuncts = fuRule
    where fuRule (Fu cps) = (Fu . rd . map cpRule) cps
          fuRule r        = Fu [cpRule r]
          cpRule (Cp r)   = Cp (fRule r)
          cpRule r        = fRule r
          fRule (F ts)    = F ts
          fRule  r        = F [r]

   showClause  :: Fspc -> Clauses -> String
   showClause fSpec cl
    = "\nRule: "++showADLcode fSpec rule ++concat
       [if null conj then "\nNo clauses" else
        "\nConjunct: "++showADLcode fSpec conjunct++
        concat ["\n   Clause: "++showADLcode fSpec d| d<-conj]
       | (conjunct, conj)<-zip (conjuncts rule) (cl_conjNF cl)]
      where rule = cl_rule cl

-- The function allClauses yields an expression which has constructor Fu in every case.
   allClauses :: Rule -> Clauses
   allClauses rule = Clauses [allShifts conj| conj<-conjuncts rule] rule

   allShifts :: Expression -> [Expression]
   allShifts conjunct = rd [simplify (normFlp e')| e'<-shiftL conjunct++shiftR conjunct, not (isTrue e')]
    where
       normFlp (Fu []) = Fu []
       normFlp (Fu fs) = if length [m| f<-fs, m<-morlist f, mphyin m] <= length [m| f<-fs, m<-morlist f, not (mphyin m)]
                         then Fu (map flp fs) else (Fu fs)
       normFlp _ = error ("!Fatal (module Calc 61): normFlp must be applied to Fu expressions only, look for mistakes in shiftL or shiftR")

   shiftL :: Expression -> [Expression]
   shiftL r
    | length antss+length conss /= length fus = error ("!Fatal (module Calc 65): shiftL will not handle argument of the form "++showADL r)
    | null antss || null conss                = [disjuncts r|not (null fs)] --  shiftL doesn't work here.
    | idsOnly antss                           = [Fu ([Cp (F [Tm (mIs srcA)])]++map F conss)]
    | otherwise                               = [Fu ([ Cp (F (if null ts then id' css else ts))
                                                     | ts<-ass++if null ass then [id' css] else []]++
                                                     [ F (if null ts then id' ass else ts)
                                                     | ts<-css++if null css then [id' ass] else []])
                                                | (ass,css)<-rd(move antss conss)
                                                , if null css then error "!Fatal (module Calc 73): null css in shiftL" else True
                                                , if null ass then error "!Fatal (module Calc 74): null ass in shiftL" else True
                                                ]
    where
     Fu fs = disjuncts r
     fus = filter (not.isIdent) fs
     antss = [ts | Cp (F ts)<-fus]
     conss = [ts | F ts<-fus]
     srcA = -- if null antss  then error ("!Fatal (module Calc 81): empty antecedent in shiftL ("++showHS options "" r++")") else
            if length (eqClass order [ source (head ants) | ants<-antss])>1 then error ("!Fatal (module Calc82): shiftL ("++showADL r++")\nin calculation of srcA\n"++show (eqClass order [ source (head ants) | ants<-antss])) else
            foldr1 lub [ source (head ants) | ants<-antss]
     id' ass = [Tm (mIs c)]
      where a = (source.head.head) ass
            c = if not (a `order` b) then error ("!Fatal (module Calc 86): shiftL ("++showADL r++")\nass: "++show ass++"\nin calculation of c = a `lub` b with a="++show a++" and b="++show b) else
                a `lub` b
            b = (target.last.last) ass
   -- It is imperative that both ass and css are not empty.
     move :: [Expressions] -> [Expressions] -> [([Expressions],[Expressions])]
     move ass [] = [(ass,[])]
     move ass css
      = (ass,css):
        if and ([not (idsOnly (F cs))| cs<-css]) -- idsOnly (F [])=True, so:  and [not (null cs)| cs<-css]
        then [ts| length (eqClass (==) (map head css)) == 1
                , isUni h
                , ts<-move [[flp h]++as|as<-ass] (map tail css)]++
             [ts| length (eqClass (==) (map last css)) == 1
                , isInj l
                , ts<-move [as++[flp l]|as<-ass] (map init css)]
        else []
        where h=head (map head css); l=head (map last css)

   shiftR :: Expression -> [Expression]
   shiftR r
    | length antss+length conss /= length fus = error ("!Fatal (module Calc 106): shiftR will not handle argument of the form "++showADL r)
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
     srcA = if null conss then error ("!Fatal (module Calc 119): empty consequent in shiftR ("++showADL r++")") else
            if length (eqClass order [ source (head cons) | cons<-conss])>1
            then error ("Fatal (module Calc120): shiftR ("++showADL r++")\nin calculation of srcA\n"++show (eqClass order [ source (head cons) | cons<-conss]))
            else foldr1 lub [ source (head cons) | cons<-conss]
     id' css = [Tm (mIs c)]
      where a = (source.head.head) css
            c = if not (a `order` b)
                then error ("!Fatal (module Calc 126): shiftR ("++showADL r++")\nass: "++show css++"\nin calculation of c = a `lub` b with a="++show a++" and b="++show b)
                else a `lub` b
            b = (target.last.last) css
     move :: [Expressions] -> [Expressions] -> [([Expressions],[Expressions])]
     move [] css = [([],css)]
     move ass css
      = (ass,css):
        if and [not (null as)| as<-ass]
        then [ts| length (eqClass (==) (map head ass)) == 1
                , isSur h
                , ts<-move (map tail ass) [[flp h]++cs|cs<-css]]++
             [ts| length (eqClass (==) (map last ass)) == 1
                , isTot l
                , ts<-move (map init ass) [cs++[flp l]|cs<-css]]
        else []
        where h=head (map head ass); l=head (map last ass)

-- assembleECAs :: [Rule] -> [ECArule]
-- Deze functie neemt verschillende clauses samen met het oog op het genereren van code.
-- Hierdoor kunnen grotere brokken procesalgebra worden gegenereerd.
   assembleECAs :: (Morphism->Bool) -> [Rule] -> [ECArule]
   assembleECAs visible ruls = [ecarule i| (ecarule,i) <- zip protoECAs [1..]]
      where
       protoECAs
        = [ ECA (On ev m) delt action
          | quads<-eqCl fst4 mccrs, (m,_,_,_)<-take 1 quads, Tm delt<-[delta (sign m)]
          , ev<-[Ins,Del]
          , action <- [ All
                        [ Chc [ (if isFalse viols   then Nop else
--                               if not (visible m) then Blk [(viols,causes)] else
                                 doCode visible ev toExpr viols)
                                 [(conj,rd' nr causes)]  -- the motivation for these actions
                              | clEq <- eqCl snd4 conjEq, clause@(Fu fus) <- map snd4 (take 1 clEq)
                              , clause' <- [ subst (m, actSem Ins m (delta (sign m))) clause]
                              , viols   <- [ conjNF (Cp clause') ]
                              , causes  <- [ [r|(_,_,_,rs)<-clEq, r<-rs] ]
                              , frExpr  <- [ if ev==Ins
                                             then Fu [f| f<-fus, isNeg f]
                                             else Fu [f| f<-fus, isPos f] ]
                              , m `elem` map makeInline (mors frExpr)
                              , toExpr  <- [ if ev==Ins
                                             then Fu [      f| f<-fus, isPos f]
                                             else Fi [notCp f| f<-fus, isNeg f] ]
                              ]
                              (rd [(j,rs)| (_,_,j,rs)<-conjEq])  -- to supply motivations on runtime
                        | conjEq <- eqCl thd4 quads
                        , conj <- map thd4 (take 1 conjEq)
           --           , (not.null.lambda Ins (Tm m)) conj  -- causes infinite loop
           --           , not (checkMono conj Ins m)         -- causes infinite loop
                        , conj'<- [subst (m, actSem Ins m (delta (sign m))) conj]
                        , (not.isTrue.conjNF) (Fu[Cp conj,conj']) -- the system must act to restore invariance     
                        ]
                        (rd [(j,rs)| (_,_,j,rs)<-quads])  -- to supply motivations on runtime
                      ]
          ]
       mccrs
        = [ (m,clause,conj,rd' nr [r|(_,_,_,r)<-cl])
          | cl<- eqCl f [ (m,clause,conj,rule)
                        | rule<-ruls
                        , conj <- conjuncts rule
                        , clause<-allShifts conj
                        , m<-rd (map makeInline (mors clause))
                        , visible m]
          , (m,clause,conj,_) <- take 1 cl
          ]
        where f (m,clause,conj,_) = (m,clause,conj)
       fst4 (w,_,_,_) = w
       snd4 (_,x,_,_) = x
       thd4 (_,_,y,_) = y

-- testService :: Fspc -> ObjectDef -> String
-- Deze functie is bedoeld om te bedenken hoe services moeten worden afgeleid uit een vers vertaalde ObjectDef.
-- Nadat deze goed werkt kunnen de bewijsgenerator en de codegenerator worden gemaakt.
   testService :: Fspc -> ObjectDef -> String
   testService fSpec object
    = "\nService "++ objnm object++"("++chain ", " [showADL m++":"++name (target m)| m<-rels]++")\n"++
      " - The parameters correspond to editable fields in a UI-service.\n   "++
      showADLcode fSpec object++"\n"++
      " - Invariants:\n   "++chain "\n   " [showADLcode fSpec rule    | rule<-invariants]++"\n"++
      " - Derivation of clauses for ECA-rules:"   ++
      concat [showClause fSpec (allClauses rule) | rule<-invariants]++"\n"++
      " - ECA rules:"++concat  [ "\n\n   "++showECA fSpec "\n>     "  (normECA eca (error ("TODO: hier moet een declaratie Delta staan")))
                                 ++"\n------ Derivation ----->"++showProof (showECA fSpec "\n>     ") (proofPA (ecaAction eca))++"\n<------End Derivation --"
                               | eca<-ecaRs]++"\n\n"++
      " - Visible relations:\n   "++chain "\n   " (spread 80 ", " [showADLcode fSpec m  | m<-vis])++"\n"
    where
        rels = rd (recur object)
         where recur obj = [editMph (objctx o)| o<-objats obj, editable (objctx o)]++[m| o<-objats obj, m<-recur o]
        vis        = rd (map makeInline rels++map (mIs.target) rels)
        visible m  = makeInline m `elem` vis
        invariants = [rule| rule<-rules fSpec, not (null (map makeInline (mors rule) `isc` vis))]
        ecaRs      = assembleECAs visible invariants
        editable (Tm Mph{})  = True
        editable _           = False
        editMph (Tm m@Mph{}) = m
        editMph e            = error("!Fatal (module Calc): cannot determine an editable declaration in a composite expression: "++show e)


   deriveProofs :: Fspc -> String
   deriveProofs fSpec
    = --"\nSignals for "++name fSpec++"\n--------------\n"++
      --proof (signals fSpec)++
      "\nRules for "++name fSpec++"\n--------------\n"++
      chain "\n--------------\n"
      [ chain "\n" ["   "++stmt++if null comment then "" else "\n<=> { "++comment++". }"
                   | (stmt,comment)<-cleanup (derivation rule)]
      | rule<-rules fSpec]++
      "\n--------------\n"++ -- TODO: make an ontological analysis, which explains the delete behaviour.
      "Ontological analysis: \n  "++
      chain "\n\n  " [name o++"("++chain ", " [name a++"["++(name.target.ctx) a++"]"|a<-attributes o]++"):\n  "
                     | o<-serviceS fSpec]++
      "\n--------------\n"++
      "Analyzing services: \n     "++
      chain "\n     " [testService fSpec o| o<-serviceS fSpec]++
      "\n--------------\n"
      where 
        derivation rule = 
         case rule of
          Ru{} -> [ (showADL rule, if null prf then "Translates directly to conjunctive normal form" else "Convert into conjunctive normal form")
                  , (showPr prf  , "")
                  ]++
                  if (rrsrt rule)==Truth then [] else
                     [ ("\nViolations are computed by (conjNF . Cp . normexpr) rule:\n     "++
                        (cfProof. Cp . normExpr) rule++"\n"
                       , "")
                     , ("\nConjuncts:\n     "++
                        chain "\n     " (rd[ showADL conjunct
                                           | conjunct<-conjuncts rule])
                       , "")
                     , ("\n"++showClause fSpec (allClauses rule)
                       , "")
                     , ("\nAvailable code fragments on rule "++show (nr rule)++":\n     "++
                          chain "\n     " [showADL (makeRule rule r)++ " yields\n"++chain "\n\n"
                                          [ "event = "++show ev++" "++showADL m++"\n"++
                                            showADL r++"["++showADL m++":="++showADL (actSem ev m (delta (sign m)))++"] = r'\n"++
                                            "r'    = "++cfProof r'++"\n"++
                                            "viols = r'-"++cfProof (Cp r')++"\n"++
                                            "violations, considering that the valuation of "++showADL m++" has just been changed to "++showADL (actSem ev m (delta (sign m)))++
                                            "            "++cfProof (Cp r) -- ++"\n"++
                                      --      "reaction? evaluate r -: r' ("++(showADL.conjNF) (Fu[Cp r,r'])++")"++
                                      --         cfProof (Fu[Cp r,r'])++"\n"++
                                      --      "delta: r-/\\r' = "++cfProof (Fi[notCp r,r'])++
                                      --      "\nNow compute a reaction\n(isTrue.conjNF) (Fu[Cp r,r']) = "++show ((isTrue.conjNF) (Fu[Cp r,r']))++"\n"++
                                      --      (if null (lambda ev (Tm m) r)
                                      --       then "lambda "++showADL m++" ("++showADL r++") = empty\n"
                                      --       else {- for debug purposes:
                                      --               "lambda "++show ev++" "++showADL m++" ("++showADL r++") = \n"++(chain "\n\n".map showPr.lambda ev (Tm m)) r++"\n"++
                                      --               "derivMono ("++showADL r++") "++show ev++" "++showADL m++"\n = "++({-chain "\n". map -}showPr.derivMono r ev) m++"\n"++
                                      --               "\nNow compute checkMono r ev m = \n"++show (checkMono r ev m)++"\n"++ -}
                                      --            if (isTrue.conjNF) (Fu[Cp r,r'])
                                      --            then "A reaction is not required, because  r -: r'. Proof:"++cfProof (Fu[Cp r,r'])++"\n"
                                      --            else if checkMono r ev m
                                      --            then "A reaction is not required, because  r -: r'. Proof:"{-++(showPr.derivMono r ev) m-}++"NIET TYPECORRECT: (showPr.derivMono r ev) m"++"\n"  --WAAROM? Stef, gaarne herstellen...Deze fout vond ik nadat ik het type van showProof had opgegeven.
                                      --            else let Tm delt = delta (sign m) in
                                      --                 "An appropriate reaction on this event is\n"++
                                      --                 showECA fSpec "\n  " (ECA (On ev m) delt (doCode visible Ins r viols conj [rule]) 0)
                                      --      )
                                          | m<-rd [m'|x<-mors r, m'<-[x,flp x], inline m', not (isIdent m')] -- TODO: include proofs that allow: isIdent m'
                                          , ev<-[Ins,Del]
                                          , r'<-[subst (m, actSem ev m (delta (sign m))) r]
                                      --  , viols<-[conjNF (Cp r')]
                                          , True ]  -- (isTrue.conjNF) (Fu[Cp r,r'])
                                         | Clauses cs _<-[allClauses rule]
                                      -- , conjs<-[conjuncts rule]
                                      -- , (cls,conj)<-zip cs conjs
                                         , r<-[ Fu fus | fus<-cs]
                                         ]
                       , "")
                     ] where prf = nfProof (normExpr rule)
                             cfProof = showPr . nfPr True False . simplify
                             showPr = showProof (showADLcode fSpec)
          Sg{} ->    [ (showADL rule          , if null prf then "Translates directly to conjunctive normal form" else "Convert into conjunctive normal form")
                     , (showProof (showADLcode fSpec) prf      , "")
                     ]++
                     [ ("\nConjuncts:\n     "++
                        chain "\n     " (rd[showADL conjunct
                                           |conjunct<-conjuncts (srsig rule)])
                       , "")
                     , ("\nClauses:\n     "++
                        chain "\n     " (rd[showADL (makeRule (srsig rule) r)
                                           |Clauses conjs _<-[allClauses (srsig rule)], r<-[ Fu fus | fus<-conjs]])
                       , "")
                     ] where prf = nfProof (normExpr (srsig rule))
          Gc{} -> undefined
          Fr{} -> undefined 
        cleanup :: [(String,String)] -> [(String,String)]
        cleanup [x] = [x]
        cleanup ((x,c):(x',c'):xs) = if x==x' then rest else (x,c): rest where rest = cleanup ((x',c'):xs)
        cleanup [] = []

   checkMono :: Expression
             -> InsDel
             -> Morphism
             -> Bool
   checkMono expr ev m = simplify expr == simplify (antecedent conclusion) &&
                         simplify (subst (m, actSem ev m (delta (sign m))) expr) ==
                         simplify (consequent conclusion)
     where (conclusion,_,_) = last (derivMono expr ev m)

   actSem :: InsDel -> Morphism -> Expression -> Expression
   actSem Ins m (Tm d) | makeInline m==makeInline d = Tm m
                       | otherwise                  = Fu[Tm m,Tm d]
   actSem Ins m delt   = disjNF (Fu[Tm m,delt])
   actSem Del m (Tm d) | makeInline m==makeInline d = Fi[]
                       | otherwise                  = Fi[Tm m, Cp (Tm d)]
   actSem Del m delt   = conjNF (Fi[Tm m,Cp delt])
 --  actSem Del m delt = Fi[m,Cp delt]

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

   -- | de functie doCode beschrijft de voornaamste mogelijkheden om een expressie delta' te verwerken in expr (met tOp'==Ins of tOp==Del)
   doCode :: (Morphism->Bool)        -- the morphisms that may be changed
          -> InsDel
          -> Expression              --  the expression in which a delete or insert takes place
          -> Expression              --  the delta to be inserted or deleted
          -> [(Expression,[Rule])]   --  the motivation, consisting of the conjuncts (traced back to their rules) that are being restored by this code fragment.
          -> PAclause
   doCode editable tOp' expr1 delta1 motive = doCod delta1 tOp' expr1 motive
    where
      doCod deltaX tOp exprX motiv =
        case (tOp, exprX) of
          (_ ,  Fu [])   -> error ("!Fatal (module Calc 361): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (Fu [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++").")
          (_ ,  Fi [])   -> error ("!Fatal (module Calc 363): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (Fi [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++").")
          (_ ,  F [])    -> error ("!Fatal (module Calc 365): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (F [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++").")
          (_ ,  Fd [])   -> error ("!Fatal (module Calc 367): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (Fd [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++").")
          (_ ,  Fu [t])  -> doCod deltaX tOp t motiv
          (_ ,  Fi [t])  -> doCod deltaX tOp t motiv
          (_ ,  F [t])   -> doCod deltaX tOp t motiv
          (_ ,  Fd [t])  -> doCod deltaX tOp t motiv
          (Ins, Cp x)    -> doCod deltaX Del x motiv
          (Del, Cp x)    -> doCod deltaX Ins x motiv
          (Ins, Fu fs)   -> Chc [ doCod deltaX Ins f motiv | f<-fs{-, not (f==expr1 && Ins/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Ins, Fi fs)   -> All [ doCod deltaX Ins f []    | f<-fs ] motiv
          (Ins, F ts)    -> Chc [ Chc [ New c (\x->All [fLft x, fRht x] motiv) motiv
                                      , Sel c (F ls) fLft motiv
                                      , Sel c (flp(F rs)) fRht motiv
                                      ] motiv
                                | (ls,rs)<-chop ts, c<-[source (F rs) `lub` target (F ls)]
                                , fLft<-[(\atom->doCod (disjNF (Fu[F [Tm (Mp1 atom [] c),v (c,source deltaX),deltaX],Cp (F rs)])) Ins (F rs) [])]
                                , fRht<-[(\atom->doCod (disjNF (Fu[F [deltaX,v (target deltaX,c),Tm (Mp1 atom [] c)],Cp (F ls)])) Ins (F ls) [])]
                                ] motiv
          (Del, F ts)    -> Chc [ Chc [ Sel c (Fi [F ls,flp(F rs)]) (\_->Rmv c (\x->All [fLft x, fRht x] motiv) motiv) motiv
                                      , Sel c (Fi [F ls,flp(F rs)]) fLft motiv
                                      , Sel c (Fi [F ls,flp(F rs)]) fRht motiv
                                      ] motiv
                                | (ls,rs)<-chop ts, c<-[source (F rs) `lub` target (F ls)]
                                , fLft<-[(\atom->doCod (disjNF (Fu[F [Tm (Mp1 atom [] c),v (c,source deltaX),deltaX],Cp (F rs)])) Del (F rs) [])]
                                , fRht<-[(\atom->doCod (disjNF (Fu[F [deltaX,v (target deltaX,c),Tm (Mp1 atom [] c)],Cp (F ls)])) Del (F ls) [])]
                                ] motiv
          (Del, Fu fs)   -> All [ doCod deltaX Del f []    | f<-fs{-, not (f==expr1 && Del/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Del, Fi fs)   -> Chc [ doCod deltaX Del f motiv | f<-fs ] motiv
-- Op basis van de Morgan is de procesalgebra in het geval van (Ins, Fd ts)  afleidbaar uit uit het geval van (Del, F ts) ...
          (_  , Fd ts)   -> doCod deltaX tOp (Cp (F (map Cp ts))) motiv
          (_  , K0 x)    -> doCod (deltaK0 deltaX tOp x) tOp x motiv
          (_  , K1 x)    -> doCod (deltaK1 deltaX tOp x) tOp x motiv
          (_  , Tm m)    -> (if editable m then Do tOp exprX (disjNF deltaX) motiv else Blk [(Tm m,rd' nr [r|(_,rs)<-motiv, r<-rs])])
          (_ , _)        -> error ("!Fatal (module Calc 400): Non-exhaustive patterns in the recursive call doCod ("++showADL deltaX++") "++show tOp++" ("++showADL exprX++"),\n"++
                                   "within function doCode "++show tOp'++" ("++showADL exprX++") ("++showADL delta1++").")


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

{-
   niClauses :: Expression -> [Expression]
   niClauses cl'  = [ hc | hc@(Fu fus)<-allClauses cl'
                        , not (and [idsOnly e'| (Cp e')<-fus] || and [idsOnly t| t@(F _)<-fus])
                   ]
   lClause :: Expression -> Expression
   lClause cl'    = head (niClauses cl'++[cl'])
   rClause :: Expression -> Expression
   rClause cl'    = last ([cl']++niClauses cl')
-}

   makeRule :: Rule -> Expression -> Rule
   makeRule r expr =
     case expr of
           (Fu []) -> error ("!Fatal (module Calc): erroneous call to function makeRule r ("++showADL expr++").")
           (Fu ts) -> if or [isNeg t|t<-ts] 
                      then r { rrsrt = Implication
                             , rrant = Fi [notCp t|t<-ts,isNeg t]
                             , rrcon = Fu [t|t<-ts,isPos t]
                             , r_cpu = []}
                      else r { rrsrt = Truth
                             , rrant = error ("!Fatal (module Calc): erroneous call to antecedent of r "++showADL expr)
                             , rrcon = Fu ts -- obsolete: error ("Module Calc: Loop found!! (hier stond eerst \"r\").")    -- WAAROM?  Stef, hier zat volgens mij de bug. Kan jij dit herstellen?
                             , r_cpu = []
                             }  
           _ -> case disjNF expr of
                      Fu{} -> makeRule r (disjNF expr)
                      _    -> r { rrsrt = Truth
                                , rrant = (error ("!Fatal (module Calc): erroneous call to antecedent of r "++showADL expr))
                                , rrcon = expr
                                , r_cpu = []}
                        

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
   showProof sh [(expr,_,_)]        = "\n      "++sh expr++"\n"
   showProof sh ((expr,ss,equ):prf) = "\n      "++sh expr++
                                      "\n"++(if null ss then "\n   "++equ else if null equ then chain " " ss else "   "++equ++" { "++chain "; " ss++" }")++
                                      showProof sh prf
                                      --where e'= if null prf then "" else let (expr,_,_):_ = prf in showHS options "" expr 
   showProof _  []                  = ""


   derivMono :: Expression -> InsDel -> Morphism -> [(Rule, [String], String)]
   derivMono expr tOp m' = f (head (lambda tOp (Tm m') expr++[[]])) (start tOp)
    where
     f:: [(Expression, [String], whatever)] -> (Expression, Expression) -> [(Rule, [String], String)]   -- WAAROM?? Stef, graag enige uitleg van wat hier gebeurt...
     f [] (_,_) = []
     f [(e',_,_)] (neg',pos')
      = [(rule (subst (m',neg') e') (subst (m',pos') e'),[],"")]
     f ((e',["omkeren"],_): prf@((_,_,_):_)) (neg',pos')
      = (rule (subst (m',neg') e') (subst (m',pos') e'),["r -: s  <=>  s- -: r-"],"<=>"):
         f prf (pos',neg')
     f ((e1,_,_): prf@((e2,_,_):_)) (neg',pos')
      = (rule (subst (m',neg') e1) (subst (m',pos') e1),["Monotony of "++showOp e2],"==>"):
         f prf (neg',pos')
         
     start Ins  = (Tm m',Fu [Tm m',delta (sign m')])
     start Del  = (Fi [Tm m',Cp (delta (sign m'))],Tm m')
     rule :: Expression -> Expression -> Rule
     rule neg' pos' | isTrue neg' = Ru { rrsrt = Truth
                                       , rrant = error ("!Fatal (module Calc): illegal reference to antecedent in rule ("++showADL neg'++") ("++showADL pos'++")")
                                       , rrfps = Nowhere
                                       , rrcon = pos'
                                       , r_cpu = []
                                       , rrxpl = ""
                                       , rrtyp = sign neg' {- (neg `lub` pos) -}
                                       , rrdcl = Nothing
                                       , runum = 0
                                       , r_pat = ""}
                    | otherwise   = Ru { rrsrt = Implication
                                       , rrant = neg'
                                       , rrfps = Nowhere
                                       , rrcon = pos'
                                       , r_cpu = []
                                       , rrxpl = ""
                                       , rrtyp = sign neg' {- (neg `lub` pos) -}
                                       , rrdcl = Nothing
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
                     Tc{}     -> error("!Fatal (module Calc): call to showOp (Tc x) in module Calc.hs")



   lambda :: InsDel -> Expression -> Expression -> [Proof Expression]
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
       first _ = error "!Fatal (module Calc): wrong pattern in first"
