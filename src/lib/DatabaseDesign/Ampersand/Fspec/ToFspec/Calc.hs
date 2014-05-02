{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module DatabaseDesign.Ampersand.Fspec.ToFspec.Calc
            ( deriveProofs 
            , lambda
            , checkMono
            , showProof, showPrf
          --  , testInterface
            )
where

   import DatabaseDesign.Ampersand.Basics         (fatalMsg,Collection (isc),Identified(..),eqCl)
   import Data.List hiding (head)
   import GHC.Exts (sortWith)
--   import Data.ByteString.Char8
--   import Data.ByteString.Lazy.Char8
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith)
   import DatabaseDesign.Ampersand.ADL1
   import DatabaseDesign.Ampersand.ADL1.Expression
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import DatabaseDesign.Ampersand.Fspec.ShowADL (ShowADL(..), showREL)
   import DatabaseDesign.Ampersand.Fspec.ShowECA (showECA)
   import DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec
   import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms        (conjNF,disjNF,cfProof,dfProof,nfProof,simplify,normPA) --,proofPA) -- proofPA may be used to test derivations of PAclauses.
   import DatabaseDesign.Ampersand.Misc            (Lang(..),Options(..),PandocFormat(ReST),string2Blocks)
   import Text.Pandoc
   import Text.Pandoc.Builder
   import Text.Pandoc.Readers.Textile
   import Prelude hiding (head)
-- import Debug.Trace
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ToFspec.Calc"

   head :: [a] -> a
   head [] = fatal 30 "head must not be used on an empty list!"
   head (a:_) = a

-- testInterface :: Fspc -> Interface -> String
-- Deze functie is bedoeld om te bedenken hoe interfaces moeten worden afgeleid uit een vers vertaalde ObjectDef.
-- Nadat deze goed werkt kunnen de bewijsgenerator en de codegenerator worden gemaakt.
--   testInterface :: Options -> Fspc -> Interface -> String
--   testInterface flags fSpec ifc
--    = "\nInterface "++ name ifc++"("++intercalate ", " [showADL r++":"++name (target r) | r<-rels]++")\n"++
--      " - The parameters correspond to editable fields in a user interface.\n   "++
--      showADL ifc++"\n"++
--      " - Invariants:\n   "++intercalate "\n   " [showADL rule    | rule<-invs]++"\n"++
--      " - Derivation of clauses for ECA-rules:"   ++
--      concat [showClause fSpec (allClauses flags rule) | rule<-invs]++"\n"++
--{-
--      " - ECA rules:"++concat  [ "\n\n   "++showECA "\n>     "  (eca{ecaAction=normPA (ecaAction eca)})
--                                 ++"\n------ Derivation ----->"++showProof (showECA "\n>     ") (proofPA (ecaAction eca))++"\n<------End Derivation --"
--                               | eca<-ecaRs]++"\n\n"++
---}
--      " - Visible relations:\n   "++intercalate "\n   " (spread 80 ", " [showADL r  | r<-vis])++"\n"
--    where
----        showQ i (rel, shs,conj,r)
----         = "\nQuad "++show i++":\nrelation: "++showADL rel++":\nshifts: "++concat ["\n"++showADLe s |s<-shs]++"\nconjunct: "++showADL conj++"\nrule: "++showADL r++""
----TODO: Deze code komt ook voor in ADL2Fspec.hs. Dat lijkt dubbelop, en derhalve niet goed.
--        rels = nub (recur (ifcObj ifc))
--         where recur obj = [editMph (objctx o) | o<-attributes obj, editable (objctx o)]++[r | o<-attributes obj, r<-recur o]
--        vis        = nub (rels++map (I . target) rels)
--   --     visible r  = r `elem` vis
--        invs       = [rule | rule<-invariants fSpec, (not.null) (map makeDeclaration (relsUsedIn rule) `isc` vis)]
--   --     qs         = vquads fSpec
--   --     ecaRs      = assembleECAs qs
----        editable (ERel Rel{} _)  = True    --WHY?? Stef, welke functie is de juiste?? TODO deze functie staat ook in ADL2Fspec.hs, maar is daar ANDERS(!)...
----        editable _               = False
----        editMph (ERel r@Rel{} _) = r       --WHY?? Stef, welke functie is de juiste?? TODO deze functie staat ook in ADL2Fspec.hs, maar is daar ANDERS(!)...
----        editMph e                = fatal 64 $ "cannot determine an editable declaration in a composite expression: "++show e
--        -- De functie spread verspreidt strings over kolommen met een breedte van n.
--        -- Deze functie garandeert dat alle strings worden afgedrukt in de aangegeven volgorde.
--        -- Hij probeert daarbij zo weinig mogelijk regels te gebruiken,
--        -- en alleen de grens van n te overschrijden als een string zelf langer is dan n.
--        spread :: Int -> String -> [String] -> [String]
--        spread n str = f ""
--         where f stored []       = [stored | not (null stored)]
--               f [] (cs:css)     = f cs css
--               f stored (cs:css) | length stored > n = stored: f cs css
--                                 | length new   <= n = f new css 
--                                 | otherwise         = stored: f cs css
--                                   where new = stored++str++cs

   interText _ [] = ""
   interText str (xs:xss) = xs<>str<>interText str xss
   
   deriveProofs :: Options -> Fspc -> Blocks
   deriveProofs flags fSpec
    = para ("Rules and their conjuncts for "<>(str.name) fSpec)<>
--   conjuncts = map disjuncts.exprIsc2list.conjNF.rrexp
      bulletList [ para ("rule r:   "<>str (showADL r)<>linebreak<>
                         "rrexp r:  "<>str (showADL (rrexp r))<>linebreak<>
                         "conjNF:   "<>str (showADL (conjNF (rrexp r)))<>linebreak<>
                         interText linebreak [ "     conj: "<>str (showADL conj) | conj<-conjuncts r]
                        )
                 | r<-grules fSpec++vrules fSpec]<>
      para ("Transformation of user specified rules into ECA rules for "<>(str.name) fSpec)<>
      para (linebreak<>"--------------"<>linebreak<>"First step: determine the "<>(str.show.length) qs<>" quads:")<>
      bulletList [ para ( "-- quad ------------"<>linebreak<>"When relation "<>str (showADL rel)<>" is changed,"
                          <>linebreak<>str (showADL r)
                          <>(if length (cl_conjNF ccrs)<=1 then space else " ("<>(str.show.length.cl_conjNF) ccrs<>" conjuncts)")
                          <>" must be restored."<>linebreak<>"This quad has conjunct: "<>(str.showADL.rc_conjunct) x
                          <>" and "<>(str.show.length.rc_dnfClauses) x<>" dnf clauses."
                        ) <>
                   bulletList [ para (linebreak<>"Dnf clause "<>str (showADL dc)) | dc<-rc_dnfClauses x]
                 | Quad rel ccrs<-qs, let r=cl_rule ccrs , x<-cl_conjNF ccrs ] <>
      para (linebreak<>linebreak<>"Second step: assemble dnf clauses.") <>
      bulletList [ para ( "Dnf clause "<>str (showADL dc)
                          <>linebreak<>"is derived from rule "<>str (showADL r)
                          <>linebreak
                          <>case ms of
                               []    -> "No relations affect this clause."
                               [rel] -> "It can be called when relation " <>str (showADL rel)<>" is affected."
                               _     -> "It can be called when relations "<>str (commaEng "or" [showADL rel | rel<-ms])<>" are affected."
                        )
                 | (ms,dc,r)<-
                       [ (nub [ dcl |(dcl,_,_)<-cl],dc,r)
                       | cl<-eqCl (\(_,_,dc)->dc) [(dcl,dc,r) |Quad dcl ccrs<-qs, let r=cl_rule ccrs, x<-cl_conjNF ccrs, dc<-rc_dnfClauses x]
                       , let (_,dc,r) = head cl
                       ]
                 ]<>
      para (linebreak<>"Third step: determine "<>(str.show.length.udefrules) fSpec<>" ECA rules"<>
            if verboseP flags
             then " (Turn --verbose off if you want to see ECA rules only)"
             else " (Turn on --verbose if you want to see more detail)"
           )<>
      bulletList [ para ( "--------------"<>linebreak)<>derivation rule
                 | verboseP flags, rule<-udefrules fSpec]<>
      bulletList [ para ( "-- ECA Rule "<>(str.show.ecaNum) ecarule<>" ---------"<>linebreak
                          <>str (showECA "\n  " ecarule{ecaAction=normPA (ecaAction ecarule)}))<>
                   bulletList [ para (linebreak<>"delta expression"<>linebreak<>space<>str (showADL d)
                                      <>linebreak<>"derivation:"
                                     )<>
                                (showProof showADL.nfProof showADL) d<>  -- nfProof produces its result in disjunctive normal form
                                para ("disjunctly normalized delta expression"<>linebreak<>(str.showADL.disjNF) d)
                              | verboseP flags, e@Do{}<-[ecaAction ecarule], let d = paDelta e ]
                 | ecarule <- ecaRs]
      {-
      ++
      [ linebreak<>"--------------", linebreak, linebreak<>"Fourth step: cascade blocking rules"
      , linebreak
      ]++
      interText []
        [ [linebreak<>"-- Raw ECA rule "<>(str.show.ecaNum) er<>"------------"<>linebreak<>str (showECA "\n  " er)]
        | er<- ecaRs]
      ++
      [ linebreak<>"--------------", linebreak, linebreak<>"Fifth step: preEmpt the rules (= optimize)"
      , linebreak
      ]++
{- TODO: readdress preEmpt. It is wrong
      interText []
        [ [linebreak<>"-- Preempted ECA rule "<>(str.show.ecaNum) er<>"------------"<>linebreak<>str (showECA "\n  " er)]
        | er<- preEmpt ecaRs]
      ++ -}
{-
      [ linebreak<>"--------------", linebreak]
      ++ -- TODO: make an ontological analysis, which explains the delete behaviour.
      [ Str "Ontological analysis: ", linebreak<>"  "]
      ++
      interText [linebreak, linebreak<>"  "] 
          [ [Str (name ifc)<>"("]
            ++ interText ", "
                 [str (name a)<>"["<>(str.name.target.ctx) a<>"]"
                 |a<-attributes (ifcObj ifc)]
            ++ [Str "):", linebreak<>"  "]
          | ifc<-interfaceS fSpec]
      ++
      [ linebreak<>"--------------", linebreak
      <>"Analyzing interfaces:", linebreak<>"     "]
      ++
      interText [linebreak<>"     "] 
         [[Str (testInterface flags fSpec ifc)]
         | ifc<-take 1 (interfaceG fSpec)]
      ++
      [ linebreak<>"--------------", linebreak]
      -}
-}
      where 
  --     visible _  = True -- We take all quads into account.
       qs         = vquads fSpec  -- the quads that are derived for this fSpec specify dnf clauses, meant to maintain rule r, to be called when relation rel is affected (rel is in r).
          --   assembleECAs :: [Quad] -> [ECArule]
       ecaRs      = assembleECAs qs  -- this creates all ECA rules from the available quads. They are still raw (unoptimized).
       commaEng :: String -> [String] -> String
       commaEng  _  []       = ""
       commaEng  _  [x]      = x
       commaEng chs [x,y]    = x++" "++chs++" "++y
       commaEng chs (x:y:ys) = x++", "++commaEng chs (y:ys)
       derivation :: Rule -> Blocks
       derivation rule 
         = ( if exx'==e
             then para (str (showADL rule)<>" is already in conjunctive normal form")
             else para ("Convert into conjunctive normal form")
           )<>
           showProof showADL ((e,[],"<=>"):prf)<>
           para "Violations are computed by (disjNF . ECpl . normexpr) rule:\n     "<>
           (disjProof. notCpl. rrexp) rule<>
           bulletList
               [ para("Conjunct: "<>str (showADL (rc_conjunct x))<>
                      if length (rc_dnfClauses x)>1 then " has "<>(str.show.length.rc_dnfClauses) x<>" clauses:" else "" )<>
                 bulletList [ para ("Clause: "<>str (showADL clause)) | clause<-(rc_dnfClauses x)]<>
                 para (if length (rc_dnfClauses x)>1 then "For each clause<>let us analyse the insert- and delete events." else "")<>
                 bulletList
                     [ para ("Clause: "<>str (showADL clause)<>" may be affected by the following events:")<>
                       bulletList
                           [ para ("event = "<>str (show ev)<>space<>str (showADL dcl)<>" means doing the following substitution")<>
                             para (str (showADL clause<>"["<>showREL dcl<>":="<>showADL (actSem ev (EDcD dcl) (delta (sign dcl)))<>"] = clause'"))<>
                             para ("clause' = "<>str (showADL ex')<>
                                   if clause'==ex'
                                   then ", which is already in conjunctive normal form."<>linebreak
                                   else ", which has conjunctive normal form: "<>linebreak<>str (showADL clause')
                                  )<>
                             para ("Let us compute the violations to see whether invariance is maintained."<>linebreak<>
                                   "This means to negate the result (notClau = notCpl clause'): ")<>
                             (showProof showADL. cfProof showADL) notClau<>
                             para ("So, notClau has CNF: "<>str (showADL viols )<>linebreak<>
                                   ( if viols==viols'
                                     then "This expression is in disjunctive normal form as well."
                                     else str ("In DNF, notClau is:  "<>showADL viols'<>".")))<>
                             ( if isTrue clause'
                               then para ("This result proves the absence of violations, so a reaction of doing nothing is appropriate."<>linebreak
                                          <>"Just for fun, let us try to derive whether clause |- clause' is true... ")<>
                                    (showProof showADL. cfProof showADL) (expr .|-. clause')
                               else para ("This result does not prove the absence of violations, so we cannot conclude that invariance is maintained."<>linebreak<>
                                          "We must compute a reaction to compensate for violations..."<>linebreak<>
                                          "That would be to reinsert violations that originate from "<>
                                          ( if ev==Ins
                                            then str (showADL (conjNF negs))<>" into "<> str (showADL (disjNF poss))<>"."
                                            else str (showADL (disjNF poss))<>" into "<> str (showADL (conjNF negs))<>"."
                                          )<>linebreak<>"deltFr: ")<>
                                    (showProof showADL. dfProof showADL) deltFr<>
                                    para "This yields the following ECA action:"<>
                                    codeBlock (showECA "\n     " (ECA (On ev dcl) delt act 0))
                             {-     <> "To finish the analysis of case "<>str (show ev)<>space<>str (showADL dcl)
                                       <>", let us compute the contents of "<>str (showADL toExpr)<>" after insertion of viols."<>linebreak
                                       <>
                                    ( if length (nub [sign viols, sign viols', sign toExpr])>1
                                      then fatal 248 ("viols"<>showSign (sign viols) <>"   "<>showADL viols <>"\n"<>
                                                      "viols'"<>showSign (sign viols')<>"  "<>showADL viols'<>"\n"<>
                                                      "toExpr"<>showSign (sign toExpr)<>"  "<>showADL toExpr)
                                      else if ev==Ins
                                      then (showProof showADL. cfProof showADL) (viols'.\/.toExpr)<>linebreak
                                      else (showProof showADL. dfProof showADL) (notCpl viols./\.toExpr)<>linebreak
                                    ) -}
                             )
                           | dcl <- relsUsedIn r
                           , let EDcD delt = delta (sign dcl)  -- delt is a placeholder for the pairs that have been inserted or deleted in rel.
                           , ev<-[Ins,Del]
                           , let ex'     = subst (dcl, actSem ev (EDcD dcl) (delta (sign dcl))) expr -- the clause after the edit action
                           , let clause' = conjNF ex'                                                -- its CNF
                           , let notClau = notCpl clause'                                            -- the violations after the edit action
                           , let viols   = conjNF notClau                                            -- the violations after the edit action
                           , let viols'  = disjNF notClau                                            -- the violations after the edit action
                           , let negs    = if (length.nub.map sign) antcs>1
                                           then fatal 265 ("type inconsistencies in antcs: "<>show (map showADL antcs))
                                           else foldr (./\.) vee antcs
                           , let poss    = if (length.nub.map sign) conss>1
                                           then fatal 265 ("type inconsistencies in conss: "<>show (map showADL conss))
                                           else foldr (.\/.) (notCpl vee) conss
                           , let frExpr  = if ev==Ins
                                           then disjNF (notCpl negs)
                                           else disjNF poss
                           , let deltFr  = if sign poss/=sign negs
                                           then fatal 274 ("type inconsistencies in deltFr: "<>showADL clause)
                                           else if ev==Ins
                                           then (subst (dcl, actSem ev (EDcD dcl) (delta (sign dcl)))) negs ./\. notCpl poss
                                           else (notCpl . subst (dcl, actSem ev (EDcD dcl) (delta (sign dcl)))) poss ./\. negs
                           , let deltFr' = disjNF deltFr
                           , dcl `elem` relsMentionedIn frExpr
                           , let toExpr  = if ev==Ins
                                           then disjNF poss
                                           else disjNF (notCpl negs)
                           , let visible _ = True
                           , if length (nub (map sign [toExpr, deltFr', expr]))>1
                             then fatal 285 "type problem"
                             else True
                           , let act = genPAclause visible Ins toExpr deltFr' [(expr,[rule])]
                           ]
                     | clause@(Dnf antcs conss) <- rc_dnfClauses x
                     , let expr = dnf2expr clause, let vee = EDcV (sign expr)
                     ]
               | let Clauses ts r = allClauses flags rule, x <-ts
               ]
{-
           [ str ("Available code fragments on rule "<>name rule<>":", linebreak ]<>
           interText [linebreak] [showADL rule<> " yields\n"<>interText "\n\n"
                                   [ ["event = ", str (show ev), space, str (showADL rel), linebreak ] <>
                                     [str (showADL r<>"["<>showADL rel<>":="<>showADL (actSem ev (EDcD rel) (delta (sign rel)))<>"] = r'"), linebreak ] <>
                                     ["r'    = "] <> conjProof r' <> [linebreak ] <>
                                     ["viols = r'-"] <> disjProof (ECpl r') <> [ linebreak ] <>
                                     "violations, considering that the valuation of "<>showADL rel<>" has just been changed to "<>showADL (actSem ev (EDcD rel) (delta (sign rel)))<>
                                     "            "<>conjProof (ECpl r) <>"\n"<>
                                     "reaction? evaluate r |- r' ("<>(str.showADL.conjNF) (notCpl r .\/. r')<>")"<>
                                        conjProof (notCpl r .\/. r')<>"\n"<>
                                     "delta: r-/\\r' = "<>conjProof (EIsc[notCpl r,r'])<>
                                     "\nNow compute a reaction\n(isTrue.conjNF) (notCpl r .\/. r') = "<>show ((isTrue.conjNF) (notCpl r .\/. r'))<>"\n"<>
                                     (if null (lambda ev (ERel rel ) r)
                                      then "lambda "<>showADL rel<>" ("<>showADL r<>") = empty\n"
                                      else -- for debug purposes:
                                           -- "lambda "<>show ev<>" "<>showADL rel<>" ("<>showADL r<>") = \n"<>(interText "\n\n".map showPr.lambda ev (ERel rel)) r<>"\n"<>
                                           -- "derivMono ("<>showADL r<>") "<>show ev<>" "<>showADL rel<>"\n = "<>({-interText "\n". map -}showPr.derivMono r ev) rel<>"\n"<>
                                           -- "\nNow compute checkMono r ev rel = \n"<>show (checkMono r ev rel)<>"\n"<>
                                           if (isTrue.conjNF) (notCpl r .\/. r')
                                           then "A reaction is not required, because  r |- r'. Proof:"<>conjProof (notCpl r .\/. r')<>"\n"
                                           else if checkMono r ev rel
                                           then "A reaction is not required, because  r |- r'. Proof:"{-<>(str.showPr.derivMono r ev) rel-}<>"NIET TYPECORRECT: (showPr.derivMono r ev) rel"<>"\n"  --WHY? Stef, gaarne herstellen...Deze fout vond ik nadat ik het type van showProof had opgegeven.
                                           else let ERel _ _ = delta (sign rel) in
                                                "An appropriate reaction on this event is required."
                                           --     showECA "\n  " (ECA (On ev rel) delt (genPAclause visible Ins r viols conj [rule]) 0)
                                     )
                                   | rel<-relsUsedIn r   -- nub [x |x<-relsUsedIn r, not (isIdent x)] -- TODO: include proofs that allow: isIdent rel'
                                   , ev<-[Ins,Del]
                                   , r'<-[subst (rel, actSem ev (EDcD rel) (delta (sign rel))) r]
                        --        , viols<-[conjNF (ECpl r')]
                                   , True ]  -- (isTrue.conjNF) (notCpl r .\/. r')
                                  | r<-[dc | cs<-[allClauses flags rule], (_,dnfClauses)<-cl_conjNF cs, dc<-dnfClauses]
                                  ]
-}
           where e = rrexp rule
                 prf = cfProof showADL e
                 (exx',_,_) = last prf
            --     conjProof = showProof showADL . cfProof showADL
                 disjProof = showProof showADL . dfProof showADL
--                 showPr    = showProof showADL  -- hoort bij de uitgecommentaarde code hierboven...
       --TODO: See ticket #105

-- Stel we voeren een actie a uit, die een(1) van de volgende twee is:
--        {r} INS rel INTO expr {r'}       ofwel
--        {r} DEL rel FROM expr {r'}
-- Dan toetst checkMono of r|-r' waar is op grond van de afleiding uit derivMono.
-- Als dat waar is, betekent dat dat invariant r waar blijft wanneer actie a wordt uitgevoerd.
   checkMono :: Expression
             -> InsDel
             -> Declaration
             -> Bool
   checkMono expr ev dcl
     = case ruleType conclusion of
        Truth -> fatal 247 "derivMono came up with a Truth!"
        _     -> simplify expr == simplify (antecedent conclusion) &&
                 simplify (subst (dcl, actSem ev (EDcD dcl) (delta (sign dcl))) expr) ==
                 simplify (consequent conclusion)
     where (conclusion,_,_) = last (derivMono expr ev dcl)


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

   showProof :: (expr->String) -> Proof expr -> Blocks
   showProof shw [(expr,_,_)]        = (para.str.shw) expr
   showProof shw ((expr,ss,equ):prf) = (para.str.shw) expr<>
                                       para (if null ss  then str equ else
                                             if null equ then str (unwords ss) else
                                             str equ<>str (" { "++intercalate " and " ss++" }"))<>
                                       showProof shw prf
                                       --where e'= if null prf then "" else let (expr,_,_):_ = prf in showHS options "" expr 
   showProof _  []                   = fromList []

   showPrf :: (expr->String) -> Proof expr -> [String]
   showPrf shw [(expr,_,_)]        = [ "    "++shw expr]
   showPrf shw ((expr,ss,equ):prf) = [ "    "++shw expr] ++
                                     (if null ss  then [ equ ] else
                                      if null equ then [ unwords ss ] else
                                      [ equ++" { "++intercalate " and " ss++" }" ])++
                                     showPrf shw prf
   showPrf _  []                   = []

-- derivMono provides a derivation to prove that (precondition) r is a subset of (postcondition) r'.
-- This is useful in proving that an action {expr} a {expr'} maintains its invariant, i.e. that  expr|-expr'  holds (proven by monotony properties)
-- Derivmono gives a derivation only.
   derivMono :: Expression -> InsDel -> Declaration -> [(Rule, [String], String)]
   derivMono expr -- preconditie van actie a
             tOp  -- de actie (Ins of Del)
             dcl' -- re relatie, zodat de actie bestaat uit INSERT rel' INTO expr of DELETE rel' FROM expr
    = f (head (lambda tOp (EDcD dcl') expr++[[]])) (start tOp)
    where
     f :: [(Expression, [String], whatever)] 
        -> (Expression, Expression) 
        -> [(Rule, [String], String)]
     f [] (_,_) = []
     f [(e',_,_)] (neg',pos')
      = [(rule (subst (dcl',neg') e') (subst (dcl',pos') e'),[],"")]
     f ((e',["invert"],_): prf@((_,_,_):_)) (neg',pos')
      = (rule (subst (dcl',neg') e') (subst (dcl',pos') e'),["r |- s  <=>  s- |- r-"],"<=>"):
         f prf (pos',neg')
     f ((e1,_,_): prf@((e2,_,_):_)) (neg',pos')
      = (rule (subst (dcl',neg') e1) (subst (dcl',pos') e1),["Monotony of "++showOp e2],"==>"):
         f prf (neg',pos')
         
     start Ins  = (EDcD dcl',EDcD dcl' .\/. delta (sign dcl'))
     start Del  = (EDcD dcl' ./\. notCpl (delta (sign dcl')),EDcD dcl')

     rule :: Expression -> Expression -> Rule
     rule neg' pos' | isTrue neg' = Ru { rrnm  = ""
                                       , rrfps = Origin "rule generated for isTrue neg' by Calc"
                                       , rrexp = pos'
                                       , rrmean = AMeaning
                                                  [A_Markup Dutch   ReST (string2Blocks ReST "Waarom wordt deze regel hier aangemaakt? (In Calc.hs, regel 402)")
                                                  ,A_Markup English ReST (string2Blocks ReST "Why is this rule created? (In Calc.hs, line 403)")]  --TODO Stef, gaarne de explanations aanvullen/verwijderen. Dank! Han.
                                       , rrmsg = []
                                       , rrviol = Nothing
                                       , rrtyp = sign neg' {- (neg `meet` pos) -}
                                       , rrdcl = Nothing
                                       , r_env = ""
                                       , r_usr = Multiplicity
                                       , isSignal = fatal 336 $ "erroneous reference to isSignal in rule ("++showADL neg'++") |- ("++showADL pos'++")"
                                       , srrel = fatal 337 $ "erroneous reference to srrel in rule ("++showADL neg'++") |- ("++showADL pos'++")"
                                       }
                    | otherwise   = Ru { rrnm  = ""
                                       , rrfps = Origin "rule generated for not(isTrue neg') by Calc"
                                       , rrexp = neg' .|-. pos'
                                       , rrmean = AMeaning
                                                  [A_Markup Dutch   ReST (string2Blocks ReST "Waarom wordt deze regel hier aangemaakt? (In Calc.hs, regel 332)")
                                                  ,A_Markup English ReST (string2Blocks ReST "Why is this rule created? (In Calc.hs, line 333)")]  --TODO Stef, gaarne de explanations aanvullen/verwijderen. Dank! Han.
                                       , rrmsg = []
                                       , rrviol = Nothing
                                       , rrtyp = sign neg' {- (neg `meet` pos) -}
                                       , rrdcl = Nothing
                                       , r_env = ""
                                       , r_usr = Multiplicity
                                       , isSignal = fatal 352 $ "illegal reference to isSignal in rule ("++showADL neg'++") |- ("++showADL pos'++")"
                                       , srrel = fatal 353 $ "illegal reference to srrel in rule ("++showADL neg'++") |- ("++showADL pos'++")"
                                       }
     showOp expr' = case expr' of
                     EEqu{} -> "="
                     EImp{} -> "|-"
                     EIsc{} -> "/\\"
                     EUni{} -> "\\/"
                     EDif{} -> "-"
                     ELrs{} -> "/"
                     ERrs{} -> "\\"
                     EDia{} -> "<>"
                     ECps{} -> ";"
                     ERad{} -> "!"
                     EPrd{} -> "*"
                     EKl0{} -> "*"
                     EKl1{} -> "+"
                     EFlp{} -> "~"
                     ECpl{} -> "-"
                     _      -> ""

{- The purpose of function lambda is to generate a derivation.
Rewrite rules:
-r;-s -> -(r!s)
-}
   lambda :: InsDel -> Expression 
                    -> Expression 
                    -> [Proof Expression]
   lambda tOp' e' expr' = [reversePrf[(e'',text,op)
                          | (e'',_,text,op)<-prf]
                          | prf<-lam tOp' e' expr' ]
    where
     lam :: InsDel -> Expression -> Expression ->
            [[(Expression,Expression -> Expression,[String],String)]]
     lam tOp e3 expr =
          case expr of
             EIsc{}  | e3==expr             -> [[(e3,id,[],"")]]
                     | length (const' expr)>0 -> [(expr,\_->expr,      [derivtext tOp "mono" (inter' expr) expr],"<--") :prf
                                                 | prf<-lam tOp e3 (inter' expr)
                                                 ]
                     | and [isNeg f |f<-exprIsc2list expr]
                                            -> let deMrg = deMorganEIsc expr in
                                               [(expr, deMorganEIsc, [derivtext tOp "equal" deMrg expr],"==") :prf | prf<-lam tOp e3 deMrg]
                     | or[null p |p<-fPrfs] -> []
                     | otherwise            -> [(expr,\_->expr,    [derivtext tOp "mono" (first lc) expr],"<--") : lc]
             EUni{}  | e3==expr             -> [[(e3,id,[],"")]]
                     | length (const' expr)>0 -> [(expr,\_->expr, [derivtext tOp "mono" (inter' expr) expr],"<--") :prf
                                                      | prf<-lam tOp e3 (inter' expr)
                                                      ]
                     | and [isNeg f |f<-exprUni2list expr]
                                            -> let deMrg = deMorganEUni expr in
                                               [(expr, deMorganEUni, [derivtext tOp "equal" deMrg expr],"==") :prf | prf<-lam tOp e3 deMrg]
                     | or[null p |p<-fPrfs] -> []
                     | otherwise            -> [(expr,\_->expr,    [derivtext tOp "mono" (first lc) expr],"<--") : lc]
             ECps{}  | e3==expr             -> [[(e3,id,[],"")]]
                     | and [isNeg f |f<-exprCps2list expr]
                                            -> let deMrg = deMorganECps expr in
                                               [(expr, deMorganECps, [derivtext tOp "equal" deMrg expr],"==")
                                               :prf
                                               | prf<-lam tOp e3 deMrg
                                               ] -- isNeg is nog niet helemaal correct.
                     | or[null p|p<-fPrfs]  -> []
                     | otherwise            -> [(expr,\_->expr,    [derivtext tOp "mono" (first lc) expr],"<--"): lc]        
             ERad{}  | e3==expr             -> [[(e3,id,[],"")]]
                     | and [isNeg f |f<-exprRad2list expr]
                                            -> let deMrg = deMorganERad expr in
                                               [(expr, deMorganERad, [derivtext tOp "equal" deMrg expr],"==") :prf | prf<-lam tOp e3 deMrg] -- isNeg is nog niet helemaal correct.
                     | or[null p |p<-fPrfs] -> []
                     | otherwise            -> [(expr,\_->expr,    [derivtext tOp "mono" (first lc) expr],"<--"): lc]
             EKl0 x                         -> [(expr,\e->EKl0 e,[derivtext tOp "mono" x expr],"<--") :prf   | prf<-lam tOp e3 x]
             EKl1 x                         -> [(expr,\e->EKl1 e,[derivtext tOp "mono" x expr],"<--") :prf   | prf<-lam tOp e3 x]
             ECpl x                         -> [(expr,\e->ECpl e,["invert"],"<--") :prf | prf<-lam (inv tOp) e3 x]
             EBrk x                         -> lam tOp e3 x
             _                              -> [[(e3,id,[],"")]]
           where
               sgn   = sign expr
               fPrfs = case expr of
                        EUni{} -> [lam tOp e3 f |f<-exprUni2list expr, isVar f e3]
                        EIsc{} -> [lam tOp e3 f |f<-exprIsc2list expr, isVar f e3]
                        ECps{} -> [lam tOp e3 f |f<-exprCps2list expr, isVar f e3]
                        ERad{} -> [lam tOp e3 f |f<-exprRad2list expr, isVar f e3]
                        _      -> fatal 428 ("fPrfs  is not defined.Consult your dealer!")
               lc = longstcomn vars++concat (drop (length rc-1) (sortWith length rc))
               rc = remainders vars vars
               vars = map head fPrfs
               const' e@EUni{} = [f |f<-exprUni2list e, isConst f e3]
               const' e@EIsc{} = [f |f<-exprIsc2list e, isConst f e3]
               const' expr'' = fatal 440 $ "'const'("++ show expr''++")' is not defined.Consult your dealer!"
               inter' e@EUni{} = foldr (.\/.) (notCpl (EDcV sgn)) [f |f<-exprUni2list e, isVar f e3]
               inter' e@EIsc{} = if and [sgn==sign f | f<-exprIsc2list e, isVar f e3]
                                 then foldr (./\.) (EDcV sgn) [f | f<-exprIsc2list e, isVar f e3]
                                 else fatal 532 ("signature error in inter'  "++show [(showADL f,showSign (sign f)) | f<-exprIsc2list e, isVar f e3])
               inter' expr'' = fatal 443 $ "'inter'("++ show expr''++")' is not defined.Consult your dealer!"
 --      lam tOp e f       = []

  -- longstcomn determines the longest prefix common to all xs in xss.
     longstcomn :: (Eq a) => [[(a, b, c, d)]] -> [(a, b, c, d)]
     longstcomn xss | or [null xs | xs<-xss]      = []
                    | length (eqCl first xss)==1 = head [head prf | prf<-xss]: longstcomn [tail prf | prf<-xss]
                    | otherwise                  = []
    -- remainders determines the remainders.
     remainders :: (Eq a) => [[(a, b, c, d)]] -> [[(a, b, c, d)]] -> [[(a, b, c, d)]]
     remainders _ xss | or [null xs | xs<-xss]      = xss
                      | length (eqCl first xss)==1 = remainders xss [tail prf | prf<-xss]
                      | otherwise                  = xss
     isConst :: (ConceptStructure a, ConceptStructure b) => a->b->Bool
     isConst e f = null (relsUsedIn e `isc` relsUsedIn f)
     isVar :: (ConceptStructure a, ConceptStructure b) => a->b->Bool
     isVar e f   = not (isConst e f)
     derivtext :: InsDel -> String -> Expression -> Expression -> String
     derivtext tOp "invert" e'' expr = sh tOp++showADL e''++" means "++sh (inv tOp)++showADL expr++"."
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
     first _ = fatal 472 "wrong pattern in first"
     
   ruleType :: Rule -> RuleType
   ruleType r = case rrexp r of
                 EEqu{} -> Equivalence
                 EImp{} -> Implication
                 _      -> Truth


