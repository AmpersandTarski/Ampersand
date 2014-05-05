{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module DatabaseDesign.Ampersand.Fspec.ToFspec.Calc
            ( deriveProofs 
            , lambda
            , checkMono
            , showProof, showPrf, assembleECAs, conjuncts, genPAclause
          --  , testInterface
            )
where

   import DatabaseDesign.Ampersand.Basics         (fatalMsg,Collection (isc),Identified(..),eqCl,Flippable(..))
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
   import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms  (delta,conjNF,disjNF,cfProof,dfProof,nfProof,simplify,normPA,proofPA)
   import DatabaseDesign.Ampersand.Misc            (Lang(..),Options(..),PandocFormat(ReST),string2Blocks)
   import Text.Pandoc.Builder
   import Prelude hiding (head)
-- import Debug.Trace
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ToFspec.Calc"

   head :: [a] -> a
   head [] = fatal 30 "head must not be used on an empty list!"
   head (a:_) = a

   conjuncts :: Rule -> [DnfClause]
   conjuncts = map disjuncts.exprIsc2list.conjNF.rrexp
   disjuncts :: Expression -> DnfClause
   disjuncts conj = (split (Dnf [] []).exprUni2list) conj
    where split :: DnfClause -> [Expression] -> DnfClause
          split (Dnf antc cons) (ECpl e: rest) = split (Dnf (e:antc) cons) rest
          split (Dnf antc cons) (     e: rest) = split (Dnf antc (e:cons)) rest
          split dc             []             = dc


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
--                                 ++"\n------ Derivation ----->"++showProof (codeBlock.showECA "\n>     ") (proofPA (ecaAction eca))++"\n<------End Derivation --"
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

   deriveProofs :: Options -> Fspc -> Blocks
   deriveProofs flags fSpec
    = para ("Rules and their conjuncts for "<>(str.name) fSpec)<>
      bulletList [ para ("rule r:   "<>str (showADL r)<>linebreak<>
                         "rrexp r:  "<>str (showADL (rrexp r))<>linebreak<>
                         "conjNF:   "<>str (showADL (conjNF (rrexp r)))<>linebreak<>
                         interText linebreak [ "     conj: "<>str (showADL conj) | conj<-conjuncts r ]
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
      ( if verboseP flags then para ( "--------------"<>linebreak)<>bulletList derivations else fromList [] )<>
      bulletList [ para ( "-- ECA Rule "<>(str.show.ecaNum) ecarule<>" ---------")<>
                   codeBlock (showECA "\n  " ecarule{ecaAction=normPA (ecaAction ecarule)})<>
                   bulletList [ para (linebreak<>"delta expression"<>linebreak<>space<>str (showADL d)
                                      <>linebreak<>"derivation:"
                                     )<>
                                (showProof (para.str.showADL).nfProof showADL) d<>  -- nfProof produces its result in disjunctive normal form
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
       commaEng :: String -> [String] -> String
       commaEng  _  []       = ""
       commaEng  _  [x]      = x
       commaEng chs [x,y]    = x++" "++chs++" "++y
       commaEng chs (x:y:ys) = x++", "++commaEng chs (y:ys)
    -- interText :: (Data.String.IsString a, Data.Monoid.Monoid a) => a -> [a] -> a
       interText _ [] = ""
       interText inbetween (xs:xss) = xs<>inbetween<>interText inbetween xss
       derivations :: [Blocks]
       ecaRs :: [ECArule]
       (ecaRs, derivations) = unzip (assembleECAs qs)
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
           where e = rrexp rule
                 prf = cfProof showADL e
                 (exx',_,_) = last prf
            --     conjProof = showProof (para.str.showADL) . cfProof showADL
                 disjProof = showProof (para.str.showADL) . dfProof showADL
--                 showPr    = showProof (para.str.showADL)  -- hoort bij de uitgecommentaarde code hierboven...
       --TODO: See ticket #105
-}

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

   showProof :: (expr->Blocks) -> Proof expr -> Blocks
   showProof shw [(expr,_,_)]        = shw expr
   showProof shw ((expr,ss,equ):prf) = shw expr<>
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
   lambda tOp' e' expr' = [reversePrf[(e'',txt,op)
                          | (e'',_,txt,op)<-prf]
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
     derivtext _ txt _ _ = txt
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


   -- | Action semantics for inserting a delta into a relation dcl.
   actSem :: InsDel -> Expression -> Expression -> Expression
   actSem Ins dcl delt | sign dcl/=sign delt = fatal 598 "Type error in actSem Ins"
                       | dcl==delt           = dcl
                       | otherwise           = disjNF (dcl .\/. delt)
   actSem Del dcl delt | sign dcl/=sign delt = fatal 598 "Type error in actSem Del"
                       | dcl==delt           = notCpl (EDcV (sign dcl))
                       | otherwise           = conjNF (dcl ./\. notCpl delt)

-- | assembleECAs  assembles larger chunks of code, because it combines acts that are triggered by the same event.
   assembleECAs :: [Quad] -> [(ECArule,Blocks)]
   assembleECAs qs
    = [eca i | (eca,i) <- zip ecas [(1::Int)..]]
      where
       ecas :: [Int->(ECArule,Blocks)]
       ecas
         = [ (\ruleNr->( ECA ecaEvt delt normEcaAct ruleNr
                       , para ("Let us analyse what happens "<>str (show (On ev dcl))<>".")<>
                         bulletList [ txt | (_,_,_,txt)<-acts]<>
                         ( if length ecaProof>1
                           then para ("The resulting action is:")<>
                                showProof (codeBlock.showECA "\n>     ") ecaProof
                           else fromList []
                         )<>
                         para ("These results lead to the following ECA-rule:")<>
                         (codeBlock.showECA "\n     ".ecaRule) ruleNr
                       )
             )
           | relEq <- eqCl qDcl qs               -- Gather the quads with the same declaration (qDcl). A quad has a declaration (qDcl) and clauses (qClauses)
           , let dcl = (qDcl.head) relEq         -- This is the relation in which a delta is being inserted or deleted.
           , let quadClauses = (nub.map qClauses) relEq -- these are the clauses
           , let EDcD delt   = delta (sign dcl)  -- delt is a placeholder for the pairs that have been inserted or deleted in rel.
           , ev<-[Ins,Del]
           , let acts = [ -- go through all the events that affect that clause:
                          (normPA act, conj, rule,
                          para ("Let us analyse clause "<>str (showADL expr)<>" from rule "<>(singleQuoted.str.name) rule<>".")<>
                          para ("event = "<>str (show ev)<>space<>str (showREL dcl)<>" means doing the following substitution")<>
                          para (str (showADL clause<>"["<>showREL dcl<>":="<>showADL (actSem ev (EDcD dcl) (delta (sign dcl)))<>"] = clause'"))<>
                          para ("clause' = "<>str (showADL ex')<>
                                if clause'==ex'
                                then ", which is already in conjunctive normal form."<>linebreak
                                else ", which has conjunctive normal form: "<>linebreak<>str (showADL clause')
                               )<>
                          para ("Let us compute the violations to see whether invariance is maintained."<>linebreak<>
                                "This means to negate the result (notClau = notCpl clause'): ")<>
                          (showProof (para.str.showADL). cfProof showADL) notClau<>
                          para ("So, notClau has CNF: "<>str (showADL viols )<>linebreak<>
                                ( if viols==viols'
                                  then "This expression is in disjunctive normal form as well."
                                  else str ("In DNF, notClau is:  "<>showADL viols'<>".")))<>
                          ( if isTrue clause'
                            then para ("This result proves the absence of violations, so a reaction of doing nothing is appropriate."<>linebreak
                                       <>"Just for fun, let us try to derive whether clause |- clause' is true... ")<>
                                 (showProof (para.str.showADL). cfProof showADL) (expr .|-. clause')
                            else para ("This result does not prove the absence of violations, so we cannot conclude that invariance is maintained."<>linebreak<>
                                       "We must compute a reaction to compensate for violations..."<>linebreak<>
                                       "That would be to reinsert violations that originate from "<>
                                       ( if ev==Ins
                                         then str (showADL (conjNF negs))<>" into "<> str (showADL (disjNF poss))<>"."
                                         else str (showADL (disjNF poss))<>" into "<> str (showADL (conjNF negs))<>"."
                                       )<>linebreak<>"deltFr: ")<>
                                 (showProof (para.str.showADL). dfProof showADL) deltFr<>
                                 ( let pr=proofPA act in
                                   if length pr>1
                                   then para "Now let us remove redundancy from the ECA action:"<>
                                        showProof (codeBlock.showECA "\n     ") (proofPA act)
                                   else fromList []
                                 )
                          {-     <> "To finish the analysis of case "<>str (show ev)<>space<>str (showADL dcl)
                                    <>", let us compute the contents of "<>str (showADL toExpr)<>" after insertion of viols."<>linebreak
                                    <>
                                 ( if length (nub [sign viols, sign viols', sign toExpr])>1
                                   then fatal 248 ("viols"<>showSign (sign viols) <>"   "<>showADL viols <>"\n"<>
                                                   "viols'"<>showSign (sign viols')<>"  "<>showADL viols'<>"\n"<>
                                                   "toExpr"<>showSign (sign toExpr)<>"  "<>showADL toExpr)
                                   else if ev==Ins
                                   then (showProof (para.str.showADL). cfProof showADL) (viols'.\/.toExpr)<>linebreak
                                   else (showProof (para.str.showADL). dfProof showADL) (notCpl viols./\.toExpr)<>linebreak
                                 ) -}
                          ))
                        | Clauses ts rule<-quadClauses, conj<-ts                  -- get conjuncts from the clauses
                        , clause@(Dnf antcs conss) <- rc_dnfClauses conj          -- the DNF form of each clause
                        , let expr    = dnf2expr clause                           -- Note that this differs from:  rc_conjunct conj
                        , let vee     = EDcV (sign expr)
                        , let ex'     = subst (dcl, actSem ev (EDcD dcl) (delta (sign dcl))) expr -- the clause after the edit action
                        , let clause' = conjNF ex'                                                -- its CNF
                        , not (isTrue clause')
                        , let notClau = notCpl clause'                                            -- the violations after the edit action
                        , let viols   = conjNF notClau                                            -- the violations after the edit action
                        , let viols'  = disjNF notClau                                            -- the violations after the edit action
                        , let negs    = if (length.nub.map sign) (vee:antcs)>1
                                        then fatal 265 ("type inconsistencies in antcs: "++show (map showADL (vee:antcs)))
                                        else foldr (./\.) vee antcs
                        , let poss    = if (length.nub.map sign) (vee:conss)>1
                                        then fatal 265 ("type inconsistencies in conss: "++show (map showADL (vee:conss)))
                                        else foldr (.\/.) (notCpl vee) conss
                        , let frExpr  = if ev==Ins
                                        then disjNF (notCpl negs)
                                        else disjNF poss
                        , let deltFr  = if sign poss/=sign negs
                                        then fatal 274 ("type inconsistencies in deltFr: "++showADL clause)
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
           , let ecaAct = ALL (map fst4 acts)
                              [ (rc_conjunct conj,[rule]) | (_,conj,rule,_)<-acts] --motivation is of type [(Expression,[Rule])]
           , let normEcaAct = normPA ecaAct
           , let ecaProof = proofPA ecaAct
           , let ecaEvt = On ev dcl
           , let ecaRule = ECA ecaEvt delt normEcaAct
           ]
       fst4 (x,_,_,_) = x

-- | de functie genPAclause beschrijft de voornaamste mogelijkheden om een expressie delta' te verwerken in expr (met tOp'==Ins of tOp==Del)
-- TODO: Vind een wetenschappelijk artikel waar de hier beschreven transformatie uitputtend wordt behandeld.
-- TODO: Deze code is onvolledig en misschien zelfs fout....
   genPAclause :: (Declaration->Bool)        -- ^True if a relation may be changed (i.e. is editable)
                  -> InsDel                  -- ^the type of action: Insert or Delete
                  -> Expression              -- ^the expression in which a delete or insert takes place
                  -> Expression              -- ^the delta to be inserted or deleted
                  -> [(Expression,[Rule])]   -- ^the motivation, consisting of the conjuncts (traced back to their rules) that are being restored by this code fragment.
                  -> PAclause
   genPAclause editAble tOp' expr1 delta1 motive = genPAcl delta1 tOp' expr1
    where
      testPA i l r ex
       = if (source l,target r)/=(source ex,target ex)
         then fatal i ("test with sign deltaX = ["++show (source l)++"*"++show (target r)++"],  and sign expr = "++show (sign ex)++":\ndeltaX = "++showADL (l.:.r)++"\nexpr = "++show ex)
         else if source r/=target l
         then fatal i ("test with source r = "++show (source r)++",  and target l = "++show (target l)++":\nl"++showSign (sign l)++" = "++showADL l++"\nr"++showSign (sign r)++" = "++showADL r++"\nexpr = "++show ex)
         else id

      genPAcl deltaX tOp expr =
        case (tOp, expr) of
          (_  , EEqu{})     -> Blk [(expr, nub [r |(_,rs)<-motive, r<-rs])]
          (_  , EImp{})     -> Blk [(expr, nub [r |(_,rs)<-motive, r<-rs])]
          (_ ,  EFlp x)     -> genPAcl (flp deltaX) tOp x
          (_ ,  EBrk x)     -> genPAcl deltaX tOp x
          (Ins, ECpl x)     -> genPAcl deltaX Del x
          (Del, ECpl x)     -> genPAcl deltaX Ins x
          (Ins, EUni{})     -> CHC [ genPAcl deltaX Ins f | f<-exprUni2list expr{-, not (f==expr1 && Ins/=tOp') -}] motive -- the filter prevents self compensating PA-clauses.
          (Ins, EIsc{})     -> ALL [ genPAcl deltaX Ins f | f<-exprIsc2list expr ] motive
          (Del, EUni{})     -> ALL [ genPAcl deltaX Del f | f<-exprUni2list expr {-, not (f==expr1 && Del/=tOp') -}] motive -- the filter prevents self compensating PA-clauses.
          (Del, EIsc{})     -> CHC [ genPAcl deltaX Del f | f<-exprIsc2list expr ] motive
          (Ins, EDif (l,r)) -> CHC [ genPAcl deltaX Ins l, genPAcl deltaX Del r ] motive
          (Del, EDif (l,r)) -> CHC [ genPAcl deltaX Del l, genPAcl deltaX Ins r ] motive
          (Ins, EDia (l,r)) -> CHC [ ALL [ genPAcl (testPA 986 (deltaX) (flp r)          l $ deltaX.:.flp r         ) Ins l
                                         , genPAcl (testPA 987 (flp l) (deltaX)          r $ flp l.:.deltaX         ) Ins r] motive
                                   , ALL [ genPAcl (testPA 988 (deltaX) (notCpl (flp r)) l $ deltaX.:.notCpl (flp r)) Del l
                                         , genPAcl (testPA 989 (deltaX) (flp r)          l $ deltaX.:.flp r         ) Ins l] motive
                                   , ALL [ genPAcl (testPA 990 (notCpl (flp l)) (deltaX) r $ notCpl (flp l).:.deltaX) Del r
                                         , genPAcl (testPA 991 (flp l) (deltaX)          r $ flp l.:.deltaX         ) Ins r] motive
                                   , ALL [ genPAcl (testPA 992 (deltaX) (notCpl (flp r)) l $ deltaX.:.notCpl (flp r)) Del l
                                         , genPAcl (testPA 993 (notCpl (flp l)) (deltaX) r $ notCpl (flp l).:.deltaX) Del r] motive
                                   ] motive
          (Del, EDia (l,r)) -> GCH [ (Del, (testPA 995 (deltaX) (flp r)          l $ deltaX.:.flp r),          genPAcl (EMp1 "a" (source l).*.EMp1 "b" (target l)) tOp l)
                                   , (Ins, (testPA 996 (deltaX) (flp (notCpl r)) l $ deltaX.:.flp (notCpl r)), genPAcl (EMp1 "a" (source l).*.EMp1 "b" (target l)) tOp l)
                                   , (Del, (testPA 997 (flp l) (deltaX)          r $ flp l.:.deltaX),          genPAcl (EMp1 "a" (source r).*.EMp1 "b" (target r)) tOp r)
                                   , (Ins, (testPA 998 (notCpl (flp l)) (deltaX) r $ notCpl (flp l).:.deltaX), genPAcl (EMp1 "a" (source r).*.EMp1 "b" (target r)) tOp r)
                                   ] motive
          (Ins, ERrs (l,r)) -> CHC [ genPAcl (testPA 1000 (notCpl r) (flp deltaX) l $ notCpl r.:.flp deltaX) Del l
                                   , genPAcl (testPA 1001 (l) (deltaX)            r $ l.:.deltaX)            Ins r
                                   ] motive
          (Del, ERrs (l,r)) -> GCH [ (Ins, (testPA 1003 (notCpl r) (flp deltaX) l $ notCpl r.:.flp deltaX), genPAcl (EMp1 "a" (source l).*.EMp1 "b" (target l)) tOp l)
                                   , (Del, (testPA 1004 (l) (deltaX)            r $ l.:.deltaX),            genPAcl (EMp1 "a" (source r).*.EMp1 "b" (target r)) tOp r)
                                   ] motive
          (Ins, ELrs (l,r)) -> CHC [ genPAcl (testPA 1006 (flp deltaX) (notCpl l) r $ flp deltaX.:.notCpl l) Del r
                                   , genPAcl (testPA 1007 (deltaX) (r)            l $ deltaX.:.r           ) Ins l
                                   ] motive
          (Del, ELrs (l,r)) -> GCH [ (Ins, (testPA 1009 (flp deltaX) (notCpl l) r $ flp deltaX.:.notCpl l), genPAcl (EMp1 "a" (source r).*.EMp1 "b" (target r)) tOp r)
                                   , (Del, (testPA 1010 (deltaX) (r)            l $ deltaX.:.r),            genPAcl (EMp1 "a" (source l).*.EMp1 "b" (target l)) tOp l)
                                   ] motive
          (Ins, ECps (l,r)) -> CHC [ GCH [ (Ins, (testPA 1012 (deltaX) (flp r) l $ deltaX.:.flp r), genPAcl (EMp1 "a" (source l).*.EMp1 "b" (target l)) tOp l)
                                         , (Ins, (testPA 1013 (flp l) (deltaX) r $ flp l.:.deltaX), genPAcl (EMp1 "a" (source r).*.EMp1 "b" (target r)) tOp r)
                                         ] motive
                                   , New (source r) (\x->ALL [ genPAcl (deltaX.*.EMp1 x (target l)) Ins l
                                                             , genPAcl (EMp1 x (source r).*.deltaX) Ins r] motive) motive
                                   ] motive
          (Del, ECps (l,r)) -> CHC [ genPAcl (testPA 1018 (deltaX) (flp r) l $ deltaX.:.flp r) Del l
                                   , genPAcl (testPA 1019 (flp l) (deltaX) r $ flp l.:.deltaX) Del r
                                   ] motive
          (Ins, ERad (l,r)) -> CHC [ genPAcl (testPA 1021 (deltaX) (notCpl (flp r)) l $ deltaX.:.notCpl (flp r)) Ins l
                                   , genPAcl (testPA 1022 (notCpl (flp l)) (deltaX) r $ notCpl (flp l).:.deltaX) Ins r
                                   ] motive
          (Del, ERad (l,r)) -> CHC [ GCH [ (Del, (testPA 1024 (deltaX) (flp r) l $ deltaX.:.flp r), genPAcl (EMp1 "a" (source l).*.EMp1 "b" (target l)) tOp l)
                                         , (Del, (testPA 1025 (flp l) (deltaX) r $ flp l.:.deltaX), genPAcl (EMp1 "a" (source r).*.EMp1 "b" (target r)) tOp r)
                                         ] motive
                                   , New (source r) (\_->Nop motive) motive
                                   ] motive
          (Ins, EPrd (l,r)) -> ALL [ genPAcl (EDcV (Sign ONE (source deltaX)).:.deltaX) Ins (EDcV (Sign ONE (source r)).:.r)
                                   , genPAcl (deltaX.:.EDcV (Sign (target deltaX) ONE)) Ins (l.:.EDcV (Sign (target l) ONE))
                                   ] motive
          (Del, EPrd (l,r)) -> ALL [ genPAcl (EDcV (Sign ONE (source deltaX)).:.deltaX) Del (EDcV (Sign ONE (source r)).:.r)
                                   , genPAcl (deltaX.:.EDcV (Sign (target deltaX) ONE)) Del (l.:.EDcV (Sign (target l) ONE))
                                   ] motive
          (_  , EKl0 x )    -> genPAcl (deltaK0 deltaX tOp x) tOp x
          (_  , EKl1 x )    -> genPAcl (deltaK1 deltaX tOp x) tOp x
          (_  , EDcD d)     -> if editAble d then Do tOp d deltaX motive else Blk [(expr, nub [r |(_,rs)<-motive, r<-rs])]
          (_  , EDcI c)     -> if editAble (Isn c) then Do tOp (Isn c) deltaX motive else Blk [(expr, nub [r |(_,rs)<-motive, r<-rs])]
          (_  , EDcV{})     -> Blk [(expr, nub [r |(_,rs)<-motive, r<-rs])]
          (_  , EMp1{})     -> Blk [(expr, nub [r |(_,rs)<-motive, r<-rs])]
          (_  , EEps{})     -> Nop [(expr, nub [r |(_,rs)<-motive, r<-rs])]

{-        (_ , _)           -> fatal 767 ( "(Stef?) Non-exhaustive patterns in the recursive call\n"
                                       ++"doCod ("++showADL deltaX++") -- deltaX\n      "++show tOp++"  -- tOp\n      ("++showADL expr++") -- expr\n"++
                                         "within function\ndoCode "++show tOp'++"  -- tOp'\n       ("++showADL expr1++") -- expr1\n       ("++showADL delta1++") -- delta1\n"++
                                         concat
                                         [ "while trying to maintain conjunct "++showADL conjunct++
                                           "\nfrom rule "++intercalate "\n          " [show r | r<-rs]
                                         | (conjunct,rs)<-motive ] ++
                                         if null motive then "null motive" else ""
                                         )
-}

      deltaK0 :: t -> InsDel -> t1 -> t
      deltaK0 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta |- x*)
      deltaK0 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x* leeg is)
      deltaK1 :: t -> InsDel -> t1 -> t
      deltaK1 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta |- x+)
      deltaK1 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x+ leeg is)

