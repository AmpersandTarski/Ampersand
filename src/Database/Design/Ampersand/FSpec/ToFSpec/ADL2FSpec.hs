module Database.Design.Ampersand.FSpec.ToFSpec.ADL2FSpec
         (makeFSpec, preEmpt, editable) where

import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Core.Poset
import Prelude hiding (Ord(..))
import Database.Design.Ampersand.ADL1.Rule
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms 
import Database.Design.Ampersand.FSpec.ToFSpec.ADL2Plug
import Database.Design.Ampersand.FSpec.ToFSpec.Calc
import Database.Design.Ampersand.FSpec.ShowADL
import Text.Pandoc
import Data.Maybe
import Data.List
import Data.Char
import Data.Function

fatal :: Int -> String -> a
fatal = fatalMsg "FSpec.ToFSpec.ADL2FSpec"

makeFSpec :: Options -> A_Context -> FSpec
makeFSpec opts context = fSpec
 where
     fSpec =
        FSpec { fsName       = name context
              , getOpts      = opts
              , fspos        = ctxpos context
              , themes       = themesInScope
              , pattsInScope = pattsInThemesInScope
              , procsInScope = procsInThemesInScope
              , rulesInScope = rulesInThemesInScope
              , declsInScope = declsInThemesInScope
              , concsInScope = concsInThemesInScope
              , cDefsInScope = cDefsInThemesInScope
              , gensInScope  = gensInThemesInScope
              , fsLang       = fromMaybe (ctxlang context) (language opts)  -- The language for printing this specification is taken from the command line options (language opts). If none is specified, the specification is printed in the language in which the context was defined (ctxlang context).
              , vprocesses   = allProcs
              , vplugInfos   = definedplugs
              , plugInfos    = allplugs
              , interfaceS   = map enrichIfc (ctxifcs context) -- interfaces specified in the Ampersand script
              , interfaceG   = [ifc | ifc<-interfaceGen, let ctxrel = objctx (ifcObj ifc)
                                    , isIdent ctxrel && source ctxrel==ONE
                                      || ctxrel `notElem` map (objctx.ifcObj) (interfaceS fSpec)
                                    , allInterfaces opts]  -- generated interfaces
              , fSwitchboard = switchboard fSpec
              , fActivities  = [ makeActivity fSpec rul | rul <-processRules context]
              , fRoleRels    = mayEdit   context  -- fRoleRels says which roles may change the population of which relation.
              , fRoleRuls    = maintains context  -- fRoleRuls says which roles maintain which rules.
              , fRoles       = roles context
              , vrules       = vRules
              , grules       = gRules
              , invars       = invariants context
              , allRules     = allrules
              , vconjs       = allConjs   -- note that equality on Conjunct a and b is defined as:  rc_conjunct a == rc_conjunct b
              , vquads       = allQuads
              , vEcas        = {-preEmpt opts . -} fst (assembleECAs fSpec (allDecls fSpec))   -- TODO: preEmpt gives problems. Readdress the preEmption problem and redo, but properly.
              , vrels        = calculatedDecls
              , allUsedDecls = relsUsedIn context
              , allDecls     = relsDefdIn context
              , allConcepts  = concs context `uni` [ONE]
              , kernels      = constructKernels
              , fsisa        = let f gen = case gen of
                                            Isa{} -> [(genspc gen, gengen gen)]
                                            IsE{} -> [(genspc gen, g ) | g<-genrhs gen]
                               in concatMap f (gens context)
              , vpatterns    = patterns context
              , vgens        = gens context
              , vIndices     = identities context
              , vviews       = viewDefs context
              , conceptDefs  = ctxcds context
              , fSexpls      = ctxps context
              , metas        = ctxmetas context
              , initialPops  = initialpops
              , allViolations  = [ (r,vs)
                                 | r <- allrules, not (isSignal r)
                                 , let vs = ruleviolations (gens context) initialpops r, not (null vs) ]
              , initialSignals = [ (rule, conjViols) 
                                 | rule <- vRules, isSignal rule -- TODO: maybe use conjs from quads? (will need to group them on rule)
                                 , let conjViols =
                                         [ (c, viols) 
                                         | c <- makeCjcts opts rule
                                         , let viols = conjunctViolations (gens context) initialpops c, not $ null viols
                                         ]
                                 , not $ null conjViols
                                 ]
              }
     themesInScope = if null (ctxthms context)   -- The names of patterns/processes to be printed in the functional specification. (for making partial documentation)
                     then map name (patterns context) ++ map name allProcs
                     else ctxthms context
     pattsInThemesInScope = filter (\p -> name p `elem` themesInScope) (patterns context)
     procsInThemesInScope = filter (\p -> name p `elem` themesInScope) (ctxprocs context)
     cDefsInThemesInScope = filter (\cd -> cdfrom cd `elem` themesInScope) (ctxcds context)
     rulesInThemesInScope = ctxrs context `uni` concatMap prcRules procsInThemesInScope `uni` concatMap ptrls pattsInThemesInScope
     declsInThemesInScope = ctxds context `uni` concatMap prcDcls  procsInThemesInScope `uni` concatMap ptdcs pattsInThemesInScope
     concsInThemesInScope = concs (ctxrs context)  `uni`  concs procsInThemesInScope  `uni`  concs pattsInThemesInScope
     gensInThemesInScope  = ctxgs context ++ concatMap prcGens procsInThemesInScope ++ concatMap ptgns pattsInThemesInScope

     enrichIfc :: Interface -> Interface
     enrichIfc ifc
      = ifc{ ifcEcas = fst (assembleECAs fSpec editables)
           , ifcControls = [ conj
                           | conj<-allConjs
                           , (not.null) (ifcParams ifc `isc` primsMentionedIn (rc_conjunct conj))
                           , maybe True (\(p,_) -> p `notElem` [Uni,Inj]) $ rrdcl (rc_orgRule conj)
                           -- If the conjunct's rule comes from a declared property, don't include the conjunct if the property is Uni or Inj.
                           ]
           }
        where editables = [d | EDcD d<-ifcParams ifc]++[Isn c | EDcI c<-ifcParams ifc]

     initialpops = [ PRelPopu{ popdcl = popdcl (head eqclass)
                             , popps  = (nub.concat) [ popps pop | pop<-eqclass ]
                             }
                   | eqclass<-eqCl popdcl [ pop | pop@PRelPopu{}<-populations ] ] ++
                   [ PCptPopu{ popcpt = popcpt (head eqclass)
                             , popas  = (nub.concat) [ popas pop | pop<-eqclass ]
                             }
                   | eqclass<-eqCl popcpt [ pop | pop@PCptPopu{}<-populations ] ]
       where populations = ctxpopus context++concatMap prcUps (processes context)++concatMap ptups (patterns context)       

     allQuads = makeQuads opts allrules
     allConjs = nubBy ((==) `on` rc_id) (concatMap qConjuncts allQuads)
--      isInvariantQuad q = null [r | (r,rul)<-maintains context, rul==qRule q]
     allrules = vRules ++ gRules
     vRules = udefrules context   -- all user defined rules
     gRules = multrules context++identityRules context
     allProcs = [ FProc {fpProc = p
                        ,fpActivities =selectActs p
                        } | p<-ctxprocs context ]
                where selectActs p   = [act | act<-fActivities fSpec
                                            , (not.null) (selRoles p act)]
                      selRoles p act = [r | (r,rul)<-maintains context, rul==actRule act, r `elem` roles p]
     -- | allDecs contains all user defined plus all generated relations plus all defined and computed totals.
     calcProps :: Declaration -> Declaration
     calcProps d = d{decprps_calc = Just calculated}
         where calculated = decprps d `uni` [Tot | d `elem` totals]
                                      `uni` [Sur | d `elem` surjectives]
     calculatedDecls = map calcProps (relsDefdIn context)
     constructKernels = foldl f (group (delete ONE (concs context))) (gens context)
         where f disjuncLists g = concat haves : nohaves
                 where
                   (haves,nohaves) = partition (not.null.intersect (concs g)) disjuncLists
  -- determine relations that are total (as many as possible, but not necessarily all)
     totals      = [ d |       EDcD d  <- totsurs ]
     surjectives = [ d | EFlp (EDcD d) <- totsurs ]
     totsurs :: [Expression]
     totsurs
      = nub [rel | q<-makeQuads opts (invariants context), isIdent (qDcl q)
                 , x<-qConjuncts q, Dnf antcs conss<-rc_dnfClauses x
                 , let antc = conjNF opts (foldr (./\.) (EDcV (sign (head (antcs++conss)))) antcs)
                 , isRfx antc -- We now know that I is a subset of the antecedent of this dnf clause.
                 , cons<-map exprCps2list conss
            -- let I |- r;s;t be an invariant rule, then r and s and t~ and s~ are all total.
                 , rel<-init cons++[flp r | r<-tail cons]
                 ]

     --------------
     --making plugs
     --------------
     vsqlplugs = [ (makeUserDefinedSqlPlug context p) | p<-ctxsql context] --REMARK -> no optimization like try2specific, because these plugs are user defined
     definedplugs = map InternalPlug vsqlplugs
                 ++ map ExternalPlug (ctxphp context)
     allplugs = definedplugs ++      -- all plugs defined by the user
                genPlugs             -- all generated plugs
     genPlugs = [InternalPlug (rename p (qlfname (name p)))
                | p <- uniqueNames (map name definedplugs) -- the names of definedplugs will not be changed, assuming they are all unique
                                   (makeGeneratedSqlPlugs opts context totsurs entityRels)
                ]
     -- relations to be saved in generated plugs: if decplug=True, the declaration has the BYPLUG and therefore may not be saved in a database
     -- WHAT -> is a BYPLUG?
     entityRels = [ d | d<-calculatedDecls, not (decplug d)] -- The persistent relations.

     qlfname x = if null (namespace opts) then x else "ns"++namespace opts++x

     --TODO151210 -> Plug A is overbodig, want A zit al in plug r
--CONTEXT Temp
--PATTERN Temp
--r::A*B[TOT].
--t::E*ECps[UNI].
--ENDPATTERN
--ENDCONTEXT
{-
    **************************************
    * Plug E                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    * t  [UNI]                             *
    **************************************
    * Plug ECps                            *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    **************************************
    * Plug B                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    **************************************
    * Plug A                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    **************************************
    * Plug r                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    * r  [TOT]                             *
    **************************************
-}
     -------------------
     --END: making plugs
     -------------------
     -------------------
     --making interfaces
     -------------------
     -- interfaces (type ObjectDef) can be generated from a basic ontology. That is: they can be derived from a set
     -- of relations together with multiplicity constraints. That is what interfaceG does.
     -- This is meant to help a developer to build his own list of interfaces, by providing a set of interfaces that works.
     -- The developer may relabel attributes by names of his own choice.
     -- This is easier than to invent a set of interfaces from scratch.

     -- Rule: a interface must be large enough to allow the required transactions to take place within that interface.
     -- Attributes of an ObjectDef have unique names within that ObjectDef.

--- generation of interfaces:
--  Ampersand generates interfaces for the purpose of quick prototyping.
--  A script without any mention of interfaces is supplemented
--  by a number of interface definitions that gives a user full access to all data.
--  Step 1: select and arrange all relations to obtain a set cRels of total relations
--          to ensure insertability of entities (signal declarations are excluded)
     cRels = [     EDcD d  | d@Sgn{}<-relsDefdIn context, not(deciss d), isTot d, not$decplug d]++
             [flp (EDcD d) | d@Sgn{}<-relsDefdIn context, not(deciss d), not (isTot d) && isSur d, not$decplug d]
--  Step 2: select and arrange all relations to obtain a set dRels of injective relations
--          to ensure deletability of entities (signal declarations are excluded)
     dRels = [     EDcD d  | d@Sgn{}<-relsDefdIn context, not(deciss d), isInj d, not$decplug d]++
             [flp (EDcD d) | d@Sgn{}<-relsDefdIn context, not(deciss d), not (isInj d) && isUni d, not$decplug d]
--  Step 3: compute longest sequences of total expressions and longest sequences of injective expressions.
     maxTotPaths = clos cRels   -- maxTotPaths = cRels+, i.e. the transitive closure of cRels
     maxInjPaths = clos dRels   -- maxInjPaths = dRels+, i.e. the transitive closure of dRels
     --    Warshall's transitive closure algorithm, adapted for this purpose:
     clos :: [Expression] -> [[Expression]]
     clos xs
      = foldl f [ [ x ] | x<-xs] (nub (map source xs) `isc` nub (map target xs))
        where
          f :: [[Expression]] -> A_Concept -> [[Expression]]
          f q x = q ++ [l ++ r | l <- q, x == target (last l),
                                 r <- q, x == source (head r), null (l `isc` r)]

--  Step 4: i) generate interfaces starting with INTERFACE concept: I[Concept]
--          ii) generate interfaces starting with INTERFACE concepts: V[ONE*Concept]
--          note: based on a theme one can pick a certain set of generated interfaces (there is not one correct set)
--                default theme => generate interfaces from the clos total expressions and clos injective expressions (see step1-3).
--                student theme => generate interface for each concept with relations where concept is source or target (note: step1-3 are skipped)
     interfaceGen = step4a ++ step4b
     step4a
      | theme opts == StudentTheme
      = [Ifc { ifcClass = Nothing
             , ifcParams = params
             , ifcArgs   = []
             , ifcObj    = Obj { objnm   = name c ++ " (instantie)"
                               , objpos  = Origin "generated object for interface for each concept in TblSQL or ScalarSQL"
                               , objctx  = EDcI c
                               , objmsub = Just . Box c $
                                           Obj { objnm   = "I["++name c++"]"
                                                , objpos  = Origin "generated object: step 4a - default theme"
                                                , objctx  = EDcI c
                                                , objmsub = Nothing
                                                , objstrs = [] }
                                           :[Obj { objnm   = name dcl ++ "::"++name (source dcl)++"*"++name (target dcl)
                                                 , objpos  = Origin "generated object: step 4a - default theme"
                                                 , objctx  = if source dcl==c then EDcD dcl else flp (EDcD dcl)
                                                 , objmsub = Nothing
                                                 , objstrs = [] }
                                            | dcl <- directdecls]
                               , objstrs = []
                               }
             , ifcEcas   = fst (assembleECAs fSpec directdecls)
             , ifcControls = [ conj | conj<-allConjs, (not.null) (params `isc` primsMentionedIn (rc_conjunct conj))]
             , ifcPos    = Origin "generated interface for each concept in TblSQL or ScalarSQL"
             , ifcPrp    = "Interface " ++name c++" has been generated by Ampersand."
             , ifcRoles  = []
             }
        | c<-concs fSpec
        , let directdecls = [ d | d<-relsDefdIn fSpec, c `elem` concs d]
        , let params = map EDcD directdecls
        ]
      --end student theme
      --otherwise: default theme
      | otherwise --note: the uni of maxInj and maxTot may take significant time (e.g. -p while generating index.htm)
                  --note: associations without any multiplicity are not in any Interface
                  --note: scalars with only associations without any multiplicity are not in any Interface
      = let recur es
             = [ Obj { objnm   = showADL t
                     , objpos  = Origin "generated recur object: step 4a - default theme"
                     , objctx  = t
                     , objmsub = Just . Box (target t) $ recur [ pth | (_:pth)<-cl, not (null pth) ]
                     , objstrs = [] }
               | cl<-eqCl head es, (t:_)<-take 1 cl] --
            -- es is a list of expression lists, each with at least one expression in it. They all have the same source concept (i.e. source.head)
            -- Each expression list represents a path from the origin of a box to the attribute.
            -- 16 Aug 2011: (recur trace es) is applied once where es originates from (maxTotPaths `uni` maxInjPaths) both based on clos
            -- Interfaces for I[Concept] are generated only for concepts that have been analysed to be an entity.
            -- These concepts are collected in gPlugConcepts
            gPlugConcepts = [ c | InternalPlug plug@TblSQL{}<-genPlugs , (c,_)<-take 1 (cLkpTbl plug) ]
            -- Each interface gets all attributes that are required to create and delete the object.
            -- All total attributes must be included, because the interface must allow an object to be deleted.
        in
        [Ifc { ifcClass    = Nothing
             , ifcParams   = map EDcD editables
             , ifcArgs     = []
             , ifcObj      = Obj { objnm   = name c
                                 , objpos  = Origin "generated object: step 4a - default theme"
                                 , objctx  = EDcI c
                                 , objmsub = Just . Box c $ objattributes
                                 , objstrs = [] }
             , ifcEcas     = fst (assembleECAs fSpec editables)
             , ifcControls = [ conj | conj<-allConjs, (not.null) (params `isc` primsMentionedIn (rc_conjunct conj))]
             , ifcPos      = Origin "generated interface: step 4a - default theme"
             , ifcPrp      = "Interface " ++name c++" has been generated by Ampersand."
             , ifcRoles    = []
             }
        | cl <- eqCl (source.head) [ pth | pth<-maxTotPaths `uni` maxInjPaths, (source.head) pth `elem` gPlugConcepts ]
        , let objattributes = recur cl
        , not (null objattributes) --de meeste plugs hebben in ieder geval I als attribuut
        , --exclude concept A without cRels or dRels (i.e. A in Scalar without total associations to other plugs)
          not (length objattributes==1 && isIdent(objctx(head objattributes)))
        , let e0=head cl, if null e0 then fatal 284 "null e0" else True
        , let c=source (head e0)
        , let editables = [ d | EDcD d <- concatMap primsMentionedIn (expressionsIn objattributes)]++
                          [ Isn cpt |  EDcI cpt <- concatMap primsMentionedIn (expressionsIn objattributes)]
        , let params = map EDcD editables
        ]
     --end otherwise: default theme
     --end stap4a
     step4b --generate lists of concept instances for those concepts that have a generated INTERFACE in step4a
      = [Ifc { ifcClass    = ifcClass ifcc
             , ifcParams   = ifcParams ifcc
             , ifcArgs     = ifcArgs   ifcc
             , ifcObj      = Obj { objnm   = nm
                                 , objpos  = Origin "generated object: step 4b"
                                 , objctx  = EDcI ONE
                                 , objmsub = Just . Box ONE $ [att]
                                 , objstrs = [] }
             , ifcEcas     = ifcEcas     ifcc
             , ifcControls = ifcControls ifcc
             , ifcPos      = ifcPos      ifcc
             , ifcPrp      = ifcPrp      ifcc
             , ifcRoles    = []
             }
        | ifcc<-step4a
        , let c   = source(objctx (ifcObj ifcc))
              nm'::Int->String
              nm' 0  = plural (fsLang fSpec) (name c)
              nm' i  = plural (fsLang fSpec) (name c) ++ show i
              nms = [nm' i |i<-[0..], nm' i `notElem` map name (ctxifcs context)]
              nm
                | theme opts == StudentTheme = name c
                | null nms = fatal 355 "impossible"
                | otherwise = head nms
              att = Obj (name c) (Origin "generated attribute object: step 4b") (EDcV (Sign ONE c)) Nothing []
        ]
     ----------------------
     --END: making interfaces
     ----------------------

editable :: Expression -> Bool   --TODO deze functie staat ook in Calc.hs...
editable (EDcD Sgn{}) = True
editable _            = False

{- makeActivity turns a process rule into an activity definition.
Each activity can be mapped to a single interface.
A call to such an interface takes the population of the current context to another population,
while maintaining all invariants.
-}
makeActivity :: FSpec -> Rule -> Activity
makeActivity fSpec rul
 = let s = Act{ actRule   = rul
              , actTrig   = decls
              , actAffect = nub [ d' | (d,_,d')<-clos affectPairs, d `elem` decls]
              , actQuads  = invQs
              , actEcas   = [eca | eca<-vEcas fSpec, eDcl (ecaTriggr eca) `elem` decls]
              , actPurp   = [Expl { explPos = OriginUnknown
                                  , explObj = ExplRule (name rul)
                                  , explMarkup = A_Markup { amLang   = Dutch
                                                          , amFormat = ReST
                                                          , amPandoc = [Plain [Str "Waartoe activiteit ", Quoted SingleQuote [Str (name rul)], Str" bestaat is niet gedocumenteerd." ]]
                                                          }
                                  , explUserdefd = False
                                  , explRefIds = ["Regel "++name rul]
                                  }
                            ,Expl { explPos = OriginUnknown
                                  , explObj = ExplRule (name rul)
                                  , explMarkup = A_Markup { amLang   = English
                                                          , amFormat = ReST
                                                          , amPandoc = [Plain [Str "For what purpose activity ", Quoted SingleQuote [Str (name rul)], Str" exists remains undocumented." ]]
                                                          }
                                  , explUserdefd = False
                                  , explRefIds = ["Regel "++name rul]
                                  }
                            ]
              } in s
 where
-- relations that may be affected by an edit action within the transaction
     decls        = relsUsedIn rul
-- the quads that induce automated action on an editable relation.
-- (A quad contains the conjunct(s) to be maintained.)
-- Those are the quads that originate from invariants.
     invQs       = [q | q<-vquads fSpec, (not.isSignal.qRule) q
                      , (not.null) ((relsUsedIn.qRule) q `isc` decls)] -- SJ 20111201 TODO: make this selection more precise (by adding inputs and outputs to a quad).
-- a relation affects another if there is a quad (i.e. an automated action) that links them
     affectPairs = [(qDcl q,[q], d) | q<-invQs, d<-(relsUsedIn.qRule) q]
-- the relations affected by automated action
--      triples     = [ (r,qs,r') | (r,qs,r')<-clos affectPairs, r `elem` rels]
----------------------------------------------------
--  Warshall's transitive closure algorithm in Haskell, adapted to carry along the intermediate steps:
----------------------------------------------------
     clos :: (Eq a,Eq b) => [(a,[b],a)] -> [(a,[b],a)]     -- e.g. a list of pairs, with intermediates in between
     clos xs
       = foldl f xs (nub (map fst3 xs) `isc` nub (map thd3 xs))
         where
          f q x = q `un`
                     [(a, qs `uni` qs', b') | (a, qs, b) <- q, b == x,
                      (a', qs', b') <- q, a' == x]
          ts `un` [] = ts
          ts `un` ((a',qs',b'):ts')
           = ([(a,qs `uni` qs',b) | (a,qs,b)<-ts, a==a' && b==b']++
              [(a,qs,b)           | (a,qs,b)<-ts, a/=a' || b/=b']++
              [(a',qs',b')        | (a',b') `notElem` [(a,b) |(a,_,b)<-ts]]) `un` ts'

-- Quads embody the "switchboard" of rules. A quad represents a "proto-rule" with the following meaning:
-- whenever relation r is affected (i.e. tuples in r are inserted or deleted),
-- the rule may have to be restored using functionality from one of the clauses.
-- The rule is carried along for traceability.
makeQuads :: Options -> [Rule] -> [Quad]
makeQuads opts rs
 = [ Quad { qDcl     = d
          , qRule    = rule
          , qConjuncts = makeCjcts opts rule
          }
   | rule<-rs, d<-relsUsedIn rule
   ]

-- The function makeCjcts yields an expression which has constructor EUni in every case.
makeCjcts :: Options -> Rule -> [Conjunct]
makeCjcts opts rule = [Cjct { rc_id = "conj_"++rrnm rule++"_"++show (i :: Int)
                       , rc_orgRule = rule
                       , rc_conjunct = expr
                       , rc_dnfClauses = allShifts opts (expr2dnfClause expr)
                       }
                 | (expr,i)<-zip (conjuncts opts rule) [0..]
                 ]
   where
      expr2dnfClause :: Expression -> DnfClause

      expr2dnfClause conj = (split (Dnf [] []).exprUni2list) conj
       where
         split :: DnfClause -> [Expression] -> DnfClause
         split (Dnf antc cons) (ECpl e: rest) = split (Dnf (e:antc) cons) rest
         split (Dnf antc cons) (     e: rest) = split (Dnf antc (e:cons)) rest
         split dc              []             = dc

allShifts :: Options -> DnfClause -> [DnfClause]
allShifts opts conjunct =  (map head.eqClass (==).filter pnEq.map normDNF) (shiftL conjunct++shiftR conjunct)  -- we want to nub all dnf-clauses, but nub itself does not do the trick...
-- allShifts conjunct = error $ show conjunct++concat [ "\n"++show e'| e'<-shiftL conjunct++shiftR conjunct] -- for debugging
 where
 {-
  diagnostic
   = intercalate "\n  "
       [ "shiftL: [ "++intercalate "\n          , " [showHS opts "\n            " e | e<-shiftL conjunct    ]++"\n          ]"
       , "shiftR: [ "++intercalate "\n          , " [showHS opts "\n            " e | e<-shiftR conjunct    ]++"\n          ]"
       ] -}
  shiftL :: DnfClause -> [DnfClause]
  shiftL dc@(Dnf antcs conss)
   | null antcs || null conss = [dc] --  shiftL doesn't work here. This is just to make sure that both antss and conss are really not empty
   | otherwise                = [ Dnf ass (case css of
                                            [] -> let antcExpr = foldr1 (./\.) ass in
                                                  if isEndo antcExpr then [EDcI (source antcExpr)] else fatal 425 "antcExpr should be endorelation"
                                            _  -> css
                                          )
                                | (ass,css)<-nub (move antcs conss)
                                ]
   where
   -- example:  r;s /\ p;r |- x;y   and suppose x and y are both univalent.
   --  antcs =  [ r;s, p;r ]
   --  conss =  [ x;y ]
    move :: [Expression] -> [Expression] -> [([Expression],[Expression])]
    move ass [] = [(ass,[])]
    move ass css
     = (ass,css):
       if and [ (not.isEDcI) cs | cs<-css]     -- all cs are nonempty because: (not.and.map isEDcI) cs ==> not (null cs)
       then [ts | let headEs = map headECps css
                , length (eqClass (==) headEs) == 1                    -- example: True, because map head css == [ "x" ]
                , let h=head headEs                                    -- example: h= "x"
                , isUni h                                              -- example: assume True
                , ts<-move [if source h==source as then flp h.:.as else fatal 455 "type mismatch"
                           |as<-ass] (map tailECps css)]++ -- example: ts<-move [ [flp "x","r","s"], [flp "x","p","r"] ]  [ ["y","z"] ]
            [ts | let lastEs = map lastECps css
                , length (eqClass (==) lastEs) == 1
                , let l=head lastEs
                , isInj l
                , ts<-move [if target as==target l then as.:.flp l else fatal 461 "type mismatch"
                           |as<-ass] (map initECps css)]   -- example: ts<-move [ ["r","s",flp "z"], ["p","r",flp "z"] ]  [ ["x","y"] ]
       else []
   -- Here is (informally) what the example does:
   -- move [ r;s , p;r ] [ x;y ]
   -- ( [ r;s , p;r ] , [ x;y ] ): [ ts | ts<-move [flp x.:.as | as<-[ r;s , p;r ] [ y ] ] ]
   -- ( [ r;s , p;r ] , [ x;y ] ): ( [ x~;r;s , x~;p;r ] , [ y ] ): [ ts | ts<-move [flp y.:.as | as<-[ y~;x~;r;s , y~;x~;p;r ] [] ] ]
   -- ( [ r;s , p;r ] , [ x;y ] ): ( [ x~;r;s , x~;p;r ] , [ y ] ): [ [ y~;x~;r;s , y~;x~;p;r ] , [] ] ]
   -- [ ( [ r;s , p;r ] , [ x;y ] ), ( [ x~;r;s , x~;p;r ] , [ y ] ), ( [ y~;x~;r;s , y~;x~;p;r ] , [] ) ]

  shiftR :: DnfClause -> [DnfClause]
  shiftR dc@(Dnf antcs conss)
   | null antcs || null conss = [dc] --  shiftR doesn't work here. This is just to make sure that both antss and conss are really not empty
   | otherwise                = [ Dnf (case ass of
                                        [] -> let consExpr = foldr1 (.\/.) css in
                                              if source consExpr==target consExpr then [EDcI (source consExpr)] else fatal 463 "consExpr should be endorelation"
                                        _  -> ass
                                      ) css
                                | (ass,css)<-nub (move antcs conss)
                                ]
   where
   -- example  "r;s /\ r;r |- x;y"   and suppose r is both surjective.
   --  ass =  [ r;s , r;r ]
   --  css =  [ x;y ]
    move :: [Expression] -> [Expression] -> [([Expression],[Expression])]
    move ass css =
     case ass of
      [] -> [] -- was [([EDcI (target (last css))],css)]
      _  ->
       (ass,css):
       if and [ (not.isEDcI) as | as<-ass]
       then [ts | let headEs = map headECps ass
                , length (eqClass (==) headEs) == 1                      -- example: True, because map headECps ass == [ "r", "r" ]
                , let h=head headEs                                      -- example: h= "r"
                , isSur h                                                -- example: assume True
                , ts<-move (map tailECps ass) [if source h==source cs then flp h.:.cs else fatal 496 "type mismatch"
                                              |cs<-css]]++   -- example: ts<-move  [["s"], ["r"]] [ [flp "r","x","y","z"] ]
            [ts | let lastEs = map lastECps ass
                , length (eqClass (==) lastEs) == 1                      -- example: False, because map lastECps ass == [ ["s"], ["r"] ]
                , let l=head lastEs
                , isTot l
                , ts<-move (map initECps ass) [if target cs==target l then cs.:.flp l else fatal 502 "type mismatch"
                                              |cs<-css]]     -- is dit goed? cs.:.flp l wordt links zwaar, terwijl de normalisator rechts zwaar maakt.
       else []
   -- Here is (informally) what the example does:
   -- move [ r;s , r;r ] [ x;y ]
   -- ( [ r;s , r;r ] , [ x;y ] ): move [ s , r ] [ r~;x;y ]
   -- ( [ r;s , r;r ] , [ x;y ] ): ( [ s , r ]  , [ r~;x;y ] ) : []
   -- [ [ r;s , r;r ] , [ x;y ] ), ( [ s , r ]  , [ r~;x;y ] ) ]
   --  diagnostic
   --    = "\n  antcs: [ "++intercalate "\n         , " [showADL a | a<-antcs ]++"\n       ]"++
   --      "\n  conss: [ "++intercalate "\n         , " [showADL c | c<-conss ]++"\n       ]"++
   --      "\n  move:  [ "++intercalate "\n         , " ["("++sh " /\\ " as++"\n           ,"++sh " \\/ " cs++")" | (as,cs)<-move antcs conss ]++"\n       ]"
   --  sh :: String -> [Expression] -> String
   --  sh str es = intercalate str [ showADL e | e<-es]

  normDNF :: DnfClause -> DnfClause
  normDNF (Dnf antcs conss) = Dnf lhs rhs
   where lhs = case antcs of
                [] -> []
                _  -> (exprIsc2list . conjNF opts . foldr1 (./\.)) antcs
         rhs = case conss of
                [] -> []
                _  -> (exprUni2list . disjNF opts . foldr1 (.\/.)) conss

  pnEq :: DnfClause -> Bool
  pnEq (Dnf antcs conss) = antcs/=conss

  headECps :: Expression -> Expression
  headECps expr = f expr
   where f (ECps (l@ECps{},_)) = f l
         f (ECps (l,_)) = l
         f _ = expr

  tailECps :: Expression -> Expression
  tailECps expr = f expr
   where f (ECps (ECps (l,r),q)) = f (ECps (l, ECps (r,q)))
         f (ECps (_,r)) = r
         f _ = EDcI (target expr)

  initECps :: Expression -> Expression
  initECps expr = f expr
   where f (ECps (l, ECps (r,q))) = initECps (ECps (ECps (l,r),q))
         f (ECps (l,_)) = l
         f _ = EDcI (source expr)

  lastECps :: Expression -> Expression
  lastECps expr = f expr
   where f (ECps (_,r@ECps{})) = f r
         f (ECps (_,r)) = r
         f _ = expr

  isEDcI :: Expression -> Bool
  isEDcI EDcI{} = True
  isEDcI _ = False

-- If one rule r blocks upon an event, e.g. e@(ON Ins rel), while another ECA rule r'
-- maintains something else with that same event e, we can save r' the trouble.
-- After all, event e will block anyway.
-- preEmpt tries to simplify ECArules by predicting whether a rule will block.
preEmpt :: Options -> [ECArule] -> [ECArule]
preEmpt opts ers = pr [length ers] (10::Int)
 where
  pr :: [Int] -> Int -> [ECArule]
  pr ls n
    | n == 0              = fatal 633 $ "too many cascading levels in preEmpt "++show ls
    | (not.null) cascaded = pr (length cascaded:ls)
                            -- ([er{ecaAction=normPA opts (ecaAction er)} | er<-cascaded] ++uncasced)
                               (n-1)
    | otherwise           = [er{ecaAction=normPA opts (ecaAction er)} | er<-uncasced]
   where
-- preEmpt divides all ECA rules in uncascaded rules and cascaded rules.
-- cascaded rules are those rules that have a Do component with event e, where e is known to block (for some other reason)
    new  = [er{ecaAction=normPA opts (ecaAction er)} | er<-ers]
    cascaded = [er{ecaAction=action'} | er<-new, let (c,action') = cascade (eDcl (ecaTriggr er)) (ecaAction er), c]
    uncasced = [er |                    er<-new, let (c,_)       = cascade (eDcl (ecaTriggr er)) (ecaAction er), not c]
-- cascade inserts a block on the place where a Do component exists that matches the blocking event.
--  cascade :: Relation -> PAclause -> (Bool, PAclause)
  cascade dcl (Do srt to _ _) | (not.null) blkErs = (True, ecaAction (head blkErs))
   where blkErs = [er | er<-ers
                      , Blk _<-[ecaAction er]
                      , let t = ecaTriggr er
                      , eSrt t == srt
                      , eDcl t == to
                      , not (dcl ==to)
                      ]
  cascade  _  c@Do{}           = (False, c)
  cascade rel (New c clause m) = ((fst.cascade rel.clause) "dummystr", New c (snd.cascade rel.clause) m)
  cascade rel (Rmv c clause m) = ((fst.cascade rel.clause) "dummystr", Rmv c (snd.cascade rel.clause) m)
--cascade rel (Sel c e cl m)   = ((fst.cascade rel.cl) "dummystr",     Sel c e (snd.cascade rel.cl)   m)
  cascade rel (CHC ds m)       = (any (fst.cascade rel) ds, CHC (map (snd.cascade rel) ds) m)
  cascade rel (ALL ds m)       = (any (fst.cascade rel) ds, ALL (map (snd.cascade rel) ds) m)
  cascade  _  (Nop m)          = (False, Nop m)
  cascade  _  (Blk m)          = (False, Blk m)
  cascade  _  (Let _ _ _)  = fatal 611 "Deze constructor is niet gedefinieerd" -- HJO, 20131205:Toegevoegd om warning te verwijderen
  cascade  _  (Ref _)      = fatal 612 "Deze constructor is niet gedefinieerd" -- HJO, 20131205:Toegevoegd om warning te verwijderen
  cascade  _  (GCH{})      = fatal 655 "Deze constructor is niet gedefinieerd" -- SJO, 20140428:Toegevoegd om warning te verwijderen

switchboard :: FSpec -> Fswitchboard
switchboard fSpec
 = Fswtch
    { fsbEvIn  = eventsIn
    , fsbEvOut = eventsOut
    , fsbConjs = conjs
    , fsbECAs  = ecas
    }
   where
     qs :: [Quad]
     qs        = makeQuads (getOpts fSpec) (invariants fSpec)
     (ecas, _) = assembleECAs fSpec (allDecls fSpec)
     conjs     = nub [ (qRule q, rc_conjunct x) | q<-qs, x<-qConjuncts q]
     eventsIn  = nub [ecaTriggr eca | eca<-ecas ]
     eventsOut = nub [evt | eca<-ecas, evt<-eventsFrom (ecaAction eca)]

class Identified a => Rename a where
 rename :: a->String->a
 -- | the function uniqueNames ensures case-insensitive unique names like sql plug names
 uniqueNames :: [String]->[a]->[a]
 uniqueNames taken xs
  = [p | cl<-eqCl (map toLower.name) xs  -- each equivalence class cl contains (identified a) with the same map toLower (name p)
       , p <-if name (head cl) `elem` taken || length cl>1
             then [rename p (name p++show i) | (p,i)<-zip cl [(1::Int)..]]
             else cl
    ]

instance Rename PlugSQL where
 rename p x = p{sqlname=x}
