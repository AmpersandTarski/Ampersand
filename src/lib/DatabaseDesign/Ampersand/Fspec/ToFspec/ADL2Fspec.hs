{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec
    (makeFspec,allClauses, quads, preEmpt, editable)
  where
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Core.Poset
   import Prelude hiding (Ord(..),head)
   import DatabaseDesign.Ampersand.ADL1.Rule
   import DatabaseDesign.Ampersand.Basics
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.ADL1
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import DatabaseDesign.Ampersand.Misc
   import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms  --  (delta,conjNF,disjNF,normPA)
   import DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Plug
   import DatabaseDesign.Ampersand.Fspec.ToFspec.Calc
--   import DatabaseDesign.Ampersand.Fspec.ShowHS -- only for diagnostic purposes during debugging
   import DatabaseDesign.Ampersand.Fspec.ShowADL
   import Text.Pandoc
   import Data.Maybe
   import Data.List (nub,nubBy,intersect,partition,group,delete)
   import DatabaseDesign.Ampersand.ADL1.Expression
   import Data.Char        (toLower)
--   import Debug.Trace -- only for diagnostic purposes during debugging
   head :: [a] -> a
   head [] = fatal 30 "head must not be used on an empty list!"
   head (a:_) = a

   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ToFspec.ADL2Fspec"

   makeFspec :: Options -> A_Context -> Fspc
   makeFspec flags context = fSpec
    where
        fSpec =
            Fspc { fsName       = name context
                 , fspos        = ctxpos context
                 , themes       = themesInScope
                 , pattsInScope = pattsInThemesInScope
                 , procsInScope = procsInThemesInScope
                 , rulesInScope = rulesInThemesInScope
                 , declsInScope = declsInThemesInScope
                 , concsInScope = concsInThemesInScope
                 , cDefsInScope = cDefsInThemesInScope
                 , gensInScope  = gensInThemesInScope
                 , fsLang       = fromMaybe (ctxlang context) (language flags)  -- The language for printing this specification is taken from the command line options (language flags). If none is specified, the specification is printed in the language in which the context was defined (ctxlang context).
                 , vprocesses   = allProcs
                 , vplugInfos   = definedplugs
                 , plugInfos    = allplugs
                 , interfaceS   = ctxifcs context -- interfaces specified in the Ampersand script
                 , interfaceG   = [ifc | ifc<-interfaceGen, let ctxrel = objctx (ifcObj ifc)
                                       , isIdent ctxrel && source ctxrel==ONE
                                         || ctxrel `notElem` map (objctx.ifcObj) (interfaceS fSpec)
                                       , allInterfaces flags]  -- generated interfaces
                 , fSwitchboard = switchboard flags fSpec
                 , fActivities  = [ makeActivity fSpec rul | rul <-processRules context]
                 , fRoleRels    = mayEdit   context  -- fRoleRels says which roles may change the population of which relation.
                 , fRoleRuls    = maintains context  -- fRoleRuls says which roles maintain which rules.
                 , fRoles       = roles context
                 , vrules       = vRules
                 , grules       = gRules
                 , invars       = invariants context
                 , allRules     = allrules
                 , vconjs       = let equalOnConjunct a b = rc_conjunct a == rc_conjunct b
                                  in nubBy equalOnConjunct (concatMap (cl_conjNF.qClauses)allQuads)
                 , vquads       = allQuads
                 , vEcas        = let (ecas,_)=(unzip.assembleECAs) fSpec in {-preEmpt-} ecas -- TODO: preEmpt gives problems. Readdress the preEmption problem and redo, but properly.
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
                 , allViolations = [(r,vs) |r<-allrules, not (isSignal r), let vs = ruleviolations (gens context) initialpops r,  not (null vs)]
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

        allQuads = quads flags (\_->True) allrules
        initialpops = [ PRelPopu{ popdcl = popdcl (head eqclass)
                                , popps  = (nub.concat) [ popps pop | pop<-eqclass ] 
                                } 
                      | eqclass<-eqCl popdcl [ pop | pop@PRelPopu{}<-populations ] ] ++
                      [ PCptPopu{ popcpt = popcpt (head eqclass)
                                , popas  = (nub.concat) [ popas pop | pop<-eqclass ] 
                                } 
                      | eqclass<-eqCl popcpt [ pop | pop@PCptPopu{}<-populations ] ]
          where populations = ctxpopus context++concatMap prcUps (processes context)++concatMap ptups (patterns context)

--      isInvariantQuad q = null [r | (r,rul)<-maintains context, rul==cl_rule (qClauses q)]
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
         = nub [rel | q<-quads flags visible (invariants context), isIdent (qDcl q)
                    , x<-cl_conjNF (qClauses q), Dnf antcs conss<-rc_dnfClauses x
                    , let antc = conjNF (foldr (./\.) (EDcV (sign (head (antcs++conss)))) antcs)
                    , isRfx antc -- We now know that I is a subset of the antecedent of this dnf clause.
                    , cons<-map exprCps2list conss
               -- let I |- r;s;t be an invariant rule, then r and s and t~ and s~ are all total.
                    , rel<-init cons++[flp r | r<-tail cons]
                    ]
          where
            visible _ = True -- for computing totality, we take all quads into account.

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
                                      (makeGeneratedSqlPlugs flags context totsurs entityRels)
                   ]
        -- relations to be saved in generated plugs: if decplug=True, the declaration has the BYPLUG and therefore may not be saved in a database
        -- WHAT -> is a BYPLUG?
        entityRels = [ d | d<-calculatedDecls, not (decplug d)] -- The persistent relations.

        qlfname x = if null (namespace flags) then x else "ns"++namespace flags++x

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
         | theme flags == StudentTheme 
         = [Ifc { ifcParams = directdeclsExprs
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
                                              :[Obj { objnm   = case dcl of
                                                                  EDcD d -> name d ++ "::"++name (source d)++"*"++name (target d)
                                                                  _      -> fatal 246 "Invalid expression for a parameter."
                                                    , objpos  = Origin "generated object: step 4a - default theme"
                                                    , objctx  = if source dcl==c then dcl else flp dcl
                                                    , objmsub = Nothing
                                                    , objstrs = [] }
                                               | dcl <- directdeclsExprs]
                                  , objstrs = [] }
                , ifcPos    = Origin "generated interface for each concept in TblSQL or ScalarSQL"
                , ifcPrp    = "Interface " ++name c++" has been generated by Ampersand."
                , ifcRoles = []
                }
           | c<-concs fSpec, let directdeclsExprs = [EDcD d | d<-relsDefdIn fSpec, c `elem` concs d]]
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
           [Ifc { ifcParams = [ p | p <- concatMap primitives (expressionsIn objattributes), not (isIdent p)]
                , ifcArgs   = []
                , ifcObj    = Obj { objnm   = name c
                                  , objpos  = Origin "generated object: step 4a - default theme"
                                  , objctx  = EDcI c
                                  , objmsub = Just . Box c $ objattributes
                                  , objstrs = [] }
                , ifcPos    = Origin "generated interface: step 4a - default theme"
                , ifcPrp    = "Interface " ++name c++" has been generated by Ampersand."
                , ifcRoles  = []
                }
           | cl <- eqCl (source.head) [ pth | pth<-maxTotPaths `uni` maxInjPaths, (source.head) pth `elem` gPlugConcepts ]
           , let objattributes = recur cl
           , not (null objattributes) --de meeste plugs hebben in ieder geval I als attribuut
           , --exclude concept A without cRels or dRels (i.e. A in Scalar without total associations to other plugs) 
             not (length objattributes==1 && isIdent(objctx(head objattributes)))  
           , let e0=head cl, if null e0 then fatal 284 "null e0" else True
           , let c=source (head e0)
           ]
        --end otherwise: default theme
        --end stap4a
        step4b --generate lists of concept instances for those concepts that have a generated INTERFACE in step4a 
         = [Ifc { ifcParams = ifcParams ifcc
                , ifcArgs   = ifcArgs   ifcc
                , ifcObj    = Obj { objnm   = nm
                                  , objpos  = Origin "generated object: step 4b"
                                  , objctx  = EDcI ONE
                                  , objmsub = Just . Box ONE $ [att]
                                  , objstrs = [] }
                , ifcPos    = ifcPos  ifcc
                , ifcPrp    = ifcPrp  ifcc
                , ifcRoles  = []
                }
           | ifcc<-step4a
           , let c   = source(objctx (ifcObj ifcc))
                 nm'::Int->String
                 nm' 0  = plural (fsLang fSpec) (name c)
                 nm' i  = plural (fsLang fSpec) (name c) ++ show i
                 nms = [nm' i |i<-[0..], nm' i `notElem` map name (ctxifcs context)]
                 nm
                   | theme flags == StudentTheme = name c
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
   makeActivity :: Fspc -> Rule -> Activity
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
        invQs       = [q | q@(Quad _ ccrs)<-vquads fSpec, (not.isSignal.cl_rule.qClauses) q
                         , (not.null) ((relsUsedIn.cl_rule) ccrs `isc` decls)] -- SJ 20111201 TODO: make this selection more precise (by adding inputs and outputs to a quad).
-- a relation affects another if there is a quad (i.e. an automated action) that links them
        affectPairs = [(qDcl q,[q], d) | q<-invQs, d<-(relsUsedIn.cl_rule.qClauses) q]
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
   quads :: Options -> (Declaration->Bool) -> [Rule] -> [Quad]
   quads flags visible rs
    = [ Quad { qDcl     = d
             , qClauses = allClauses flags rule
             }
      | rule<-rs, d<-relsUsedIn rule, visible d
      ]

-- The function allClauses yields an expression which has constructor EUni in every case.
   allClauses :: Options -> Rule -> Clauses
   allClauses flags rule = Clauses [RC { rc_int = i
                                       , rc_rulename = name rule
                                       , rc_conjunct = dnf2expr dnfClause
                                       , rc_dnfClauses = allShifts flags dnfClause
                                       } | (dnfClause,i)<-zip (conjuncts rule) [0..] ] rule

   allShifts :: Options -> DnfClause -> [DnfClause]
   allShifts _ conjunct = (map head.eqCl (disjNF.dnf2expr)) [ e'| e'<-shiftL conjunct++shiftR conjunct]  -- we want to nub all dnf-clauses, but nub itself does not do the trick...
-- allShifts conjunct = error $ show conjunct++concat [ "\n"++show e'| e'<-shiftL conjunct++shiftR conjunct] -- for debugging
    where
    {-
     diagnostic
      = intercalate "\n  "
          [ "shiftL: [ "++intercalate "\n          , " [showHS flags "\n            " e | e<-shiftL conjunct    ]++"\n          ]"
          , "shiftR: [ "++intercalate "\n          , " [showHS flags "\n            " e | e<-shiftR conjunct    ]++"\n          ]"
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
   preEmpt :: [ECArule] -> [ECArule]
   preEmpt ers = pr [length ers] (10::Int)
    where
     pr :: [Int] -> Int -> [ECArule]
     pr ls n
       | n == 0              = fatal 633 $ "too many cascading levels in preEmpt "++show ls
       | (not.null) cascaded = pr (length cascaded:ls)
                               -- ([er{ecaAction=normPA (ecaAction er)} | er<-cascaded] ++uncasced)
                                  (n-1)
       | otherwise           = [er{ecaAction=normPA (ecaAction er)} | er<-uncasced]
      where
-- preEmpt divides all ECA rules in uncascaded rules and cascaded rules.
-- cascaded rules are those rules that have a Do component with event e, where e is known to block (for some other reason)
       new  = [er{ecaAction=normPA (ecaAction er)} | er<-ers]
       cascaded = [er{ecaAction=action'} | er<-new, let (c,action') = cascade (eDcl (ecaTriggr er)) (ecaAction er), c]
       uncasced = [er |                    er<-new, let (c,_)       = cascade (eDcl (ecaTriggr er)) (ecaAction er), not c]
-- cascade inserts a block on the place where a Do component exists that matches the blocking event.
--     cascade :: Relation -> PAclause -> (Bool, PAclause)
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
--   cascade rel (Sel c e cl m)   = ((fst.cascade rel.cl) "dummystr",     Sel c e (snd.cascade rel.cl)   m)
     cascade rel (CHC ds m)       = (any (fst.cascade rel) ds, CHC (map (snd.cascade rel) ds) m)
     cascade rel (ALL ds m)       = (any (fst.cascade rel) ds, ALL (map (snd.cascade rel) ds) m)
     cascade  _  (Nop m)          = (False, Nop m)
     cascade  _  (Blk m)          = (False, Blk m)
     cascade  _  (Let _ _ _)  = fatal 611 "Deze constructor is niet gedefinieerd" -- HJO, 20131205:Toegevoegd om warning te verwijderen
     cascade  _  (Ref _)      = fatal 612 "Deze constructor is niet gedefinieerd" -- HJO, 20131205:Toegevoegd om warning te verwijderen
     cascade  _  (GCH{})      = fatal 655 "Deze constructor is niet gedefinieerd" -- SJO, 20140428:Toegevoegd om warning te verwijderen
        

   switchboard :: Options -> Fspc -> Fswitchboard
   switchboard flags fSpec
    = Fswtch
       { fsbEvIn  = eventsIn
       , fsbEvOut = eventsOut
       , fsbConjs = conjs
       , fsbECAs  = ecas
       }
      where
        qs :: [Quad]
        qs        = quads flags visible (invariants fSpec)
        (ecas, _) = unzip (assembleECAs fSpec)
        conjs     = nub [ (cl_rule ccrs,rc_conjunct x) | Quad _ ccrs<-qs, x<-cl_conjNF ccrs]
        eventsIn  = nub [ecaTriggr eca | eca<-ecas ]
        eventsOut = nub [evt | eca<-ecas, evt<-eventsFrom (ecaAction eca)]
        visible _ = True

-- Auxiliaries
-- chop :: [a] -> [([a], [a])]
-- chop xs = [ splitAt i xs | i <- [1..length xs-1]]
   
   
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
   