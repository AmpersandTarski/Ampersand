{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec 
    (makeFspec,actSem, delta, allClauses, quads, assembleECAs, preEmpt, genPAclause, editable, conjuncts)
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
   import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms  --  (conjNF,disjNF,normPA, exprUni2list, exprIsc2list, exprCps2list)
   import DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Plug
--   import DatabaseDesign.Ampersand.Fspec.ShowHS -- only for diagnostic purposes during debugging
   import DatabaseDesign.Ampersand.Fspec.ShowADL
   import Text.Pandoc
   import Data.List (nub,nubBy,intercalate,intersect,partition,group,delete)
   import DatabaseDesign.Ampersand.ADL1.Expression
   import Data.Char        (toLower)
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
                   -- interfaceS contains the interfaces defined in the Ampersand script.
                   -- interfaces are meant to create user interfaces, programming interfaces and messaging interfaces.
                   -- A generic user interface (the Lonneker interface) is already available.
                 , themes       = ctxthms context      -- The names of patterns/processes to be printed in the functional specification. (for making partial documentation)
                 , fsLang       = ctxlang context      -- The default language for this specification, if specified at all.
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
                 , vEcas        = {-preEmpt-} assembleECAs [q | q<-vquads fSpec, isInvariantQuad q] -- TODO: preEmpt gives problems. Readdress the preEmption problem and redo, but properly.
                 , vrels        = calculatedDecls
                 , allUsedDecls = declsUsedIn context
                 , allDecls     = alldecls
                 , allConcepts  = concs context
                 , kernels      = constructKernels
                 , fsisa        = []
                 , vpatterns    = patterns context
                 , vgens        = gens context
                 , vIndices     = identities context
                 , vviews       = viewDefs context
                 , vConceptDefs = conceptDefs context
                 , fSexpls      = ctxps context
                 , metas        = ctxmetas context
                 , initialPops  = initialpops
                 , allViolations = [(r,vs) |r<- allrules, not (isSignal r), let vs = ruleviolations (gens context) initialpops r,  not (null vs)]
                 }
        alldecls = declarations context
        allQuads = quads flags (\_->True) allrules
        initialpops = userDefPops++singletonpops
        userDefPops = ctxpopus context
        singletonpops = [ PCptPopu{ popcpt = head [c | EMp1 _ c<-cl ]
                                  , popas  =      [a | EMp1 a c<-cl, not (name c=="SESSION") ] 
                                  } 
                        | cl<-eqCl (\(EMp1 _ c)->c) (mp1Exprs context)]
        isInvariantQuad q = null [r | (r,rul)<-maintains context, rul==cl_rule (qClauses q)]
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
        calculatedDecls = map calcProps alldecls
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
        -- declarations to be saved in generated plugs: if decplug=True, the declaration has the BYPLUG and therefore may not be saved in a database
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
--  Step 1: select and arrange all declarations to obtain a set cRels of total relations
--          to ensure insertability of entities (signal declarations are excluded)
        cRels = [     EDcD d  | d@Sgn{}<-declarations context, not(deciss d), isTot d, not$decplug d]++
                [flp (EDcD d) | d@Sgn{}<-declarations context, not(deciss d), not (isTot d) && isSur d, not$decplug d]
--  Step 2: select and arrange all declarations to obtain a set dRels of injective relations
--          to ensure deletability of entities (signal declarations are excluded)
        dRels = [     EDcD d  | d@Sgn{}<-declarations context, not(deciss d), isInj d, not$decplug d]++
                [flp (EDcD d) | d@Sgn{}<-declarations context, not(deciss d), not (isInj d) && isUni d, not$decplug d]
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
--                student theme => generate interface for each concept with declarations where concept is source or target (note: step1-3 are skipped)
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
           | c<-concs fSpec, let directdeclsExprs = [EDcD d | d<-declarations fSpec, c `elem` concs d]]
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
                                     , explRefId = "Regel "++name rul
                                     }
                               ,Expl { explPos = OriginUnknown
                                     , explObj = ExplRule (name rul)
                                     , explMarkup = A_Markup { amLang   = English
                                                             , amFormat = ReST
                                                             , amPandoc = [Plain [Str "For what purpose activity ", Quoted SingleQuote [Str (name rul)], Str" exists remains undocumented." ]]
                                                             }
                                     , explUserdefd = False
                                     , explRefId = "Regel "++name rul
                                     }
                               ]
                 } in s
    where
-- declarations that may be affected by an edit action within the transaction
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
             fst3 (a,_,_) = a
             thd3 (_,_,c) = c
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
   allShifts _ conjunct = nub [ e'| e'<-shiftL conjunct++shiftR conjunct]
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
                                                     if source antcExpr==target antcExpr then [EDcI (source antcExpr)] else fatal 425 "antcExpr should be endorelation"
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
                   , ts<-move [flp h.:.as |as<-ass] (map tailECps css)]++ -- example: ts<-move [ [flp "x","r","s"], [flp "x","p","r"] ]  [ ["y","z"] ]
               [ts | let lastEs = map lastECps css
                   , length (eqClass (==) lastEs) == 1
                   , let l=head lastEs
                   , isInj l
                   , ts<-move [as.:.flp l |as<-ass] (map initECps css)]   -- example: ts<-move [ ["r","s",flp "z"], ["p","r",flp "z"] ]  [ ["x","y"] ]
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
                   , ts<-move (map tailECps ass) [flp h.:.cs |cs<-css]]++   -- example: ts<-move  [["s"], ["r"]] [ [flp "r","x","y","z"] ]
               [ts | let lastEs = map lastECps ass
                   , length (eqClass (==) lastEs) == 1                      -- example: False, because map lastECps ass == [ ["s"], ["r"] ]
                   , let l=head lastEs
                   , isTot l
                   , ts<-move (map initECps ass) [cs.:.flp l |cs<-css]]     -- is dit goed? cs.:.flp l wordt links zwaar, terwijl de normalisator rechts zwaar maakt.
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


-- Deze functie neemt verschillende clauses samen met het oog op het genereren van code.
-- Hierdoor kunnen grotere brokken procesalgebra worden gegenereerd.
   assembleECAs :: [Quad] -> [ECArule]
   assembleECAs qs
    = [] -- [er{ecaAction=normPA (ecaAction er)} | (ecarule,i) <- zip ecas [(1::Int)..], let er=ecarule i]
      where
       -- the quads that are derived for this fSpec contain dnf clauses.
       -- A dnf clause dc that is generated from rule r contains the information how to restore the truth of r.
       -- Suppose an insert or delete event on a relation rel has occurred and rel is used in rule r.
       -- A restore action from dnf clause dc will then restore the truth of r.
       
       -- First, we harvest the quads from fSpec in quadruples.
       -- rel    is a relation that may be affected by an (insert- or delete-) event.
       -- dnfClauses is a set of dnf clauses that can restore the truth of conj after rel has been affected.
       -- conj   is an expression that must remain true at all times.
       -- rul    is the rule from which the above have been derived (for traceability)
       -- these quadruples are organized per relation.
       -- This puts together all dnf clauses we need for each relation.
       relEqCls = eqCl fst4 [(dcl,rc_dnfClauses x,rc_conjunct x,cl_rule ccrs) | Quad dcl ccrs<-qs, x <-cl_conjNF ccrs]
       -- The eca rules can now be assembled from the available material
       ecas
        = [ ECA (On ev dcl) delt act
          | relEq <- relEqCls                   -- The material required for one relation
          , let (dcl,_,_,_) = head relEq        -- This is the relation
          , let EDcD delt   = delta (sign dcl)  -- delt is a placeholder for the pairs that have been inserted or deleted in rel.
          , ev<-[Ins,Del]                       -- This determines the event: On ev rel
          , let act = ALL [ CHC [ (if isTrue  clause' || isTrue step then Nop else
                                   if isFalse clause'                then Blk else
--                                 if not (visible rel) then Blk else
                                   let visible _ = True in genPAclause visible ev toExpr viols
                                  ) [(conj,causes)]  -- the motivation for these actions
                                | clause@(Dnf antcs conss) <- dnfClauses
                                , let expr    = dnf2expr clause
                                , let sgn     = sign expr -- different dnf clauses may have different signatures
                                , let clause' = conjNF (subst (dcl, actSem ev dcl (delta (sign dcl))) expr)
                                , let step    = conjNF (notCpl expr .\/. clause')
                                , let viols   = conjNF (notCpl clause')
                                , let negs    = foldr (./\.) (EDcV sgn) antcs
                                , let poss    = foldr (.\/.) (notCpl (EDcV sgn)) conss
                                , let frExpr  = if ev==Ins
                                                then conjNF negs
                                                else conjNF poss
                                , dcl `elem` relsUsedIn frExpr
                                , let toExpr = if ev==Ins
                                               then conjNF poss
                                               else conjNF (notCpl negs)
                                ]
                                [(conj,causes)]  -- to supply motivations on runtime
                          | conjEq <- eqCl snd3 [(dnfClauses,conj,rule) | (_,dnfClauses,conj,rule)<-relEq]
                          , let causes               = nub (map thd3 conjEq)
                          , let (dnfClauses,conj,_) = head conjEq
                          ]
                          [(conj,nub [r |(_,_,_,r)<-cl]) | cl<-eqCl thd4 relEq, let (_,_,conj,_) = head cl]  -- to supply motivations on runtime
          ]
       fst4 (w,_,_,_) = w
       snd3 (_,y,_) = y
       thd3 (_,_,z) = z
       thd4 (_,_,z,_) = z

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
     cascade rel (Sel c e cl m)   = ((fst.cascade rel.cl) "dummystr",     Sel c e (snd.cascade rel.cl)   m)
     cascade rel (CHC ds m)       = (any (fst.cascade rel) ds, CHC (map (snd.cascade rel) ds) m)
     cascade rel (ALL ds m)       = (any (fst.cascade rel) ds, ALL (map (snd.cascade rel) ds) m)
     cascade  _  (Nop m)          = (False, Nop m)
     cascade  _  (Blk m)          = (False, Blk m)
     cascade  _  (Let _ _ _)  = fatal 611 "Deze constructor is niet gedefinieerd" -- HJO, 20131205:Toegevoegd om warning te verwijderen
     cascade  _  (Ref _)      = fatal 612 "Deze constructor is niet gedefinieerd" -- HJO, 20131205:Toegevoegd om warning te verwijderen
        
   conjuncts :: Rule -> [DnfClause]
   conjuncts = map disjuncts.exprIsc2list.conjNF.rrexp
   disjuncts :: Expression -> DnfClause
   disjuncts conj = (split (Dnf [] []).exprUni2list) conj
    where split :: DnfClause -> [Expression] -> DnfClause
          split (Dnf antc cons) (ECpl e: rest) = split (Dnf (e:antc) cons) rest
          split (Dnf antc cons) (     e: rest) = split (Dnf antc (e:cons)) rest
          split dc             []             = dc

   -- | Action semantics for inserting a delta into a relation dcl.
   actSem :: InsDel -> Declaration -> Expression -> Expression
   actSem Ins dcl e@(EDcD d) | dcl==d        = EDcD dcl
                             | otherwise     = EDcD dcl .\/. e
   actSem Ins dcl delt | sign dcl/=sign delt = fatal 598 "Type error in actSem Ins"
                       | otherwise           = disjNF (EDcD dcl .\/. delt)
   actSem Del dcl e@(EDcD d) | dcl==d        = notCpl (EDcV (sign d))
                             | otherwise     = EDcD dcl ./\. notCpl e
   actSem Del dcl delt | sign dcl/=sign delt = fatal 603 "Type error in actSem Del"
                       | otherwise           = conjNF (EDcD dcl ./\. notCpl delt)

   delta :: Sign -> Expression
   delta sgn
    = EDcD   Sgn { decnm   = "Delta"
                 , decsgn  = sgn
                 , decprps = []
                 , decprps_calc = Nothing
                 , decprL  = ""
                 , decprM  = ""
                 , decprR  = ""
                 , decMean = AMeaning [ A_Markup Dutch   ReST (string2Blocks ReST "TODO: Doel van deze Delta relatie opschrijven")
                                      , A_Markup English ReST (string2Blocks ReST "TODO: Write down the purpose of this Delta relation.")
                                      ]
                 , decConceptDef = Nothing
                 , decfpos = Origin ("generated relation (Delta "++show sgn++")")
                 , deciss  = True
                 , decusr  = False
                 , decISA  = False
                 , decpat  = ""
                 , decplug = True
                 } 

   -- | de functie genPAclause beschrijft de voornaamste mogelijkheden om een expressie delta' te verwerken in expr (met tOp'==Ins of tOp==Del)
-- TODO: Vind een wetenschappelijk artikel waar de hier beschreven transformatie uitputtend wordt behandeld.
-- TODO: Deze code is onvolledig en misschien zelfs fout....
   genPAclause :: (Declaration->Bool)        -- ^True if a relation may be changed (i.e. is editable)
                  -> InsDel                  -- ^the type of action: Insert or Delete
                  -> Expression              -- ^the expression in which a delete or insert takes place
                  -> Expression              -- ^the delta to be inserted or deleted
                  -> [(Expression,[Rule])]   -- ^the motivation, consisting of the conjuncts (traced back to their rules) that are being restored by this code fragment.
                  -> PAclause
   genPAclause editAble tOp' expr1 delta1 motive = genPAcl delta1 tOp' expr1 motive
    where
      genPAcl deltaX tOp exprX motiv =
        case (tOp, exprX) of
          (_ ,  EFlp x)     -> genPAcl (flp deltaX) tOp x motiv
          (_ ,  EBrk x)     -> genPAcl deltaX tOp x motiv
          (Ins, ECpl x)     -> genPAcl deltaX Del x motiv
          (Del, ECpl x)     -> genPAcl deltaX Ins x motiv
          (Ins, e@EUni{})   -> CHC [ genPAcl deltaX Ins f motiv | f<-exprUni2list e{-, not (f==expr1 && Ins/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Ins, e@EIsc{})   -> ALL [ genPAcl deltaX Ins f []    | f<-exprIsc2list e ] motiv
          (Ins, e@ECps{})   -> CHC [ {- the following might be useful for diagnostics:
                                     if showADL exprX=="project;partof~"
                                     then fatal 702 ( "Diagnostic error\n"++
                                                      "chop (exprCps2list e)= "++show [(map showADL ls, map showADL rs)| (ls,rs)<-chop (exprCps2list e) ]++
                                                      "\nConcepts c="++showADL c++",  target els="++showADL (target els)++",  source ers="++showADL (source ers)++
                                                      "\nfLft \"x\"= "++showADL (fLft "x") ++
                                                      "\nfRht \"x\"= "++showADL (fRht "x")
                                                    )
                                     else -}
                                     if els==flp ers
                                     then CHC [ New c fLft motiv
                                              , Sel c els fLft motiv
                                              ] motiv
                                     else CHC [ New c (\x->ALL [fLft x, fRht x] motiv) motiv
                                              , Sel c els fLft motiv
                                              , Sel c (flp ers) fRht motiv
                                              ] motiv
                                   | (ls,rs)<-chop (exprCps2list e)
                                   , let els=foldCompose ls
                                   , let ers=foldCompose rs
                                   , let c=source ers  -- SJ 20131202: Note that target ers==source els
                                   , let fLft atom = genPAcl (disjNF ((EMp1 atom (source ers) .*. deltaX) .\/. notCpl ers)) Ins ers []
                                   , let fRht atom = genPAcl (disjNF ((deltaX .*. EMp1 atom (target els)) .\/. notCpl els)) Ins els []
                                   ] motiv
                               where foldCompose xs = case xs of
                                                       [] -> fatal 659 "Going into (foldr1 (.:.)) with an empty list"
                                                       _  -> foldr1 (.:.) xs
{- Problem: how to insert Delta into r;s
This corresponds with:  genPAclause editAble Ins (ECps (r,s)) Delta motive
Let us solve it mathematically,  and gradually transform via pseudo-code into Haskell code.
The problem is how to find dr and ds such that 
   Delta \/ r;s  |-  (dr\/r) ; (ds\/s)
Since r;s  |-  (dr\/r) ; (ds\/s) is true by definition, we simplify:
   Delta  |-  (dr\/r) ; (ds\/s)
One way of doing this is the "bow tie" solution, which uses one element, c, and chooses dr = Delta*c  and ds = c*Delta.
There is a fair chance, however, that this solution breaks existing rules.
For example, if Delta< has multiple elements and r is injective, we have a violation. (Similarly, if Delta> has multiple elements and s is univalent)
Since we make no assumptions on the rules that r and s must satisfy, let us look for more subtle solutions.
For instance, we might choose dr = d;s~  and ds = r~;d, for that part of Delta that is not in r;s (let d = Delta-r;s)
The approach is to do this first, then see which links are left and connect these last ones with a bow tie approach.

SEQ [ ASSIGN d (EDif (delta,e))
    , ALL [ FOREACH (x,y) FROM d DO
              SEQ [ SEL z FROM SELECT (x',z) FROM r WHERE x==x'
                  , INSERT (z,y) INTO s
                  ]
          , FOREACH (x,y) FROM d DO
              SEQ [ SEL z FROM SELECT (z,y') FROM s WHERE y==y'
                  , INSERT (x,z) INTO r
                  ]
          , SEQ [ ASSIGN delta' [ (x,y) | (x,y)<-d, x `notElem` dom e, y `notElem` cod e]
                , CHC [ NEW z IN target r `meet` source s
                      , SEL z FROM contents (target r `meet` source s)
                      ]
                , INSERT [ (z,y) | (x,y)<-delta' ] INTO s
                , INSERT [ (x,z) | (x,y)<-delta' ] INTO r
                ]
          ]
    ]

On this algorithm, there are some attractive optimizations. 
Some code generation can be prevented, when we know that r or s are not editable.
Generation of a SEL statement can be prevented, when we know that r is univalent or s is injective.

This is what becomes of it:

SEQ [ ASSIGN d (EDif (delta,e))
    , COMMENT "Variable d contains only those links that need to be inserted into e."
    , CONDITIONAL not (null d)
      (ALL((if editable s
            then [ COMMENT "The following statement inserts tuples only on the right side of the composition."
                 , FOREACH (x,y) FROM d DO
                    SEQ [ if injective s
                          then            SELECT (x',z) FROM r WHERE x==x'
                          else SEL z FROM SELECT (x',z) FROM r WHERE x==x'
                        , INSERT (z,y) INTO s
                        ]
                 ]
            else [] )++
           (if editable r
            then [ COMMENT "The following statement inserts tuples only on the left side of the composition."
                 , FOREACH (x,y) FROM d DO
                    SEQ [ if injective s
                          then            SELECT (z,y') FROM s WHERE y==y'
                          else SEL z FROM SELECT (z,y') FROM s WHERE y==y'
                        , INSERT (x,z) INTO r
                        ]
                 ]
            else [] )++
           (if editable r && editable s
            then [ COMMENT "All links that can be added just in r or just in s have now been added. Now we finish the rest with a bow tie."
                 , COMMENT "The following statement inserts tuples both on the left and the right side of the composition."
                 , SEQ [ ASSIGN delta' [ (x,y) | (x,y)<-d, x `notElem` dom e, y `notElem` cod e]
                       , CHC [ NEW z IN target r `meet` source s
                             , SEL z FROM contents (target r `meet` source s)
                             ]
                       , INSERT [ (z,y) | (x,y)<-delta' ] INTO s
                       , INSERT [ (x,z) | (x,y)<-delta' ] INTO r
                       ]
                 ]
            else [] )))
    ]

We can use an "Algebra of Imperative Programs" to move towards real code. Suppose we have the following data structure of imperative programs:
data Program = SEQ [Program]               -- execute programs in sequence
             | CONDITIONAL Cond Program    -- evaluate the computation (Cond), which yields a boolean result, and execute the program if it is true
             | CHC [Program]               -- execute precisely one program from the list
             | ALL [Program]               -- execute all programs in arbitrary order (may even be parallel)
             | New String Concept Program  -- create a named variable (the String) of type C (the concept) and execute the program, which may use this concept.
             | NewAtom PHPRel Concept      -- assign a brand new atom of type Concept to the PHPRel, which must be a PHPvar (i.e. a variable).
             | ASSIGN PHPRel PHPExpression -- execute a relation expression in PHP and assign its result to the PHPRel, which must be a PHPvar (i.e. a variable).
             | Insert PHPExpression DBrel  -- execute the computation and insert its result into the database
             | Delete PHPExpression DBrel  -- execute the computation and delete its result from the database
             | COMMENT String              -- ignore this. This is part of the algebra, so we can generate commented code.
             | Nop                         -- do nothing
             | Blk [(Expression,[Rule])]   -- abort, leaving an error message that motivates this. The motivation consists of the conjuncts (traced back to their rules) that are being restored by this code fragment.
data Cond    = NotNull PHPRel  -- a condition on a PHPRel

In order to proceed to the next refinement, we have enriched the PHPExpression data structure with variables (PHPvar) and queries (PHPqry) 
We need a variable in PHP that represents a relation, which gets the label PHPvar.
We also need to query an Expression on the SQL server. The result of that query is relation contents in PHP, which gets the label PHPqry.
The 'almost Haskell' program fragment for generating an insertion of Delta into r;s is now:
let delta = PHPvar{ phpVar = "delta"       -- this is the input
                  , phptyp = phpsign (sign e)    -- the type is used nowhere, but it feels good to know the type.
                  } in
let d     = PHPvar{ phpVar = "d"           -- this is the input, from which everything already in  Ecps es  is removed.
                  , phptyp = phpsign (sign e)    -- the type is used nowhere, but it feels good to know the type.
                  } in
let newAtoms = [ NewAtom (PHPvar (head (name c):show i) c | (r,s,i)<-zip3 (init es) (tail es) [1..], let c=target(r) `meet` source(s)] in
SEQ [ ASSIGN d (PHPEDif (PHPERel delta, PHPRel (PHPqry e)))    -- let d = Delta - e0;e1;e2;...
    , COMMENT "d contains all links that must be inserted in r;s"
    , CONDITIONAL (NotNull d)
      (SEQ [ ALL ( (if editable(head es)    
                    then [ Insert (PHPECps [PHPERel d, PHPERel (PHPqry (flp (ECps (tail es))))]) (mkDBrel (head es)) ]
                    else []) ++
                   (if editable(last es)
                    then [ Insert (PHPECps [PHPERel (PHPqry (flp (ECps (init es)))), PHPERel d]) (mkDBrel (last es)) ]
                    else []) )
           , COMMENT "All links that can be added just in r or just in s have now been added. Now we finish the rest with a bow tie."] ++
           if or [not (editable e) | e<-es] || or [ not (editable (I c)) | NewAtom _ c<-newAtoms ]  then [] else
           [ -- let us see which links are left to be inserted
             ASSIGN d (PHPEDif (PHPERel d, PHPRel (PHPqry (ECps es))))
           , COMMENT "d contains the remaining links to be inserted in r;s"
           , CONDITIONAL (NotNull d)
             (SEQ ( newAtoms ++
                    ALL ( let vs = [ v | NewAtom v _ <- newAtoms ] in
                          [ Insert (PHPEPrd [PHPERel d, PHPERel (PHPvar (head vs))]) (mkDBrel (head es)) ]++
                          [ Insert (PHPEprd [PHPERel cl, PHPERel cr]) (mkDBrel e) | (cl,cr,e)<-zip (init vs) (tail vs) tail (init es))] ++
                          [ Insert (PHPEPrd [PHPERel (PHPvar (last vs)), PHPERel d]) (mkDBrel (last es)) ])
             )    )
           ]
      )
    ]

In order to proceed to the real code, we must realize that mkDBrel is not defined yet.
The argument of mkDBrel is an Expression. This can be an ERel{} or something different.
In case it is an ERel{}, the insert can be translated directly to a database action.
If it is not, the insert can be treated as a recursive call to genPAcl, which breaks down the expression further until it reaches a relation.
let delta = PHPvar{ phpVar = "delta"       -- this is the input
                  , phptyp = phpsign (sign e)    -- the type is used nowhere, but it feels good to know the type.
                  } in
let d     = PHPvar{ phpVar = "d"           -- this is the input, from which everything already in  Ecps es  is removed.
                  , phptyp = phpsign (sign e)    -- the type is used nowhere, but it feels good to know the type.
                  } in
let newAtoms = [ NewAtom (PHPvar (head (name c):show i) c | (r,s,i)<-zip3 (init es) (tail es) [1..], let c=target(r) `meet` source(s)] in
SEQUENCE [ ASSIGN d (PHPEDif (PHPERel delta, PHPRel (PHPqry (ECps es))))
         , COMMENT "d contains all links to be inserted in r;s"
         , CONDITIONAL (NotNull d)
           (SEQUENCE([ ALL ( (if editable(head es)
                              then [ if isRel (head es)
                                     then Insert (PHPECps (PHPERel d, PHPERel (PHPqry (flp (ECps (tail es)))))) (let PHPRel r = head es in r)
                                     else genPAclause editAble Ins (head es) (PHPECps [PHPERel d, PHPERel (PHPqry (flp (ECps (tail es))))]) motive
                                   ]
                              else []) ++
                             (if editable(last es)
                              then [ if isRel (last es)
                                     then Insert (PHPECps [PHPERel (PHPqry (flp (ECps (init es)))), PHPERel d]) (let PHPRel r = last es in r)
                                     else genPAclause editAble Ins (last es) (PHPECps [PHPERel (PHPqry (flp (ECps (init es)))), PHPERel d]) motive
                                   ]
                              else []) )
                     , COMMENT "All links that can be added just in r or just in s have now been added. Now we finish the rest with a bow tie."] ++
                     if or [not (editable e) | e<-es] || or [ not (editable (I c)) | NewAtom _ c<-newAtoms ]  then [] else
                     [ -- let us see which links are left to be inserted
                       ASSIGN d (PHPEDif (PHPERel d, PHPRel (PHPqry (ECps es))))
                     , COMMENT "d contains the remaining links to be inserted in r;s"
                     , CONDITIONAL (NotNull d)
                       (SEQ ( newAtoms ++
                              ALL ( let vs = [ v | NewAtom v _ <- newAtoms ] in
                                    [ if isRel (head es)
                                      then Insert (PHPEPrd [PHPERel d, PHPERel (PHPvar (head vs))]) (let PHPRel r = head es in r)
                                      else genPAclause editAble Ins (head es) (PHPEPrd [PHPERel d, PHPERel (PHPvar (head vs))]) motive]++
                                    [ Insert (PHPEprd [PHPERel cl, PHPERel cr]) (mkDBrel e) | (cl,cr,e)<-zip (init vs) (tail vs) tail (init es))] ++
                                    [ if isRel (last es)
                                      then Insert (PHPEPrd [PHPERel (PHPvar (last vs)), PHPERel d]) (let PHPRel r = last es in r)
                                      else genPAclause editAble Ins (last es) (PHPEPrd [PHPERel d, PHPERel (PHPvar (head vs))]) motive ])
                       )    )
                     ])
           )
         ]

-}
          (Del, e@ECps{})   -> CHC [ if els==flp ers
                                     then CHC [ Sel c (disjNF els) (\_->Rmv c fLft motiv) motiv
                                              , Sel c (disjNF els) fLft motiv
                                              ] motiv
                                     else CHC [ Sel c (disjNF (els ./\. flp ers)) (\_->Rmv c (\x->ALL [fLft x, fRht x] motiv) motiv) motiv
                                              , Sel c (disjNF (els ./\. flp ers)) fLft motiv
                                              , Sel c (disjNF (els ./\. flp ers)) fRht motiv
                                              ] motiv
                                   | (ls,rs)<-chop (exprCps2list e)
                                   , let ers=foldCompose rs
                                   , let els=foldCompose ls
                                   , let c=source ers  -- SJ 20131202: Note that target ers==source els
                                   , let fLft atom = genPAcl (disjNF ((EMp1 atom (source ers) .*. deltaX) .\/. notCpl ers)) Del ers []  -- TODO (SJ 26-01-2013) is this double code?
                                   , let fRht atom = genPAcl (disjNF ((deltaX .*. EMp1 atom (target els)) .\/. notCpl els)) Del els []
                                   ] motiv
                               where foldCompose xs = case xs of
                                                       [] -> fatal 851 "Going into (foldr1 (.:.)) with an empty list"
                                                       _  -> foldr1 (.:.) xs
{- Purpose: how to delete Delta from ECps es
This corresponds with:  genPAclause editAble Del (ECps es) Delta motive
One way of doing this is to select from es one subexpression, e, that is editable.
By removing the proper links from that particular e, all chains are broken.

Splitting the terms on one particular subexpression can be done as follows:
  triples = [ (take i es, es!!i, drop (i+1) es) | i<-[0..length es-1]  ]
This gives us triples, (left,e,right), each of which is a candidate for deletion.
There is no point in having two candidates, (left,e,right) and (left',e',right'), with e==e'.
Since e and e' are aliases of the underlying database relation, deletion from e need not be repeated by deletion from e'.
That is why double candidates may be excluded. Here is how:
  triples = [ (take i es, es!!i, drop (i+1) es) | i<-[0..length es-1], es!!i `notElem` (take i es)  ]
If e is a relation and editable as well, this relation can be used for deletion.
The elements from e to be deleted are those, that establish a link between cod(left) and dom(right).
In order to eliminate these, no link from the expression  left~;e;right~  may survive.
If we sum up the alternatives in pseudocode, we get:
CHC [ DELETE ((ECps (reverse (map flp left) ++ [e] ++ reverse (map flp right))) FROM e
    | (left,ERel e _,right)<-triples
    , editable e ]
In order to refine further to real code, we must realize that deletion can be done in relations only. Not in expressions.
So if e is a relation, the DELETE can be executed.
If it is an expression, the function genPAclause can be called recursively.
We now get the following code fragment:
CHC [ if isRel e
      then DELETE (ECps (reverse (map flp left) ++ [e] ++ reverse (map flp right))) FROM  (let PHPRel r = e in r)
      else genPAclause editAble Del e (ECps (reverse (map flp left) ++ [e] ++ reverse (map flp right))) motive
    | (left,ERel e _,right)<-triples
    , editable e ]

-}
          (Del, e@EUni{}) -> ALL [ genPAcl deltaX Del f []    | f<-exprUni2list e {-, not (f==expr1 && Del/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Del, e@EIsc{}) -> CHC [ genPAcl deltaX Del f motiv | f<-exprIsc2list e ] motiv
-- Op basis van De Morgan is de procesalgebra in het geval van (Ins, ERad ts)  afleidbaar uit uit het geval van (Del, ECps ts) ...
          (_  , e@ERad{}) -> genPAcl deltaX tOp (deMorganERad e) motiv
          (_  , EPrd{})   -> fatal 896 "TODO"
          (_  , EKl0 x )  -> genPAcl (deltaK0 deltaX tOp x) tOp x motiv
          (_  , EKl1 x )  -> genPAcl (deltaK1 deltaX tOp x) tOp x motiv
          (_  , e@(EDcD d)) -> -- fatal 742 ("DIAG ADL2Fspec 764:\ndoCod ("++showADL deltaX++") "++show tOp++" ("++showADL exprX++"),\n"
                                   -- -- ++"\nwith disjNF deltaX:\n "++showADL (disjNF deltaX))
                                 if editAble d then Do tOp d deltaX motiv else Blk [(e, nub [r |(_,rs)<-motiv, r<-rs])]

-- HJO, 20130423: *LET OP!! De volgende code is er bij gefreubeld om geen last te hebben van de fatal767, maar is niet goed. *****
-- Dit is in overleg met Stef, die deze hele code toch compleet wil herzien, i.v.m. de nieuwe typechecker.
-- SJC, 20131012: Dan lijkt me dit een goed moment om deze regel weer uit te commentaren, aangezien ik nu de typechecker aan het herzien ben
--   ik doe dit even met een fatal ipv `echt' commentaar
-- SJ 20131117: I think not. Refactoring of this generator is nigh, though holding uppe other developments cannot be tolerated! So ye following code shall remain wrong rather than fatal... Long live the King!  ^_^ 
          (_ , e@EDcV{}) -> Blk [(e, nub [r |(_,rs)<-motiv, r<-rs])]
          (_ , _)        -> fatal 767 ( "(Stef?) Non-exhaustive patterns in the recursive call\n"
                                       ++"doCod ("++showADL deltaX++") -- deltaX\n      "++show tOp++"  -- tOp\n      ("++showADL exprX++") -- exprX\n"++
                                         "within function\ndoCode "++show tOp'++"  -- tOp'\n       ("++showADL expr1++") -- expr1\n       ("++showADL delta1++") -- delta1\n"++
                                         concat
                                         [ "while trying to maintain conjunct "++showADL conjunct++
                                           "\nfrom rule "++intercalate "\n          " [show r | r<-rs]
                                         | (conjunct,rs)<-motive ] ++
                                         if null motive then "null motive" else ""
                                         )

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
        ecas      = assembleECAs qs
        conjs     = nub [ (cl_rule ccrs,rc_conjunct x) | Quad _ ccrs<-qs, x<-cl_conjNF ccrs]
        eventsIn  = nub [ecaTriggr eca | eca<-ecas ]
        eventsOut = nub [On tOp dcl | eca<-ecas, doAct<-dos (ecaAction eca), let Do tOp e _ _=doAct, EDcD dcl<-[EDcD e, flp $ EDcD e]] -- TODO (SJ 26-01-2013) silly code: ERel rel _<-[ERel e (sign e), flp $ ERel e (sign e)] Why is this?
        visible _ = True

-- Auxiliaries
   chop :: [a] -> [([a], [a])]
   chop xs = [ splitAt i xs | i <- [1..length xs-1]]
   
   deltaK0 :: t -> InsDel -> t1 -> t
   deltaK0 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta |- x*)
   deltaK0 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x* leeg is)
   deltaK1 :: t -> InsDel -> t1 -> t
   deltaK1 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta |- x+)
   deltaK1 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x+ leeg is)

   
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
   