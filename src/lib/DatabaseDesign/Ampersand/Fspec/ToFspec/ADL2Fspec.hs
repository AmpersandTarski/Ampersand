{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec 
    (makeFspec,actSem, delta, allClauses, conjuncts, quads, assembleECAs, preEmpt, genPAclause, editable, editMph)
  where
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Core.Poset
   import Prelude hiding (Ord(..))
   import DatabaseDesign.Ampersand.ADL1.Rule
   import DatabaseDesign.Ampersand.Basics
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.ADL1
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import DatabaseDesign.Ampersand.Misc
   import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms    (conjNF,disjNF,normPA,simplify,isI)
--   import DatabaseDesign.Ampersand.Fspec.Plug
   import DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Plug       (makeSqlPlug,makeEntities,rel2plug)
   import DatabaseDesign.Ampersand.Fspec.ShowADL
--   import DatabaseDesign.Ampersand.Fspec.ShowHS
--   import DatabaseDesign.Ampersand.Fspec.FPA (FPA(..))
   import Text.Pandoc
   import Data.List (nub,intercalate)
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ToFspec.ADL2Fspec"

   makeFspec :: Options -> A_Context -> Fspc
   makeFspec flags context = fSpec
    where
        allQuads = quads flags (\_->True) allrules
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
                                       , isI ctxrel && source ctxrel==ONE
                                         || ctxrel `notElem` map (objctx.ifcObj) (interfaceS fSpec)
                                       , allInterfaces flags]  -- generated interfaces
                 , fSwitchboard = switchboard flags fSpec
                 , fActivities  = [ makeActivity fSpec rul | rul <-processRules context]
                 , fRoleRels    = mayEdit   context  -- fRoleRels says which roles may change the population of which relation.
                 , fRoleRuls    = maintains context  -- fRoleRuls says which roles maintain which rules.
                 , vrules       = udefrules context   -- all user defined rules
                 , grules       = multrules context++keyrules context
                 , allRules     = allrules
                 , vconjs       = nub [conj | Quad _ ccrs<-allQuads, (conj,_)<-cl_conjNF ccrs]
                 , vquads       = allQuads
                 , vEcas        = {-preEmpt-} assembleECAs [q | q<-vquads fSpec, isInvariantQuad q] -- TODO: preEmpt gives problems. Readdress the preEmption problem and redo, but properly.
                 , vrels        = allDecs -- contains all user defined plus all generated relations plus all defined and computed totals.
                 , fsisa        = [(s,g) | let (gE,classes)=ctxpo context, cl<-classes, s<-cl, g<-cl, s `gE` g==DatabaseDesign.Ampersand.Core.Poset.LT
                                         , null [c | c<-cl, s `gE` c==DatabaseDesign.Ampersand.Core.Poset.LT
                                                          , c `gE` g==DatabaseDesign.Ampersand.Core.Poset.LT]
                                  ]
                 , vpatterns    = patterns context
                 , vgens        = gens context
                 , vkeys        = keyDefs context
                 , vConceptDefs = conceptDefs context
                 , fSexpls      = [ xpl { explObj = case explObj xpl of ExplContext str -> ExplFspc str; _ -> explObj xpl } -- All explanations are uses as-is. Only the context-explanations are relabeled to Fspc-explanations.
                                  | xpl<-ctxps context]
                 , metas        = ctxmetas context
                 , vctxenv      = ctxenv context
                 , hasPopulations = (not.null.ctxpopus) context
                 , userDefPops  = userdefpops
                 , allViolations = [(r,vs) |r<- allrules, let vs = ruleviolations userdefpops r,  not (null vs)]
                 } 
        userdefpops = ctxpopus context
        isInvariantQuad q = null [r | (r,rul)<-maintains context, rul==cl_rule (qClauses q)]
        allrules = multrules context++keyrules context++udefrules context
        allProcs = [ FProc {fpProc = p
                           ,fpActivities =selectActs p
                           } | p<-ctxprocs context ]
                   where selectActs p   = [act | act<-fActivities fSpec
                                               , (not.null) (selRoles p act)]
                         selRoles p act = [r | (r,rul)<-maintains context, rul==actRule act, r `elem` roles p]
        -- | allDecs contains all user defined plus all generated relations plus all defined and computed totals.
        allDecs = [ d{decprps_calc = decprps d `uni` [Tot |      ERel r <-totals, d==makeDeclaration r]
                                               `uni` [Sur |EFlp (ERel r)<-totals, d==makeDeclaration r]}
                  | d<-declarations context]
     -- determine relations that are total (as many as possible, but not necessarily all)
        totals :: [Expression]
        totals
         = nub [rel | q<-quads flags visible (invariants fSpec), isIdent (qMorph q)
                    , (_,hcs)<-cl_conjNF (qClauses q), EUni fus<-hcs
                    , antc<-[(conjNF.EIsc) [notCpl f | f<-fus, isNeg f]], isI antc
                    , f<-fus, isPos f
                    , rel<-tots f   -- actually, rel is an expression, but in most cases it will be (ERel r) or (EFlp (ERel r))
                    ]
           where tots (ECps fs) = init fs++[flp r | r<-tail fs]  -- let I |- r;s;t be a rule, then r and s and t~ and s~ must all be total.
                 tots _ = []
                 visible _ = True -- for computing totality, we take all quads into account.

        --------------
        --making plugs
        --------------
        vsqlplugs = [InternalPlug (makeSqlPlug context p) | p<-ctxsql context] --REMARK -> no optimization like try2specific, because these plugs are user defined
        vphpplugs = [ExternalPlug p | p<-ctxphp context]
        definedplugs = vsqlplugs ++ vphpplugs
        allplugs = definedplugs ++      -- all plugs defined by the user
                   [InternalPlug (rename p (qlfname (name p)))
                   | p <- uniqueNames (map name definedplugs) -- the names of definedplugs will not be changed, assuming they are all unique
                                      (gPlugs ++ relPlugs)
                   ]
        -- all plugs with at least one flduniq=True field generated by the compiler
        gPlugs :: [PlugSQL]
        gPlugs   = makeEntities savedRels [p |InternalPlug p<-vsqlplugs]
        -- all plugs for relations not touched by definedplugs and gplugs
        relPlugs :: [PlugSQL]
        relPlugs = [ rel2plug rel totals --(see rel2plug in Plug.hs)
                   | rel<-savedRels
                   , Inj `notElem` multiplicities rel
                   , Uni `notElem` multiplicities rel]
        -- declarations to be saved in generated plugs: if decplug=True, the declaration has the BYPLUG and therefore may not be saved in a database
        -- WHAT -> is a BYPLUG?
        savedRels= [makeRelation d | d<-filter (not.decplug) allDecs]

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
    * Plug ECps                               *
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
-- WHY don't we use the gplugs that have already been generated? In that way we can be sure that the interfaces
-- are functionally correct!
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
        cRels = [    ERel (makeRelation d)  | d<-declarations context, not(deciss d), isTot d, not$decplug d]++
                [EFlp (ERel (makeRelation d)) | d<-declarations context, not(deciss d), not (isTot d) && isSur d, not$decplug d]
--  Step 2: select and arrange all declarations to obtain a set cRels of injective relations
--          to ensure deletability of entities (signal declarations are excluded)
        dRels = [    ERel (makeRelation d)  | d<-declarations context, not(deciss d), isInj d, not$decplug d]++
                [EFlp (ERel (makeRelation d)) | d<-declarations context, not(deciss d), not (isInj d) && isUni d, not$decplug d]
--  Step 3: compute maximally total expressions and maximally injective expressions.
        maxTotExprs = clos cRels
        maxInjExprs = clos dRels
        --    Warshall's transitive closure algorithm, adapted for this purpose:
        clos :: [Expression] -> [Expression]
        clos xs
         = foldl f [ECps [ x ] | x<-xs] (nub (map source xs) `isc` nub (map target xs))
           where
             f :: [Expression] -> A_Concept -> [Expression]
             f q x = q ++
                        [ECps (ls ++ rs) | l@(ECps ls) <- q, x <= target l,
                         r@(ECps rs) <- q, x <= source r, null (ls `isc` rs)]

--  Step 4: i) generate interfaces starting with INTERFACE concept: I[Concept]
--          ii) generate interfaces starting with INTERFACE concepts: V[ONE*Concept] 
--          note: based on a theme one can pick a certain set of generated interfaces (there is not one correct set)
--                default theme => generate interfaces from the clos total expressions and clos injective expressions (see step1-3).
--                student theme => generate interface for each concept with declarations where concept is source or target (note: step1-3 are skipped)
        interfaceGen = step4a ++ step4b
        step4a
         | theme flags == StudentTheme 
         = [Ifc { ifcName   = name c ++ " (instantie)"
                , ifcParams = map makeRelation directdecls
                , ifcViols  = []
                , ifcArgs   = []
                , ifcObj    = Obj { objnm   = name c ++ " (instantie)"
                                  , objpos  = Origin "generated object for interface for each concept in TblSQL or ScalarSQL"
                                  , objctx  = ERel (I c)
                                  , objmsub = Just . Box $
                                              Obj { objnm   = "I["++name c++"]"
                                                   , objpos  = Origin "generated object: step 4a - default theme"
                                                   , objctx  = ERel (I c)
                                                   , objmsub = Nothing
                                                   , objstrs = [] }
                                              :[Obj { objnm   = name d ++ "::"++name(source d)++"*"++name(target d)
                                                    , objpos  = Origin "generated object: step 4a - default theme"
                                                    , objctx  = if source rel==c then rel else EFlp rel
                                                    , objmsub = Nothing
                                                    , objstrs = [] }
                                               | d <- directdecls, let rel=ERel (makeRelation d)]
                                  , objstrs = [] }
                , ifcPos    = Origin "generated interface for each concept in TblSQL or ScalarSQL"
                , ifcExpl   = "Interface " ++name c++" has been generated by Ampersand."
                , ifcRoles = []
                }
           | c<-concs fSpec, let directdecls = [d | d<-declarations fSpec, c `elem` concs d]]
         --end student theme
         --otherwise: default theme
         | otherwise --note: the uni of maxInj and maxTot may take significant time (e.g. -p while generating index.htm)
                     --note: associations without any multiplicity are not in any Interface
                     --note: scalars with only associations without any multiplicity are not in any Interface
         = let recur trace es
                = [ Obj { objnm   = if isTypeable t
                                    then showADL t
                                    else fatal 298 ("Expression "++showADL t++" contains untypeable elements.")
                        , objpos  = Origin "generated recur object: step 4a - default theme"
                        , objctx  = t
                        , objmsub  = Just . Box $ recur (trace++[c]) cl
                        , objstrs = [] }
                  | cl<-eqCl getfchead es, ECps (t:_)<-take 1 cl, let c=source t, c `notElem` trace ]
               --getfchead assumes an ECps expression (see ticket #108).
               --16 Aug 2011: (recur trace es) is applied once where es originates from (maxTotExprs `uni` maxInjExprs) both based on clos
               --             By implementation clos returns ECps expression only.
               getfchead (ECps (t:_)) = t
               getfchead _ = fatal 305 "not an ECps expression"  
           in
           [Ifc { ifcName   = name c
                , ifcParams = [ rel | rel<-mors objattributes, not (isIdent rel)]
                , ifcViols  = []
                , ifcArgs   = []
                , ifcObj    = Obj { objnm   = name c
                                  , objpos  = Origin "generated object: step 4a - default theme"
                                  , objctx  = ERel (I c)
                                  , objmsub  = Just . Box $ objattributes
                                  , objstrs = [] }
                , ifcPos    = Origin "generated interface: step 4a - default theme"
                , ifcExpl   = "Interface " ++name c++" has been generated by Ampersand."
                , ifcRoles  = []
                }
           | cl <- eqCl source (maxTotExprs `uni` maxInjExprs)
           , let objattributes = recur [] cl
           , not (null objattributes) --de meeste plugs hebben in ieder geval I als attribuut
           , --exclude concept A without cRels or dRels (i.e. A in Scalar without total associations to other plugs) 
             not (length objattributes==1 && isIdent(objctx(head objattributes)))  
           , let e0=head cl, let c=source e0
           ]
        --end otherwise: default theme
        --end stap4a
        step4b --generate lists of concept instances for those concepts that have a generated INTERFACE in step4a 
         = [Ifc { ifcName   = nm
                , ifcParams = ifcParams ifcc
                , ifcViols  = ifcViols  ifcc
                , ifcArgs   = ifcArgs   ifcc
                , ifcObj    = Obj { objnm   = nm
                                  , objpos  = Origin "generated object: step 4b"
                                  , objctx  = ERel (I ONE)
                                  , objmsub = Just . Box $ [att]
                                  , objstrs = [] }
                , ifcPos    = ifcPos  ifcc
                , ifcExpl   = ifcExpl ifcc
                , ifcRoles  = []
                }
           | ifcc<-step4a
           , let c   = source(objctx (ifcObj ifcc))
                 nm'::Int->String
                 nm' 0  = plural (language flags)(name c)
                 nm' i  = plural (language flags)(name c) ++ show i
                 nms = [nm' i |i<-[0..], nm' i `notElem` map name (ctxifcs context)]
                 nm
                   | theme flags == StudentTheme = name c
                   | null nms = fatal 355 "impossible"
                   | otherwise = head nms
                 att = Obj (name c) (Origin "generated attribute object: step 4b") (ERel (V (Sign ONE c))) Nothing []
           ]
        ----------------------
        --END: making interfaces
        ----------------------


   editable :: Expression -> Bool   --TODO deze functie staat ook in Calc.hs...
   editable (ERel Rel{} ) = True
   editable _            = False

   editMph :: Expression -> Relation  --TODO deze functie staat ook in Calc.hs...
   editMph (ERel r) = r
   editMph e        = fatal 361 $ "cannot determine an editable declaration in a composite expression: "++show e

{- makeActivity turns a process rule into an activity definition.
Each activity can be mapped to a single interface.
A call to such an interface takes the population of the current context to another population,
while maintaining all invariants.
-}
   makeActivity :: Fspc -> Rule -> Activity
   makeActivity fSpec rul
    = let s = Act{ actRule   = rul
                 , actTrig   = rels
                 , actAffect = nub [ r' | (r,_,r')<-clos affectPairs, r `elem` rels]
                 , actQuads  = invQs
                 , actEcas   = [eca | eca<-vEcas fSpec, eRel (ecaTriggr eca) `elem` rels]
                 , actFPA    = NO   -- TODO: this is erroneous. check with IFPUG standard
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
-- relations that may be affected by an edit action within the transaction
        rels        = mors rul
-- the quads that induce automated action on an editable relation.
-- (A quad contains the conjunct(s) to be maintained.)
-- Those are the quads that originate from invariants.
        invQs       = [q | q@(Quad _ ccrs)<-vquads fSpec, (not.isSignal.cl_rule.qClauses) q
                         , (not.null) ((nub.mors.cl_rule) ccrs `isc` rels)]
-- a relation affects another if there is a quad (i.e. an automated action) that links them
        affectPairs = [(qMorph q,[q],r) | q<-invQs, r<-(mors.cl_rule.qClauses) q]
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
   quads :: Options -> (Relation->Bool) -> [Rule] -> [Quad]
   quads flags visible rs
    = [ Quad r (Clauses [ (conj,allShifts flags conj)
                        | conj <- conjuncts rule
      --                , (not.null.lambda Ins (ERel r)) conj  -- causes infinite loop
      --                , not (checkMono conj Ins r)         -- causes infinite loop
      --                , let conj' = subst (r, actSem Ins r (delta (sign r))) conj
      --                , (not.isTrue.conjNF) (EUni[ECpl conj,conj']) -- the system must act to restore invariance     
                        ]
                        rule)
      | rule<-rs, r<-mors rule, visible r
      ]

-- The function allClauses yields an expression which has constructor EUni in every case.
   allClauses :: Options -> Rule -> Clauses
   allClauses flags rule = Clauses [(conj,allShifts flags conj) | conj<-conjuncts rule] rule

   allShifts :: Options -> Expression -> [Expression]
   allShifts _ conjunct = nub [simplify e' | e'<-shiftL conjunct++shiftR conjunct, not (isTrue e')]
    where
    {-
     diagnostic
      = intercalate "\n  "
          [ "shft L: [ "++intercalate "\n          , " [showHS flags "\n            " e | e<-shiftL conjunct    ]++"\n          ]"
          , "shft R: [ "++intercalate "\n          , " [showHS flags "\n            " e | e<-shiftR conjunct    ]++"\n          ]"
          ] -}
     shiftL :: Expression -> [Expression]
     shiftL r
      | length antss+length conss /= length fus = fatal 498 $ "shiftL will not handle argument of the form "++showADL r
      | null antss || null conss                = [disjuncts r |not (null fs)] --  shiftL doesn't work here.
      | all idsOnly (concat antss)              = [EUni (ECpl (ERel (I srcA)) : map ECps conss)]
      | otherwise                               = [EUni ([ ECpl (ECps (if null ts then id' css else ts))
                                                           | ts<-ass++[id' css | null ass]
                                                         ]++
                                                         [ ECps (if null ts then id' ass else ts)
                                                           | ts<-css++[id' ass | null css]
                                                         ] )
                                                   | (ass,css)<-nub(move antss conss)
                                                   , if null css then fatal 506 "null css in shiftL" else True
                                                   , if null ass then fatal 507 "null ass in shiftL" else True
                                                  ]
      where
      {-
       diagnostic
        = intercalate "\n  "
          [ "fs:    [ "++intercalate "\n         , " [showHS flags "\n           " f | f<-fs    ]++"\n       ]"
          , "antss: [ "++intercalate "\n         , " [showHS flags "\n           " a | a<-antss ]++"\n       ]"
          , "conss: [ "++intercalate "\n         , " [showHS flags "\n           " c | c<-conss ]++"\n       ]"
          , "srcA:  "++showHS flags "" srcA
          ] -}
       EUni fs = disjuncts r                -- informal example [  "-(r;s)", "-(p;r)", "x;y;z" ] coming from  "r;s /\ p;r |- x;y;z"
       fus = filter (not.isI) fs
       antss = [ts | ECpl (ECps ts)<-fus]   -- e.g. [ ["r","s"], ["p","r"] ]
       conss = [ts | ECps ts<-fus]          -- e.g. [ ["x","y","z"] ]
       srcA
        | null antss =
          fatal 514 $ "empty antecedent in shiftL (" ++ showADL r ++ ")"
        | length (eqClass (<==>) [source (head ants) | ants <- antss]) > 1 =   -- make sure that foldr1 join [source (head ants) | ants <- antss] is defined...
          fatal 515 $ "shiftL (" ++ showADL r ++ ")\nin calculation of srcA\n" ++ show (eqClass (<==>) [source (head ants) | ants <- antss])
        | otherwise = foldr1 join [source (head ants) | ants <- antss]
       id' ass = if all null (take 1 ass ++ take 1 (reverse ass))
                 then fatal 474 "It is imperative that ass is not empty"
                 else [ERel (I c) ]
        where a = (source.head.head) ass
              c = if a <==> b then a `join` b else
                  fatal 519 $ "shiftL ("++showADL r++")\nass: "++show ass++"\nin calculation of c = a `join` b with a="++show a++" and b="++show b
              b = (target.last.last) ass
     -- It is imperative that both ass and css are not empty.
       move :: [[Expression]] -> [[Expression]] -> [([[Expression]],[[Expression]])]
       move ass [] = [(ass,[])]
       move ass css
        = (ass,css):
          if and [not (idsOnly (ECps cs)) | cs<-css] -- idsOnly (ECps [])=True, so:  and [not (null cs) | cs<-css]
          then [ts | length (eqClass (==) (map head css)) == 1
                   , isUni h
                   , ts<-move [flp h : as |as<-ass] (map tail css)]++
               [ts | length (eqClass (==) (map last css)) == 1
                   , isInj l
                   , ts<-move [as++[flp l] |as<-ass] (map init css)]
          else []
          where h=head (map head css); l=head (map last css)

     shiftR :: Expression -> [Expression]
     shiftR r
      | length antss+length conss /= length fus = fatal 541 $ "shiftR will not handle argument of the form "++showADL r
      | null antss || null conss                = [disjuncts r |not (null fs)] --  shiftR doesn't work here.
      | all idsOnly (concat conss)              = [EUni (map (ECpl . ECps) antss ++ [ECps [ERel (I srcA)]])]
      | otherwise                               = [EUni ([ ECpl (ECps (if null ts then id' css else ts))
                                                       | ts<-ass++[id' css | null ass]]++
                                                       [ ECps (if null ts then id' ass else ts)
                                                       | ts<-css++[id' ass | null css]])
                                                  | (ass,css)<-nub(move antss conss)]
      where
      {-
       diagnostic
        = intercalate "\n  "
          [ "fs:    [ "++intercalate "\n         , " [showHS flags "\n           " f | f<-fs    ]++"\n       ]"
          , "antss: [ "++intercalate "\n         , " [showHS flags "\n           " a | a<-antss ]++"\n       ]"
          , "conss: [ "++intercalate "\n         , " [showHS flags "\n           " c | c<-conss ]++"\n       ]"
          , "srcA:  "++showHS flags "" srcA
          , "move:  [ "++intercalate "\n         , " [show c | c<-move antss conss ]++"\n       ]"
          ] -}
       EUni fs = disjuncts r  -- fs is a list of expressions
       fus = filter (not.isI) fs
       antss = [ts | ECpl (ECps ts)<-fus]
       conss = [ts | ECps ts<-fus]
       srcA 
        | null conss =
           fatal 554 $ "empty consequent in shiftR ("++showADL r++")"
        | length (eqClass (<==>) [ source (head cons) | cons<-conss]) > 1 =   -- make sure that foldr1 join [ source (head cons) | cons<-conss] is defined
           fatal 556 $ "shiftR ("++showADL r++")\nin calculation of srcA\n"++show (eqClass (<==>) [ source (head cons) | cons<-conss])
        | otherwise = foldr1 join [ source (head cons) | cons<-conss]
       id' css = if all null css then fatal 518 "It is imperative that css is not empty" else
                 [ERel (I c) ]
        where a = (source.head.head) css
              c = if a <==> b then a `join` b
                  else fatal 561 $ "shiftR ("++showADL r++")\nass: "++show css++"\nin calculation of c = a `join` b with a="++show a++" and b="++show b ++ ". "
              b = (target.last.last) css
       move :: [[Expression]] -> [[Expression]] -> [([[Expression]],[[Expression]])]
       move [] css = [([],css)]
       move ass css
        = (ass,css):
          if and [not (null as) | as<-ass]
          then [ts | length (eqClass (==) (map head ass)) == 1
                   , isSur h
                   , ts<-move (map tail ass) [flp h:cs |cs<-css]]++
               [ts | length (eqClass (==) (map last ass)) == 1
                   , isTot l
                   , ts<-move (map init ass) [cs++[flp l] |cs<-css]]
          else []
          where h=head (map head ass); l=head (map last ass)
     

-- Deze functie neemt verschillende clauses samen met het oog op het genereren van code.
-- Hierdoor kunnen grotere brokken procesalgebra worden gegenereerd.
   assembleECAs :: [Quad] -> [ECArule]
   assembleECAs qs
    = [er{ecaAction=normPA (ecaAction er)} | (ecarule,i) <- zip ecas [(1::Int)..], let er=ecarule i]
      where
       -- the quads that are derived for this fSpec contain horn clauses.
       -- A Horn clause h that is generated from rule r contains the information how to restore the truth of r.
       -- Suppose an insert or delete event on a relation rel has occurred and rel is used in rule r.
       -- A restore action from Horn clause h will then restore the truth of r.
       
       -- First, we harvest the quads from fSpec in quadruples.
       -- rel    is a relation that may be affected by an (insert- or delete-) event.
       -- shifts is a set of Horn clauses that can restore the truth of conj after rel has been affected.
       -- conj   is an expression that must remain true at all times.
       -- rul    is the rule from which the above have been derived (for traceability)
       -- these quadruples are organized per relation.
       -- This puts together all Horn clauses we need for each relation.
       relEqCls = eqCl fst4 [(rel,shifts,conj,cl_rule ccrs) | Quad rel ccrs<-qs, (conj,shifts)<-cl_conjNF ccrs]
       -- The eca rules can now be assembled from the available material
       ecas
        = [ ECA (On ev rel) delt act
          | relEq <- relEqCls                 -- The material required for one relation
          , let (rel,_,_,_) = head relEq      -- This is the relation
          , let ERel delt = delta (sign rel)  -- delt is a placeholder for the pairs that have been inserted or deleted in rel.
          , ev<-[Ins,Del]                     -- This determines the event: On ev rel
          , let act = All [ Chc [ (if isTrue  clause' || isTrue step then Nop else
                                   if isFalse clause'                then Blk else
--                                 if not (visible rel) then Blk else
                                   let visible _ = True in genPAclause visible ev toExpr viols
                                  ) [(conj,causes)]  -- the motivation for these actions
                                | clause@(EUni fus) <- shifts
                                , let clause' = conjNF (subst (rel, actSem ev rel (delta (sign rel))) clause)
                                , let step    = conjNF (EUni[ECpl clause,clause'])
                                , let viols   = conjNF (notCpl clause')
                                , let negs    = EUni [f | f<-fus, isNeg f]
                                , let poss    = EUni [f | f<-fus, isPos f]
                                , let frExpr  = if ev==Ins
                                                then conjNF negs
                                                else conjNF poss
                                , rel `elem` mors frExpr
                                , let toExpr = if ev==Ins
                                               then conjNF poss
                                               else conjNF (notCpl negs)
                                ]
                                [(conj,causes)]  -- to supply motivations on runtime
                          | conjEq <- eqCl snd3 [(shifts,conj,rule) | (_,shifts,conj,rule)<-relEq]
                          , let causes          = nub (map thd3 conjEq)
                          , let (shifts,conj,_) = head conjEq
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
       cascaded = [er{ecaAction=action'} | er<-new, let (c,action') = cascade (eRel (ecaTriggr er)) (ecaAction er), c]
       uncasced = [er |                    er<-new, let (c,_)       = cascade (eRel (ecaTriggr er)) (ecaAction er), not c]
-- cascade inserts a block on the place where a Do component exists that matches the blocking event.
--     cascade :: Relation -> PAclause -> (Bool, PAclause)
     cascade rel (Do srt (ERel to) _ _) | (not.null) blkErs = (True, ecaAction (head blkErs))
      where blkErs = [er | er<-ers
                         , Blk _<-[ecaAction er]
                         , let t = ecaTriggr er
                         , eSrt t == srt
                         , eRel t == to
                         , rel    /= to
                         ]
     cascade  _  c@Do{}           = (False, c)
     cascade rel (New c clause m) = ((fst.cascade rel.clause) "dummystr", New c (snd.cascade rel.clause) m)
     cascade rel (Rmv c clause m) = ((fst.cascade rel.clause) "dummystr", Rmv c (snd.cascade rel.clause) m)
     cascade rel (Sel c e cl m)   = ((fst.cascade rel.cl) "dummystr",     Sel c e (snd.cascade rel.cl)   m)
     cascade rel (Chc ds m)       = (any (fst.cascade rel) ds, Chc (map (snd.cascade rel) ds) m)
     cascade rel (All ds m)       = (any (fst.cascade rel) ds, All (map (snd.cascade rel) ds) m)
     cascade  _  (Nop m)          = (False, Nop m)
     cascade  _  (Blk m)          = (False, Blk m)
     cascade  _  (Let _ _ _)    = fatal 593 "Undefined cascade. Please contact your dealer!"
     cascade  _  (Ref _)        = fatal 594 "Undefined cascade. Please contact your dealer!"
   conjuncts :: Rule -> [Expression]
   conjuncts = fiRule.conjNF.rrexp
    where fiRule (EIsc fis) = {- map disjuncts -} fis
          fiRule r        = [ {- disjuncts -} r]

-- The function disjuncts yields an expression which has constructor EUni in every case.
   disjuncts :: Expression -> Expression
   disjuncts = uniRule
    where uniRule (EUni cps) = (EUni . nub . map cplRule) cps
          uniRule r          = EUni [cplRule r]
          cplRule (ECpl r)   = ECpl (cpsRule r)
          cplRule r          = cpsRule r
          cpsRule (ECps ts)  = ECps ts
          cpsRule r          = ECps [r]

   actSem :: InsDel -> Relation -> Expression -> Expression
   actSem Ins rel (ERel r) | rel==r    = ERel rel 
                         | otherwise = EUni[ERel rel ,ERel r ]
   actSem Ins rel delt   = disjNF (EUni[ERel rel ,delt])
   actSem Del rel (ERel r) | rel==r    = EIsc[]
                         | otherwise = EIsc[ERel rel , ECpl (ERel r )]
   actSem Del rel delt   = conjNF (EIsc[ERel rel ,ECpl delt])
 --  actSem Del rel delt = EIsc[rel,ECpl delt]

   delta :: Sign -> Expression
   delta (Sign s t)
    = ERel (makeRelation
             Sgn { decnm   = "Delta"
                 , decsgn  = Sign s t
                 , decprps = []
                 , decprps_calc = []
                 , decprL  = ""
                 , decprM  = ""
                 , decprR  = ""
                 , decMean = AMeaning [ A_Markup Dutch   ReST (string2Blocks ReST "TODO: Doel van deze Delta relatie opschrijven")
                                      , A_Markup English ReST (string2Blocks ReST "TODO: Write down the purpose of this Delta relation.")
                                      ]
                 , decConceptDef = Nothing
                 , decfpos = Origin "generated relation (Delta)"
                 , deciss  = True
                 , decusr  = False
                 , decpat  = ""
                 , decplug = True
                 }) 

   -- | de functie genPAclause beschrijft de voornaamste mogelijkheden om een expressie delta' te verwerken in expr (met tOp'==Ins of tOp==Del)
-- TODO: Vind een wetenschappelijk artikel waar de hier beschreven transformatie uitputtend wordt behandeld.
-- TODO: Deze code is onvolledig en misschien zelfs fout....
   genPAclause :: (Relation->Bool)           -- the relations that may be changed
             -> InsDel                  -- the type of action: Insert or Delete
             -> Expression              -- the expression in which a delete or insert takes place
             -> Expression              -- the delta to be inserted or deleted
             -> [(Expression,[Rule])]   -- the motivation, consisting of the conjuncts (traced back to their rules) that are being restored by this code fragment.
             -> PAclause
   genPAclause editAble tOp' expr1 delta1 motive = genPAcl delta1 tOp' expr1 motive
    where
      genPAcl deltaX tOp exprX motiv =
        let err i str = fatal i ("genPAcl ("++showADL deltaX++") "++show tOp++str++
                                 "within function genPAclause "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++").") in
        case (tOp, exprX) of
          (_ ,  EFlp x)   -> genPAcl (flp deltaX) tOp x motiv
          (_ ,  EBrk x)   -> genPAcl deltaX tOp x motiv
          (_ ,  ETyp x t) -> if sign x==sign t then genPAcl deltaX tOp x motiv else
                             fatal 691 "TODO: implement narrowing."
          (_ ,  EUni [])  -> Blk motiv
          (_ ,  EIsc [])  -> Nop motiv
          (_ ,  ECps [])  -> err 681 " ECps [],\n"
          (_ ,  ERad [])  -> err 683 " ERad [],\n"
          (_ ,  EPrd [])  -> err 697 " EPrd [],\n"
          (_ ,  EUni [t]) -> genPAcl deltaX tOp t motiv
          (_ ,  EIsc [t]) -> genPAcl deltaX tOp t motiv
          (_ ,  ECps [t]) -> genPAcl deltaX tOp t motiv
          (_ ,  ERad [t]) -> genPAcl deltaX tOp t motiv
          (_ ,  EPrd [t]) -> genPAcl deltaX tOp t motiv
          (Ins, ECpl x)   -> genPAcl deltaX Del x motiv
          (Del, ECpl x)   -> genPAcl deltaX Ins x motiv
          (Ins, EUni fs)  -> Chc [ genPAcl deltaX Ins f motiv | f<-fs{-, not (f==expr1 && Ins/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Ins, EIsc fs)  -> All [ genPAcl deltaX Ins f []    | f<-fs ] motiv
          (Ins, ECps ts)  -> Chc [ {- the following might be useful for diagnostics:
                                   if showADL exprX=="project;partof~"
                                   then fatal 702 ( "Diagnostic error\n"++
                                                    "chop ts= "++show [(map showADL ls, map showADL rs)| (ls,rs)<-chop ts ]++
                                                    "\nConcepts c="++showADL c++",  target (ECps ls)="++showADL (target (ECps ls))++",  source (ECps rs)="++showADL (source (ECps rs))++
                                                    "\nfLft \"x\"= "++showADL (fLft "x") ++
                                                    "\nfRht \"x\"= "++showADL (fRht "x")
                                                  )
                                   else -}
                                   if ECps ls==flp (ECps rs)
                                   then Chc [ New c fLft motiv
                                            , Sel c (ECps ls) fLft motiv
                                            ] motiv
                                   else Chc [ New c (\x->All [fLft x, fRht x] motiv) motiv
                                            , Sel c (ECps ls) fLft motiv
                                            , Sel c (flp(ECps rs)) fRht motiv
                                            ] motiv
                                 | (ls,rs)<-chop ts
                                 , if source (ECps rs) <==> target (ECps ls) then True else err 690 " ECps ts,\n" -- ensure that 'join' on the following line may be called
                                 , let c = source (ECps rs) `join` target (ECps ls)
                                 , let fLft atom = genPAcl (disjNF (EUni[EPrd [ERel (Mp1 atom c),deltaX],ECpl (ECps rs)])) Ins (ECps rs) []
                                 , let fRht atom = genPAcl (disjNF (EUni[EPrd [deltaX,ERel (Mp1 atom c)],ECpl (ECps ls)])) Ins (ECps ls) []
                                 ] motiv
{- Problem: how to insert Delta into r;s
This corresponds with:  genPAclause editAble Ins (ECps [r,s]) Delta motive
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

Here is an algorithm in pseudocode to do just that:  compute the insertion of a Delta into r;s:
LET d = EVAL (delta - r;s);             -- compute which pairs are to be inserted
IF not (null d)                         -- if there is nothing to insert, we're done
THEN BEGIN
     IF editable(r) THEN INS (d;s~) INTO r;
     IF editable(s) THEN INS (r~;d) INTO s;
     LET d' = (delta - r;s);            -- see what is left to evaluate
     IF not (null d')
     THEN                               -- get a new element to construct a bow tie
          Chc [ NEW c FROM target(r) `lub` source(s)
              , SEL c FROM ( -cod(r) /\ -dom(s) )
              ];
          IF editable(r) THEN INS EVAL (d*c) INTO r;
          IF editable(s) THEN INS EVAL (c*d) INTO s;
     END
END

In practice, we do not have r;s, but ECps es.
Besides, we must take into account which expressions are evaluated by the SQL server and which by the PHP server.
Here is the same algorithm, operationalized a bit more yet still in pseudocode. It computes how to insert a Delta into (ECps es):
let d = EVAL (EDif (delta, ECps es))
let newAtoms = [ newAtom (target(r) `lub` source(s)) | (r,s)<-zip (init es) (tail es)]
IN IF not (null d)
   THEN Seq ( [ All ( (if editable(head es) then [ INS (Ecps [d, flp (ECps (tail es))]) INTO head es ] else []) ++
                      (if editable(last es) then [ INS (Ecps [flp (ECps (init es)), d]) INTO last es ] else [])  )
              , IF and [editable e | e<-es] && and [ editable (I c) | c<-cs ]
                THEN let d' = EVAL (EDif (d, ECps es))
                     in IF not (null d')
                        THEN All ( map INS newAtoms ++
                                   [ INS (EPrd [d, head newAtoms]) INTO (head es) ]++
                                   [ INS (Eprd [cl, cr]) INTO e | (cl,cr,e)<-zip (init newAtoms) (tail newAtoms) tail (init es))] ++
                                   [ INS (EPrd [last newAtoms, d]) INTO (last es) ] )
              ]
            )
            
We can use an "Algebra of Imperative Programs" to move towards real code. Suppose we have the following data structure of imperative programs:
data Program = Sequence [Program]          -- execute programs in sequence
             | Select Cond Program         -- evaluate the computation (Cond), which yields a boolean result, and execute the program if it is true
             | Sequence [Program]          -- execute programs in sequence
             | Choice [Program]            -- execute precisely one program from the list
             | All [Program]               -- execute all programs in arbitrary order (may even be parallel)
             | New String Concept Program  -- create a named variable (the String) of type C (the concept) and execute the program, which may use this concept.
             | NewAtom PHPRel Concept      -- assign a brand new atom of type Concept to the PHPRel, which must be a PHPvar (i.e. a variable).
             | Assign PHPRel PHPExpression -- execute a relation expression in PHP and assign its result to the PHPRel, which must be a PHPvar (i.e. a variable).
             | Insert PHPExpression DBrel  -- execute the computation and insert its result into the database
             | Delete PHPExpression DBrel  -- execute the computation and delete its result from the database
             | Comment String              -- ignore this. This is part of the algebra, so we can generate commented code.
             | Nop                         -- do nothing
             | Blk [(Expression,[Rule])]   -- abort, leaving an error message that motivates this. The motivation consists of the conjuncts (traced back to their rules) that are being restored by this code fragment.
data Cond    = NotNull PHPRel  -- a condition on a PHPRel

In order to proceed to the next refinement, we have enriched the PHPExpression data structure with variables (PHPvar) and queries (PHPqry) 
We need a variable in PHP that represents a relation, which gets the label PHPvar.
We also need to query an Expression on the SQL server. The result of that query is relation contents in PHP, which gets the label PHPqry.
The 'almost Haskell' program fragment for generating an insertion of Delta into r;s is now:
let delta = PHPvar{ phpVar = "delta"       -- this is the input
                  , phptyp = phpsign (comp2php (ECps es))    -- the type is used nowhere, but it feels good to know the type.
                  } in
let d     = PHPvar{ phpVar = "d"           -- this is the input, from which everything already in  Ecps es  is removed.
                  , phptyp = phpsign (comp2php (ECps es))    -- the type is used nowhere, but it feels good to know the type.
                  } in
let newAtoms = [ NewAtom (PHPvar (head (name c):show i) c | (r,s,i)<-zip3 (init es) (tail es) [1..], let c=target(r) `lub` source(s)] in
Sequence [ Assign d (PHPEDif (PHPERel delta, PHPRel (PHPqry (ECps es))))    -- let d = Delta - e0;e1;e2;...
         , Comment "d contains all links to be inserted in r;s"
         , Select (NotNull d)
                  (Sequence([ All ( (if editable(head es)    
                                     then [ Insert (PHPECps [PHPERel d, PHPERel (PHPqry (flp (ECps (tail es))))]) (mkDBrel (head es)) ]
                                     else []) ++
                                    (if editable(last es)
                                     then [ Insert (PHPECps [PHPERel (PHPqry (flp (ECps (init es)))), PHPERel d]) (mkDBrel (last es)) ]
                                     else []) )
                            , Comment "All links that can be added just in r or just in s have now been added. Now we finish the rest with a bow tie."] ++
                            if or [not (editable e) | e<-es] || or [ not (editable (I c)) | NewAtom _ c<-newAtoms ]  then [] else
                            [ -- let us see which links are left to be inserted
                              Assign d (PHPEDif (PHPERel d, PHPRel (PHPqry (ECps es))))
                            , Comment "d contains the remaining links to be inserted in r;s"
                            , Select (NotNull d)
                                     (Seq ( newAtoms ++
                                            All ( let vs = [ v | NewAtom v _ <- newAtoms ] in
                                                  [ Insert (PHPEPrd [PHPERel d, PHPERel (PHPvar (head vs))]) (mkDBrel (head es)) ]++
                                                  [ Insert (PHPEprd [PHPERel cl, PHPERel cr]) (mkDBrel e) | (cl,cr,e)<-zip (init vs) (tail vs) tail (init es))] ++
                                                  [ Insert (PHPEPrd [PHPERel (PHPvar (last vs)), PHPERel d]) (mkDBrel (last es)) ])
                                     )    )
                            ])
                  )
         ]

In order to proceed to the real code, we must realize that mkDBrel is not defined yet.
The argument of mkDBrel is an Expression. This can be an ERel{} or something different.
In case it is an ERel{}, the insert can be translated directly to a database action.
If it is not, the insert can be treated as a recursive call to genPAcl, which breaks down the expression further until it reaches a relation.
let delta = PHPvar{ phpVar = "delta"       -- this is the input
                  , phptyp = phpsign (comp2php (ECps es))    -- the type is used nowhere, but it feels good to know the type.
                  } in
let d     = PHPvar{ phpVar = "d"           -- this is the input, from which everything already in  Ecps es  is removed.
                  , phptyp = phpsign (comp2php (ECps es))    -- the type is used nowhere, but it feels good to know the type.
                  } in
let newAtoms = [ NewAtom (PHPvar (head (name c):show i) c | (r,s,i)<-zip3 (init es) (tail es) [1..], let c=target(r) `lub` source(s)] in
Sequence [ Assign d (PHPEDif (PHPERel delta, PHPRel (PHPqry (ECps es))))
         , Comment "d contains all links to be inserted in r;s"
         , Select (NotNull d)
                  (Sequence([ All ( (if editable(head es)
                                     then [ if isRel (head es)
                                            then Insert (PHPECps [PHPERel d, PHPERel (PHPqry (flp (ECps (tail es))))]) (let PHPRel r = head es in r)
                                            else genPAclause editAble Ins (head es) (PHPECps [PHPERel d, PHPERel (PHPqry (flp (ECps (tail es))))]) motive
                                          ]
                                     else []) ++
                                    (if editable(last es)
                                     then [ if isRel (last es)
                                            then Insert (PHPECps [PHPERel (PHPqry (flp (ECps (init es)))), PHPERel d]) (let PHPRel r = last es in r)
                                            else genPAclause editAble Ins (last es) (PHPECps [PHPERel (PHPqry (flp (ECps (init es)))), PHPERel d]) motive
                                          ]
                                     else []) )
                            , Comment "All links that can be added just in r or just in s have now been added. Now we finish the rest with a bow tie."] ++
                            if or [not (editable e) | e<-es] || or [ not (editable (I c)) | NewAtom _ c<-newAtoms ]  then [] else
                            [ -- let us see which links are left to be inserted
                              Assign d (PHPEDif (PHPERel d, PHPRel (PHPqry (ECps es))))
                            , Comment "d contains the remaining links to be inserted in r;s"
                            , Select (NotNull d)
                                     (Seq ( newAtoms ++
                                            All ( let vs = [ v | NewAtom v _ <- newAtoms ] in
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

          (Del, ECps ts) -> Chc [ if ECps ls==flp (ECps rs)
                                  then Chc [ Sel c (disjNF (ECps ls)) (\_->Rmv c fLft motiv) motiv
                                           , Sel c (disjNF (ECps ls)) fLft motiv
                                           ] motiv
                                  else Chc [ Sel c (disjNF (EIsc [ECps ls,flp(ECps rs)])) (\_->Rmv c (\x->All [fLft x, fRht x] motiv) motiv) motiv
                                           , Sel c (disjNF (EIsc [ECps ls,flp(ECps rs)])) fLft motiv
                                           , Sel c (disjNF (EIsc [ECps ls,flp(ECps rs)])) fRht motiv
                                           ] motiv
                                | (ls,rs)<-chop ts
                                , if source (ECps rs) <==> target (ECps ls) then True else err 855 " ECps ts,\n" -- ensure that 'join' on the following line may be called
                                , let c = source (ECps rs) `join` target (ECps ls)
                                , let fLft atom = genPAcl (disjNF (EUni[EPrd [ERel (Mp1 atom c),deltaX],ECpl (ECps rs)])) Del (ECps rs) []
                                , let fRht atom = genPAcl (disjNF (EUni[EPrd [deltaX,ERel (Mp1 atom c)],ECpl (ECps ls)])) Del (ECps ls) []
                                ] motiv
{- Problem: how to delete Delta from ECps es
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
Chc [ DELETE ((ECps (reverse (map flp left) ++ [e] ++ reverse (map flp right))) FROM e
    | (left,ERel e,right)<-triples
    , editable e ]
In order to refine further to real code, we must realize that deletion can be done in relations only. Not in expressions.
So if e is a relation, the DELETE can be executed.
If it is an expression, the function genPAclause can be called recursively.
We now get the following code fragment:
Chc [ if isRel e
      then DELETE (ECps (reverse (map flp left) ++ [e] ++ reverse (map flp right))) FROM  (let PHPRel r = e in r)
      else genPAclause editAble Del e (ECps (reverse (map flp left) ++ [e] ++ reverse (map flp right))) motive
    | (left,ERel e,right)<-triples
    , editable e ]

-}
          (Del, EUni fs)   -> All [ genPAcl deltaX Del f []    | f<-fs{-, not (f==expr1 && Del/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Del, EIsc fs)   -> Chc [ genPAcl deltaX Del f motiv | f<-fs ] motiv
-- Op basis van de Morgan is de procesalgebra in het geval van (Ins, ERad ts)  afleidbaar uit uit het geval van (Del, ECps ts) ...
          (_  , ERad ts)   -> genPAcl deltaX tOp (ECpl (ECps (map notCpl ts))) motiv
          (_  , EPrd _)    -> fatal 745 "TODO"
          (_  , EKl0 x)    -> genPAcl (deltaK0 deltaX tOp x) tOp x motiv
          (_  , EKl1 x)    -> genPAcl (deltaK1 deltaX tOp x) tOp x motiv
          (_  , ERel m)   -> -- fatal 742 ("DIAG ADL2Fspec 764:\ndoCod ("++showADL deltaX++") "++show tOp++" ("++showADL exprX++"),\n"
                                   -- -- ++"\nwith disjNF deltaX:\n "++showADL (disjNF deltaX))
                             if editAble m then Do tOp exprX deltaX motiv else Blk [(ERel m ,nub [r |(_,rs)<-motiv, r<-rs])]
          (_ , _)         -> fatal 767 ( "Non-exhaustive patterns in the recursive call\ndoCod ("++showADL deltaX++") -- deltaX\n      "++show tOp++"  -- tOp\n      ("++showADL exprX++") -- exprX\n"++
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
        qs        = quads flags visible (invariants fSpec++multrules fSpec++keyrules fSpec)
        ecas      = assembleECAs qs
        conjs     = nub [ (cl_rule ccrs,c) | Quad _ ccrs<-qs, (c,_)<-cl_conjNF ccrs]
        eventsIn  = nub [ecaTriggr eca | eca<-ecas ]
        eventsOut = nub [On tOp rel | eca<-ecas, doAct<-dos (ecaAction eca), let Do tOp e _ _=doAct, ERel rel<-[e,flp e]]
        visible _ = True
        
-- Auxiliaries
   chop :: [t] -> [([t], [t])]
   chop [_]    = []
   chop (x:xs) = ([x],xs): [(x:l, r) | (l,r)<-chop xs]
   chop []     = []

   deltaK0 :: t -> InsDel -> t1 -> t
   deltaK0 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta |- x*)
   deltaK0 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x* leeg is)
   deltaK1 :: t -> InsDel -> t1 -> t
   deltaK1 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta |- x+)
   deltaK1 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x+ leeg is)



   -- TODO: @Stef: Why is this needed?  
   idsOnly :: Expression -> Bool
   idsOnly e' = and [isIdent r | r<-mors' e'] -- > tells whether all the arguments are equivalent to I
             where mors' :: Expression -> [Relation]
                   mors' = foldrMapExpression rdcons id []   -- yields a list of relations from e
                   rdcons :: Eq a => a -> [a] -> [a]
                   rdcons r ms = if r `elem` ms then ms else r:ms
