module Database.Design.Ampersand.FSpec.ToFSpec.ADL2FSpec
         (makeFSpec) where

import Prelude hiding (Ord(..))
import Data.Char
import Data.List
import Data.Maybe
import Text.Pandoc
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Core.Poset
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.FSpec.Crud
import Database.Design.Ampersand.FSpec.ToFSpec.ADL2Plug
import Database.Design.Ampersand.FSpec.ToFSpec.Calc
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms 
import Database.Design.Ampersand.FSpec.ShowADL
import qualified Data.Set as Set

fatal :: Int -> String -> a
fatal = fatalMsg "FSpec.ToFSpec.ADL2FSpec"

makeFSpec :: Options -> A_Context -> FSpec
makeFSpec opts context
 =      FSpec { fsName       = name context
              , originalContext = context 
              , getOpts      = opts
              , fspos        = ctxpos context
              , themes       = themesInScope
              , pattsInScope = pattsInThemesInScope
              , rulesInScope = rulesInThemesInScope
              , declsInScope = declsInThemesInScope 
              , concsInScope = concsInThemesInScope
              , cDefsInScope = cDefsInThemesInScope
              , gensInScope  = gensInThemesInScope
              , fsLang       = printingLanguage
              , vplugInfos   = definedplugs
              , plugInfos    = allplugs
              , interfaceS   = fSpecAllInterfaces -- interfaces specified in the Ampersand script
              , roleInterfaces = fSpecRoleInterfaces
              , interfaceG   = [ifc | ifc<-interfaceGen, let ctxrel = objctx (ifcObj ifc)
                                    , isIdent ctxrel && source ctxrel==ONE
                                      || ctxrel `notElem` map (objctx.ifcObj) fSpecAllInterfaces
                                    , allInterfaces opts]  -- generated interfaces
              , fSwitchboard
                  = Fswtch
                           { fsbEvIn  = nub (map ecaTriggr allVecas) -- eventsIn
                           , fsbEvOut = nub [evt | eca<-allVecas, evt<-eventsFrom (ecaAction eca)] -- eventsOut
                           , fsbConjs = nub [ (qRule q, rc_conjunct x) | q <- filter (not . isSignal . qRule) allQuads
                                                                       , x <- qConjuncts q]
                           , fsbECAs  = allVecas
                           }

              , fDeriveProofs = deriveProofs opts context 
              , fActivities  = allActivities
              , fRoleRels    = nub [(role,decl) -- fRoleRels says which roles may change the population of which relation.
                                   | rr <- ctxRRels context
                                   , decl <- rrRels rr
                                   , role <- rrRoles rr
                                   ] 
              , fRoleRuls    = nub [(role,rule)   -- fRoleRuls says which roles maintain which rules.
                                   | rule <- allrules
                                   , role <- maintainersOf rule
                                   ]
              , fRoles       = nub (concatMap arRoles (ctxrrules context)++
                                    concatMap rrRoles (ctxRRels context)++
                                    concatMap ifcRoles (ctxifcs context)
                                   ) 
              , fallRules = allrules
              , vrules       = filter      isUserDefined  allrules
              , grules       = filter (not.isUserDefined) allrules
              , invariants   = filter (not.isSignal)      allrules
              , vconjs       = allConjs
              , allConjsPerRule = fSpecAllConjsPerRule
              , allConjsPerDecl = fSpecAllConjsPerDecl
              , allConjsPerConcept = fSpecAllConjsPerConcept
              , vquads       = allQuads
              , vEcas        = allVecas
              , vrels        = calculatedDecls
              , allUsedDecls = relsUsedIn context
              , allDecls     = fSpecAllDecls
              , allConcepts  = fSpecAllConcepts
              , kernels      = constructKernels
              , cptTType     = (\cpt -> representationOf contextinfo cpt)
              , fsisa        = concatMap genericAndSpecifics (gens context)
              , vpatterns    = patterns context
              , vgens        = gens context
              , vIndices     = identities context
              , vviews       = viewDefs context
              , lookupView   = lookupView'
              , getDefaultViewForConcept = getDefaultViewForConcept'
              , conceptDefs  = ctxcds context
              , fSexpls      = ctxps context
              , metas        = ctxmetas context
              , crudInfo     = mkCrudInfo fSpecAllConcepts fSpecAllDecls fSpecAllInterfaces
              , atomsInCptIncludingSmaller = atomValuesOf contextinfo initialpopsDefinedInScript
              , tableContents = tblcontents contextinfo initialpopsDefinedInScript
              , pairsInExpr  = pairsinexpr
              , allViolations  = [ (r,vs)
                                 | r <- allrules -- Removed following, because also violations of invariant rules are violations.. , not (isSignal r)
                                 , let vs = ruleviolations r, not (null vs) ]
              , allExprs     = expressionsIn context
              , allSigns     = nub $ map sign fSpecAllDecls ++ map sign (expressionsIn context)
              , initialConjunctSignals = [ (conj, viols) | conj <- allConjs 
                                         , let viols = conjunctViolations conj
                                         , not $ null viols
                                         ]
              , contextInfo = contextinfo
              , specializationsOf = smallerConcepts (gens context)
              , generalizationsOf = largerConcepts  (gens context)
              , editableConcepts = nub . concatMap editablecpts . fSpecRoleInterfaces
              }
   where           
     editablecpts :: Interface -> [A_Concept]
     editablecpts ifc = editables (ifcObj ifc)
        where
          editables :: ObjectDef -> [A_Concept]
          editables obj = 
             case objmsub obj of
               Nothing       -> case objctx obj of
                                  EDcD rel -> f target rel
                                  EFlp (EDcD rel) -> f source rel
                                  _ -> []
               Just (InterfaceRef _ _) -> []
               Just (Box _ _ objs)     -> foldr (uni) [] (map editables objs)
          f sORt dcl = [sORt dcl | dcl `elem` ifcParams ifc]
     pairsinexpr  :: Expression -> [AAtomPair]
     pairsinexpr = fullContents contextinfo initialpopsDefinedInScript
     ruleviolations :: Rule -> [AAtomPair]
     ruleviolations r = case rrexp r of
          EEqu{} -> (cra >- crc) ++ (crc >- cra)
          EInc{} -> cra >- crc
          _      -> pairsinexpr (EDcV (sign (consequent r))) >- crc  --everything not in con
          where cra = pairsinexpr (antecedent r)
                crc = pairsinexpr (consequent r)
     conjunctViolations :: Conjunct -> [AAtomPair]
     conjunctViolations conj =
       let vConts    = Set.fromList $ pairsinexpr (EDcV (sign (rc_conjunct conj)))
           conjConts = Set.fromList $ pairsinexpr (rc_conjunct conj)
       in  Set.toList $ vConts `Set.difference` conjConts 

     contextinfo = contextInfoOf context

     fSpecAllConcepts = concs context
     fSpecAllDecls = relsDefdIn context
     fSpecAllInterfaces :: [Interface]
     fSpecAllInterfaces = map enrichIfc (ctxifcs context)
       where
          enrichIfc :: Interface -> Interface
          enrichIfc ifc
           = ifc{ ifcEcas = fst . assembleECAs opts context $ ifcParams ifc
                , ifcControls = makeIfcControls (ifcParams ifc) allConjs
                }
     fSpecRoleInterfaces :: Role -> [Interface]
     fSpecRoleInterfaces role = filter (forThisRole role) fSpecAllInterfaces
     forThisRole ::Role -> Interface -> Bool
     forThisRole role interf = case ifcRoles interf of
                                     []   -> True -- interface is for all roles
                                     rs  -> role `elem` rs
     
     themesInScope = if null (ctxthms context)   -- The names of patterns/processes to be printed in the functional specification. (for making partial documentation)
                     then map name (patterns context)
                     else ctxthms context
     pattsInThemesInScope = filter (\p -> name p `elem` themesInScope) (patterns context)
     cDefsInThemesInScope = filter (\cd -> cdfrom cd `elem` themesInScope) (ctxcds context)
     rulesInThemesInScope = ctxrs context `uni` concatMap ptrls pattsInThemesInScope
     declsInThemesInScope = ctxds context `uni` concatMap ptdcs pattsInThemesInScope
     concsInThemesInScope = concs (ctxrs context) `uni`  concs pattsInThemesInScope
     gensInThemesInScope  = ctxgs context ++ concatMap ptgns pattsInThemesInScope

     initialpopsDefinedInScript = 
                   [ let dcl = popdcl (head eqclass)
                     in ARelPopu{ popsrc = source dcl
                                , poptgt = target dcl
                                , popdcl = dcl
                                , popps  = (nub.concat) [ popps pop | pop<-eqclass ]
                                }
                   | eqclass<-eqCl popdcl [ pop | pop@ARelPopu{}<-populations ] ] ++
                   [ ACptPopu{ popcpt = popcpt (head eqclass)
                             , popas  = (nub.concat) [ popas pop | pop<-eqclass ]
                             }
                   | eqclass<-eqCl popcpt [ pop | pop@ACptPopu{}<-populations ] ]
       where populations = ctxpopus context++concatMap ptups (patterns context)       
     allConjs = makeAllConjs opts allrules
     fSpecAllConjsPerRule :: [(Rule,[Conjunct])]
     fSpecAllConjsPerRule = converse [ (conj, rc_orgRules conj) | conj <- allConjs ]
     fSpecAllConjsPerDecl = converse [ (conj, relsUsedIn $ rc_conjunct conj) | conj <- allConjs ] 
     fSpecAllConjsPerConcept = converse [ (conj, smaller (source r) `uni` smaller (target r)) | conj <- allConjs, r <- relsMentionedIn $ rc_conjunct conj ]
                               where smaller :: A_Concept -> [A_Concept]
                                     smaller cpt = [cpt] `uni` smallerConcepts (gens context) cpt
     allQuads = quadsOfRules opts allrules 
     
     allrules = map setIsSignal (allRules context)
        where setIsSignal r = r{isSignal = (not.null) (maintainersOf r)}
     maintainersOf :: Rule -> [Role]
     maintainersOf r 
       = concatMap arRoles . filter forThisRule . ctxrrules $ context
         where
          forThisRule :: A_RoleRule -> Bool
          forThisRule x = name r `elem` arRules x
     isUserDefined rul =
       case r_usr rul of
         UserDefined  -> True
         Multiplicity -> False
         Identity     -> False
     allActivities :: [Activity]
     allActivities = map makeActivity (filter isSignal allrules)
     allVecas = {-preEmpt opts . -} fst (assembleECAs opts context fSpecAllDecls)   -- TODO: preEmpt gives problems. Readdress the preEmption problem and redo, but properly.
     -- | allDecs contains all user defined plus all generated relations plus all defined and computed totals.
     calcProps :: Declaration -> Declaration
     calcProps d = d{decprps_calc = Just calculated}
         where calculated = decprps d `uni` [Tot | d `elem` totals]
                                      `uni` [Sur | d `elem` surjectives]
     calculatedDecls = map calcProps fSpecAllDecls
     constructKernels = foldl f (group (delete ONE fSpecAllConcepts)) (gens context)
         where f disjuncLists g = concat haves : nohaves
                 where
                   (haves,nohaves) = partition (not.null.intersect (concs g)) disjuncLists
  -- determine relations that are total (as many as possible, but not necessarily all)
     totals      = [ d |       EDcD d  <- totsurs ]
     surjectives = [ d | EFlp (EDcD d) <- totsurs ]
     totsurs :: [Expression]
     totsurs
      = nub [rel | q<-filter (not . isSignal . qRule) allQuads -- all quads for invariant rules
                 , isIdent (qDcl q)
                 , x<-qConjuncts q, dnf<-rc_dnfClauses x
                 , let antc = conjNF opts (foldr (./\.) (EDcV (sign (head (antcs dnf++conss dnf)))) (antcs dnf))
                 , isRfx antc -- We now know that I is a subset of the antecedent of this dnf clause.
                 , cons<-map exprCps2list (conss dnf)
            -- let I |- r;s;t be an invariant rule, then r and s and t~ and s~ are all total.
                 , rel<-init cons++[flp r | r<-tail cons]
                 ]
  -- Lookup view by id in fSpec.
     lookupView' :: String -> ViewDef
     lookupView'  viewId =
       case filter (\v -> vdlbl v == viewId) $ viewDefs context of
         []   -> fatal 174 $ "Undeclared view " ++ show viewId ++ "." -- Will be caught by static analysis
         [vd] -> vd
         vds  -> fatal 176 $ "Multiple views with id " ++ show viewId ++ ": " ++ show (map vdlbl vds) -- Will be caught by static analysis
     
     -- Return the default view for concpt, which is either the view for concpt itself (if it has one) or the view for
     -- concpt's smallest superconcept that has a view. Return Nothing if there is no default view.
     getDefaultViewForConcept' :: A_Concept -> Maybe ViewDef
     getDefaultViewForConcept' concpt =
       case [ vd 
            | vd@Vd{vdcpt = c, vdIsDefault = True} <- viewDefs context
            ,  c `elem` (concpt : largerConcepts (gens context) concpt) 
            ] of
         []     -> Nothing
         (vd:_) -> Just vd

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
     cRels = [     EDcD d  | d@Sgn{}<-fSpecAllDecls, isTot d, not$decplug d]++
             [flp (EDcD d) | d@Sgn{}<-fSpecAllDecls, not (isTot d) && isSur d, not$decplug d]
--  Step 2: select and arrange all relations to obtain a set dRels of injective relations
--          to ensure deletability of entities (signal declarations are excluded)
     dRels = [     EDcD d  | d@Sgn{}<-fSpecAllDecls, isInj d, not$decplug d]++
             [flp (EDcD d) | d@Sgn{}<-fSpecAllDecls, not (isInj d) && isUni d, not$decplug d]
--  Step 3: compute longest sequences of total expressions and longest sequences of injective expressions.
     maxTotPaths = map (:[]) cRels   -- note: instead of computing the longest sequence, we take sequences of length 1, the function clos1 below is too slow!
     maxInjPaths = map (:[]) dRels   -- note: instead of computing the longest sequence, we take sequences of length 1, the function clos1 below is too slow!
     --    Warshall's transitive closure algorithm, adapted for this purpose:
--     clos1 :: [Expression] -> [[Expression]]
--     clos1 xs
--      = foldl f [ [ x ] | x<-xs] (nub (map source xs) `isc` nub (map target xs))
--        where
--          f :: [[Expression]] -> A_Concept -> [[Expression]]
--          f q x = q ++ [l ++ r | l <- q, x == target (last l),
--                                 r <- q, x == source (head r), null (l `isc` r)]

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
             , ifcObj    = Obj { objnm   = name cpt ++ " (instantie)"
                               , objpos  = Origin "generated object for interface for each concept in TblSQL or ScalarSQL"
                               , objctx  = EDcI cpt
                               , objcrud = def
                               , objmView = Nothing
                               , objmsub = Just . Box cpt Nothing $
                                            Obj { objnm   = "I["++name cpt++"]"
                                                , objpos  = Origin "generated object: step 4a - default theme"
                                                , objctx  = EDcI cpt
                                                , objcrud = def
                                                , objmView = Nothing
                                                , objmsub = Nothing
                                                , objstrs = [] }
                                           :[Obj { objnm   = name dcl ++ "::"++name (source dcl)++"*"++name (target dcl)
                                                 , objpos  = Origin "generated object: step 4a - default theme"
                                                 , objctx  = if source dcl==cpt then EDcD dcl else flp (EDcD dcl)
                                                 , objcrud = def
                                                 , objmView = Nothing
                                                 , objmsub = Nothing
                                                 , objstrs = [] }
                                            | dcl <- params]
                               , objstrs = []
                               }
             , ifcEcas   = fst (assembleECAs opts context params)
             , ifcControls = makeIfcControls params allConjs
             , ifcPos    = Origin "generated interface for each concept in TblSQL or ScalarSQL"
             , ifcPrp    = "Interface " ++name cpt++" has been generated by Ampersand."
             , ifcRoles  = []
             }
        | cpt<-fSpecAllConcepts
        , let params = [ d | d<-fSpecAllDecls, cpt `elem` concs d]
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
                     , objcrud = def
                     , objmView = Nothing
                     , objmsub = Just . Box (target t) Nothing $ recur [ pth | (_:pth)<-cl, not (null pth) ]
                     , objstrs = [] }
               | cl<-eqCl head es, (t:_)<-take 1 cl] --
            -- es is a list of expression lists, each with at least one expression in it. They all have the same source concept (i.e. source.head)
            -- Each expression list represents a path from the origin of a box to the attribute.
            -- 16 Aug 2011: (recur es) is applied once where es originates from (maxTotPaths `uni` maxInjPaths) both based on clos
            -- Interfaces for I[Concept] are generated only for concepts that have been analysed to be an entity.
            -- These concepts are collected in gPlugConcepts
            gPlugConcepts = [ c | InternalPlug plug@TblSQL{}<-genPlugs , (c,_)<-take 1 (cLkpTbl plug) ]
            -- Each interface gets all attributes that are required to create and delete the object.
            -- All total attributes must be included, because the interface must allow an object to be deleted.
        in
        [Ifc { ifcClass    = Nothing
             , ifcParams   = params
             , ifcArgs     = []
             , ifcObj      = Obj { objnm   = name c
                                 , objpos  = Origin "generated object: step 4a - default theme"
                                 , objctx  = EDcI c
                                 , objcrud = def
                                 , objmView = Nothing
                                 , objmsub = Just . Box c Nothing $ objattributes
                                 , objstrs = [] }
             , ifcEcas     = fst (assembleECAs opts context params)
             , ifcControls = makeIfcControls params allConjs
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
        , let params = [ d | EDcD d <- concatMap primsMentionedIn (expressionsIn objattributes)]++
                       [ Isn cpt |  EDcI cpt <- concatMap primsMentionedIn (expressionsIn objattributes)]
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
                                 , objcrud = def
                                 , objmView = Nothing
                                 , objmsub = Just . Box ONE Nothing $ [att]
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
              nm' 0  = plural printingLanguage (name c)
              nm' i  = plural printingLanguage (name c) ++ show i
              nms = [nm' i |i<-[0..], nm' i `notElem` map name (ctxifcs context)]
              nm
                | theme opts == StudentTheme = name c
                | null nms = fatal 355 "impossible"
                | otherwise = head nms
              att = Obj (name c) (Origin "generated attribute object: step 4b") (EDcV (Sign ONE c)) def Nothing Nothing []
        ]
     ----------------------
     --END: making interfaces
     ----------------------
     printingLanguage = fromMaybe (ctxlang context) (language opts)  -- The language for printing this specification is taken from the command line options (language opts). If none is specified, the specification is printed in the language in which the context was defined (ctxlang context).

        {- makeActivity turns a process rule into an activity definition.
        Each activity can be mapped to a single interface.
        A call to such an interface takes the population of the current context to another population,
        while maintaining all invariants.
        -}
     makeActivity :: Rule -> Activity
     makeActivity rul
         = let s = Act{ actRule   = rul
                      , actTrig   = decls
                      , actAffect = nub [ d' | (d,_,d')<-clos2 affectPairs, d `elem` decls]
                      , actQuads  = invQs
                      , actEcas   = [eca | eca<-allVecas, eDcl (ecaTriggr eca) `elem` decls]
                      , actPurp   = [Expl { explPos = OriginUnknown
                                          , explObj = ExplRule (name rul)
                                          , explMarkup = A_Markup { amLang   = Dutch
                                                                  , amPandoc = [Plain [Str "Waartoe activiteit ", Quoted SingleQuote [Str (name rul)], Str" bestaat is niet gedocumenteerd." ]]
                                                                  }
                                          , explUserdefd = False
                                          , explRefIds = ["Regel "++name rul]
                                          }
                                    ,Expl { explPos = OriginUnknown
                                          , explObj = ExplRule (name rul)
                                          , explMarkup = A_Markup { amLang   = English
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
             invQs       = [q | q<-allQuads, (not.isSignal.qRule) q
                              , (not.null) ((relsUsedIn.qRule) q `isc` decls)] -- SJ 20111201 TODO: make this selection more precise (by adding inputs and outputs to a quad).
        -- a relation affects another if there is a quad (i.e. an automated action) that links them
             affectPairs = [(qDcl q,[q], d) | q<-invQs, d<-(relsUsedIn.qRule) q]
        -- the relations affected by automated action
        --      triples     = [ (r,qs,r') | (r,qs,r')<-clos affectPairs, r `elem` rels]
        ----------------------------------------------------
        --  Warshall's transitive closure algorithm in Haskell, adapted to carry along the intermediate steps:
        ----------------------------------------------------
             clos2 :: (Eq a,Eq b) => [(a,[b],a)] -> [(a,[b],a)]     -- e.g. a list of pairs, with intermediates in between
             clos2 xs
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
        
makeIfcControls :: [Declaration] -> [Conjunct] -> [Conjunct]
makeIfcControls params allConjs
 = [ conj 
   | conj<-allConjs
   , (not.null) (map EDcD params `isc` primsMentionedIn (rc_conjunct conj))
   -- Filtering for uni/inj invariants is pointless here, as we can only filter out those conjuncts for which all
   -- originating rules are uni/inj invariants. Conjuncts that also have other originating rules need to be included
   -- and the uni/inj invariant rules need to be filtered out at a later stage (in Generate.hs).
   ]
  

class Named a => Rename a where
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
     

tblcontents :: ContextInfo -> [Population] -> PlugSQL -> [[Maybe AAtomValue]]
tblcontents ci ps plug
   = case plug of
     ScalarSQL{} -> [[Just x] | x<-atomValuesOf ci ps (cLkp plug)]
     BinSQL{}    -> [[(Just . apLeft) p,(Just . apRight) p] |p<-fullContents ci ps (mLkp plug)]
     TblSQL{}    -> 
 --TODO15122010 -> remove the assumptions (see comment data PlugSQL)
 --fields are assumed to be in the order kernel+other,
 --where NULL in a kernel field implies NULL in the following kernel fields
 --and the first field is unique and not null
 --(r,s,t)<-mLkpTbl: s is assumed to be in the kernel, fldexpr t is expected to hold r or (flp r), s and t are assumed to be different
       case fields plug of 
         []   -> fatal 593 "no fields in plug."
         f:fs -> transpose
                 ( map Just cAtoms
                 : [case fExp of
                       EDcI c -> [ if a `elem` atomValuesOf ci ps c then Just a else Nothing | a<-cAtoms ]
                       _      -> [ (lkp a . fullContents ci ps) fExp | a<-cAtoms ]
                   | fld<-fs, let fExp=fldexpr fld
                   ]
                 )
                 where
                   cAtoms = (atomValuesOf ci ps. source . fldexpr) f
                   lkp a pairs
                    = case [ p | p<-pairs, a==apLeft p ] of
                       [] -> Nothing
                       [p] -> Just (apRight p)
                       _ -> fatal 428 ("(this could happen when using --dev flag, when there are violations)\n"++
                               "Looking for: '"++showValADL a++"'.\n"++
                               "Multiple values in one field. \n"
                               )
                        
                        