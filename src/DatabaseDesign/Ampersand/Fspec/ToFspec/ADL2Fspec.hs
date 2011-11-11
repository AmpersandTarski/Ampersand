{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec 
    (makeFspec,actSem, delta, allClauses, conjuncts, quads, assembleECAs, preEmpt, doCode, editable, editMph)
  where
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Core.Poset
   import Prelude hiding (Ord(..))
   import DatabaseDesign.Ampersand.Basics                       (fatalMsg,Collection(..),Identified(..),uniqueNames,eqCl, eqClass)
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.ADL1
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import DatabaseDesign.Ampersand.Misc                         (Options(..),DocTheme(..),plural)
   import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms    (conjNF,disjNF,normPA,simplify,isI)
   import DatabaseDesign.Ampersand.Fspec.Plug
   import DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Plug       (makeSqlPlug,makeEntities,rel2plug)
   import DatabaseDesign.Ampersand.Fspec.ShowADL
--   import DatabaseDesign.Ampersand.Fspec.ShowHS
--   import DatabaseDesign.Ampersand.Fspec.FPA (FPA(..))
   import Data.List (nub,intercalate)
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ToFspec.ADL2Fspec"

   makeFspec :: Options -> A_Context -> Fspc
   makeFspec flags context = fSpec
    where
        allQuads = quads (\_->True) (rules context++multrules context++keyrules context)
        fSpec =
            Fspc { fsName       = name context
                   -- interfaceS contains the interfaces defined in the Ampersand script.
                   -- interfaces are meant to create user interfaces, programming interfaces and messaging interfaces.
                   -- A generic user interface (the Lonneker interface) is already available.
                 , vprocesses   = allProcs
                 , vplugInfos   = definedplugs
                 , plugInfos    = allplugs
                 , interfaceS   = ctxifcs context -- interfaces specified in the Ampersand script
                 , interfaceG   = [ifc | ifc<-interfaceGen, let ctxrel = objctx (ifcObj ifc)
                                       , isI ctxrel && source ctxrel==ONE
                                         || ctxrel `notElem` map (objctx.ifcObj) (interfaceS fSpec)
                                       , allInterfaces flags]  -- generated interfaces
                 , fSwitchboard = switchboard fSpec
                 , fActivities  = [ makeActivity fSpec rul | rul <-processRules context]
                 , fRoleRels    = mayEdit   context  -- fRoleRels says which roles may change the population of which relation.
                 , fRoleRuls    = maintains context  -- fRoleRuls says which roles maintain which rules.
                 , vrules       = rules context   -- all user defined rules
                 , grules       = multrules context++keyrules context
                 , vconjs       = nub [conj | Quad _ ccrs<-allQuads, (conj,_)<-cl_conjNF ccrs]
                 , vquads       = allQuads
                 , vEcas        = {-preEmpt-} assembleECAs [q | q<-vquads fSpec, isInvariantQuad q] -- TODO: preEmpt gives problems. Readdress the preEmption problem and redo, but properly.
                 , vrels        = allDecs -- contains all user defined plus all generated relations.
                 , fsisa        = [(gengen g,genspc g) | g<-ctxgs context]
                 , vpatterns    = patterns context
                 , vgens        = gens context
                 , vkeys        = keyDefs context
                 , vConceptDefs = conceptDefs context
                 , fSexpls      = [ xpl { explObj = case explObj xpl of ExplContext str -> ExplFspc str; _ -> explObj xpl } -- All explanations are uses as-is. Only the context-explanations are relabeled to Fspc-explanations.
                                  | xpl<-ctxps context]
                 , vctxenv      = ctxenv context
                 }
        isInvariantQuad q = null [r | (r,rul)<-maintains context, rul==cl_rule (qClauses q)]
        allProcs = [ FProc {proc = p
                           ,activities =selectActs p
                           } | p<-ctxprocs context ]
                   where selectActs p   = [act | act<-fActivities fSpec
                                               , (not.null) (selRoles p act)]
                         selRoles p act = [r | (r,rul)<-maintains context, rul==actRule act, r `elem` roles p]
        allDecs = [ d{decprps_calc = multiplicities d `uni` [Tot |    ERel r <-totals, d==makeDeclaration r]
                                                      `uni` [Sur |EFlp (ERel r)<-totals, d==makeDeclaration r]}
                  | d<-declarations context -- , deciss d || decusr d
                  ] {- ++
                  [ Sgn  { decnm   = "ISA"
                         , decsgn  = sign g
                         , decprps = [Uni,Tot,Inj]
                         , decprps_calc = []
                         , decprL  = ""
                         , decprM  = "is a"
                         , decprR  = ""
                         , decMean = ""
                         , decpopu = []
                         , decfpos = Origin "generated declaration ISA"
                         , deciss  = True
                         , decusr  = False
                         , decpat  = ""
                         , decplug = False
                         } | g<-gens context] -}
     -- determine relations that are total (as many as possible, but not necessarily all)
        totals :: [Expression]
        totals
         = nub [rel | q<-quads visible (invariants fSpec), isIdent (qMorph q)
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
                   [InternalPlug (rename p (qlfname (name p))) | p <- uniqueNames
                          (map name definedplugs) -- the names of definedplugs will not be changed, assuming they are all unique
                          (gPlugs ++ relPlugs)
                    ]
        -- all plugs with at least one flduniq=True field generated by the compiler
        gPlugs :: [PlugSQL]
        gPlugs   = makeEntities context savedRels [p |InternalPlug p<-vsqlplugs]
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

--  Step 4: a) generate interfaces starting with INTERFACE concept: I[Concept]
--          b) generate interfaces starting with INTERFACE concepts: V[ONE*Concept] 
--          note: based on a theme one can pick a certain set of generated interfaces (there is not one correct set)
--                default theme => generate interfaces from the clos total expressions and clos injective expressions (see step1-3).
--                                 PRO: any rule set can be maintained AND every relation can be edited
--                                 CON: step 3 may explode leading to unacceptable compile time
--                student theme => generate interfaces based on plugs i.e. INTERFACE entityplug: I[ID] hiding I[ID] (note: step1-3 are skipped)
--                                 For every concept A in ScalarSQL and TblSQL (cLkp) there is a INTERFACE with attributes:
--                                   -> each non-bijective kernelfield B required by A (non-bijective flds with less or equal NULLs)
--                                      -> each total attrfield r with source r=B (non-lists) (note: uni&tot r are required for A)
--                                   -> each bijective (required) kernelfield C of A (bijective flds with equal NULLs)
--                                      -> each total attrfield r with source r=C (non-lists) (note: uni&tot r are required for A)
--                                   -> each attrfield r with source r=A (non-lists)
--                                   -> each target_m_field where r in BinSQL with source r=A (lists)
--                                   -> each non-bijective kernelfield D that requires A (non-bijective flds with more or equal NULLs)
--                                 note: the INTERFACE of C is similar to that of A, only instance of C is $id 
--                                 note: the INTERFACE of B contains links to the interface of A, C and D
--                                       => if A and ID are bijective, then all fields in the plug + some BinSQL can be edited by INTERFACE gen:I[GEN]
--                                 PRO-CON: opposite of PRO-CON of default theme
--                                 note: student rules in the atlas are always signal rules (not maintained)
--                                       and every r can be edited in the student theme, 
--                                           because every concept A is in exactly one TblSQL or ScalarSQL 
--                                           (i.e. INTERFACE A:I[A] exists with attributes for each target_m_field where r in BinSQL with source r=A)
--                                       thus, no CON for contexts with only process rules 
        interfaceGen = step4a ++ step4b
        step4a
         | theme flags == StudentTheme 
         = let cptifcs = [(c,cfld,p) | InternalPlug p@TblSQL{}<-allplugs, (c,cfld)<-cLkpTbl p]
                      ++ [(cLkp p,column p,p) | InternalPlug p@(ScalarSQL{})<-allplugs]
               binplugs = [p | InternalPlug p@(BinSQL{})<-allplugs]
               --bijective or non-bijective required kernelfields (excl.field for ID and cfld )
               reqks p cfld = [kfld | kfld<-requiredFields p cfld,iskey p kfld, kfld/=cfld]
               --atts of kfld required by cfld
               reqatts kfld p cfld = [attfld | (kfld',attfld)<-attrels p, kfld==kfld', attfld `elem` requiredFields p cfld]
               --cfld is att of flds
               attof p cfld = [attfld | (cfld',attfld)<-attrels p, cfld==cfld',cfld/=attfld]
               --atts of cfld
               myatts c = [attfld | InternalPlug p@(TblSQL{})   <-allplugs, (_,attfld)<-attrels p, target(fldexpr attfld)==c]
               --objats of Obj{kfld} within interface for cfld
               katts kfld p cfld
                 = [Obj { objnm   = "katts"++ fldname attfld
                        , objpos  = Origin "generated object katts"
                        , objctx  = plugpath p kfld attfld --composition from kfld to attfld
                        , objats  = []
                        , objstrs = [] }
                        |attfld<-reqatts kfld p cfld]
               --non-bijective kernelfields that require cfld
               ksreq p cfld = [kfld |kfld<-tblfields p,iskey p kfld, requires p (kfld,cfld), kfld `notElem` bijectivefields p cfld]
               --objats for interface for concept c (see comment above)
               catts p c cfld
                 = [Obj { objnm   = "reqks"++ show(plugpath p cfld kfld) --TODO -> nice name? (fldname of kernel field is not always nice)
                        , objpos  = Origin "generated object reqks"
                        , objctx  = plugpath p cfld kfld --composition from cfld to kfld
                        , objats  = katts kfld p cfld
                        , objstrs = [] }
                   | kfld<-reqks p cfld] 
                   ++ 
                   [Obj { objnm   = "myatts"++ fldname attfld
                               , objpos  = Origin "generated object myatts"
                               , objctx  = flp(fldexpr attfld) 
                               , objats  = []
                               , objstrs = [] }
                   | attfld<-myatts c] 
                   ++ 
                   [Obj { objnm   = "attof"++ fldname attfld
                               , objpos  = Origin "generated object attof"
                               , objctx  = plugpath p cfld attfld --composition from cfld to attfld
                               , objats  = []
                               , objstrs = [] }
                   | attfld<-attof p cfld] 
                   ++
                   [Obj { objnm   = "bin"++ name bp
                        , objpos  = Origin "generated object bin"
                        , objctx  = if source(mLkp bp)==c then ERel (mLkp bp)  else flp (ERel (mLkp bp))
                        , objats  = []
                        , objstrs = [] }
                   | bp<-binplugs, source(mLkp bp)==c || target(mLkp bp)==c]
                   ++
                   [Obj { objnm   = "ksreq"++ show(plugpath p cfld kfld) --TODO -> nice name? (fldname of kernel field is not always nice)
                        , objpos  = Origin "generated object ksreq"
                        , objctx  = plugpath p cfld kfld --composition from cfld to kfld
                        , objats  = [] --note: atts of kfld are not required for cfld (kfld isn't either)
                        , objstrs = [] }
                   | kfld<-ksreq p cfld]
           in
           --interface for each concept in TblSQL or ScalarSQL
           [Ifc { ifcName   = name c
                , ifcParams = [rel | rel<-mors p, not (isIdent rel)]
                , ifcViols  = []
                , ifcArgs   = []
                , ifcObj    = Obj { objnm   = name c
                                  , objpos  = Origin "generated object for interface for each concept in TblSQL or ScalarSQL"
                                  , objctx  = case p of TblSQL{} -> ERel (I c) ; _ -> fldexpr cfld
                                  , objats  = catts p c cfld
                                  , objstrs = [] }
                , ifcPos    = Origin "generated interface for each concept in TblSQL or ScalarSQL"
                , ifcExpl   = "Interface " ++name c++" has been generated by Ampersand."
                }
           | (c,cfld,p)<-cptifcs] 
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
                        , objats  = recur (trace++[c]) cl
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
                                  , objats  = objattributes
                                  , objstrs = [] }
                , ifcPos    = Origin "generated interface: step 4a - default theme"
                , ifcExpl   = "Interface " ++name c++" has been generated by Ampersand."
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
                                  , objats  = [att]
                                  , objstrs = [] }
                , ifcPos    = ifcPos  ifcc
                , ifcExpl   = ifcExpl ifcc
                }
           | ifcc<-step4a
           , let c   = source(objctx (ifcObj ifcc))
                 nm'::Int->String
                 nm' 0  = plural (language flags)(name c)
                 nm' i  = plural (language flags)(name c) ++ show i
                 nms = [nm' i |i<-[0..], nm' i `notElem` map name (ctxifcs context)]
                 nm = if null nms then fatal 355 "impossible" else head nms 
                 att = Obj (name c) (Origin "generated attribute object: step 4b") (ERel (V (Sign ONE c))) [] []
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
                 , actXpls   = []   -- TODO: this is erroneous. Should contain an explanation.
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
   quads :: (Relation->Bool) -> [Rule] -> [Quad]
   quads visible rs
    = [ Quad r (Clauses [ (conj,allShifts conj)
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
   allClauses :: Rule -> Clauses
   allClauses rule = Clauses [(conj,allShifts conj) | conj<-conjuncts rule] rule

   allShifts :: Expression -> [Expression]
   allShifts conjunct = nub [simplify e' | e'<-xshiftL conjunct++shiftR conjunct, not (isTrue e')]
{- used to be nicer, by normalizing on 'flp'. But that yields overlapping types since january of 2011...
   Feel free to restore, because I couldn't. (SJ)
   allShifts conjunct = nub [simplify (normFlp e') | e'<-shiftL conjunct++shiftR conjunct, not (isTrue e')]
    where
       normFlp :: Expression -> Expression
       normFlp (EUni []) = EUni []
       normFlp (EUni fs) = if length [r | f<-fs, r<-morlist f, inline r] <= length [r | f<-fs, r<-morlist f, not (inline r)]
                         then EUni (map flp fs) else (EUni fs)
       normFlp _ = fatal 492 $ "normFlp must be applied to EUni expressions only, look for mistakes in shiftL or shiftR"
-}

    where
     xshiftL :: Expression -> [Expression]
     xshiftL r
      | length antss+length conss /= length fus = fatal 498 $ "shiftL will not handle argument of the form "++showADL r
      | null antss || null conss                = [disjuncts r |not (null fs)] --  shiftL doesn't work here.
      | all idsOnly (concat antss)              = [EUni (ECpl (ECps [ERel (I srcA)]):map ECps conss)]
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
       EUni fs = disjuncts r                -- informal example [  "-(r;s)", "-(p;r)", "x;y;z" ] coming from  "r;s /\ p;r |- x;y;z"
       fus = filter (not.isI) fs
       antss = [ts | ECpl (ECps ts)<-fus]   -- e.g. [ ["r","s"], ["p","r"] ]
       conss = [ts | ECps ts<-fus]          -- e.g. [ ["x","y","z"] ]
       srcA
        | null antss =
          fatal 514 $ "empty antecedent in shiftL (" ++ showADL r ++ ")"
        | length (eqClass (<==>) [source (head ants) | ants <- antss]) > 1 =
          fatal 515 $ "shiftL (" ++ showADL r ++ ")\nin calculation of srcA\n" ++ show (eqClass (<==>) [source (head ants) | ants <- antss])
        | otherwise = foldr1 join [source (head ants) | ants <- antss]
       id' ass = [ERel (I c) ]
        where a = (source.head.head) ass
              c = if not (a <==> b) then fatal 519 $ "shiftL ("++showADL r++")\nass: "++show ass++"\nin calculation of c = a `join` b with a="++show a++" and b="++show b else
                  a `join` b
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
      | all idsOnly (concat conss)              = [EUni (ECpl (ECps [ERel (I srcA)]) : map ECps antss)]
      | otherwise                               = [EUni ([ ECpl (ECps (if null ts then id' css else ts))
                                                       | ts<-ass++[id' css | null ass]]++
                                                       [ ECps (if null ts then id' ass else ts)
                                                       | ts<-css++[id' ass | null css]])
                                                  | (ass,css)<-nub(move antss conss)]
      where
       EUni fs = disjuncts r  -- fs is a list of expressions
       fus = filter (not.isI) fs
       antss = [ts | ECpl (ECps ts)<-fus]
       conss = [ts | ECps ts<-fus]
       srcA 
        | null conss =
           fatal 554 $ "empty consequent in shiftR ("++showADL r++")"
        | length (eqClass (<==>) [ source (head cons) | cons<-conss]) > 1 =
           fatal 556 $ "shiftR ("++showADL r++")\nin calculation of srcA\n"++show (eqClass (<==>) [ source (head cons) | cons<-conss])
        | otherwise = foldr1 join [ source (head cons) | cons<-conss]
       id' css = [ERel (I c) ]
        where a = (source.head.head) css
              c = if not (a <==> b)
                  then fatal 561 $ "shiftR ("++showADL r++")\nass: "++show css++"\nin calculation of c = a `join` b with a="++show a++" and b="++show b ++ ". "
                  else a `join` b
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
                                   let visible _ = True in doCode visible ev toExpr viols
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

   conjuncts :: Rule -> [Expression]
   conjuncts = fiRule.conjNF.rrexp
    where fiRule (EIsc fis) = {- map disjuncts -} fis
          fiRule r        = [ {- disjuncts -} r]

-- The function disjuncts yields an expression which has constructor EUni in every case.
   disjuncts :: Expression -> Expression
   disjuncts = fuRule
    where fuRule (EUni cps) = (EUni . nub . map cpRule) cps
          fuRule r        = EUni [cpRule r]
          cpRule (ECpl r)  = ECpl (fRule r)
          cpRule r        = fRule r
          fRule (ECps ts)    = ECps ts
          fRule  r        = ECps [r]

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
    = ERel (makeRelation Sgn { decnm   = "Delta"
                             , decsgn  = Sign s t
                             , decprps = []
                             , decprps_calc = []
                             , decprL  = ""
                             , decprM  = ""
                             , decprR  = ""
                             , decMean = []
                             , decpopu = []
                             , decfpos = Origin "generated relation (Delta)"
                             , deciss  = True
                             , decusr  = False
                             , decpat  = ""
                             , decplug = True
                             }) 

   -- | de functie doCode beschrijft de voornaamste mogelijkheden om een expressie delta' te verwerken in expr (met tOp'==Ins of tOp==Del)
-- TODO: Vind een wetenschappelijk artikel waar de hier beschreven transformatie uitputtend wordt behandeld.
-- TODO: Deze code is onvolledig en misschien zelfs fout....
   doCode :: (Relation->Bool)           -- the relations that may be changed
             -> InsDel                  -- the type of action: Insert or Delete
             -> Expression              -- the expression in which a delete or insert takes place
             -> Expression              -- the delta to be inserted or deleted
             -> [(Expression,[Rule])]   -- the motivation, consisting of the conjuncts (traced back to their rules) that are being restored by this code fragment.
             -> PAclause
   doCode editAble tOp' expr1 delta1 motive = doCod delta1 tOp' expr1 motive
    where
      doCod deltaX tOp exprX motiv =
        case (tOp, exprX) of
          (_ ,  EFlp x)   -> doCod (flp deltaX) tOp x motiv
          (_ ,  EBrk x)   -> doCod deltaX tOp x motiv
          (_ ,  ETyp x t) -> if sign x==sign t then doCod deltaX tOp x motiv else
                             fatal 691 "TODO: implement narrowing."
          (_ ,  EUni [])  -> Blk motiv
          (_ ,  EIsc [])  -> Nop motiv
          (_ ,  ECps [])  -> fatal 681 $ "doCod ("++showADL deltaX++") "++show tOp++" ECps [],\n"++
                                        "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++")."
          (_ ,  ERad [])  -> fatal 683 $ "doCod ("++showADL deltaX++") "++show tOp++" ERad [],\n"++
                                        "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++")."
          (_ ,  EPrd [])  -> fatal 697 $ "doCod ("++showADL deltaX++") "++show tOp++" EPrd [],\n"++
                                        "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++")."
          (_ ,  EUni [t]) -> doCod deltaX tOp t motiv
          (_ ,  EIsc [t]) -> doCod deltaX tOp t motiv
          (_ ,  ECps [t]) -> doCod deltaX tOp t motiv
          (_ ,  ERad [t]) -> doCod deltaX tOp t motiv
          (_ ,  EPrd [t]) -> doCod deltaX tOp t motiv
          (Ins, ECpl x)   -> doCod deltaX Del x motiv
          (Del, ECpl x)   -> doCod deltaX Ins x motiv
          (Ins, EUni fs)  -> Chc [ doCod deltaX Ins f motiv | f<-fs{-, not (f==expr1 && Ins/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Ins, EIsc fs)  -> All [ doCod deltaX Ins f []    | f<-fs ] motiv
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
                                , let c = source (ECps rs) `join` target (ECps ls)
                                , let fLft atom = doCod (disjNF (EUni[EPrd [ERel (Mp1 atom c),deltaX],ECpl (ECps rs)])) Ins (ECps rs) []
                                , let fRht atom = doCod (disjNF (EUni[EPrd [deltaX,ERel (Mp1 atom c)],ECpl (ECps ls)])) Ins (ECps ls) []
                                ] motiv
          (Del, ECps ts) -> Chc [ if ECps ls==flp (ECps rs)
                                  then Chc [ Sel c (disjNF (ECps ls)) (\_->Rmv c fLft motiv) motiv
                                           , Sel c (disjNF (ECps ls)) fLft motiv
                                           ] motiv
                                  else Chc [ Sel c (disjNF (EIsc [ECps ls,flp(ECps rs)])) (\_->Rmv c (\x->All [fLft x, fRht x] motiv) motiv) motiv
                                           , Sel c (disjNF (EIsc [ECps ls,flp(ECps rs)])) fLft motiv
                                           , Sel c (disjNF (EIsc [ECps ls,flp(ECps rs)])) fRht motiv
                                           ] motiv
                                | (ls,rs)<-chop ts
                                , let c = source (ECps rs) `join` target (ECps ls)
                                , let fLft atom = doCod (disjNF (EUni[EPrd [ERel (Mp1 atom c),deltaX],ECpl (ECps rs)])) Del (ECps rs) []
                                , let fRht atom = doCod (disjNF (EUni[EPrd [deltaX,ERel (Mp1 atom c)],ECpl (ECps ls)])) Del (ECps ls) []
                                ] motiv
          (Del, EUni fs)   -> All [ doCod deltaX Del f []    | f<-fs{-, not (f==expr1 && Del/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Del, EIsc fs)   -> Chc [ doCod deltaX Del f motiv | f<-fs ] motiv
-- Op basis van de Morgan is de procesalgebra in het geval van (Ins, ERad ts)  afleidbaar uit uit het geval van (Del, ECps ts) ...
          (_  , ERad ts)   -> doCod deltaX tOp (ECpl (ECps (map ECpl ts))) motiv
          (_  , EPrd ts)   -> fatal 745 "TODO"
          (_  , EKl0 x)    -> doCod (deltaK0 deltaX tOp x) tOp x motiv
          (_  , EKl1 x)    -> doCod (deltaK1 deltaX tOp x) tOp x motiv
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

   switchboard :: Fspc -> Fswitchboard
   switchboard fSpec
    = Fswtch
       { fsbEvIn  = eventsIn
       , fsbEvOut = eventsOut
       , fsbConjs = conjs
       , fsbECAs  = ecas
       }
      where
        qs        = quads visible (invariants fSpec++multrules fSpec++keyrules fSpec)
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
