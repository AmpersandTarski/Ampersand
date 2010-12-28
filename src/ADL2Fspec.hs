{-# OPTIONS_GHC -Wall #-}
module ADL2Fspec (makeFspec,actSem, delta, allClauses, conjuncts, quads, assembleECAs, preEmpt, doCode)
  where
   import Collection     (Collection(rd,rd',uni,isc,(>-)))
   import CommonClasses  (ABoolAlg(..),uniqueNames)
   import Adl
   import Auxiliaries    (eqCl, eqClass)
   import Data.Fspec
   import Options        (Options(language,genPrototype))
   import NormalForms(conjNF,disjNF,normPA,simplify)
   import Data.Plug
   import Data.ADL2Plug
   import Char
   import ShowADL
   import FPA
   import Languages(plural)
   
   makeFspec :: Options -> Context -> Fspc
   makeFspec flags context = fSpec
    where
        allQuads = quads (\_->True) (rules context++multrules context++keyrules context)
        fSpec =
            Fspc { fsName       = if genPrototype flags 
                                  then "ctx" ++ (name context) --ctx to get unique name for php if there are (plural) concept names equal to context name
                                  else (name context) 
                   -- serviceS contains the services defined in the ADL-script.
                   -- services are meant to create user interfaces, programming interfaces and messaging interfaces.
                   -- A generic user interface (the Lonneker interface) is already available.
                 , vplugs       = definedplugs
                 , plugs        = allplugs
                 , serviceS     = attributes context -- services specified in the ADL script
                 , serviceG     = [ o| o<-serviceGen
                                     , isIdent (objctx o) && source (objctx o)==cptS
                                     || not (objctx o `elem` map objctx (serviceS fSpec))]  -- generated services
                 , services     = [ makeFservice context allQuads a | a <-serviceS fSpec++serviceG fSpec]
                 , roleServices = let lookp sv = if length servFs == 1 then head servFs else
                                                 error("!Fatal (module ADL2Fspec 40): Mistake in the type checker. It should check that all services have unique names.")
                                                 where servFs = [s|s<-services fSpec, name s==sv] in
                                  [(role,svc)| RS rs svcs _<-ctxros context                 -- ^ roleServices says which roles may use which service
                                             , sv<-svcs, let svc=lookp sv
                                             , role<-rs]
                 , mayEdit      = [(role,makeDeclaration m)| RR rs ms _<-ctxmed context     -- ^ mayEdit says which roles may change the population of which relation.
                                                           , m<-ms, role<-rs]
                 , vrules       = rules context++signals context
                 , grules       = number (length (rules context++signals context)) (multrules context++keyrules context)
                 , vconjs       = rd [conj| Quad _ ccrs<-allQuads, (conj,_)<-cl_conjNF ccrs]
                 , vquads       = allQuads
                 , vrels        = allDecs -- contains all user defined plus all generated relations.
                 , fsisa        = ctxisa context
                 , vpatterns    = patterns context
                 , vgens        = gens context
                 , vkeys        = keyDefs context
                 , pictPatts    = [] --Nothing
                 , vConceptDefs = conceptDefs context
                 , fSexpls      = fSexpls'
                 , vctxenv      = ctxenv context
                 }
        number n rs = [r{runum=i} | (i,r)<-zip [n..] rs]
        allDecs = [ d{decprps_calc = multiplicities d `uni` [Tot|m<-totals, d==makeDeclaration m, inline m]
                                                      `uni` [Sur|m<-totals, d==makeDeclaration m, not (inline m)]}
                  | d<-declarations context -- , deciss d || decusr d
                  ]++
                  [ Sgn  { decnm   = "isa"
                         , desrc   = source g
                         , detrg   = target g
                         , decprps = [Uni,Tot,Inj]
                         , decprps_calc = []
                         , decprL  = ""
                         , decprM  = "is a"
                         , decprR  = ""
                         , decpopu = []
                         , decfpos = Nowhere
                         , decid   = 0
                         , deciss  = True
                         , decusr  = False
                         , decpat  = ""
                         , decplug = True
                         } | g<-gens context]

        totals :: Morphisms
        totals
         = rd [ m | q<-quads visible (rules fSpec), isIdent (qMorph q)
                  , (_,hcs)<-cl_conjNF (qClauses q), Fux fus<-hcs
                  , antc<-[(conjNF.Fix) [notCp f| f<-fus, isNeg f]], isIdent antc
                  , f<-fus, isPos f
                  , m<-tots f
                  ]
           where tots (F fs) = [m| Tm m _<-take 1 fs]++[flp m| Tm m _<-take 1 (reverse fs)]
                 tots _ = []
                 visible _ = True -- for computing totality, we take all quads into account.

        --------------
        --making plugs
        --------------
        vsqlplugs = [PlugSql (makeSqlPlug context p)| p<-ctxsql context] --REMARK -> no optimization like try2specific, because these plugs are user defined
        vphpplugs = [PlugPhp (makePhpPlug p)| p<-ctxphp context]
        definedplugs = vsqlplugs ++ vphpplugs
        allplugs = definedplugs ++      -- all plugs defined by the user
                   [PlugSql p | p <- uniqueNames
                          (map name definedplugs) -- the names of definedplugs will not be changed, assuming they are all unique
                          (gPlugs ++ relPlugs)
                    ]
        -- all plugs with at least one flduniq=True field generated by the compiler
        gPlugs :: [PlugSQL]
        gPlugs   = makeTblPlugs context savedDecs [p|PlugSql p<-vsqlplugs]
        -- all plugs for relations not touched by definedplugs and gplugs
        relPlugs :: [PlugSQL]
        relPlugs = [ mor2plug (makeMph d) totals --(see mor2plug in Plug.hs)
                   | d<-savedDecs
                   , not (Inj `elem` multiplicities d)
                   , not (Uni `elem` multiplicities d)]
        -- declarations to be saved in generated plugs: if decplug=True, the declaration has the BYPLUG and therefore may not be saved in a database
        -- WHAT -> is a BYPLUG?
        savedDecs= (filter (not.decplug) allDecs)

--TODO151210 -> Plug A is overbodig, want A zit al in plug r
--CONTEXT Temp
--PATTERN Temp
--r::A*B[TOT].
--t::E*F[UNI].
--ENDPATTERN
--ENDCONTEXT
{-
    **************************************
    * Plug E                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    * t  [UNI]                             *
    **************************************
    * Plug F                               *
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

        -- services (type ObjectDef) can be generated from a basic ontology. That is: they can be derived from a set
        -- of relations together with multiplicity constraints. That is what serviceG does.
        -- This is meant to help a developer to build his own list of services, by providing a set of services that works.
        -- The developer will want to assign his own labels and maybe add or rearrange attributes.
        -- This is easier than to invent a set of services from scratch.

        -- Rule: a service must be large enough to allow the required transactions to take place within that service.
        -- Attributen van elk object hebben unieke namen.

--- generation of services:
--  ADL generates services for the purpose of quick prototyping. A script without any mention of services is supplemented
--  by a number of service definitions that gives a user full access to all data.
--  Step 1: select and arrange all declarations to obtain a set cRels of total relations
--          to ensure insertability of entities (signal declarations are excluded)
        cRels = [     morph d | d<-declarations context, not(deciss d), isTot d, not$decplug d]++
                [flp (morph d)| d<-declarations context, not(deciss d), not (isTot d) && isSur d, not$decplug d]
--  Step 2: select and arrange all declarations to obtain a set cRels of injective relations
--          to ensure deletability of entities (signal declarations are excluded)
        dRels = [     morph d | d<-declarations context, not(deciss d), isInj d, not$decplug d]++
                [flp (morph d)| d<-declarations context, not(deciss d), not (isInj d) && isUni d, not$decplug d]
--  Step 3: compute maximally total expressions and maximally injective expressions.
--  BECAUSE
--   (GMI): Moet voor een concept dat 'los staat' (geen attribuut van, en heeft zelf geen attributen)
--          geen service genereerd worden? => SERVICE Concept: I[Concept]
--          VOORBEELD: PATTERN x r::A*B. s::B*C. t::A*C. ENDPATTERN geen multipliciteiten=>serviceGen=[]
--  Antwoord: Jawel!
        maxTotExprs = clos cRels
        maxInjExprs = clos dRels
--  Step 4: generate services from the maximally total expressions and maximally injective expressions.
--          and generate V[ONE*Concept] for each concept
        serviceGen
         = [ Obj (name c)         -- objnm
                 Nowhere          -- objpos
                 (Tm (mIs c)(-1)) -- objctx
                 Nothing          -- objctx_proof
                 objattributes    -- objats
                 []               -- objstrs
           | cl <- eqCl source (maxTotExprs `uni` maxInjExprs)
           , let objattributes = recur [] cl
           , not (null objattributes) -- de meeste plugs hebben in ieder geval I als attribuut
           , let e0=head cl, let c=source e0
           , map toLower (name c) `notElem` map (map toLower.name) scalarPlugs -- exclude scalar SQL-tables
           ]
           ++ 
           [ Obj (plural (language flags)(name c))         -- objnm
                 Nowhere          -- objpos
                 (Tm (mIs S)(-1)) -- objctx
                 Nothing          -- objctx_proof
                 [att]            -- objats
                 []               -- objstrs
             --TODO: (GMI) Do we have a general function that produces the set of all concepts?
       --  | c<-(\(Isa isas cs) -> rd$[c|c@(C{})<-cs]++[c|(c,_)<-isas]++[c|(_,c)<-isas]) (ctxisa context)
             --SJ: yes, there is...
           | c<-concs context
           , let att = Obj (name c) Nowhere (Tm (V [cptS,c] (cptS,c))(-1)) Nothing [] []
           ]
        --REMARK151210 -> only used to exclude scalar SQL-tables, thus scalar PHP plugs (if something like that exists) are not considered
        scalarPlugs = [PlugSql p|PlugSql p@(ScalarSQL{})<-allplugs]
--  Auxiliaries for generating services:
        morph d = Mph (name d) (pos d) [] (source d,target d) True d
--    Warshall's transitive closure algorithm, adapted for this purpose:
        clos :: Morphisms -> Expressions
        clos xs
         = f [F [Tm x (-1)]| x<-xs] (rd (map source xs) `isc` rd (map target xs))
           where
            f q (x:xs') = f (q ++ [F (ls++rs)| l@(F ls)<-q, x<=target l
                                             , r@(F rs)<-q, x<=source r
                                             , null (ls `isc` rs)
                                             ]) xs'
            f q []      = q
       
        recur trace es
         = [ Obj (showADLcode fSpec t)     -- objnm
                 Nowhere                   -- objpos
                 t                         -- objctx
                 Nothing                   -- objctx_proof
                 (recur (trace++[c]) cl)   -- objats
                 []                        -- objstrs
           | cl<-eqCl (\(F ts)->head ts) es, F ts<-take 1 cl, t<-[head ts], c<-[source t], c `notElem` trace ]
-- The following definition contains all explanations that are declared within the current fSpec.
        fSexpls' = explanationDeclarations flags context                ++
                   concat (map (explanationDeclarations flags)(patterns context))


   editable :: Expression -> Bool   --TODO deze functie staat ook in Calc.hs...
   editable (Tm Mph{} _)  = True
   editable (Tm I{} _)    = True
   editable _           = False

   editMph :: Expression -> Morphism  --TODO deze functie staat ook in Calc.hs...
   editMph (Tm m@Mph{} _) = m
   editMph (Tm m@I{} _)   = m
   editMph e            = error("!Fatal (module ADL2Fspec 425): cannot determine an editable declaration in a composite expression: "++show e)

   makeFservice :: Context -> [Quad] -> ObjectDef -> Fservice
   makeFservice context _ object
    = let s = Fservice{ fsv_objectdef = object  -- the object from which the service is drawn
-- The relations that may be edited by the user of this service are represented by fsv_insrels and fsv_delrels.
-- Editing means that tuples can be added to or removed from the population of the relation.
-- The relations in which the user may insert elements:
                      , fsv_insrels   = map makeInline rels>-[makeInline m|er<-ecaRs, On Ins m<-[ecaTriggr er], Blk _<-[ecaAction er]]
-- The relations from which the user may remove elements:
                      , fsv_delrels   = map makeInline rels>-[makeInline m|er<-ecaRs, On Del m<-[ecaTriggr er], Blk _<-[ecaAction er]]
-- The rules that may be affected by this service
                      , fsv_rules     = invariants
                      , fsv_quads     = qs
-- The ECA-rules that may be used by this service to restore invariants. TODO: de Delta-parameter is nog fout!
                      , fsv_ecaRules  = [\_->er{ecaAction = action'}| er<-ecaRs, let action'=ecaAction er]
-- All signals that are visible in this service
                      , fsv_signals   = [sig|sig<-signals context]
-- All fields/parameters of this service
                      , fsv_fields    = srvfields
-- All concepts of which this service can create new instances
                      , fsv_creating  = [c| c<-rd (map target (fsv_insrels s)), t<-fsv_ecaRules s, ecaTriggr (t arg)==On Ins (mIs c)]
-- All concepts of which this service can delete instances
                      , fsv_deleting  = [c| c<-rd (map target (fsv_delrels s)), t<-fsv_ecaRules s, ecaTriggr (t arg)==On Del (mIs c)]
                      , fsv_fpa       = case depth object of -- Valideren in de FPA-wereld
                                          0 -> NO
                                          1 -> IF Eenvoudig
                                          2 -> IF Eenvoudig
                                          3 -> IF Gemiddeld
                                          _ -> IF Moeilijk 
                      , fsv_expls     = [] -- TODO: alle explanations verzamelen van dingen die in de service zitten
                      } in s
    where
-- step 1: the relations that yield potentially editable fields. These relations are called "visible".
        rels = rd (recur object)
         where recur obj = [editMph (objctx o)| o<-objats obj, editable (objctx o)]++[m| o<-objats obj, m<-recur o]
        vis         = rd (map makeInline rels++map (mIs.target) rels)
        visible r   = makeInline r `elem` vis
-- step 2: the rules that must be maintained automatically, and can possibly be affected by a transaction in this service
--         If none of the relations in a rule are visible, exclude that rule...
        invariants  = [rule| rule<-rules context,  not (null (map makeInline (mors rule) `isc` vis))]
-- step 3: the quads that can be derived from these rules, considering which relations are visible.
--         (A quad contains the conjunct(s) to be maintained.)
        qs          = quads visible invariants
-- step 4: the ECA rules derived from the quads. Cascaded blocking rules are preempted to save some excess code.
        ecaRs       = preEmpt (assembleECAs visible qs)
-- step 5: signalInvs contains the rules that might possibly be maintained by the user, while performing a transaction in this service.
--         If none of the relations in a rule are visible, exclude the rule...
--        signalInvs  = [rule| rule<-signals context, not (null (map makeInline (mors rule) `isc` vis))]
-- step 6: ECA rules derived from the signalInvs. These rules may be used to suggest ways to the user to restore signals.
--        signalRs    = preEmpt (assembleECAs visible (quads visible signalInvs))
        depth :: ObjectDef -> Int
        depth obj   = foldr max 0 [depth o| o<-objats obj]+1
        trigs :: ObjectDef -> [Declaration->ECArule]
        trigs _  = [] -- [c | editable (objctx obj), c<-nECArules {- ,not (isBlk (ecaAction (c arg))) -} ]
        arg = error("!Todo (module ADL2Fspec 467): declaratie Delta invullen")
        srvfields = [fld 0 o| o<-objats object]
        fld :: Int -> ObjectDef -> Field
        fld sLevel obj
         = Att { fld_name     = objnm obj
               , fld_sub      = [fld (sLevel +1) o| o<-objats obj]
               , fld_expr     = objctx obj
               , fld_mph      = if editable (objctx obj)
                                then editMph (objctx obj)
                                else error("!Fatal (module ADL2Fspec 476): cannot edit a composite expression: "++show (objctx obj)++"\nPlease test editability of field "++objnm obj++" by means of fld_editable first!")
               , fld_editable = editable (objctx obj)      -- can this field be changed by the user of this service?
               , fld_list     = not (isUni (objctx obj))   -- can there be multiple values in this field?
               , fld_must     = isTot (objctx obj)         -- is this field obligatory?
               , fld_new      = True                       -- can new elements be filled in? (if no, only existing elements can be selected)
               , fld_sLevel   = sLevel                     -- The (recursive) depth of the current servlet wrt the entire service. This is used for documentation.
               , fld_insAble  = not (null insTrgs)         -- can the user insert in this field?
               , fld_onIns    = case insTrgs of
                                 []  ->  error("!Fatal (module ADL2Fspec 469): no insert functionality found in field "++objnm obj++" of service "++name obj++" on line: "++show (pos (objctx obj)))
                                 [t] ->  t
                                 _   ->  error("!Fatal (module ADL2Fspec 471): multiple insert triggers found in field "++objnm obj++" of service "++name obj++" on line: "++show (pos (objctx obj)))
               , fld_delAble  = not (null delTrgs)         -- can the user delete this field?
               , fld_onDel    = case delTrgs of
                                 []  ->  error("!Fatal (module ADL2Fspec 474): no delete functionality found in field "++objnm obj++" of service "++name obj++" on line: "++show (pos (objctx obj)))
                                 [t] ->  t
                                 _   ->  error("!Fatal (module ADL2Fspec 476): multiple delete triggers found in field "++objnm obj++" of service "++name obj++" on line: "++show (pos (objctx obj)))
               }
           where triggers = trigs obj
                 insTrgs  = [c | c<-triggers, ecaTriggr (c arg)==On Ins (makeInline (editMph (objctx obj))) ]
                 delTrgs  = [c | c<-triggers, ecaTriggr (c arg)==On Del (makeInline (editMph (objctx obj))) ]


-- Comment on fld_new:
-- Consider this: New elements cannot be filled in
--    if there is a total relation r with type obj==source r  (i.e. r comes from obj),
--    which is outside the scope of this service.
-- Why? If you were to insert a new obj, x, then r would require a new link (x,y).
--    However, since r is out of scope, you cannot insert (x,y) into r.
-- More generally, if there is an ECA rule with I[type obj] in its left hand side,
--    and a right hand side that is out of scope of this service,
--    you may not insert a new element in obj.


--   fst3 :: (a,b,c) -> a
--   fst3 (a,_,_) = a
--   snd3 :: (a,b,c) -> b
--   snd3 (_,b,_) = b

   -- Quads embody the "switchboard" of rules. A quad represents a "proto-rule" with the following meaning:
   -- whenever Morphism m is affected (i.e. tuples in m are inserted or deleted),
   -- the rule may have to be restored using functionality from one of the clauses.
   -- The rule is carried along for traceability.
   quads :: (Morphism->Bool) -> Rules  -> [Quad]
   quads visible rs
    = [ Quad m (Clauses [ (conj,allShifts conj)
                        | conj <- conjuncts rule
      --                , (not.null.lambda Ins (Tm m)) conj  -- causes infinite loop
      --                , not (checkMono conj Ins m)         -- causes infinite loop
                        , let conj' = subst (m, actSem Ins m (delta (sign m))) conj
                        , (not.isTrue.conjNF) (Fux[Cpx conj,conj']) -- the system must act to restore invariance     
                        ]
                        rule)
      | rule<-rs
      , m<-rd (map makeInline (mors rule))
      , visible m
      ]

-- The function allClauses yields an expression which has constructor Fu in every case.
   allClauses :: Rule -> Clauses
   allClauses rule = Clauses [(conj,allShifts conj)| conj<-conjuncts rule] rule

   allShifts :: Expression -> Expressions
   allShifts conjunct = rd [simplify (normFlp e')| e'<-shiftL conjunct++shiftR conjunct, not (isTrue e')]
    where
       normFlp (Fux []) = Fux []
       normFlp (Fux fs) = if length [m| f<-fs, m<-morlist f, inline m] <= length [m| f<-fs, m<-morlist f, not (inline m)]
                         then Fux (map flp fs) else (Fux fs)
       normFlp _ = error ("!Fatal (module Calc 61): normFlp must be applied to Fu expressions only, look for mistakes in shiftL or shiftR")

   shiftL :: Expression -> Expressions
   shiftL r
    | length antss+length conss /= length fus = error ("!Fatal (module Calc 65): shiftL will not handle argument of the form "++showADL r)
    | null antss || null conss                = [disjuncts r|not (null fs)] --  shiftL doesn't work here.
    | idsOnly antss                           = [Fux ([Cpx (F [Tm (mIs srcA)(-1)])]++map F conss)]
    | otherwise                               = [Fux ([ Cpx (F (if null ts then id' css else ts))
                                                     | ts<-ass++if null ass then [id' css] else []]++
                                                     [ F (if null ts then id' ass else ts)
                                                     | ts<-css++if null css then [id' ass] else []])
                                                | (ass,css)<-rd(move antss conss)
                                                , if null css then error "!Fatal (module Calc 73): null css in shiftL" else True
                                                , if null ass then error "!Fatal (module Calc 74): null ass in shiftL" else True
                                                ]
    where
     Fux fs = disjuncts r
     fus = filter (not.isIdent) fs
     antss = [ts | Cpx (F ts)<-fus]
     conss = [ts | F ts<-fus]
     srcA = -- if null antss  then error ("!Fatal (module Calc 81): empty antecedent in shiftL ("++showHS options "" r++")") else
            if length (eqClass order [ source (head ants) | ants<-antss])>1 then error ("!Fatal (module Calc 82): shiftL ("++showADL r++")\nin calculation of srcA\n"++show (eqClass order [ source (head ants) | ants<-antss])) else
            foldr1 lub [ source (head ants) | ants<-antss]
     id' ass = [Tm (mIs c) (-1)]
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

   shiftR :: Expression -> Expressions
   shiftR r
    | length antss+length conss /= length fus = error ("!Fatal (module Calc 106): shiftR will not handle argument of the form "++showADL r)
    | null antss || null conss                = [disjuncts r|not (null fs)] --  shiftR doesn't work here.
    | idsOnly conss                           = [Fux ([Cpx (F [Tm (mIs srcA)(-1)])]++map F antss)]
    | otherwise                               = [Fux ([ Cpx (F (if null ts then id' css else ts))
                                                     | ts<-ass++if null ass then [id' css] else []]++
                                                     [ F (if null ts then id' ass else ts)
                                                     | ts<-css++if null css then [id' ass] else []])
                                                | (ass,css)<-rd(move antss conss)]
    where
     Fux fs = disjuncts r
     fus = filter (not.isIdent) fs
     antss = [ts | Cpx (F ts)<-fus]
     conss = [ts | F ts<-fus]
     srcA = if null conss then error ("!Fatal (module Calc 119): empty consequent in shiftR ("++showADL r++")") else
            if length (eqClass order [ source (head cons) | cons<-conss])>1
            then error ("Fatal (module Calc120): shiftR ("++showADL r++")\nin calculation of srcA\n"++show (eqClass order [ source (head cons) | cons<-conss]))
            else foldr1 lub [ source (head cons) | cons<-conss]
     id' css = [Tm (mIs c) (-1)]
      where a = (source.head.head) css
            c = if not (a `order` b)
                then error ("!Fatal (module Calc 126): shiftR ("++showADL r++")\nass: "++show css++"\nin calculation of c = a `lub` b with a="++show a++" and b="++show b ++ ". " )
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

-- Deze functie neemt verschillende clauses samen met het oog op het genereren van code.
-- Hierdoor kunnen grotere brokken procesalgebra worden gegenereerd.
   assembleECAs :: (Morphism->Bool) -> [Quad] -> [ECArule]
   assembleECAs visible qs
    = [ecarule i| (ecarule,i) <- zip ecas [(1::Int)..]]
      where
       mphEqCls = eqCl fst4 [(m,shifts,conj,cl_rule ccrs)| Quad m ccrs<-qs, (conj,shifts)<-cl_conjNF ccrs]
       ecas
        = [ ECA (On ev m) delt act
          | mphEq <- mphEqCls
          , let (m,_,_,_) = head mphEq
          , let Tm delt _ = delta (sign m)
          , ev<-[Ins,Del]
          , let act = All [ Chc [ (if isTrue  clause'   then Nop else
                                   if isTrue  step      then Nop else
                                   if isFalse clause'   then Blk else
--                                 if not (visible m) then Blk else
                                   doCode visible ev toExpr viols)
                                   [(conj,causes)]  -- the motivation for these actions
                                | clause@(Fux fus) <- shifts
                                , let clause' = conjNF (subst (m, actSem Ins m (delta (sign m))) clause)
                                , let step    = conjNF (Fux[Cpx clause,clause'])
                                , let viols   = conjNF (notCp clause')
                                , let negs    = Fux [f| f<-fus, isNeg f]
                                , let poss    = Fux [f| f<-fus, isPos f]
                                , let frExpr  = if ev==Ins
                                                then conjNF negs
                                                else conjNF poss
                                , m `elem` map makeInline (mors frExpr)
                                , let toExpr = if ev==Ins
                                               then conjNF poss
                                               else conjNF (notCp negs)
                                ]
                                [(conj,causes)]  -- to supply motivations on runtime
                          | conjEq <- eqCl snd3 [(shifts,conj,rule)| (_,shifts,conj,rule)<-mphEq]
                          , let causes          = rd' nr (map thd3 conjEq)
                          , let (shifts,conj,_) = head conjEq
                          ]
                          [(conj,rd' nr [r|(_,_,_,r)<-cl])| cl<-eqCl thd4 mphEq, let (_,_,conj,_) = head cl]  -- to supply motivations on runtime
          ]
       fst4 (w,_,_,_) = w
       snd3 (_,y,_) = y
       thd3 (_,_,z) = z
       thd4 (_,_,z,_) = z

-- If one rule r blocks upon an event, e.g. e@(ON Ins m), while another ECA rule r'
-- maintains something else with that same event e, we can save r' the trouble.
-- After all, event e will block anyway.
-- preEmpt tries to simplify ECArules by predicting whether a rule will block.
   preEmpt :: [ECArule] -> [ECArule]
   preEmpt ers = pr [length ers] (10::Int)
    where
     pr :: [Int] -> Int -> [ECArule]
     pr ls n
       | n == 0     = error ("!Fatal (module ADL2Fspec 674): too many cascading levels in preEmpt "++show ls)
       | (not.null) cascaded = pr (length cascaded:ls)
                               -- ([er{ecaAction=normPA (ecaAction er)}| er<-cascaded] ++uncasced)
                                  (n-1)
       | otherwise           = [er{ecaAction=normPA (ecaAction er)}| er<-uncasced]
      where
-- preEmpt divides all ECA rules in uncascaded rules and cascaded rules.
-- cascaded rules are those rules that have a Do component with event e, where e is known to block (for some other reason)
       new  = [er{ecaAction=normPA (ecaAction er)}| er<-ers]
       cascaded = [er{ecaAction=action'}| er<-new, let (c,action') = cascade (eMhp (ecaTriggr er)) (ecaAction er), c]
       uncasced = [er|                   er<-new, let (c,_)      = cascade (eMhp (ecaTriggr er)) (ecaAction er), not c]
-- cascade inserts a block on the place where a Do component exists that matches the blocking event.
     cascade :: Morphism -> PAclause -> (Bool, PAclause)
     cascade mph (Do srt (Tm to _) _ _) | (not.null) blkErs = (True, ecaAction (head blkErs))
      where blkErs = [er| er<-ers
                        , Blk _<-[ecaAction er]
                        , let t = ecaTriggr er
                        , eSrt t==srt
                        , makeInline (eMhp t) == makeInline to
                        , makeInline mph      /= makeInline to
                        ]
     cascade  _  c@Do{}           = (False, c)
     cascade mph (New c clause m) = ((fst.cascade mph.clause) "dummystr", New c (\str->(snd.cascade mph.clause) str) m)
     cascade mph (Rmv c clause m) = ((fst.cascade mph.clause) "dummystr", Rmv c (\str->(snd.cascade mph.clause) str) m)
     cascade mph (Sel c e cl m)   = ((fst.cascade mph.cl) "dummystr",     Sel c e (\str->(snd.cascade mph.cl) str)   m)
     cascade mph (Chc ds m)       = (or (map (fst.cascade mph) ds), Chc (map (snd.cascade mph) ds) m)
     cascade mph (All ds m)       = (or (map (fst.cascade mph) ds), All (map (snd.cascade mph) ds) m)
     cascade  _  (Nop m)          = (False, Nop m)
     cascade  _  (Blk m)          = (False, Blk m)

   conjuncts :: Rule -> Expressions
   conjuncts = fiRule.conjNF.normExpr
    where fiRule (Fix fis) = {- map disjuncts -} fis
          fiRule r        = [ {- disjuncts -} r]

-- The function disjuncts yields an expression which has constructor Fu in every case.
   disjuncts :: Expression -> Expression
   disjuncts = fuRule
    where fuRule (Fux cps) = (Fux . rd . map cpRule) cps
          fuRule r        = Fux [cpRule r]
          cpRule (Cpx r)   = Cpx (fRule r)
          cpRule r        = fRule r
          fRule (F ts)    = F ts
          fRule  r        = F [r]

   actSem :: InsDel -> Morphism -> Expression -> Expression
   actSem Ins m (Tm d _) | makeInline m==makeInline d = Tm m (-1)
                       | otherwise                  = Fux[Tm m (-1),Tm d (-1)]
   actSem Ins m delt   = disjNF (Fux[Tm m (-1),delt])
   actSem Del m (Tm d _) | makeInline m==makeInline d = Fix[]
                       | otherwise                  = Fix[Tm m (-1), Cpx (Tm d (-1))]
   actSem Del m delt   = conjNF (Fix[Tm m (-1),Cpx delt])
 --  actSem Del m delt = Fi[m,Cp delt]

   delta :: (Concept, Concept) -> Expression
   delta (a,b)  = Tm (makeMph (Sgn { decnm   = "Delta"
                                   , desrc   = a
                                   , detrg   = b
                                   , decprps = []
                                   , decprps_calc = []
                                   , decprL  = ""
                                   , decprM  = ""
                                   , decprR  = ""
                                   , decpopu = []
                                   , decfpos = Nowhere
                                   , decid   = 0
                                   , deciss  = True
                                   , decusr  = False
                                   , decpat  = ""
                                   , decplug = True
                                   })) (-1)

   -- | de functie doCode beschrijft de voornaamste mogelijkheden om een expressie delta' te verwerken in expr (met tOp'==Ins of tOp==Del)
-- TODO: Vind een wetenschappelijk artikel waar de hier beschreven transformatie uitputtend wordt behandeld.
-- TODO: Deze code is onvolledig en misschien zelfs fout....
   doCode :: (Morphism->Bool)        --  the morphisms that may be changed
          -> InsDel
          -> Expression              --  the expression in which a delete or insert takes place
          -> Expression              --  the delta to be inserted or deleted
          -> [(Expression,Rules )]   --  the motivation, consisting of the conjuncts (traced back to their rules) that are being restored by this code fragment.
          -> PAclause
   doCode editAble tOp' expr1 delta1 motive = doCod delta1 tOp' expr1 motive
    where
      doCod deltaX tOp exprX motiv =
        case (tOp, exprX) of
          (_ ,  Fux [])   -> Blk motiv
          (_ ,  Fix [])   -> Nop motiv
          (_ ,  F [])    -> error ("!Fatal (module Calc 366): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (F [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++").")
          (_ ,  Fdx [])   -> error ("!Fatal (module Calc 368): doCod ("++showADL deltaX++") "++show tOp++" "++showADL (Fdx [])++",\n"++
                                     "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++").")
          (_ ,  Fux [t])  -> doCod deltaX tOp t motiv
          (_ ,  Fix [t])  -> doCod deltaX tOp t motiv
          (_ ,  F [t])   -> doCod deltaX tOp t motiv
          (_ ,  Fdx [t])  -> doCod deltaX tOp t motiv
          (Ins, Cpx x)    -> doCod deltaX Del x motiv
          (Del, Cpx x)    -> doCod deltaX Ins x motiv
          (Ins, Fux fs)   -> Chc [ doCod deltaX Ins f motiv | f<-fs{-, not (f==expr1 && Ins/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Ins, Fix fs)   -> All [ doCod deltaX Ins f []    | f<-fs ] motiv
          (Ins, F ts)    -> Chc [ if F ls==flp (F rs)
                                  then Chc [ New c fLft motiv
                                           , Sel c (F ls) fLft motiv
                                           ] motiv
                                  else Chc [ New c (\x->All [fLft x, fRht x] motiv) motiv
                                           , Sel c (F ls) fLft motiv
                                           , Sel c (flp(F rs)) fRht motiv
                                           ] motiv
                                | (ls,rs)<-chop ts
                                , let c = source (F rs) `lub` target (F ls)
                                , let fLft = (\atom->doCod (disjNF (Fux[F [Tm (Mp1 atom [] c)(-1),v (c,source deltaX),deltaX],Cpx (F rs)])) Ins (F rs) [])
                                , let fRht = (\atom->doCod (disjNF (Fux[F [deltaX,v (target deltaX,c),Tm (Mp1 atom [] c)(-1)],Cpx (F ls)])) Ins (F ls) [])
                                ] motiv
          (Del, F ts)    -> Chc [ if F ls==flp (F rs)
                                  then Chc [ Sel c (disjNF (F ls)) (\_->Rmv c fLft motiv) motiv
                                           , Sel c (disjNF (F ls)) fLft motiv
                                           ] motiv
                                  else Chc [ Sel c (disjNF (Fix [F ls,flp(F rs)])) (\_->Rmv c (\x->All [fLft x, fRht x] motiv) motiv) motiv
                                           , Sel c (disjNF (Fix [F ls,flp(F rs)])) fLft motiv
                                           , Sel c (disjNF (Fix [F ls,flp(F rs)])) fRht motiv
                                           ] motiv
                                | (ls,rs)<-chop ts
                                , let c = source (F rs) `lub` target (F ls)
                                , let fLft = (\atom->doCod (disjNF (Fux[F [Tm (Mp1 atom [] c)(-1),v (c,source deltaX),deltaX],Cpx (F rs)])) Del (F rs) [])
                                , let fRht = (\atom->doCod (disjNF (Fux[F [deltaX,v (target deltaX,c),Tm (Mp1 atom [] c)(-1)],Cpx (F ls)])) Del (F ls) [])
                                ] motiv
          (Del, Fux fs)   -> All [ doCod deltaX Del f []    | f<-fs{-, not (f==expr1 && Del/=tOp') -}] motiv -- the filter prevents self compensating PA-clauses.
          (Del, Fix fs)   -> Chc [ doCod deltaX Del f motiv | f<-fs ] motiv
-- Op basis van de Morgan is de procesalgebra in het geval van (Ins, Fd ts)  afleidbaar uit uit het geval van (Del, F ts) ...
          (_  , Fdx ts)   -> doCod deltaX tOp (Cpx (F (map Cpx ts))) motiv
          (_  , K0x x)    -> doCod (deltaK0 deltaX tOp x) tOp x motiv
          (_  , K1x x)    -> doCod (deltaK1 deltaX tOp x) tOp x motiv
          (_  , Tm m _)  -> -- error ("DIAG ADL2Fspec 824:\ndoCod ("++showADL deltaX++") "++show tOp++" ("++showADL exprX++"),\n"
                                   -- -- ++"\nwith disjNF deltaX:\n "++showADL (disjNF deltaX))
                            (if editAble m then Do tOp exprX (deltaX) motiv else Blk [(Tm m (-1),rd' nr [r|(_,rs)<-motiv, r<-rs])])
          (_ , _)        -> error ("!Fatal (module Calc 827): Non-exhaustive patterns in the recursive call doCod ("++showADL deltaX++") "++show tOp++" ("++showADL exprX++"),\n"++
                                   "within function doCode "++show tOp'++" ("++showADL expr1++") ("++showADL delta1++").")

   chop :: [t] -> [([t], [t])]
   chop [_]    = []
   chop (x:xs) = ([x],xs): [(x:l, r)| (l,r)<-chop xs]
   chop []     = []

   deltaK0 :: t -> InsDel -> t1 -> t
   deltaK0 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta |- x*)
   deltaK0 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x* leeg is)
   deltaK1 :: t -> InsDel -> t1 -> t
   deltaK1 delta' Ins _ = delta'  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta |- x+)
   deltaK1 delta' Del _ = delta'  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x+ leeg is)

