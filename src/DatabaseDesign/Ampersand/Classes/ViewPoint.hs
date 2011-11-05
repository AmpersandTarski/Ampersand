{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.ViewPoint (Language(..),ProcessStructure(..)) 
where
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.ADL1.Rule                    (rulefromProp, ruleviolations)
   import DatabaseDesign.Ampersand.ADL1.Prop                    (Prop(..))
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration  (Relational(..))
   import DatabaseDesign.Ampersand.Core.ParseTree               (Paire)
   import DatabaseDesign.Ampersand.ADL1.Expression              (flp)
   import DatabaseDesign.Ampersand.ADL1.Concept                 (cptos')
   import DatabaseDesign.Ampersand.Classes.ConceptStructure     (ConceptStructure(..))
   import DatabaseDesign.Ampersand.Basics                       (fatalMsg,Collection(..), Identified(..))
   import Data.List
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Classes.ViewPoint"

-- Language exists because there are many data structures that behave like an ontology, such as Pattern, P_Context, and Rule.
-- These data structures are accessed by means of a common set of functions (e.g. rules, declarations, etc.)

   class Language a where
     objectdef    :: a -> ObjectDef     -- ^ The objectdef that characterizes this viewpoint
     conceptDefs  :: a -> [ConceptDef]  -- ^ all concept definitions that are valid within this viewpoint
     declarations :: a -> [Declaration]  -- ^ all relations that exist in the scope of this viewpoint.
                                        -- ^ These are user defined declarations and all generated declarations,
                                        -- ^ i.e. one declaration for each GEN and one for each signal rule.
                                        -- ^ Don't confuse declarations with mors, which gives the relations that are
                                        -- ^ used in a.)
     --REMARK: declarations has been split up in two disjoints which used to be combined with `uni` instead of ++
     rules        :: a -> [Rule] -- ^ all user defined rules that are maintained within this viewpoint,
                                        --   which are not multiplicity- and not key rules.
     invariants   :: a -> [Rule] -- ^ all rules that are not maintained by users will be maintained by the computer.
                                        -- ^ all relations used in rules must have a valid declaration in the same viewpoint.
     multrules    :: a -> [Rule] -- ^ all multiplicityrules that are maintained within this viewpoint.
     multrules x   = [rulefromProp (declarations x) p d |d<-declarations x, p<-multiplicities d]
     keyrules     :: a -> [Rule] -- all key rules that are maintained within this viewpoint.
     keyrules x    = [rulefromKey k |k<-keyDefs x]
     keyDefs      :: a -> [KeyDef]      -- ^ all keys that are defined in a
     gens         :: a -> [A_Gen]       -- ^ all generalizations that are valid within this viewpoint
     patterns     :: a -> [Pattern]     -- ^ all patterns that are used in this viewpoint
     --TODO -> there are more rules than rules+multrules that can be violated
     violations   :: a -> [(Rule,Paire)] --the violations of rules and multrules of this viewpoint
     violations x = [(r,viol) |r<-invariants x++multrules x++keyrules x, viol<-ruleviolations r]

   class ProcessStructure a where
     processes    :: a -> [Process]      -- ^ all roles that are used in this ProcessStructure
     roles        :: a -> [String]      -- ^ all roles that are used in this ProcessStructure
     interfaces   :: a -> [Interface]     -- ^ all interfaces that are used in this ProcessStructure
     objDefs      :: a -> [ObjectDef]
     processRules :: a -> [Rule] -- ^ all process rules that are visible within this viewpoint
                                        -- ^ all relations used in rules must have a valid declaration in the same viewpoint.
     maintains    :: a -> [(String,Rule)]  -- ^ the string represents a Role
     mayEdit      :: a -> [(String,Relation)] -- ^ the string represents a Role
     workTBD      :: a -> [(Rule,Paire)] --the violations of rules and multrules of this viewpoint
     workTBD    x = [(r,viol) |r<-processRules x, viol<-ruleviolations r]
     
   rulefromKey :: KeyDef -> Rule
   rulefromKey key
     = Ru { rrnm        = name key
          , rrexp       = EImp (EIsc [ECps [attexpr,flp attexpr] | attexpr<-[objctx att |att<-kdats key]], ERel (I c))           -- the antecedent
          , rrfps       = origin key     -- position in source file
          , rrxpl       = []             -- explanation
          , rrtyp       = Sign c c       -- The type
          , rrdcl       = Nothing        -- This rule was not generated from a property of some declaration.
          , r_env       = ""             -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
          , r_usr       = False          -- This rule was not specified as a rule in the Ampersand script, but has been generated by a computer
          , r_sgl       = False          -- This is not a signal rule
          , srrel       = Sgn  { decnm   = name key
                               , decsgn  = Sign c c
                               , decprps = []
                               , decprps_calc = []
                               , decprL  = ""
                               , decprM  = ""
                               , decprR  = ""
                               , decMean = []
                               , decpopu = []
                               , decfpos = origin key
                               , deciss  = False
                               , decusr  = False
                               , decpat  = ""
                               , decplug = True
                               }
          }
     where
      c    = kdcpt key

{- obsolete?
   instance Language a => Language [a] where
    objectdef _      = fatal 86 "Cannot make an object from an arbitrary list"
    {- used to be:     Obj { objnm   = ""         --  view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                           , objpos  = Nowhere    --  position of this definition in the text of the Ampersand source file (filename, line number and column number)
                           , objctx  = ERel (I S)  --  this expression describes the instances of this object, related to their context. 
                           , objats  = []         -- the attributes, which are object definitions themselves.
                           , objstrs = []         -- directives that specify the interface.
                          -}
    conceptDefs xs   = (concat. map conceptDefs) xs
    declarations xs  = (nub . concat. map declarations) xs
    rules xs         = (concat . map rules) xs
    invariants xs    = (concat . map invariants) xs
    multrules xs     = (concat . map multrules) xs
    keyDefs xs       = (concat . map keyDefs) xs
    gens xs          = (nub . concat. map gens) xs
    patterns         = nubBy sameName.concat.map patterns -- TODO: nagaan waar wordt afgedwongen dat elk pattern door zijn naam identificeerbaar is.
        where sameName :: Pattern -> Pattern -> Bool
              sameName a b = name a == name b
    violations xs    = (concat. map violations) xs
-}

   instance ProcessStructure a => ProcessStructure [a] where
    processes     = concatMap processes
    roles         = concatMap roles
    interfaces    = concatMap interfaces
    objDefs       = concatMap objDefs
    processRules  = concatMap processRules
    maintains     = concatMap maintains
    mayEdit       = concatMap mayEdit
    workTBD       = concatMap workTBD

--   instance Language P_Context where
--    objectdef    context = Obj { objnm   = name context
--                               , objpos  = Origin "Object generated by objectdef (Language P_Context)"
--                               , objctx  = ERel (I ONE) 
--                               , objats  = map objectdef (ctx_pats context)++map objectdef (ctx_PPrcs context)
--                               , objstrs = []
--                               }
--    conceptDefs  context = ctx_cs context++conceptDefs (ctx_pats context)++conceptDefs (ctx_PPrcs context)
--    declarations context = declarations (ctx_pats context) `uni` declarations (ctx_PPrcs context) `uni` ctx_ds context
--    rules        context = rules (ctx_pats context) ++ rules (ctx_procs context) ++ [r | r<-ctx_rs context]  -- all user defined rules
--    invariants   context = [r | r<-rules context,  null  [role | (role, rul) <-maintains context, name r == name rul ]]   -- all user defined process rules
--    keyDefs      context = nub$keyDefs (ctx_pats context) ++ keyDefs (ctx_PPrcs context) ++ ctx_ks context -- TODO: Hoe wordt gezorgd dat de keys uniek identificeerbaar zijn?
--    gens         context = nub$gens (ctx_pats context) ++ gens (ctx_PPrcs context)
--    patterns     context = ctx_pats context

   instance Language A_Context where
    objectdef    context = Obj { objnm   = name context
                               , objpos  = Origin "Object generated by objectdef (Language A_Context)"
                               , objctx  = ERel (I ONE) 
                               , objats  = map objectdef (ctxpats context)
                               , objstrs = []
                               }
    conceptDefs  context = ctxcs context++concatMap conceptDefs (ctxpats context)
    declarations         = ctxdecls     -- all declarations, including those defined in patterns and processes.
    rules        context = concatMap rules (ctxpats context) ++ concatMap rules (ctxprocs context) ++ ctxrs context  -- all user defined rules
    invariants   context = [r | r<-rules context,  null  [role | (role, rul) <-maintains context, name r == name rul ]]   -- all user defined process rules
    keyDefs      context = nub$(concatMap keyDefs (ctxpats context)) ++ ctxks context -- TODO: Hoe wordt gezorgd dat de keys uniek identificeerbaar zijn?
    gens         context = concatMap gens (ctxpats context) `uni` ctxgs context
    patterns             = ctxpats


--   instance ProcessStructure P_Context where
--    processes    context = ctx_procs context
--    roles        context = nub [r | proc<-ctx_procs context, r <- roles proc]
--    interfaces   context = ctx_ifcs context
--    objDefs      context = [ifcObj s | s<-ctx_ifcs context]
--    processRules context = [r |r<-rules context, (not.null) [role | (role, rul) <-maintains context, name r == name rul ] ]
--    maintains    context = maintains (ctx_procs context)
--    mayEdit      context = mayEdit (ctx_procs context)

   instance ProcessStructure A_Context where
    processes            = ctxprocs
    roles        context = nub [r | proc<-ctxprocs context, r <- roles proc]
    interfaces           = ctxifcs
    objDefs      context = [ifcObj s | s<-ctxifcs context]
    processRules context = [r |r<-rules context, (not.null) [role | (role, rul) <-maintains context, name r == name rul ] ]
    maintains    context = maintains (ctxprocs context)
    mayEdit      context = mayEdit (ctxprocs context)

   instance Language Process where
    objectdef     _   = fatal 144 "objectdef undefined for processes"
    conceptDefs       = prcCds
    declarations      = prcDcls
    rules             = prcRules -- all user defined rules in this process
    invariants   proc = [r |r<-prcRules proc, not (isSignal r) ]
    keyDefs           = prcKds
    gens              = prcGens
    patterns      _   = []

--   instance Language P_Process where
--    objectdef     _   = fatal 155 "objectdef undefined for processes"
--    conceptDefs  proc = procCds proc
--    declarations proc = procDcls proc `uni` map makeDecl (procGens proc)
--                        where
--                         makeDecl g
--                          = Sgn  { decnm   = "isa"
--                                 , decsgn  = sign g
--                                 , decprps = [Uni,Tot,Inj]
--                                 , decprps_calc = []
--                                 , decprL  = ""
--                                 , decprM  = "is a"
--                                 , decprR  = ""
--                                 , decMean = ""
--                                 , decpopu = []
--                                 , decfpos = Origin "generated isa by declarations (Language Process)"
--                                 , deciss  = True
--                                 , decusr  = False
--                                 , decpat  = ""
--                                 , decplug = True
--                                 }
--    rules        proc = procRules proc -- all user defined rules in this process
--    invariants   proc = [r |r<-procRules proc, not (isSignal r) ]
--    keyDefs      proc = procKds proc
--    gens         proc = procGens proc
--    patterns      _   = []
--
--   instance ProcessStructure P_Process where
--    processes    proc = fatal 166 $ "process "++name proc++" not yet enriched."
--    roles        proc = nub ( [r | Maintain rols _ _ <- procRRuls proc, r<-rols]++
--                              [r | RR rols _ _ <- procRRels proc, r<-rols] )
--    interfaces    _   = []
--    objDefs       _   = []
--    processRules proc = [r |r<-procRules proc, isSignal r]
--    maintains    proc = fatal 175 $ "process "++name proc++" not yet enriched."
--    mayEdit      proc = fatal 175 $ "process "++name proc++" not yet enriched."

   instance ProcessStructure Process where
    processes    proc = [proc]
    roles        proc = nub ( [r | (r,_) <- prcRRuls proc]++
                              [r | (r,_) <- prcRRels proc] )
    interfaces    _   = []
    objDefs       _   = []
    processRules proc = [r |r<-prcRules proc, isSignal r]
    maintains         = prcRRuls  -- says which roles maintain which rules.
    mayEdit           = prcRRels  -- says which roles may change the population of which relation.
    
   instance Language Pattern where
    objectdef    pat = Obj { objnm   = name pat
                           , objpos  = origin pat
                           , objctx  = ERel (I ONE) 
                           , objats  = []
                           , objstrs = []
                           }
    conceptDefs      = ptcds
    declarations pat = ptdcs pat `uni` map makeDecl (ptgns pat)
                       where
                        makeDecl g
                         = Sgn  { decnm   = "isa"
                                , decsgn  = sign g
                                , decprps = [Uni,Tot,Inj]
                                , decprps_calc = []
                                , decprL  = ""
                                , decprM  = "is a"
                                , decprR  = ""
                                , decMean = []
                                , decpopu = [(a,b) | a <- cptos'(source g), b <- cptos'(target g), a==b]
                                , decfpos = Origin "generated isa by declarations (Language Pattern)"
                                , deciss  = True
                                , decusr  = False
                                , decpat  = ""
                                , decplug = True
                                }
    rules            = ptrls   -- all user defined rules in this pattern
    invariants   pat = [r |r<-ptrls pat, not (isSignal r)]
    keyDefs          = ptkds 
    gens             = ptgns 
    patterns     pat = [pat]

   instance ProcessStructure Pattern where
    processes     _  = []
    roles         _  = []
    interfaces    _  = []
    objDefs       _  = []
    processRules pat = [r |r<-ptrls pat, isSignal r]
    maintains     _  = []
    mayEdit       _  = []

   instance Language Rule where
    objectdef rule = Obj { objnm   = name rule
                         , objpos  = origin rule
                         , objctx  = ERel (I ONE) 
                         , objats  = []
                         , objstrs = []
                         }
    conceptDefs  _ = []
    declarations r = [srrel r | isSignal r]
    rules        r = [r]
    invariants   r = [r | not (isSignal r)]
    keyDefs      _ = []
    gens         _ = []
    patterns r     = [A_Pat{ ptnm  = ""
                         , ptpos = Origin "Nameless pattern generated by patterns (Language (Rule(Relation Concept))) "
                         , ptrls = [r]
                         , ptgns = [ Gen (Origin "Gen generated by patterns (Language (Rule(Relation Concept))) ") g s ""
                                   | g<-concs r, s<-concs r, g<s, null [x | x<-concs r>-[g,s], g<x, x<s]]
                         , ptdcs = [makeDeclaration rel | rel<-mors r]
                         , ptcds = []
                         , ptkds = []
                         , ptxps = []
                         }
                     ]
