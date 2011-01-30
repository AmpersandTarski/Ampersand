{-# OPTIONS_GHC -Wall -XFlexibleContexts -XFlexibleInstances #-}
module DatabaseDesign.Ampersand.Classes.ViewPoint (ViewPoint(..)) 
where
   import DatabaseDesign.Ampersand.ADL1.Context                 (Context(..))
   import DatabaseDesign.Ampersand.ADL1.Pattern                 (Pattern(..),Patterns)
   import DatabaseDesign.Ampersand.ADL1.Gen                     (Gen(..),Gens)
   import DatabaseDesign.Ampersand.ADL1.Rule                    (Rule(..),RuleType(..),rulefromProp, ruleviolations,Rules)
   import DatabaseDesign.Ampersand.ADL1.Prop                    (Prop(..))
   import DatabaseDesign.Ampersand.ADL1.ObjectDef               (Service(..),ObjectDef(..),ObjectDefs)
   import DatabaseDesign.Ampersand.ADL1.KeyDef                  (KeyDef(..),KeyDefs)
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration  (Relation, Declaration,Association(..),Relational(..),mIs,Declaration(..),makeDeclaration)
   import DatabaseDesign.Ampersand.ADL1.Concept                 (Concept(..),Signaling(..))
   import DatabaseDesign.Ampersand.ADL1.ConceptDef              (ConceptDefs)
   import DatabaseDesign.Ampersand.ADL1.Expression              (Expression(..))
   import DatabaseDesign.Ampersand.ADL1.Pair                    (Paire)
   import DatabaseDesign.Ampersand.Input.ADL1.FilePos                 (Numbered(..),FilePos(..))
   import DatabaseDesign.Ampersand.Classes.ConceptStructure                 (ConceptStructure(..))
   import DatabaseDesign.Ampersand.Core.Basics                 (Collection(..), Identified(..))
   import DatabaseDesign.Ampersand.Core.Basics                 (Inheritance(..))

-- ViewPoint exists because there are many data structures that behave like an ontology, such as Pattern, Context, and Rule.
-- These data structures are accessed by means of a common set of functions (e.g. rules, signals, declarations, etc.)

   class ConceptStructure a Concept => ViewPoint a where
     objectdef    :: a -> ObjectDef     -- ^ The objectdef that characterizes this viewpoint
     conceptDefs  :: a -> ConceptDefs   -- ^ all concept definitions that are valid within this viewpoint
     declarations :: a -> [Declaration Concept]  -- ^ all relations that exist in the scope of this viewpoint.
                                        -- ^ These are user defined declarations and all generated declarations,
                                        -- ^ i.e. one declaration for each GEN and one for each signal rule.
                                        -- ^ Don't confuse declarations with mors, which gives the relations that are
                                        -- ^ used in a.)
     --REMARK: declarations has been split up in two disjoints which used to be combined with `uni` instead of ++
     rules        :: a -> Rules (Relation Concept) -- ^ all rules that are maintained within this viewpoint,
                                        --   which are not signal-, not multiplicity-, and not key rules.
     signals      :: a -> Rules (Relation Concept) -- ^ all signals that are visible within this viewpoint
                                        -- ^ all relations used in signals and rules must have a valid declaration in the same viewpoint.
     multrules    :: a -> Rules (Relation Concept) -- ^ all multiplicityrules that are maintained within this viewpoint.
     multrules x   = [rulefromProp p d |d<-declarations x, p<-multiplicities d]
     keyrules     :: a -> Rules (Relation Concept) -- all key rules that are maintained within this viewpoint.
     keyrules x    = [rulefromKey k |k<-keyDefs x]
     objDefs      :: a -> ObjectDefs
     keyDefs      :: a -> KeyDefs       -- ^ all keys that are defined in a
     gens         :: a -> Gens Concept  -- ^ all generalizations that are valid within this viewpoint
     patterns     :: a -> Patterns      -- ^ all patterns that are used in this viewpoint
     isa          :: a -> Inheritance Concept
     --TODO -> there are more rules than rules+multrules that can be violated
     violations   :: a -> [(Rule (Relation Concept),Paire)] --the violations of rules and multrules of this viewpoint
     violations x = [(r,viol) |r<-rules x++multrules x, viol<-ruleviolations r]
     
   rulefromKey :: KeyDef -> Rule (Relation Concept)
   rulefromKey key
     = Ru
          Implication    -- Implication of Equivalence
          antc           -- the antecedent
          (pos key)      -- position in source file
          cons           -- right hand side (consequent)
          []             -- explanation
          (c,c)          -- The type
          Nothing        -- inference tree
          Nothing        -- This rule was not generated from a property of some declaration.
          0              -- Rule number. Will be assigned after enriching the context
          ""             -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
          False          -- This rule was not specified as a rule in the Ampersand script, but has been generated by a computer
          False          -- This is not a signal rule
          (Sgn (name key) c c [] [] "" "" "" [] (pos key) 0 False False "" True)         
     where
      c    = kdcpt key
      antc = Fix [F [attexpr,flp attexpr]| attexpr<-[objctx att|att<-kdats key]]
      cons = Tm (mIs c)(-1)
   
   instance ViewPoint a => ViewPoint [a] where
    objectdef _      = error ("!Fatal (module ViewPoint 73): Cannot make an object from an arbitrary list")
    {- used to be:     Obj { objnm   = ""         --  view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                           , objpos  = Nowhere    --  position of this definition in the text of the Ampersand source file (filename, line number and column number)
                           , objctx  = Tm (mIs S) (-1) --  this expression describes the instances of this object, related to their context. 
                           , objctx_proof = Nothing
                           , objats  = []         -- the attributes, which are object definitions themselves.
                           , objstrs = []         -- directives that specify the interface.
                          -}
    conceptDefs xs   = (concat. map conceptDefs) xs
    declarations xs  = (rd . concat. map declarations) xs
    rules xs         = (concat . map rules) xs
    signals xs       = (concat . map signals) xs
    multrules xs     = (concat . map multrules) xs
    objDefs xs       = (concat . map objDefs) xs
    keyDefs xs       = (concat . map keyDefs) xs
    gens xs          = (rd . concat. map gens) xs
    patterns         = rd' name.concat.map patterns -- TODO: nagaan waar wordt afgedwongen dat elk pattern door zijn naam identificeerbaar is.
    isa              = foldr uni empty.map isa
    violations xs    = (concat. map violations) xs

   instance ViewPoint Context where
    objectdef    context = Obj { objnm   = name context
                               , objpos  = Nowhere
                               , objctx  = Tm (mIs S) (-1)
                               , objctx_proof = Nothing
                               , objats  = map objectdef (ctxpats context)
                               , objstrs = []
                               }
    conceptDefs  context = ctxcs context++conceptDefs (ctxpats context)
    declarations context = declarations (ctxpats context) `uni` ctxds context
    rules        context = rules   (ctxpats context) ++ [r| r<-ctxrs context, not (isSignal r)]  -- all user defined rules
    signals      context = signals (ctxpats context) ++ [r| r<-ctxrs context,      isSignal r]   -- all user defined signals
    objDefs      context = [svObj s| s<-ctxsvcs context]
    keyDefs      context = rd$keyDefs (ctxpats context) ++ ctxks context -- TODO: Hoe wordt gezorgd dat de keys uniek identificeerbaar zijn?
    gens         context = gens (ctxpats context)
    patterns     context = ctxpats context
    isa          context = ctxisa  context

   instance ViewPoint Pattern where
    objectdef    pat = Obj { objnm   = name pat
                           , objpos  = Nowhere
                           , objctx  = Tm (mIs S) (-1)
                           , objctx_proof = Nothing
                           , objats  = []
                           , objstrs = []
                           }
    conceptDefs  pat = ptcds pat
    declarations pat = ptdcs pat `uni` map makeDecl (ptgns pat)
                       where
                        makeDecl g
                         = Sgn  { decnm   = "isa"
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
                                }
    rules        pat = [r|r<-ptrls pat, not (isSignal r)]
    signals      pat = [r|r<-ptrls pat,      isSignal r ]
    objDefs       _  = []
    keyDefs      pat = ptkds pat
    gens         pat = ptgns pat
    patterns     pat = [pat]
    isa          pat = Isa ts (concs pat>-[c| g<-ptgns pat,c<-[gengen g,genspc g]])
                       where ts = clear [(gengen g,genspc g)| g<-ptgns pat]


   instance ViewPoint (Rule (Relation Concept)) where
    objectdef rule = Obj { objnm   = name rule
                         , objpos  = pos rule
                         , objctx  = Tm (mIs S) (-1)
                         , objctx_proof = Nothing
                         , objats  = []
                         , objstrs = []
                         }
    conceptDefs  _ = []
    declarations r = [srrel r| isSignal r]
    rules        r = [r| not (isSignal r)]
    signals      r = [r| isSignal r]
    objDefs      _ = []
    keyDefs      _ = []
    gens         _ = []
    patterns r = [Pat{ ptnm  = ""
                     , ptrls = [r]
                     , ptgns = [G Nowhere g s ""|g<-concs r, s<-concs r, g<s, null [x| x<-concs r>-[g,s], g<x, x<s]]
                     , ptdcs = [makeDeclaration m| m<-mors r]
                     , ptcds = []
                     , ptkds = []
                     , ptxps = []
                     , testexpr = error ("!Fatal (module ViewPoint 171): Pattern without a defined testexpr") 
                     , inftestexpr = error ("!Fatal (module ViewPoint 172): Pattern without a defined inftestexpr") 
                     }
                 ]
    isa r      = Isa ts (concs r>-[c| (g,s)<-ts,c<-[g,s]])
                 where ts = [(g,s)| g<-concs r, s<-concs r, g<s, null [c|c<-concs r, g<c, c<s]]
-- was    isa r = empty

   clear :: [(Concept,Concept)] -> [(Concept,Concept)]
   clear abs' = rd [(a,b)| (a,b)<-abs', a/=b]

