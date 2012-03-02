{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.ViewPoint (Language(..),ProcessStructure(..)) where
import DatabaseDesign.Ampersand.Core.ParseTree
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Core.Poset
import Prelude hiding (Ord(..))
import DatabaseDesign.Ampersand.ADL1.Rule                    (rulefromProp, ruleviolations)
import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration  (Relational(..))
import DatabaseDesign.Ampersand.Classes.Populated            (atomsOf)
import DatabaseDesign.Ampersand.Classes.ConceptStructure     (ConceptStructure(..))
import DatabaseDesign.Ampersand.Basics                       (Collection(..), Identified(..),eqClass)
import DatabaseDesign.Ampersand.Misc.Explain
import Data.List
 
--fatal :: Int -> String -> a
--fatal = fatalMsg "Classes.ViewPoint"

-- Language exists because there are many data structures that behave like an ontology, such as Pattern, P_Context, and Rule.
-- These data structures are accessed by means of a common set of functions (e.g. rules, declarations, etc.)

class Language a where
  objectdef    :: a -> ObjectDef     -- ^ The objectdef that characterizes this viewpoint
  conceptDefs  :: a -> [ConceptDef]  -- ^ all concept definitions that are valid within this viewpoint
  declarations :: a -> [Declaration] -- ^ all relations that exist in the scope of this viewpoint.
                                     -- ^ These are user defined declarations and all generated declarations,
                                     -- ^ i.e. one declaration for each GEN and one for each signal rule.
                                     -- ^ Don't confuse declarations with mors, which gives the relations that are
                                     -- ^ used in a.)
  rules        :: a -> [Rule] -- ^ all user defined rules that are maintained within this viewpoint,
                                     --   which are not multiplicity- and not key rules.
  invariants   :: a -> [Rule] -- ^ all rules that are not maintained by users will be maintained by the computer.
                                     -- ^ all relations used in rules must have a valid declaration in the same viewpoint.
  multrules    :: a -> [Rule] -- ^ all multiplicityrules that are maintained within this viewpoint.
  multrules x   = [rulefromProp (declarations x) p d |d<-declarations x, p<-multiplicities d]
  keyrules     :: a -> [Rule] -- all key rules that are maintained within this viewpoint.
  keyrules x    = concatMap rulesFromKey $ keyDefs x
  keyDefs      :: a -> [KeyDef]      -- ^ all keys that are defined in a
  gens         :: a -> [A_Gen]       -- ^ all generalizations that are valid within this viewpoint
  patterns     :: a -> [Pattern]     -- ^ all patterns that are used in this viewpoint
  --TODO -> there are more rules than rules+multrules that can be violated
  violations   :: a -> [(Rule,Paire)] --the violations of rules and multrules of this viewpoint
  violations x = [(r,viol) |r<-invariants x++multrules x++keyrules x, viol<-ruleviolations r]
  cExperimental :: a -> Bool 
  cExperimental _ = False
  
-- | In a language, a declaration must be made for each gen.
makeDecl :: A_Gen -> Declaration
makeDecl g
  = Sgn  { decnm   = name(source g) -- best result in the sql plug
         , decsgn  = sign g
         , decprps = [Uni,Tot,Inj]
         , decprps_calc = [Uni,Tot,Inj]
         , decprL  = ""
         , decprM  = "is a"
         , decprR  = ""
         , decMean = AMeaning 
                        [ A_Markup English ReST (string2Blocks ReST ("Every "++name (source g)++" must be a " ++ name(target g)++"."))
                        , A_Markup Dutch ReST (string2Blocks ReST ("Iedere "++name (source g)++" moet een " ++ name(target g)++" zijn."))
                        ]
         , decSrcDef = ""
         , decTgtDef = ""
         , decpopu = [(a,b) | a <- (atomsOf.source) g, b <- (atomsOf.target) g, a==b]
         , decfpos = origin g
         , deciss  = True
         , decusr  = False
         , decpat  = ""
         , decplug = False
         }

class ProcessStructure a where
  processes    :: a -> [Process]       -- ^ all roles that are used in this ProcessStructure
  roles        :: a -> [String]        -- ^ all roles that are used in this ProcessStructure
  interfaces   :: a -> [Interface]     -- ^ all interfaces that are used in this ProcessStructure
  objDefs      :: a -> [ObjectDef]
  processRules :: a -> [Rule]          -- ^ all process rules that are visible within this viewpoint
                                       -- ^ all relations used in rules must have a valid declaration in the same viewpoint.
  maintains    :: a -> [(String,Rule)] -- ^ the string represents a Role
  mayEdit      :: a -> [(String,Relation)] -- ^ the string represents a Role
  workTBD      :: a -> [(Rule,Paire)]  --the violations of rules and multrules of this viewpoint
  workTBD    x = [(r,viol) |r<-processRules x, viol<-ruleviolations r]
   
rulesFromKey :: KeyDef -> [Rule]
rulesFromKey key = mkProductInjectivityRule keyExps : 
                   --mkProductTotalityRule keyExps :
                   map mkUnivalenceRule keyExps  

 where keyExps = [ (objnm att, objctx att) | KeyExp att <- kdats key ]
 
       mkProductInjectivityRule exprs = mkKeyRule "Diamond" "Diamond rule" "Diamantregel" $ 
         EImp (EIsc [ diamond expr $ EFlp expr | (_,expr) <- exprs ] , ERel (I $ kdcpt key))
       
       diamond e1 e2 = EIsc [ ERad [ECpl e1,e2], ERad [e1, ECpl e2] ]
       
       --mkProductTotalityRule exprs = mkKeyRule "PrTot" "Product totality" "Product-totaliteit" $
       --  EImp (ERel (I $ kdcpt key), EUni [ECps [expr, EFlp expr] | (_,expr) <- exprs ]) 

       mkUnivalenceRule (lbl, expr) = mkKeyRule ("Uni"++lbl) "Univalence" "Univalentie" $ EImp (ECps [EFlp expr, expr], ERel (I $ target expr)) 
 
       -- This was taken from old rulefromKey. Even after cleaning up a bit, it's still quite messy.
       mkKeyRule abbr propertyEN propertyNL expression =
         let ruleName = "key" ++ "_" ++ name key ++ "_" ++ abbr
             meaningEN = propertyEN ++ ", following from key declaration "++name key
             meaningNL = propertyNL ++ ", volgend uit key-declaratie "++name key
         in  Ru { rrnm        = ruleName
                , rrexp       = expression
                , rrfps       = origin key     -- position in source file
                , rrmean      = AMeaning 
                                  [ A_Markup English ReST (string2Blocks ReST meaningEN)
                                  , A_Markup Dutch ReST (string2Blocks ReST meaningNL)
                                  ]
                , rrmsg       = []
                , rrviol      = Nothing
                , rrtyp       = sign expression
                , rrdcl       = Nothing        -- This rule was not generated from a property of some declaration.
                , r_env       = ""             -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
                , r_usr       = False          -- This rule was not specified as a rule in the Ampersand script, but has been generated by a computer
                , r_sgl       = False          -- This is not a signal rule
                , srrel       = Sgn  { decnm   = ruleName
                                     , decsgn  = sign expression
                                     , decprps = []
                                     , decprps_calc = []
                                     , decprL  = ""
                                     , decprM  = ""
                                     , decprR  = ""
                                     , decMean = AMeaning 
                                                   [ A_Markup English ReST (string2Blocks ReST meaningEN)
                                                   , A_Markup Dutch ReST (string2Blocks ReST meaningNL)
                                                   ]
                                     , decSrcDef = ""
                                     , decTgtDef = ""
                                     , decpopu = []
                                     , decfpos = origin key
                                     , deciss  = False
                                     , decusr  = False
                                     , decpat  = ""
                                     , decplug = True
                                     }
                }

instance ProcessStructure a => ProcessStructure [a] where
  processes     = concatMap processes
  roles         = concatMap roles
  interfaces    = concatMap interfaces
  objDefs       = concatMap objDefs
  processRules  = concatMap processRules
  maintains     = concatMap maintains
  mayEdit       = concatMap mayEdit
  workTBD       = concatMap workTBD

instance Language A_Context where
  objectdef    context = Obj { objnm   = name context
                             , objpos  = Origin "Object generated by objectdef (Language A_Context)"
                             , objctx  = ERel (I ONE) 
                             , objmsub = Just . Box $ map (objectdef) (ctxpats context)
                             , objstrs = []
                             }
  conceptDefs          = ctxcds
  declarations context = uniteRels (concatMap declarations (patterns context)
                                 ++ concatMap declarations (processes context)
                                 ++ ctxds context
                                 ++ map makeDecl (gens context))
                         where
                         -- declarations with the same name, but different properties (decprps,pragma,decpopu,etc.) may exist and need to be united
                         -- decpopu, decprps and decprps_calc are united, all others are taken from the head.
                         uniteRels :: [Declaration] -> [Declaration]
                         uniteRels ds = [ d | cl<-eqClass (==) ds
                                            , let d=(head cl){ decprps      = (foldr1 uni.map decprps) cl
                                                             , decprps_calc = (foldr1 uni.map decprps_calc) cl
                                                             , decpopu      = (foldr1 uni.map decpopu) cl
                                                             }]
  rules        context = concatMap rules (ctxpats context) ++ concatMap rules (ctxprocs context) ++ ctxrs context  -- all user defined rules
  invariants   context = [r | r<-rules context,  null  [role | (role, rul) <-maintains context, name r == name rul ]]   -- all user defined process rules
  keyDefs      context = nub$(concatMap keyDefs (ctxpats context)) ++ ctxks context -- TODO: Hoe wordt gezorgd dat de keys uniek identificeerbaar zijn?
  gens         context = concatMap gens (ctxpats context) `uni` ctxgs context
  patterns             = ctxpats
  cExperimental        = ctxexperimental

instance ProcessStructure A_Context where
  processes            = ctxprocs
  roles        context = nub [r | proc<-ctxprocs context, r <- roles proc]
  interfaces           = ctxifcs
  objDefs      context = [ifcObj s | s<-ctxifcs context]
  processRules context = [r |r<-rules context, (not.null) [role | (role, rul) <-maintains context, name r == name rul ] ]
  maintains    context = maintains (ctxprocs context)
  mayEdit      context = mayEdit (ctxprocs context)


instance Language Process where
  objectdef    prc = Obj { objnm   = name prc
                         , objpos  = origin prc
                         , objctx  = ERel (I ONE) 
                         , objmsub = Nothing
                         , objstrs = []
                         }
  conceptDefs proc  = nub [cd | c<-concs proc,cd<-cptdf c,posIn (prcPos proc) cd (prcEnd proc)]
  declarations proc = prcDcls proc `uni` map makeDecl (gens proc)
  rules             = prcRules -- all user defined rules in this process
  invariants   proc = [r | r<-prcRules proc, not (isSignal r) ]
  keyDefs           = prcKds
  gens              = prcGens
  patterns      _   = []

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
                         , objmsub = Nothing
                         , objstrs = []
                         }
  conceptDefs  pat = nub [cd | c<-concs pat,cd<-cptdf c,posIn (ptpos pat) cd (ptend pat)]
  declarations pat = ptdcs pat `uni` map makeDecl (gens pat)
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
                       , objmsub = Nothing
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
                       , ptend = Origin "Nameless pattern generated by patterns (Language (Rule(Relation Concept))) "
                       , ptrls = [r]
                       , ptgns = [ Gen (Origin "Gen generated by patterns (Language (Rule(Relation Concept))) ") g s ""
                                 | g<-concs r, s<-concs r, g<s, null [x | x<-concs r>-[g,s], g<x, x<s]]
                       , ptdcs = [makeDeclaration rel | rel<-mors r]
                       , ptkds = []
                       , ptxps = []
                       }
                   ]
