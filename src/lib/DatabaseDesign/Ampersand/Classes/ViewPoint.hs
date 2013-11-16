{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.ViewPoint (Language(..),ProcessStructure(..),makeDecl) where
import DatabaseDesign.Ampersand.Core.ParseTree
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import Prelude hiding (Ord(..))
import DatabaseDesign.Ampersand.ADL1.Rule                    (rulefromProp, ruleviolations)
import DatabaseDesign.Ampersand.Classes.Relational  (Relational(multiplicities))
import DatabaseDesign.Ampersand.Classes.ConceptStructure     (ConceptStructure(..))
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Misc.Explain
import Data.List
 
fatal :: Int -> String -> a
fatal = fatalMsg "Classes.ViewPoint"

-- Language exists because there are many data structures that behave like an ontology, such as Pattern, P_Context, and Rule.
-- These data structures are accessed by means of a common set of functions (e.g. rules, declarations, etc.)

class Language a where
  objectdef :: a -> ObjectDef        -- ^ The objectdef that characterizes this viewpoint
  conceptDefs :: a -> [ConceptDef]   -- ^ all concept definitions that are valid within this viewpoint
  declarations :: a -> [Declaration] -- ^ all relations that exist in the scope of this viewpoint.
                                     --   These are user defined declarations and all generated declarations,
                                     --   i.e. one declaration for each GEN and one for each signal rule.
                                     --   Don't confuse declarations with declsUsedIn, which gives the declarations that are
                                     --   used in a.)
  udefrules :: a -> [Rule]           -- ^ all user defined rules that are maintained within this viewpoint,
                                     --   which are not multiplicity- and not identity rules.
  invariants :: a -> [Rule]          -- ^ all rules that are not maintained by users will be maintained by the computer.
                                     --   That includes multiplicity rules and identity rules, but excludes rules that are assigned to a role.
                                     -- ^ all relations used in rules must have a valid declaration in the same viewpoint.
  invariants x  = [r |r<-udefrules x, not (isSignal r)] ++ multrules x ++ identityRules x
  multrules :: a -> [Rule]           -- ^ all multiplicityrules that are maintained within this viewpoint.
  multrules x   = [rulefromProp p d |d<-declarations x, p<-multiplicities d]
  identityRules :: a -> [Rule]          -- all identity rules that are maintained within this viewpoint.
  identityRules x    = concatMap rulesFromIdentity (identities x)
  identities :: a -> [IdentityDef]      -- ^ all keys that are defined in a
  viewDefs :: a -> [ViewDef]         -- ^ all views that are defined in a
  gens :: a -> [A_Gen]               -- ^ all generalizations that are valid within this viewpoint
  patterns :: a -> [Pattern]         -- ^ all patterns that are used in this viewpoint
  
-- | In a language, a declaration must be made for each gen.
makeDecl :: A_Gen -> Declaration
makeDecl g
  = Sgn  { decnm   = name(source g) -- best result in the sql plug
         , decsgn  = sign g
         , decprps = [Uni,Tot,Inj]
         , decprps_calc = Just [Uni,Tot,Inj]
         , decprL  = ""
         , decprM  = "is a"
         , decprR  = ""
         , decMean = AMeaning 
                        [ A_Markup English ReST (string2Blocks ReST ("Every "++name (source g)++" is a " ++ name(target g)++"."))
                        , A_Markup Dutch ReST (string2Blocks ReST ("Iedere "++name (source g)++" is een " ++ name(target g)++"."))
                        ]
         , decConceptDef = Nothing
         , decfpos = origin g
         , decissX = True
         , decusrX = False
         , decISA  = True
         , decpat  = ""
         , decplug = False
         }

class ProcessStructure a where
  processes :: a -> [Process]       -- ^ all roles that are used in this ProcessStructure
  roles :: a -> [String]        -- ^ all roles that are used in this ProcessStructure
  interfaces :: a -> [Interface]     -- ^ all interfaces that are used in this ProcessStructure
  objDefs :: a -> [ObjectDef]
  processRules :: a -> [Rule]          -- ^ all process rules that are visible within this viewpoint
                                       -- ^ all relations used in rules must have a valid declaration in the same viewpoint.
  maintains :: a -> [(String,Rule)] -- ^ the string represents a Role
  mayEdit :: a -> [(String,Declaration)] -- ^ the string represents a Role
  workFromProcessRules :: [A_Gen] -> [UserDefPop] -> a -> [(Rule,Paire)]  --the violations of rules and multrules of this viewpoint
  workFromProcessRules gens' udp x = [(r,viol) |r<-processRules x, viol<-ruleviolations gens' udp r]
   
rulesFromIdentity :: IdentityDef -> [Rule]
rulesFromIdentity identity
 = [ if null (identityAts identity) then fatal 81 ("Moving into foldr1 with empty list (identityAts identity).") else
     mkKeyRule
      ( foldr1 (./\.) [  expr .:. flp expr | IdentityExp att <- identityAts identity, let expr=objctx att ]
        .|-. iExpr (idCpt identity)) ]
 {-    diamond e1 e2 = (flp e1 .\. e2) ./\. (e1 ./. flp e2)  -}
 where ruleName = "identity_" ++ name identity
       meaningEN = "Identity rule" ++ ", following from identity "++name identity
       meaningNL = "Identiteitsregel" ++ ", volgend uit identiteit "++name identity
       mkKeyRule expression =
         Ru { rrnm   = ruleName
            , rrexp  = expression
            , rrfps  = origin identity     -- position in source file
            , rrmean = AMeaning 
                         [ A_Markup English ReST (string2Blocks ReST meaningEN)
                         , A_Markup Dutch ReST (string2Blocks ReST meaningNL)
                         ]
            , rrmsg  = []
            , rrviol = Nothing
            , rrtyp  = sign expression
            , rrdcl  = Nothing        -- This rule was not generated from a property of some declaration.
            , r_env  = ""             -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
            , r_usr  = Identity            -- This rule was not specified as a rule in the Ampersand script, but has been generated by a computer
            , r_sgl  = False          -- This is not a signal rule
            , srrel  = Sgn  { decnm   = ruleName
                            , decsgn  = sign expression
                            , decprps = []
                            , decprps_calc = Nothing -- []
                            , decprL  = ""
                            , decprM  = ""
                            , decprR  = ""
                            , decMean = AMeaning 
                                          [ A_Markup English ReST (string2Blocks ReST meaningEN)
                                          , A_Markup Dutch ReST (string2Blocks ReST meaningNL)
                                          ]
                            , decConceptDef = Nothing
                            , decfpos = origin identity
                            , decissX = False
                            , decusrX = False
                            , decISA  = False
                            , decpat  = ""
                            , decplug = False
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

instance Language A_Context where
  objectdef    context = Obj { objnm   = name context
                             , objpos  = Origin "Object generated by objectdef (Language A_Context)"
                             , objctx  = iExpr ONE
                             , objmsub = Just . Box ONE $ map (objectdef) (ctxpats context)
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
                         uniteRels [] = []
                         uniteRels ds = [ d | cl<-eqClass (==) ds
                                            , let d=(head cl){ decprps      = (foldr1 uni.map decprps) cl
                                                             , decprps_calc = Nothing -- Calculation is only done in ADL2Fspc. -- was:(foldr1 uni.map decprps_calc) cl
                                                             }]
  udefrules    context = concatMap udefrules (ctxpats context) ++ concatMap udefrules (ctxprocs context) ++ ctxrs context  -- all user defined rules
--  invariants   context = [r | r<-udefrules context,  null  [role | (role, rul) <-maintains context, name r == name rul ]]   -- all user defined process rules
  identities   context = concatMap identities (ctxpats context) ++ concatMap identities (ctxprocs context) ++ ctxks context
  viewDefs     context = concatMap viewDefs (ctxpats context) ++ concatMap viewDefs (ctxprocs context) ++ ctxvs context
  gens         context = ctxgs context
  patterns             = ctxpats

instance ProcessStructure A_Context where
  processes            = ctxprocs
  roles        context = nub [r | proc<-ctxprocs context, r <- roles proc]
  interfaces           = ctxifcs
  objDefs      context = [ifcObj s | s<-ctxifcs context]
  processRules context = [r |r<-udefrules context, (not.null) [role | (role, rul) <-maintains context, name r == name rul ] ]
  maintains    context = maintains (ctxprocs context)
  mayEdit      context = mayEdit (ctxprocs context)


instance Language Process where
  objectdef    prc = Obj { objnm   = name prc
                         , objpos  = origin prc
                         , objctx  = iExpr ONE
                         , objmsub = Nothing
                         , objstrs = []
                         }
  conceptDefs proc  = nub [cd | c<-concs proc,cd<-cptdf c,posIn (prcPos proc) cd (prcEnd proc)]
  declarations proc = prcDcls proc `uni` map makeDecl (gens proc)
  udefrules         = prcRules -- all user defined rules in this process
--  invariants   proc = [r | r<-prcRules proc, not (isSignal r) ]
  identities        = prcIds
  viewDefs          = prcVds
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
                         , objctx  = iExpr ONE
                         , objmsub = Nothing
                         , objstrs = []
                         }
  conceptDefs  pat = nub [cd | c<-concs pat,cd<-cptdf c,posIn (ptpos pat) cd (ptend pat)]
  declarations pat = ptdcs pat `uni` map makeDecl (gens pat)
  udefrules        = ptrls   -- all user defined rules in this pattern
--  invariants   pat = [r |r<-ptrls pat, not (isSignal r)]
  identities       = ptids 
  viewDefs         = ptvds 
  gens             = ptgns 
  patterns     pat = [pat]

instance ProcessStructure Pattern where
  processes     _  = []
  roles         _  = []
  interfaces    _  = []
  objDefs       _  = []
  processRules pat = [r |r<-ptrls pat, isSignal r]
  maintains        = ptrruls  -- says which roles maintain which rules.
  mayEdit          = ptrrels  -- says which roles may change the population of which relation.

instance Language Rule where
  objectdef rule = Obj { objnm   = name rule
                       , objpos  = origin rule
                       , objctx  = iExpr ONE
                       , objmsub = Nothing
                       , objstrs = []
                       }
  conceptDefs  _ = []
  declarations r = [srrel r | isSignal r] -- a process rule "declares" a new relation to store violations in. That relation is "stored" in that rule. Therefore it counts as a declaration.
  udefrules    r = [r | r_usr r == UserDefined ]
--  invariants   r = [r | not (isSignal r)]
  identities   _ = []
  viewDefs     _ = []
  gens         _ = []
  patterns r     = [A_Pat{ ptnm  = "Pattern for rule "++name r
                         , ptpos = Origin "Nameless pattern generated by patterns (Language (Rule(Relation Concept))) "
                         , ptend = Origin "Nameless pattern generated by patterns (Language (Rule(Relation Concept))) "
                         , ptrls = [r]
                         , ptgns = []  -- A rule defines no Gens.
                         , ptdcs = declsUsedIn r
                         , ptups = []
                         , ptrruls = []
                         , ptrrels = []
                         , ptids = []
                         , ptvds = []
                         , ptxps = []
                         }
                   ]
