{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE LambdaCase #-}
-- unfortunately not in GHC yet, try to add this line when at GHC 8.0: {-# ApplicativeDo #-}
module Database.Design.Ampersand.ADL1.P2A_Converters (pCtx2aCtx,pCpt2aCpt)
where
import Database.Design.Ampersand.ADL1.Disambiguate
import Database.Design.Ampersand.Core.ParseTree -- (P_Context(..), A_Context(..))
import Database.Design.Ampersand.Input.ADL1.CtxError
import Database.Design.Ampersand.ADL1.Lattices -- used for type-checking
import Database.Design.Ampersand.Core.AbstractSyntaxTree hiding (sortWith, maxima, greatest)
import Database.Design.Ampersand.Classes.ViewPoint
import Database.Design.Ampersand.Classes.ConceptStructure
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Control.Monad (join)
import Prelude hiding (sequence, mapM)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Foldable (toList)
import Data.Function
import Data.Maybe
import Data.List(nub)
import Data.Char(toUpper,toLower)
import Data.Either

data Type = UserConcept String
          | BuiltIn TType
          | RepresentSeparator
          deriving (Eq,Ord,Show)

type DeclMap = Map.Map String (Map.Map SignOrd Declaration)

instance Named Type where
  name v = case typeOrConcept v of
                Right (Just x) -> "Built-in type "++show x
                Right Nothing  -> "The Generic Built-in type"
                Left  x -> "Concept "++name x

typeOrConcept :: Type -> Either A_Concept (Maybe TType)
typeOrConcept (BuiltIn TypeOfOne)  = Left$ ONE
typeOrConcept (UserConcept s)      = Left$ PlainConcept s
typeOrConcept (BuiltIn x)          = Right (Just x)
typeOrConcept RepresentSeparator = Right Nothing

pConcToType :: P_Concept -> Type
pConcToType P_Singleton = BuiltIn TypeOfOne
pConcToType p = UserConcept (name p)
aConcToType :: A_Concept -> Type
aConcToType ONE = BuiltIn TypeOfOne
aConcToType p = UserConcept (name p)

getAsType :: Origin -> Type -> Guarded (Maybe TType)
getAsType o v = case typeOrConcept v of
                  Right x -> return x
                  Left  x -> unexpectedType o x
getAsConcept :: Origin -> Type -> Guarded A_Concept
getAsConcept o v = case typeOrConcept v of
                     Right x -> unexpectedType o x
                     Left  x -> return x

mustBeConceptBecauseMath :: Type -> A_Concept
mustBeConceptBecauseMath tp
 = let fatalV = fatal 54 "A concept turned out to be a built-in type."
   in case getAsConcept fatalV tp of
        Checked v -> v
        _ -> fatalV

newtype SignOrd = SignOrd Signature
instance Ord SignOrd where
  compare (SignOrd (Sign a b)) (SignOrd (Sign c d)) = compare (name a,name b) (name c,name d)
instance Eq SignOrd where
  (==) (SignOrd (Sign a b)) (SignOrd (Sign c d)) = (name a,name b) == (name c,name d)

-- pCtx2aCtx has three tasks:
-- 1) Disambiguate the structures.
--    Disambiguation means replacing every "TermPrim" (the parsed expression) with the correct Expression (available through DisambPrim)
--    This is done by using the function "disambiguate" on the outer-most structure.
--    In order to do this, its data type must be polymorphic, as in "P_ViewSegmt a".
--    After parsing, the type has TermPrim for the type variable. In our example: "P_ViewSegmt TermPrim". Note that "type P_ViewSegment = P_ViewSegmt TermPrim".
--    After disambiguation, the type variable is (TermPrim, DisambPrim), as in "P_ViewSegmt (TermPrim, DisambPrim)"
-- 2) Typecheck the structures.
--    This changes the data-structure entirely, changing the P_ into the A_
--    A "Guarded" will be added on the outside, in order to catch both type errors and disambiguation errors.
--    Using the Applicative operations <$> and <*> causes these errors to be in parallel
-- 3) Check everything else on the A_-structure: interface references should not be cyclic, rules e.a. must have unique names, etc.
-- Part 3 is done below, the other two are done in pCtx2aCtx'


-- NOTE: Static checks like checkPurposes should ideally occur on the P-structure before type-checking, as it makes little
-- sense to do type checking when there are static errors. However, in Ampersand all collect functions (e.g. in ViewPoint)
-- only exist on the A-Structure, so we do it afterwards. Static purpose errors won't affect types, so in this case it is no problem. 

-- Check whether all purposes refer to existing objects.
checkPurposes :: A_Context -> Guarded ()
checkPurposes ctx = let topLevelPurposes = ctxps ctx
                        purposesInPatterns = concatMap ptxps (ctxpats ctx)
                        allPurposes = topLevelPurposes ++ purposesInPatterns
                        danglingPurposes = filter (isDanglingPurpose ctx) allPurposes
                    in  if null danglingPurposes then pure () else Errors $ map mkDanglingPurposeError danglingPurposes

-- Return True if the ExplObj in this Purpose does not exist.
isDanglingPurpose :: A_Context -> Purpose -> Bool
isDanglingPurpose ctx purp = 
  case explObj purp of
    ExplConceptDef concDef -> let nm = name concDef in nm `notElem` map name (concs ctx )
    ExplDeclaration decl -> let nm = name decl in nm `notElem` map name (relsDefdIn ctx) -- is already covered by type checker
    ExplRule nm -> nm `notElem` map name (udefrules ctx) 
    ExplIdentityDef nm -> nm `notElem` map name (identities ctx)
    ExplViewDef nm ->  nm `notElem` map name (viewDefs ctx)
    ExplPattern nm -> nm `notElem` map name (ctxpats ctx)
    ExplInterface nm -> nm `notElem` map name (ctxifcs ctx)
    ExplContext nm -> ctxnm ctx /= nm 
                         && False -- HJO: This line is a workaround for the issue mentioned in https://github.com/AmpersandTarski/ampersand/issues/46
                                  -- TODO: fix this when we pick up working on multiple contexts.
-- Check that interface references are not cyclic
checkInterfaceCycles :: A_Context -> Guarded ()
checkInterfaceCycles ctx = if null interfaceCycles then return () else Errors $  map mkInterfaceRefCycleError interfaceCycles
      where interfaceCycles = [ map lookupInterface iCycle | iCycle <- getCycles refsPerInterface ]
            refsPerInterface = [(name ifc, getDeepIfcRefs $ ifcObj ifc) | ifc <- ctxifcs ctx ]
            getDeepIfcRefs obj = case objmsub obj of
                                   Nothing                  -> []
                                   Just (InterfaceRef isLinkto nm _) -> [nm | not isLinkto]
                                   Just (Box _ _ objs)      -> concatMap getDeepIfcRefs objs
            lookupInterface nm = case [ ifc | ifc <- ctxifcs ctx, name ifc == nm ] of
                                   [ifc] -> ifc
                                   _     -> fatal 124 "Interface lookup returned zero or more than one result"

-- Check whether each concept has at most one default view
checkMultipleDefaultViews :: A_Context -> Guarded ()
checkMultipleDefaultViews ctx = let conceptsWithMultipleViews = [ (c,vds)| vds@(Vd{vdcpt=c}:_:_) <- eqClass ((==) `on` vdcpt) $ filter vdIsDefault (ctxvs ctx) ]
                                in  if null conceptsWithMultipleViews then return () else Errors $ map mkMultipleDefaultError conceptsWithMultipleViews

checkDanglingRulesInRuleRoles :: A_Context -> Guarded ()
checkDanglingRulesInRuleRoles ctx = case [mkDanglingRefError "Rule" nm (arPos rr)  
                                         | rr <- ctxrrules ctx
                                         , nm <- arRules rr
                                         , nm `notElem` map name (allRules ctx)
                                         ] of
                                      [] -> return ()
                                      errs -> Errors errs
checkOtherAtomsInSessionConcept :: A_Context -> Guarded ()
checkOtherAtomsInSessionConcept ctx = case [mkOtherAtomInSessionError atom
                                           | pop@ACptPopu{popcpt =cpt} <- ctxpopus ctx
                                           , name cpt == "SESSION"
                                           , atom <- filter (not.isPermittedSessionValue) (popas pop)
                                           ] of
                                        [] -> return ()
                                        errs -> Errors errs
        where isPermittedSessionValue :: AAtomValue -> Bool
              isPermittedSessionValue (AAVString _ str) = str == "_SESSION"
              isPermittedSessionValue _                 = False

pSign2aSign :: P_Sign -> Signature
pSign2aSign (P_Sign src tgt) = Sign (pCpt2aCpt src) (pCpt2aCpt tgt)
findDecls :: DeclMap -> String -> Map.Map SignOrd Declaration
findDecls declMap x = Map.findWithDefault Map.empty x declMap  -- get all declarations with the same name as x
namedRel2Decl :: DeclMap -> P_NamedRel -> Guarded Declaration
namedRel2Decl declMap o@(PNamedRel _ r Nothing)  = getOneExactly o (findDecls' declMap r)
namedRel2Decl declMap o@(PNamedRel _ r (Just s)) = getOneExactly o (findDeclsTyped declMap r (pSign2aSign s))
findDecls' :: DeclMap -> String -> [Declaration]
findDecls' declMap x = Map.elems (findDecls declMap  x)
findDeclsLooselyTyped :: DeclMap -> String -> Maybe A_Concept -> Maybe A_Concept -> [Declaration]
findDeclsLooselyTyped declMap x (Just src) (Just tgt)
 = findDeclsTyped declMap x (Sign src tgt)
   `orWhenEmpty` (findDeclsLooselyTyped declMap x (Just src) Nothing `isct` findDeclsLooselyTyped declMap x Nothing (Just tgt))
   `orWhenEmpty` (findDeclsLooselyTyped declMap x (Just src) Nothing `unin` findDeclsLooselyTyped declMap x Nothing (Just tgt))
   `orWhenEmpty` findDecls' declMap x
 where isct lsta lstb = [a | a<-lsta, a `elem` lstb]
       unin lsta lstb = nub (lsta ++ lstb)
findDeclsLooselyTyped declMap x Nothing Nothing = findDecls' declMap x
findDeclsLooselyTyped declMap x (Just src) Nothing
 = [dcl | dcl <- findDecls' declMap x, name (source dcl) == name src ]
   `orWhenEmpty` findDecls' declMap x
findDeclsLooselyTyped declMap x Nothing (Just tgt)
 = [dcl | dcl <- findDecls' declMap x, name (target dcl) == name tgt ]
   `orWhenEmpty` findDecls' declMap x
findDeclLooselyTyped :: DeclMap
                     -> P_Population
                     -> String
                     -> Maybe A_Concept
                     -> Maybe A_Concept
                     -> Guarded Declaration
findDeclLooselyTyped declMap o x src tgt = getOneExactly o (findDeclsLooselyTyped declMap x src tgt)
findDeclsTyped :: DeclMap -> String -> Signature -> [Declaration]
findDeclsTyped declMap x tp = Map.findWithDefault [] (SignOrd tp) (Map.map (:[]) (findDecls declMap x))

onlyUserConcepts :: [[Type]] -> [[A_Concept]]
onlyUserConcepts [] = []
onlyUserConcepts (lst:r)
  = case lefts (map typeOrConcept lst) of
     [] -> onlyUserConcepts r
     v -> v:onlyUserConcepts r

pCtx2aCtx :: Options -> P_Context -> Guarded A_Context
pCtx2aCtx _
 PCtx { ctx_nm     = n1
      , ctx_pos    = n2
      , ctx_lang   = lang
      , ctx_markup = pandocf
      , ctx_thms   = p_themes 
      , ctx_pats   = p_patterns
      , ctx_rs     = p_rules    
      , ctx_ds     = p_declarations
      , ctx_cs     = p_conceptdefs
      , ctx_ks     = p_identdefs
      , ctx_rrules = p_roleRules
      , ctx_rrels  = p_roleRelations
      , ctx_reprs  = p_representations
      , ctx_vs     = p_viewdefs
      , ctx_gs     = p_gens
      , ctx_ifcs   = p_interfaces
      , ctx_ps     = p_purposes
      , ctx_pops   = p_pops
      , ctx_sql    = p_sqldefs
      , ctx_php    = p_phpdefs
      , ctx_metas  = p_metas
      }
 = do contextInfo <- g_contextInfo
      decls       <- (\declsWithPops
                        -> map fst declsWithPops -- ++ concatMap ptdcs pats
                      ) <$> traverse (pDecl2aDecl n1 contextInfo deflangCtxt deffrmtCtxt) (p_declarations ++ concatMap pt_dcs p_patterns)
      let declMap = Map.map groupOnTp (Map.fromListWith (++) [(name d,[d]) | d <- decls])
            where groupOnTp lst = Map.fromListWith accumDecl [(SignOrd$ sign d,d) | d <- lst]
      pats        <- traverse (pPat2aPat declMap contextInfo) p_patterns            --  The patterns defined in this context
      rules       <- traverse (pRul2aRul declMap n1) p_rules       --  All user defined rules in this context, but outside patterns
      identdefs   <- traverse (pIdentity2aIdentity declMap) p_identdefs --  The identity definitions defined in this context, outside the scope of patterns
      viewdefs    <- traverse (pViewDef2aViewDef declMap) p_viewdefs    --  The view definitions defined in this context, outside the scope of patterns
      interfaces  <- traverse (pIfc2aIfc declMap) (p_interfaceAndDisambObjs declMap)   --  TODO: explain   ... The interfaces defined in this context, outside the scope of patterns
      purposes    <- traverse (pPurp2aPurp declMap) p_purposes          --  The purposes of objects defined in this context, outside the scope of patterns
      udpops      <- traverse (pPop2aPop declMap contextInfo) p_pops --  [Population]
      sqldefs     <- traverse (pObjDef2aObjDef declMap) p_sqldefs       --  user defined sqlplugs, taken from the Ampersand script 
      phpdefs     <- traverse (pObjDef2aObjDef declMap) p_phpdefs       --  user defined phpplugs, taken from the Ampersand script 
      allRoleRelations <- traverse (pRoleRelation2aRoleRelation declMap) (p_roleRelations ++ concatMap pt_RRels p_patterns)
      declsAndPops <- traverse (pDecl2aDecl n1 contextInfo deflangCtxt deffrmtCtxt) p_declarations
      let allConcs = Set.fromList (map (aConcToType . source) decls ++ map (aConcToType . target) decls)  :: Set.Set Type
      let soloConcs = filter (not . isInSystem genLattice) (Set.toList allConcs) :: [Type]
      let actx = ACtx{ ctxnm = n1
                     , ctxpos = n2
                     , ctxlang = deflangCtxt
                     , ctxmarkup = deffrmtCtxt
                     , ctxthms = p_themes
                     , ctxpats = pats
                     , ctxrs = rules
                     , ctxds = map fst declsAndPops
                     , ctxpopus = nub (udpops
                                     ++map snd declsAndPops
                                     ++mp1Pops contextInfo rules
                                     ++mp1Pops contextInfo pats
                                     ++mp1Pops contextInfo identdefs
                                     ++mp1Pops contextInfo viewdefs
                                     ++mp1Pops contextInfo interfaces)
                     , ctxcds = allConceptDefs
                     , ctxks = identdefs
                     , ctxrrules = allRoleRules
                     , ctxRRels = allRoleRelations
                     , ctxreprs = representationOf contextInfo
                     , ctxvs = viewdefs
                     , ctxgs = map pGen2aGen p_gens
                     , ctxgenconcs = onlyUserConcepts (concGroups ++ map (:[]) soloConcs)
                     , ctxifcs = interfaces
                     , ctxps = purposes
                     , ctxsql = sqldefs
                     , ctxphp = phpdefs
                     , ctxmetas = p_metas
                     }
      checkOtherAtomsInSessionConcept actx
      checkPurposes actx             -- Check whether all purposes refer to existing objects
      checkDanglingRulesInRuleRoles actx -- Check whether all rules in MAINTAIN statements are declared
      checkInterfaceCycles actx      -- Check that interface references are not cyclic
      checkMultipleDefaultViews actx -- Check whether each concept has at most one default view
      uniqueNames (udefrules actx)   -- Check uniquene names of: rules,
      uniqueNames (patterns  actx)   --                          patterns,
      uniqueNames (ctxvs     actx)   --                          view defs,
      uniqueNames (ctxifcs   actx)   --                          and interfaces.
      return actx
  where
    concGroups = getGroups genLatticeIncomplete :: [[Type]]
    deflangCtxt = lang -- take the default language from the top-level context
    deffrmtCtxt = fromMaybe ReST pandocf
    
    allGens = p_gens ++ concatMap pt_gns p_patterns
    
    g_contextInfo :: Guarded ContextInfo
    g_contextInfo
     = do mp <- Map.fromList . concat <$> traverse findTypes (concat (onlyUserConcepts concGroups))
          let findR = flip (Map.findWithDefault Object) mp -- default representation is Object (sometimes called `ugly identifiers')
          _ <- traverse (compareTypes (findR . pCpt2aCpt)) allGens
          return (CI { ctxiGens = map pGen2aGen p_gens
                     , representationOf = findR
                     })
    
    compareTypes f p_gen
     = sequence_ [ if (f genC == rightType) then pure () else nonMatchingRepresentTypes p_gen (f genC) rightType
                 | genC <- gen_concs p_gen ]
     where rightType = f (gen_spc p_gen)
    
    findTypes :: A_Concept -> Guarded [(A_Concept, TType)]
    findTypes h
     = case (map toList)$ toList$ findSubsets genLattice (lJoin (aConcToType h) RepresentSeparator) of
           [] -> pure$ [] -- use default
           o@[[r]] -> representAs <$> getAsType (fatal 293 (show o++", A custom type found for "++show h++" turned out to be above the RepresentSeparator, which is wrong")) r
           lst' -> multipleRepresentTypes OriginUnknown h (concatMap (take 1 . lefts . map typeOrConcept) lst')
     where representAs Nothing = fatal 304 "Something turned out to be representable as the RepresentSeparator, which should never happen" -- []
           representAs (Just v) = [(h,v)]
    p_interfaceAndDisambObjs :: DeclMap -> [(P_Interface, P_ObjDef (TermPrim, DisambPrim))]
    p_interfaceAndDisambObjs declMap = [ (ifc, disambiguate (termPrimDisAmb declMap) $ ifc_Obj ifc) | ifc <- p_interfaces ]
    
    -- story about genRules and genLattice
    -- the genRules is a list of equalities between concept sets, in which every set is interpreted as a conjunction of concepts
    -- the genLattice is the resulting optimized structure
    genRules = [ ( Set.singleton (pConcToType (gen_spc x)), Set.fromList (map pConcToType (gen_concs x)))
               | x <- allGens
               ]
    completeRules = genRules ++
               [ ( Set.singleton (userConcept cpt), Set.fromList [BuiltIn (reprdom x), userConcept cpt] )
               | x <- p_representations++concatMap pt_Reprs p_patterns
               , cpt <- reprcpts x
               ] ++
               [ ( Set.singleton RepresentSeparator
                 , Set.fromList [ BuiltIn Alphanumeric
                                , BuiltIn BigAlphanumeric
                                , BuiltIn HugeAlphanumeric
                                , BuiltIn Password
                                , BuiltIn Binary
                                , BuiltIn BigBinary
                                , BuiltIn HugeBinary
                                , BuiltIn Date
                                , BuiltIn DateTime
                                , BuiltIn Boolean
                                , BuiltIn Integer
                                , BuiltIn Float
                                -- , BuiltIn TypeOfOne -- not a valid way to represent something! Also treated differently in this code
                                , BuiltIn Object
                                , RepresentSeparator
                                ]) ]
    genLatticeIncomplete :: Op1EqualitySystem Type -- used to derive the concept groups
    genLatticeIncomplete = optimize1 (foldr addEquality emptySystem genRules)
    genLattice :: Op1EqualitySystem Type
    genLattice = optimize1 (foldr addEquality emptySystem completeRules)

    -- accumDecl is the function that combines two relations into one
    -- meanings, for instance, two should get combined into a list of meanings, et cetera
    -- positions are combined
    -- TODO
    accumDecl :: Declaration -> Declaration -> Declaration
    accumDecl a _ = a

    pDecl2aDecl ::
         String         -- The name of the pattern
      -> ContextInfo
      -> Lang           -- The default language
      -> PandocFormat   -- The default pandocFormat
      -> P_Declaration -> Guarded (Declaration,Population)
    pDecl2aDecl patNm contextInfo defLanguage defFormat pd
     = let (prL:prM:prR:_) = dec_pragma pd ++ ["", "", ""]
           dcl = Sgn { decnm   = dec_nm pd
                     , decsgn  = pSign2aSign (dec_sign pd)
                     , decprps = dec_prps pd
                     , decprps_calc = Nothing  --decprps_calc in an A_Context are still the user-defined only. prps are calculated in adl2fspec.
                     , decprL  = prL
                     , decprM  = prM
                     , decprR  = prR
                     , decMean = pMean2aMean defLanguage defFormat (dec_Mean pd)
                     , decfpos = dec_fpos pd
                     , decusr  = True
                     , decpat  = patNm
                     , decplug = dec_plug pd
                     }
       in (\aps -> (dcl,ARelPopu { popdcl = dcl
                                 , popps = aps
                                 , popsrc = source dcl
                                 , poptgt = target dcl
                                 })
          ) <$> traverse (pAtomPair2aAtomPair contextInfo dcl) (dec_popu pd)
    pGen2aGen :: P_Gen -> A_Gen
    pGen2aGen pg@PGen{}
       = Isa{gengen = pCpt2aCpt (gen_gen pg)
            ,genspc = pCpt2aCpt (gen_spc pg)
            }
    pGen2aGen pg@P_Cy{}
       = IsE { genrhs = map pCpt2aCpt (gen_rhs pg)
             , genspc = pCpt2aCpt (gen_spc pg)
             }

    castSign :: Type -> Type -> Signature
    castSign a b = Sign (mustBeConceptBecauseMath a) (mustBeConceptBecauseMath b)

    leastConcept :: A_Concept -> Type -> A_Concept
    leastConcept c str
     = case (aConcToType c `elem` leastConcepts, str `elem` leastConcepts) of
         (True, _) -> c
         (_, True) -> mustBeConceptBecauseMath str
         (_, _)    -> fatal 178 ("Either "++name c++" or "++show str++" should be a subset of the other." )
       where
         leastConcepts = findExact genLattice (Atom (aConcToType c) `Meet` (Atom str))

    castConcept :: String -> A_Concept
    castConcept "ONE" = ONE
    castConcept x     = PlainConcept { cptnm = x }
    userConcept :: String -> Type
    userConcept "ONE" = BuiltIn TypeOfOne
    userConcept x     = UserConcept x
    
    pPop2aPop :: DeclMap -> ContextInfo -> P_Population -> Guarded Population
    pPop2aPop declMap contextInfo pop = 
     case pop of
       P_RelPopu{p_nmdr = nmdr, p_popps=aps, p_src = src, p_tgt = tgt}
         -> do dcl <- (case p_mbSign nmdr of
                         Nothing -> findDeclLooselyTyped declMap pop (name nmdr) (castConcept <$> src) (castConcept <$> tgt)
                         _ -> namedRel2Decl declMap nmdr
                      )
               aps' <- traverse (pAtomPair2aAtomPair contextInfo dcl) aps
               src' <- maybeOverGuarded ((getAsConcept (origin pop) =<<) . isMoreGeneric pop dcl Src . userConcept) src
               tgt' <- maybeOverGuarded ((getAsConcept (origin pop) =<<) . isMoreGeneric pop dcl Tgt . userConcept) tgt
               return (ARelPopu { popdcl = dcl
                                , popps = aps'
                                , popsrc = fromMaybe (source dcl) src'
                                , poptgt = fromMaybe (target dcl) tgt'
                                })
       P_CptPopu{}
         -> let cpt = castConcept (p_cnme pop) in  
            (\vals
              -> ACptPopu { popcpt = cpt
                          , popas  = vals
                          }
              ) <$> traverse (pAtomValue2aAtomValue contextInfo cpt) (p_popas pop)
    -- isMoreGeneric :: a2 -> t -> SrcOrTgt -> Type -> Guarded Type
    isMoreGeneric o dcl sourceOrTarget givenType
     = if givenType `elem` findExact genLattice (Atom (getConcept sourceOrTarget dcl) `Meet` Atom givenType)
       then pure givenType
       else mkTypeMismatchError o dcl sourceOrTarget givenType
         
    
    pAtomPair2aAtomPair :: ContextInfo -> Declaration -> PAtomPair -> Guarded AAtomPair
    pAtomPair2aAtomPair contextInfo dcl pp = 
     (\l r ->
       mkAtomPair l r
     ) <$> pAtomValue2aAtomValue contextInfo (source dcl) (ppLeft  pp)
       <*> pAtomValue2aAtomValue contextInfo (target dcl) (ppRight pp)

    pAtomValue2aAtomValue :: ContextInfo -> A_Concept -> PAtomValue -> Guarded AAtomValue
    pAtomValue2aAtomValue contextInfo cpt pav =
       case unsafePAtomVal2AtomValue typ (Just cpt) pav of
        Left msg -> Errors [mkIncompatibleAtomValueError pav msg]
        Right av -> pure av
      where typ = representationOf contextInfo cpt
               

    pObjDef2aObjDef :: DeclMap -> P_ObjectDef -> Guarded ObjectDef
    pObjDef2aObjDef declMap x = pObjDefDisamb2aObjDef declMap $ disambiguate (termPrimDisAmb declMap) x

    pObjDefDisamb2aObjDef :: DeclMap -> P_ObjDef (TermPrim, DisambPrim) -> Guarded ObjectDef
    pObjDefDisamb2aObjDef declMap x = fmap fst (typecheckObjDef declMap x)

    pViewDef2aViewDef :: DeclMap -> P_ViewDef -> Guarded ViewDef
    pViewDef2aViewDef declMap x = typecheckViewDef declMap tpda
     where tpda = disambiguate (termPrimDisAmb declMap) x

    typecheckViewDef :: DeclMap -> P_ViewD (TermPrim, DisambPrim) -> Guarded ViewDef
    typecheckViewDef declMap
       o@(P_Vd { vd_pos = orig
            , vd_lbl  = lbl   -- String
            , vd_cpt  = cpt   -- Concept
            , vd_isDefault = isDefault
            , vd_html = mHtml -- Html template
            , vd_ats  = pvs   -- view segment
            })
     = (\vdts
        -> Vd { vdpos  = orig
              , vdlbl  = lbl
              , vdcpt  = pCpt2aCpt cpt
              , vdIsDefault = isDefault
              , vdhtml = mHtml
              , vdats  = vdts
              })
       <$> traverse typeCheckViewSegment pvs
     where
       typeCheckViewSegment :: (P_ViewSegmt (TermPrim, DisambPrim)) -> Guarded ViewSegment
       typeCheckViewSegment vs
        = case vs of 
           P_ViewExp{} -> 
             do (obj,b) <- typecheckObjDef declMap (vs_obj vs)
                case toList$ findExact genLattice (lMeet c (aConcToType (source (objctx obj)))) of
                              [] -> mustBeOrdered o o (Src,(source (objctx obj)),obj)
                              r  -> if b || c `elem` r then pure (ViewExp (vs_nr vs) obj{objctx = addEpsilonLeft' (head r) (objctx obj)})
                                    else mustBeBound (origin obj) [(Tgt,objctx obj)]
           P_ViewText{} -> pure$ ViewText (vs_nr vs) (vs_txt vs)
           P_ViewHtml{} -> pure$ ViewHtml (vs_nr vs) (vs_htm vs)
       c = pConcToType (vd_cpt o)
    
    isa :: Type -> Type -> Bool
    isa c1 c2 = c1 `elem` findExact genLattice (Atom c1 `Meet` Atom c2) -- shouldn't this Atom be called a Concept? SJC: Answer: we're using the constructor "Atom" in the lattice sense, not in the relation-algebra sense. c1 and c2 are indeed Concepts here
    
    typecheckObjDef :: DeclMap -> (P_ObjDef (TermPrim, DisambPrim)) -> Guarded (ObjectDef, Bool)
    typecheckObjDef declMap
       o@(P_Obj { obj_nm = nm
         , obj_pos = orig
         , obj_ctx = ctx
         , obj_crud = mCrud
         , obj_mView = mView
         , obj_msub = subs
         , obj_strs = ostrs
         })
     = do (objExpr,(srcBounded,tgtBounded)) <- typecheckTerm ctx
          crud <- pCruds2aCruds mCrud
          maybeObj <- maybeOverGuarded (pSubi2aSubi declMap objExpr tgtBounded o) subs <* typeCheckViewAnnotation objExpr mView
          case maybeObj of
               Just (newExpr,subStructures) -> return (obj crud (newExpr,srcBounded) (Just subStructures))
               Nothing                      -> return (obj crud (objExpr,srcBounded) Nothing)
     where      
      lookupView :: String -> Maybe P_ViewDef
      lookupView viewId = case [ vd | vd <- p_viewdefs, vd_lbl vd == viewId ] of
                            []   -> Nothing
                            vd:_ -> Just vd -- return the first one, if there are more, this is caught later on by uniqueness static check
                        
      typeCheckViewAnnotation :: Expression -> Maybe String -> Guarded ()
      typeCheckViewAnnotation _       Nothing       = pure ()
      typeCheckViewAnnotation objExpr (Just viewId) =
        case lookupView viewId of 
          Just vd -> let viewAnnCptStr = aConcToType $ target objExpr
                         viewDefCptStr = pConcToType $ vd_cpt vd
                         viewIsCompatible = viewAnnCptStr `isa` viewDefCptStr
                     in  if viewIsCompatible then pure () else Errors [mkIncompatibleViewError o viewId viewAnnCptStr viewDefCptStr]
          Nothing -> Errors [mkUndeclaredError "view" o viewId] 
      obj crud (e,sr) s
       = ( Obj { objnm = nm
               , objpos = orig
               , objctx = e
               , objcrud = crud
               , objmView = mView
               , objmsub = s
               , objstrs = ostrs
               }, sr)
    addEpsilonLeft',addEpsilonRight' :: Type -> Expression -> Expression -- TODO: why use primes here?
    addEpsilonLeft' a e
     = if a==aConcToType (source e) then e else EEps (leastConcept (source e) a) (castSign a (aConcToType (source e))) .:. e
    addEpsilonRight' a e
     = if a==aConcToType (target e) then e else e .:. EEps (leastConcept (target e) a) (castSign (aConcToType (target e)) a)
    addEpsilon :: Type -> Type -> Expression -> Expression
    addEpsilon s t e
     = addEpsilonLeft' s (addEpsilonRight' t e)
    pCruds2aCruds :: Maybe P_Cruds -> Guarded Cruds
    pCruds2aCruds Nothing = pure def
    pCruds2aCruds (Just (P_Cruds org str )) 
        = if nub us == us && all (\c -> c `elem` "cCrRuUdD") str
          then pure Cruds { crudOrig = org
                          , crudC    = f 'C'
                          , crudR    = f 'R'
                          , crudU    = f 'U'
                          , crudD    = f 'D'
              }
          else Errors [mkInvalidCRUDError org str]
         where us = map toUpper str
               f :: Char -> Maybe Bool
               f c 
                 | toUpper c `elem` str = Just True
                 | toLower c `elem` str = Just False
                 | otherwise            = Nothing    

    pSubi2aSubi :: DeclMap
                -> Expression -- Expression of the surrounding
                -> Bool -- Whether the surrounding is bounded
                -> P_ObjDef a -- name of where the error occured!
                -> P_SubIfc (TermPrim, DisambPrim) -- Subinterface to check
                -> Guarded ( Expression -- In the case of a "Ref", we do not change the type of the subinterface with epsilons, this is to change the type of our surrounding instead. In the case of "Box", this is simply the original expression (in such a case, epsilons are added to the branches instead)
                           , SubInterface -- the subinterface
                           )
    pSubi2aSubi declMap objExpr b o x
      = case x of
         P_InterfaceRef{si_str = ifcId} 
           ->  do (refIfcExpr,_) <- case lookupDisambIfcObj declMap ifcId of
                                         Just disambObj -> typecheckTerm $ obj_ctx disambObj -- term is type checked twice, but otherwise we need a more complicated type check method to access already-checked interfaces. TODO: hide possible duplicate errors in a nice way (that is: via CtxError)
                                         Nothing        -> Errors [mkUndeclaredError "interface" o ifcId]
                  cs <- pCruds2aCruds (si_crud x)
                  objExprEps <- typeCheckInterfaceRef o ifcId objExpr refIfcExpr
                  return (objExprEps,InterfaceRef (si_isLink x) ifcId cs)
         P_Box{}
           -> case si_box x of
                []  -> const undefined <$> (hasNone x :: Guarded SubInterface) -- error
                l   -> (\lst -> (objExpr,Box (target objExpr) (si_class x) lst)) <$> traverse (join . fmap (matchWith (target objExpr)) . typecheckObjDef declMap) l <* uniqueNames l
     where matchWith _ (ojd,exprBound)
            = if b || exprBound then
                case toList$ findExact genLattice (lMeet (aConcToType$ target objExpr) (aConcToType . source . objctx $ ojd)) of
                    [] -> mustBeOrderedLst x [(source (objctx ojd),Src, ojd)]
                    (r:_) -> pure (ojd{objctx=addEpsilonLeft' r (objctx ojd)})
              else mustBeBound (origin ojd) [(Src,objctx ojd),(Tgt,objExpr)]
    typeCheckInterfaceRef :: P_ObjDef a -> String -> Expression -> Expression -> Guarded Expression
    typeCheckInterfaceRef objDef ifcRef objExpr ifcExpr = 
      let expTarget = target objExpr
          expTargetStr = aConcToType expTarget
          ifcSource = source ifcExpr
          ifcSourceStr = aConcToType ifcSource
          refIsCompatible = expTargetStr `isa` ifcSourceStr || ifcSourceStr `isa` expTargetStr
      in  if refIsCompatible 
          then pure $ addEpsilonRight' ifcSourceStr objExpr 
          else Errors [mkIncompatibleInterfaceError objDef expTarget ifcSource ifcRef ]
    lookupDisambIfcObj :: DeclMap -> String -> Maybe (P_ObjDef (TermPrim, DisambPrim))
    lookupDisambIfcObj declMap ifcId =
      case [ disambObj | (vd,disambObj) <- (p_interfaceAndDisambObjs declMap), ifc_Name vd == ifcId ] of
        []          -> Nothing
        disambObj:_ -> Just disambObj -- return the first one, if there are more, this is caught later on by uniqueness static check
    
    typecheckTerm :: Term (TermPrim, DisambPrim) -> Guarded (Expression, (Bool, Bool))
    typecheckTerm tct
     = case tct of
         Prim (t,v) -> (\x -> (x, case t of
                                   PVee _ -> (False,False)
                                   _      -> (True,True)
                                   )) <$> pDisAmb2Expr (t,v)
         PEqu _ a b -> join $ binary  (.==.) (MBE (Src,fst) (Src,snd), MBE (Tgt,fst) (Tgt,snd)) <$> tt a <*> tt b
         PInc _ a b -> join $ binary  (.|-.) (MBG (Src,snd) (Src,fst), MBG (Tgt,snd) (Tgt,fst)) <$> tt a <*> tt b
         PIsc _ a b -> join $ binary  (./\.) (ISC (Src,fst) (Src,snd), ISC (Tgt,fst) (Tgt,snd)) <$> tt a <*> tt b
         PUni _ a b -> join $ binary  (.\/.) (UNI (Src,fst) (Src,snd), UNI (Tgt,fst) (Tgt,snd)) <$> tt a <*> tt b
         PDif _ a b -> join $ binary  (.-.)  (MBG (Src,fst) (Src,snd), MBG (Tgt,fst) (Tgt,snd)) <$> tt a <*> tt b
         PLrs _ a b -> join $ binary' (./.)  (MBE (Tgt,snd) (Tgt,fst)) ((Src,fst),(Src,snd)) Tgt Tgt <$> tt a <*> tt b
         PRrs _ a b -> join $ binary' (.\.)  (MBE (Src,fst) (Src,snd)) ((Tgt,fst),(Tgt,snd)) Src Src <$> tt a <*> tt b
         PDia _ a b -> join $ binary' (.<>.) (ISC (Tgt,fst) (Src,snd)) ((Src,fst),(Tgt,snd)) Tgt Src <$> tt a <*> tt b -- MBE would have been correct, but too restrictive
         PCps _ a b -> join $ binary' (.:.)  (ISC (Tgt,fst) (Src,snd)) ((Src,fst),(Tgt,snd)) Tgt Src <$> tt a <*> tt b
         PRad _ a b -> join $ binary' (.!.)  (MBE (Tgt,fst) (Src,snd)) ((Src,fst),(Tgt,snd)) Tgt Src <$> tt a <*> tt b -- Using MBE instead of ISC allows the programmer to use De Morgan
         PPrd _ a b -> (\(x,(s,_)) (y,(_,t)) -> (x .*. y, (s,t))) <$> tt a <*> tt b
         PKl0 _ a   -> join $ unary   EKl0   (UNI (Src, id) (Tgt, id), UNI (Src, id) (Tgt, id)) <$> tt a
         PKl1 _ a   -> join $ unary   EKl1   (UNI (Src, id) (Tgt, id), UNI (Src, id) (Tgt, id)) <$> tt a
         PFlp _ a   -> (\(x,(s,t)) -> ((EFlp x), (t,s))) <$> tt a
         PCpl _ a   -> (\(x,_) -> (ECpl x,(False,False))) <$> tt a
         PBrk _ e   -> (\(x,t) -> (EBrk x,t)) <$> tt e 
     where
      o = origin (fmap fst tct)
      tt = typecheckTerm
      -- SJC: Here is what binary, binary' and unary do:
      -- (1) Create an expression, the combinator for this is given by its first argument
      -- (2) Fill in the corresponding type-checked terms to that expression
      -- (3) For binary' only: fill in the intermediate concept too
      -- (4) Fill in the type of the new expression
      -- For steps (3) and (4), you can use the `TT' data type to specify the new type, and what checks should occur:
      -- If you don't know what to use, try MBE: it is the strictest form.
      -- In the steps (3) and (4), different type errors may arise:
      -- If the type does not exist, this yields a type error.
      -- Some types may be generalized, while others may not.
      -- When a type may be generalized, that means that the value of the expression does not change if the type becomes larger
      -- When a type may not be generalized:
      --   the type so far is actually just an estimate
      --   it must be bound by the context to something smaller, or something as big
      --   a way to do this, is by using (V[type] /\ thingToBeBound)
      -- More details about generalizable types can be found by looking at "deriv1".
      binary :: (Expression -> Expression->Expression) -- combinator
             -> ( TT ( SrcOrTgt
                     , ( (Expression, (Bool, Bool))
                       , (Expression, (Bool, Bool))
                       ) -> (Expression, (Bool, Bool))
                     )
                , TT ( SrcOrTgt
                     , ( (Expression, (Bool, Bool))
                       , (Expression, (Bool, Bool))
                       ) -> (Expression, (Bool, Bool))
                     )
                ) -- simple instruction on how to derive the type
             -> (Expression,(Bool,Bool))
             -> (Expression,(Bool,Bool)) -- expressions to feed into the combinator after translation
             -> Guarded (Expression,(Bool,Bool))
      binary  cbn     tp  e1 e2 = wrap (fst e1,fst e2) <$> deriv tp (e1,e2)
        where
         wrap (expr1,expr2) ((src,b1), (tgt,b2)) = (cbn (addEpsilon src tgt expr1) (addEpsilon src tgt expr2), (b1, b2))
      unary   cbn     tp e1      = wrap (fst e1) <$> deriv tp e1
        where
         wrap expr  ((src,b1), (tgt,b2))  = (cbn (addEpsilon src tgt expr), (b1, b2))
      binary' cbn preConcept tp side1 side2 e1 e2 = 
          do a <- deriv1 o (fmap (resolve (e1,e2)) preConcept) 
             b <- deriv' tp (e1,e2)
             wrap (fst e1,fst e2) a b
        where
         wrap _ (_,False) ((_,b1), (_,b2))
          = mustBeBound o [(p,e) | (False,p,e)<-[(b1,side1,fst e1),(b2,side2,fst e2)]]
         wrap (expr1,expr2) (cpt,True) ((_,b1), (_,b2))
          = pure (cbn (lrDecide side1 expr1) (lrDecide side2 expr2), (b1, b2))
            where lrDecide side e = case side of Src -> addEpsilonLeft' cpt e; Tgt -> addEpsilonRight' cpt e
      deriv (t1,t2) es = (,) <$> deriv1 o (fmap (resolve es) t1) <*> deriv1 o (fmap (resolve es) t2)
 
    deriv1 o x'
     = case x' of
        (MBE a@(p1,(e1,b1)) b@(p2,(e2,b2))) ->
             if (b1 && b2) || (getConcept p1 e1 == getConcept p2 e2) then (\x -> (x,b1||b2)) <$> getExactType lJoin (p1, e1) (p2, e2)
             else mustBeBound o [(p,e) | (p,(e,False))<-[a,b]]
        (MBG (p1,(e1,b1)) (p2,(e2,b2))) ->
             (\x -> (fst x,b1)) <$> getAndCheckType lJoin (p1, True, e1) (p2, b2, e2)
        (UNI (p1,(e1,b1)) (p2,(e2,b2))) ->
             (\x -> (fst x,b1 && b2)) <$> getAndCheckType lJoin (p1, b1, e1) (p2, b2, e2)
        (ISC (p1,(e1,b1)) (p2,(e2,b2))) ->
             (\(x,r) -> (x, (b1 && Set.member (getConcept p1 e1) r) || (b2 && Set.member (getConcept p2 e2) r) || (b1 && b2))
             ) <$> getAndCheckType lMeet (p1, b1, e1) (p2, b2, e2)
     where
      getExactType flf (p1,e1) (p2,e2)
       = case toList$ findExact genLattice (flf (getConcept p1 e1) (getConcept p2 e2)) of
          [] -> mustBeOrdered o (p1,e1) (p2,e2)
          r  -> pure$ head r
      getAndCheckType flf (p1,b1,e1) (p2,b2,e2)
       = case toList$ findSubsets genLattice (flf (getConcept p1 e1) (getConcept p2 e2)) of -- note: we could have used GetOneGuarded, but this is more specific
          []  -> mustBeOrdered o (p1,e1) (p2,e2)
          [r] -> case (b1 || Set.member (getConcept p1 e1) r,b2 || Set.member (getConcept p2 e2) r ) of
                   (True,True) -> pure (head (Set.toList r),r)
                   (a,b) -> mustBeBound o [(p,e) | (False,p,e)<-[(a,p1,e1),(b,p2,e2)]]
          lst -> mustBeOrderedConcLst o (p1,e1) (p2,e2) (map Set.toList lst)
    termPrimDisAmb :: DeclMap -> TermPrim -> (TermPrim, DisambPrim)
    termPrimDisAmb declMap x
     = (x, case x of
           PI _        -> Ident
           Pid _ conspt-> Known (EDcI (pCpt2aCpt conspt))
           Patm _ s Nothing -> Mp1 s
           Patm _ s (Just conspt) -> Known (EMp1 s (pCpt2aCpt conspt))
           PVee _      -> Vee
           Pfull _ a b -> Known (EDcV (Sign (pCpt2aCpt a) (pCpt2aCpt b)))
           PNamedR nr -> Rel $ disambNamedRel nr
        )
      where
        disambNamedRel (PNamedRel _ r Nothing)  = [EDcD dc | dc <- (Map.elems $ findDecls declMap r)]
        disambNamedRel (PNamedRel _ r (Just s)) = [EDcD dc | dc <- (findDeclsTyped declMap r (pSign2aSign s))]

    pIfc2aIfc :: DeclMap -> (P_Interface, P_ObjDef (TermPrim, DisambPrim)) -> Guarded Interface
    pIfc2aIfc declMap
              (P_Ifc { ifc_Params = tps
                    , ifc_Class = iclass
                    , ifc_Args = args
                    , ifc_Roles = rols
                    , ifc_Obj = _
                    , ifc_Pos = orig
                    -- , ifc_Name = nm
                    , ifc_Prp = prp
                    }, objDisamb)
        = (\ tps' obj'
             -> Ifc { ifcParams = tps'
                    , ifcClass = iclass
                    , ifcArgs = args
                    , ifcRoles = rols
                    , ifcObj = obj'
                    , ifcEcas = []      -- to be enriched in Adl2fSpec with ECA-rules
                    , ifcControls = []  -- to be enriched in Adl2fSpec with rules to be checked
                    , ifcPos = orig
                    , ifcPrp = prp
                    }) <$> traverse (namedRel2Decl declMap) tps
                       <*> pObjDefDisamb2aObjDef declMap objDisamb

    pRoleRelation2aRoleRelation :: DeclMap -> P_RoleRelation -> Guarded A_RoleRelation
    pRoleRelation2aRoleRelation declMap prr
     = (\ ds' 
        -> RR { rrRoles = rr_Roles prr
              , rrRels  = ds'
              , rrPos   = rr_Pos prr
              }) <$> traverse (namedRel2Decl declMap) (rr_Rels prr)
    pRoleRule2aRoleRule :: P_RoleRule -> A_RoleRule
    pRoleRule2aRoleRule prr
     = A_RoleRule { arRoles = mRoles prr
                  , arRules = mRules prr
                  , arPos   = mPos prr
                  }
    
    pPat2aPat :: DeclMap -> ContextInfo -> P_Pattern -> Guarded Pattern
    pPat2aPat declMap contextInfo ppat
     = f <$> traverse (pRul2aRul declMap (name ppat)) (pt_rls ppat)
         <*> traverse (pIdentity2aIdentity declMap) (pt_ids ppat) 
         <*> traverse (pPop2aPop declMap contextInfo) (pt_pop ppat)
         <*> traverse (pViewDef2aViewDef declMap) (pt_vds ppat) 
         <*> traverse (pPurp2aPurp declMap) (pt_xps ppat)
         <*> traverse (pDecl2aDecl (name ppat) contextInfo deflangCtxt deffrmtCtxt) (pt_dcs ppat)
       where
        f rules' keys' pops' views' xpls declsAndPops
           = A_Pat { ptnm  = name ppat
                   , ptpos = pt_pos ppat
                   , ptend = pt_end ppat
                   , ptrls = rules'
                   , ptgns = map pGen2aGen (pt_gns ppat)
                   , ptdcs = map fst declsAndPops
                   , ptups = pops' ++ map snd declsAndPops
                   , ptids = keys'
                   , ptvds = views'
                   , ptxps = xpls
                   }
    pRul2aRul :: DeclMap -> String -- environment name (pattern / proc name)
              -> (P_Rule TermPrim) -> Guarded Rule
    pRul2aRul declMap env = typeCheckRul env . disambiguate (termPrimDisAmb declMap)
    typeCheckRul :: 
                 String -- environment name (pattern / proc name)
              -> (P_Rule (TermPrim, DisambPrim)) -> Guarded Rule
    typeCheckRul env P_Ru { rr_fps = orig
                          , rr_nm = nm
                          , rr_exp = expr
                          , rr_mean = meanings
                          , rr_msg = msgs
                          , rr_viol = viols
                          }
     = do (exp',_) <- typecheckTerm expr
          vls <- maybeOverGuarded (typeCheckPairView orig exp') viols
          return (Ru { rrnm = nm
                     , rrexp = exp'
                     , rrfps = orig
                     , rrmean = pMean2aMean deflangCtxt deffrmtCtxt meanings
                     , rrmsg = map (pMess2aMess deflangCtxt deffrmtCtxt) msgs
                     , rrviol = vls
                     , rrtyp = sign exp'
                     , rrdcl = Nothing
                     , r_env = env
                     , r_usr = UserDefined
                     , isSignal = not . null . concatMap arRoles . filter (\x -> nm `elem` arRules x) $ allRoleRules 
                     })
    pIdentity2aIdentity :: DeclMap -> P_IdentDef -> Guarded IdentityDef
    pIdentity2aIdentity declMap pidt
     = case disambiguate (termPrimDisAmb declMap) pidt of
           P_Id { ix_lbl = lbl
                , ix_ats = isegs
                } -> (\isegs' -> Id { idPos = orig
                                    , idLbl = lbl
                                    , idCpt = conc
                                    , identityAts = isegs'
                                    }) <$> traverse pIdentSegment2IdentSegment isegs
     where conc = pCpt2aCpt (ix_cpt pidt)
           orig = ix_pos pidt
           pIdentSegment2IdentSegment :: P_IdentSegmnt (TermPrim, DisambPrim) -> Guarded IdentitySegment
           pIdentSegment2IdentSegment (P_IdentExp ojd) =
              do o <- pObjDefDisamb2aObjDef declMap ojd
                 case toList$ findExact genLattice $ aConcToType (source $ objctx o) `lJoin` aConcToType conc of
                          [] -> mustBeOrdered orig (Src, origin ojd, objctx o) pidt
                          _  -> pure $ IdentityExp o{objctx = addEpsilonLeft' (aConcToType conc) (objctx o)}
    typeCheckPairView :: Origin -> Expression -> PairView (Term (TermPrim, DisambPrim)) -> Guarded (PairView Expression)
    typeCheckPairView o x (PairView lst)
     = PairView <$> traverse (typeCheckPairViewSeg o x) lst
    typeCheckPairViewSeg :: Origin -> Expression -> (PairViewSegment (Term (TermPrim, DisambPrim))) -> Guarded (PairViewSegment Expression)
    typeCheckPairViewSeg _ _ (PairViewText orig x) = pure (PairViewText orig x)
    typeCheckPairViewSeg o t (PairViewExp orig s x)
     = do (e,(b,_)) <- typecheckTerm x
          case toList$ (findSubsets genLattice (lJoin (aConcToType (source e)) (getConcept s t))) of
                          [] -> mustBeOrdered o (Src, (origin (fmap fst x)), e) (s,t)
                          lst -> if b || and (map (aConcToType (source e) `elem`) lst)
                                 then pure (PairViewExp orig s e)
                                 else mustBeBound (origin (fmap fst x)) [(Src, e)]
    pPurp2aPurp :: DeclMap -> PPurpose -> Guarded Purpose
    pPurp2aPurp declMap
                PRef2 { pexPos    = orig     -- :: Origin
                      , pexObj    = objref   -- :: PRefObj
                      , pexMarkup = pmarkup  -- :: P_Markup
                      , pexRefIDs  = refIds  -- :: [String]
                      }
     = (\ obj -> Expl { explPos      = orig
                      , explObj      = obj
                      , explMarkup   = pMarkup2aMarkup deflangCtxt deffrmtCtxt pmarkup
                      , explUserdefd = True
                      , explRefIds   = refIds
                      })
       <$> pRefObj2aRefObj declMap objref
    pRefObj2aRefObj :: DeclMap -> PRef2Obj -> Guarded ExplObj
    pRefObj2aRefObj _       (PRef2ConceptDef  s ) = pure$ ExplConceptDef (lookupConceptDef s)
    pRefObj2aRefObj declMap (PRef2Declaration tm) = ExplDeclaration <$> (namedRel2Decl declMap tm)
    pRefObj2aRefObj _       (PRef2Rule        s ) = pure$ ExplRule s
    pRefObj2aRefObj _       (PRef2IdentityDef s ) = pure$ ExplIdentityDef s
    pRefObj2aRefObj _       (PRef2ViewDef     s ) = pure$ ExplViewDef s
    pRefObj2aRefObj _       (PRef2Pattern     s ) = pure$ ExplPattern s
    pRefObj2aRefObj _       (PRef2Interface   s ) = pure$ ExplInterface s
    pRefObj2aRefObj _       (PRef2Context     s ) = pure$ ExplContext s
    lookupConceptDef :: String -> ConceptDef
    lookupConceptDef s
     = case filter (\cd -> name cd == s) allConceptDefs of
        []    -> Cd{cdpos=OriginUnknown, cdcpt=s, cdplug=True, cddef="", cdref="", cdfrom=n1} 
        (x:_) -> x
    allConceptDefs :: [ConceptDef]
    allConceptDefs = p_conceptdefs++concatMap pt_cds p_patterns
    allRoleRules :: [A_RoleRule]
    allRoleRules = map pRoleRule2aRoleRule 
                      (p_roleRules ++ concatMap pt_RRuls p_patterns)

pDisAmb2Expr :: (TermPrim, DisambPrim) -> Guarded Expression
pDisAmb2Expr (_,Known x) = pure x
pDisAmb2Expr (_,Rel [x]) = pure x
pDisAmb2Expr (o,Rel rs)  = cannotDisambRel o rs
pDisAmb2Expr (o,_)       = cannotDisamb o

pMean2aMean :: Lang           -- The default language
            -> PandocFormat   -- The default pandocFormat
            -> [PMeaning] -> AMeaning
pMean2aMean defLanguage defFormat pmeanings
 = AMeaning [ pMarkup2aMarkup defLanguage defFormat pmarkup | PMeaning pmarkup <-pmeanings ]
pMess2aMess :: Lang           -- The default language
            -> PandocFormat   -- The default pandocFormat
            -> PMessage -> A_Markup
pMess2aMess defLanguage defFormat (PMessage x) = pMarkup2aMarkup defLanguage defFormat x
pMarkup2aMarkup :: Lang           -- The default language
                -> PandocFormat   -- The default pandocFormat
                -> P_Markup -> A_Markup
pMarkup2aMarkup defLanguage defFormat
   P_Markup  { mLang   = ml
             , mFormat = mpdf
             , mString = str
             }
 = A_Markup { amLang = fromMaybe defLanguage ml -- The language is always defined; if not by the user, then by default.
            , amPandoc = string2Blocks fmt str
            }
     where
       fmt = fromMaybe defFormat mpdf           -- The pandoc format is always defined; if not by the user, then by default.

-- helpers for generating a lattice, not having to write `Atom' all the time
-- the l in lJoin and lMeet denotes the lattice.
lJoin,lMeet :: a -> a -> FreeLattice a
lJoin a b = Join (Atom a) (Atom b)
lMeet a b = Meet (Atom a) (Atom b)
-- intended for finding the right expression on terms like (Src,fst)
resolve :: t -> (SrcOrTgt, t -> (t1, (t2, t2))) -> (SrcOrTgt, (t1, t2))
resolve es (p,f)
 = case (p,f es) of
  (Src,(e,(b,_))) -> (Src,(e,b))
  (Tgt,(e,(_,b))) -> (Tgt,(e,b))

maybeOverGuarded :: (t -> Guarded a) -> Maybe t -> Guarded (Maybe a)
maybeOverGuarded _ Nothing = pure Nothing
maybeOverGuarded f (Just x) = Just <$> f x

data TT a  -- (In order of increasing strictness. If you are unsure which to pick: just use MBE, it'll usually work fine)
 = UNI a a -- find the union of these types, return it.
 | ISC a a -- find the intersection of these types, return it.
 | MBE a a -- must be equal: must be (made) of equal type. If these types are comparable, it returns the greatest.
 | MBG a a -- The first of these types must be the greatest, if so, return it (error otherwise)
 -- SJC: difference between UNI and MBE
 -- in general, UNI is less strict than MBE:
 --   suppose A ≤ C, B ≤ C, and C is the least such concept (e.g. if A≤D and B≤D then C≤D)
 --   in this case UNI A B will yield C (if both A and B are generalizable), while MBE A B will give an error
 --   note that in case of A ≤ C, B ≤ C, A ≤ D, B ≤ D (and there is no order between C and D), both will give an error
 --   the error message, however, should be different:
 --     for MBE it says that A and B must be of the same type, and suggests adding an order between A and B
 --     for UNI it says that it cannot decide whether A \/ B is of type C or D, and suggests adding an order between C and D
 --   In addition, MBE requires that both sides are not generalizable. UNI does not, and simply propagates this property.
 -- MBG is like MBE, but will only try to generalize the right hand side (when allowed)

deriv' :: (Applicative f)
       => ((SrcOrTgt, t -> (Expression, (Bool, Bool))), (SrcOrTgt, t -> (Expression, (Bool, Bool))))
       -> t
       -> f ((Type, Bool), (Type, Bool))
deriv' (a,b) es = let (sourceOrTarget1, (e1, t1)) = resolve es a
                      (sourceOrTarget2, (e2, t2)) = resolve es b
                  in pure ((getConcept sourceOrTarget1 e1, t1), (getConcept sourceOrTarget2 e2, t2))
instance Functor TT where
  fmap f (UNI a b) = UNI (f a) (f b)
  fmap f (ISC a b) = ISC (f a) (f b)
  fmap f (MBE a b) = MBE (f a) (f b)
  fmap f (MBG a b) = MBG (f a) (f b)
  
getConcept :: Association a => SrcOrTgt -> a -> Type
getConcept Src = aConcToType . source
getConcept Tgt = aConcToType . target
