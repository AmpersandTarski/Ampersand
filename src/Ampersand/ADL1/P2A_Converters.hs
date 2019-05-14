{-# LANGUAGE LambdaCase, ImplicitParams #-}
{-# LANGUAGE ApplicativeDo, DuplicateRecordFields,OverloadedLabels #-}
module Ampersand.ADL1.P2A_Converters
    ( pCtx2aCtx
    , pCpt2aCpt
    )
where
import           Ampersand.ADL1.Disambiguate
import           Ampersand.ADL1.Lattices -- used for type-checking
import           Ampersand.ADL1.Expression
import           Ampersand.Basics hiding (to,set,conc)
import           Ampersand.Classes
import           Ampersand.Core.A2P_Converters
import           Ampersand.Core.AbstractSyntaxTree
import           Ampersand.Core.ParseTree
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec.ToFSpec.Populated(sortSpecific2Generic)
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Misc
import           Control.Arrow(first)
import           RIO.Char(toUpper,toLower)
import           Data.Foldable (toList)
import           Data.Graph (stronglyConnComp, SCC(CyclicSCC))
import           Data.Hashable
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified RIO.Set as Set
import           Data.Text (pack)

pConcToType :: P_Concept -> Type
pConcToType P_Singleton = BuiltIn TypeOfOne
pConcToType p = UserConcept (name p)
aConcToType :: A_Concept -> Type
aConcToType ONE = BuiltIn TypeOfOne
aConcToType p = UserConcept (name p)

getAsConcept :: Origin -> Type -> Guarded A_Concept
getAsConcept o v = case typeOrConcept v of
                     Right x -> unexpectedType o x
                     Left  x -> return x

userList :: [Type] -> [A_Concept]
userList = lefts . fmap typeOrConcept

mustBeConceptBecauseMath :: (?loc :: CallStack) => Type -> A_Concept
mustBeConceptBecauseMath tp
 = let fatalV :: (?loc :: CallStack) => a
       fatalV = fatal "A concept turned out to be a built-in type."
   in case getAsConcept fatalV tp of
        Checked v _ -> v
        _ -> fatalV

-- NOTE: Static checks like checkPurposes should ideally occur on the P-structure before type-checking, as it makes little
-- sense to do type checking when there are static errors. However, in Ampersand all collect functions (e.g. in ViewPoint)
-- only exist on the A-Structure, so we do it afterwards. Static purpose errors won't affect types, so in this case it is no problem. 

-- Check whether all purposes refer to existing objects.
checkPurposes :: A_Context -> Guarded ()
checkPurposes ctx = let topLevelPurposes = ctxps ctx
                        purposesInPatterns = concatMap ptxps (ctxpats ctx)
                        allPurposes = topLevelPurposes ++ purposesInPatterns
                        danglingPurposes = filter (isDanglingPurpose ctx) allPurposes
                    in  case danglingPurposes of
                      []   -> pure () 
                      x:xs -> Errors $ 
                                    mkDanglingPurposeError x NEL.:|
                                map mkDanglingPurposeError xs

-- Return True if the ExplObj in this Purpose does not exist.
isDanglingPurpose :: A_Context -> Purpose -> Bool
isDanglingPurpose ctx purp = 
  case explObj purp of
    ExplConceptDef concDef -> let nm = name concDef in nm `notElem` map name (Set.elems $ concs ctx )
    ExplRelation decl -> not (name decl `elem` Set.map name (relsDefdIn ctx)) -- is already covered by type checker
    ExplRule nm -> nm `notElem` map name (Set.elems $ udefrules ctx) 
    ExplIdentityDef nm -> nm `notElem` map name (identities ctx)
    ExplViewDef nm ->  nm `notElem` map name (viewDefs ctx)
    ExplPattern nm -> nm `notElem` map name (ctxpats ctx)
    ExplInterface nm -> nm `notElem` map name (ctxifcs ctx)
    ExplContext nm -> ctxnm ctx /= nm 
                         && False -- HJO: This line is a workaround for the issue mentioned in https://github.com/AmpersandTarski/ampersand/issues/46
                                  -- TODO: fix this when we pick up working on multiple contexts.
-- Check that interface references are not cyclic
checkInterfaceCycles :: A_Context -> Guarded ()
checkInterfaceCycles ctx = 
   case interfaceCycles of
     []   -> return ()
     x:xs -> Errors $ fmap mkInterfaceRefCycleError (x NEL.:| xs)
  where interfaceCycles :: [NEL.NonEmpty Interface]
        interfaceCycles = map ( fmap lookupInterface
                              . fromMaybe (fatal "Empty list of interfacenames is unexpected here.")
                              . NEL.nonEmpty
                              ) 
                        . getCycles $ refsPerInterface
        refsPerInterface :: [(String, [String])]
        refsPerInterface = [(name ifc, getDeepIfcRefs $ ifcObj ifc) | ifc <- ctxifcs ctx ]
        getDeepIfcRefs :: ObjectDef -> [String]
        getDeepIfcRefs obj = case objmsub obj of
                               Nothing -> []
                               Just si -> case si of 
                                           InterfaceRef{} -> if siIsLink si
                                                             then []
                                                             else [siIfcId si]
                                           Box{}          -> concatMap getDeepIfcRefs [x | BxExpr x <- siObjs si]
        lookupInterface :: String -> Interface
        lookupInterface nm = case [ ifc | ifc <- ctxifcs ctx, name ifc == nm ] of
                               [ifc] -> ifc
                               _     -> fatal "Interface lookup returned zero or more than one result"

-- Check whether each concept has at most one default view
checkMultipleDefaultViews :: A_Context -> Guarded ()
checkMultipleDefaultViews ctx = 
   case conceptsWithMultipleViews of
     []   -> return ()
     x:xs -> Errors $ fmap mkMultipleDefaultError (x NEL.:| xs)
  where
    conceptsWithMultipleViews = 
                filter (\x -> NEL.length x > 1)
              . eqClass ((==) `on` vdcpt) 
              . filter vdIsDefault $ ctxvs ctx
checkDanglingRulesInRuleRoles :: A_Context -> Guarded ()
checkDanglingRulesInRuleRoles ctx = 
   case [mkDanglingRefError "Rule" nm (arPos rr)  
        | rr <- ctxrrules ctx
        , nm <- NEL.toList $ arRules rr
        , nm `notElem` map name (Set.elems $ allRules ctx)
        ] of
     [] -> return ()
     x:xs -> Errors (x NEL.:| xs)
checkOtherAtomsInSessionConcept :: A_Context -> Guarded ()
checkOtherAtomsInSessionConcept ctx = 
   case [mkOtherAtomInSessionError atom
        | pop@ACptPopu{popcpt =cpt} <- ctxpopus ctx
        , name cpt == "SESSION"
        , atom <- popas pop
        -- SJC: I think we should not allow _SESSION in a POPULATION statement, as there is no current session at that time (_SESSION should only be allowed as Atom in expressions)
        , not (_isPermittedSessionValue atom)
        ] ++
        [ mkOtherTupleInSessionError d pr
        | ARelPopu{popsrc = src,poptgt = tgt,popdcl = d,popps = ps} <- ctxpopus ctx
        , name src == "SESSION" || name tgt == "SESSION"
        , pr <- Set.elems ps
        , (name src == "SESSION" && not (_isPermittedSessionValue (apLeft pr)))
          ||
          (name tgt == "SESSION" && not (_isPermittedSessionValue (apRight pr)))
        ]
        of
    [] -> return ()
    x:xs -> Errors (x NEL.:| xs)
  where _isPermittedSessionValue :: AAtomValue -> Bool
        _isPermittedSessionValue v@AAVString{} = aavstr v == "_SESSION"
        _isPermittedSessionValue _                 = False
warnCaseProblems :: A_Context -> Guarded ()
warnCaseProblems ctx = 
   let warnings :: [Warning]
       warnings = warns (concs ctx) 
               ++ warns (relsDefdIn ctx) 
       warns set = [ mkCaseProblemWarning x y
                   | x <- lst, y<- lst
                   , map toUpper (name x) == map toUpper (name y)
                   , name x < name y 
                   ]
            where lst = toList set
   in addWarnings warnings $ return ()
     

pSign2aSign :: P_Sign -> Signature
pSign2aSign (P_Sign src tgt) = Sign (pCpt2aCpt src) (pCpt2aCpt tgt)
findRels :: DeclMap -> String -> Map.Map SignOrd Expression
findRels declMap x = Map.findWithDefault Map.empty x declMap  -- get all relations with the same name as x
extractDecl :: P_NamedRel -> Expression -> Guarded Relation
extractDecl _ (EDcD r) = return r
extractDecl _ e = fatal $ "Expecting a declared relation, instead I found: "++show e -- to fix: return an error via a (still to be made) function in CtxError
namedRel2Decl :: DeclMap -> P_NamedRel -> Guarded Relation
namedRel2Decl declMap o@(PNamedRel _ r Nothing)  = getOneExactly o (findDecls' declMap r) >>= extractDecl o
namedRel2Decl declMap o@(PNamedRel _ r (Just s)) = getOneExactly o (findRelsTyped declMap r (pSign2aSign s)) >>= extractDecl o
findDecls' :: DeclMap -> String -> [Expression]
findDecls' declMap x = Map.elems (findRels declMap  x)
findRelsLooselyTyped :: DeclMap -> String -> Maybe A_Concept -> Maybe A_Concept -> [Expression]
findRelsLooselyTyped declMap x (Just src) (Just tgt)
 = findRelsTyped declMap x (Sign src tgt)
   `orWhenEmpty` (findRelsLooselyTyped declMap x (Just src) Nothing `isct` findRelsLooselyTyped declMap x Nothing (Just tgt))
   `orWhenEmpty` (findRelsLooselyTyped declMap x (Just src) Nothing `unin` findRelsLooselyTyped declMap x Nothing (Just tgt))
   `orWhenEmpty` findDecls' declMap x
 where isct lsta lstb = [a | a<-lsta, a `elem` lstb]
       unin lsta lstb = L.nub (lsta ++ lstb)
findRelsLooselyTyped declMap x Nothing Nothing = findDecls' declMap x
findRelsLooselyTyped declMap x (Just src) Nothing
 = [dcl | dcl <- findDecls' declMap x, name (source dcl) == name src ]
   `orWhenEmpty` findDecls' declMap x
findRelsLooselyTyped declMap x Nothing (Just tgt)
 = [dcl | dcl <- findDecls' declMap x, name (target dcl) == name tgt ]
   `orWhenEmpty` findDecls' declMap x
findDeclLooselyTyped :: DeclMap
                     -> P_NamedRel
                     -> String
                     -> Maybe A_Concept
                     -> Maybe A_Concept
                     -> Guarded Relation
findDeclLooselyTyped declMap o x src tgt = getOneExactly o (findRelsLooselyTyped declMap x src tgt) >>= extractDecl o
findRelsTyped :: DeclMap -> String -> Signature -> [Expression]
findRelsTyped declMap x tp = Map.findWithDefault [] (SignOrd tp) (Map.map (:[]) (findRels declMap x))

type DeclMap = Map.Map String (Map.Map SignOrd Expression)

onlyUserConcepts :: [[Type]] -> [[A_Concept]]
onlyUserConcepts = fmap userList

-- | pCtx2aCtx has three tasks:
-- 1. Disambiguate the structures.
--    Disambiguation means replacing every "TermPrim" (the parsed expression) with the correct Expression (available through DisambPrim)
--    This is done by using the function "disambiguate" on the outer-most structure.
--    In order to do this, its data type must be polymorphic, as in "P_ViewSegmt a".
--    After parsing, the type has TermPrim for the type variable. In our example: "P_ViewSegmt TermPrim". Note that "type P_ViewSegment = P_ViewSegmt TermPrim".
--    After disambiguation, the type variable is (TermPrim, DisambPrim), as in "P_ViewSegmt (TermPrim, DisambPrim)"
-- 2. Typecheck the structures.
--    This changes the data-structure entirely, changing the P_ into the A_
--    A "Guarded" will be added on the outside, in order to catch both type errors and disambiguation errors.
--    Using the Applicative operations <$> and <*> causes these errors to be in parallel
-- 3. Check everything else on the A_-structure: interface references should not be cyclic, rules e.a. must have unique names, etc.
pCtx2aCtx :: Options -> P_Context -> Guarded A_Context
pCtx2aCtx opts
 PCtx { ctx_nm     = n1
      , ctx_pos    = n2
      , ctx_lang   = ctxmLang
      , ctx_markup = pandocf
      , ctx_pats   = p_patterns
      , ctx_rs     = p_rules    
      , ctx_ds     = p_relations
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
      , ctx_metas  = p_metas
      }
 = do contextInfo <- g_contextInfo
      let declMap = declDisambMap contextInfo
      pats        <- traverse (pPat2aPat contextInfo) p_patterns            --  The patterns defined in this context
      uniqueNames pats
      rules       <- traverse (pRul2aRul contextInfo Nothing) p_rules       --  All user defined rules in this context, but outside patterns
      uniqueNames rules
      identdefs   <- traverse (pIdentity2aIdentity contextInfo Nothing) p_identdefs --  The identity definitions defined in this context, outside the scope of patterns
      viewdefs    <- traverse (pViewDef2aViewDef contextInfo) p_viewdefs    --  The view definitions defined in this context, outside the scope of patterns
      uniqueNames viewdefs
      interfaces  <- traverse (pIfc2aIfc contextInfo) (p_interfaceAndDisambObjs declMap)   --  TODO: explain   ... The interfaces defined in this context, outside the scope of patterns
      uniqueNames interfaces
      purposes    <- traverse (pPurp2aPurp contextInfo) p_purposes          --  The purposes of objects defined in this context, outside the scope of patterns
      udpops      <- traverse (pPop2aPop contextInfo) p_pops --  [Population]
      allRoleRelations <- traverse (pRoleRelation2aRoleRelation contextInfo) (p_roleRelations ++ concatMap pt_RRels p_patterns)
      relations <- traverse (pDecl2aDecl Nothing deflangCtxt deffrmtCtxt) p_relations
      let actx = ACtx{ ctxnm = n1
                     , ctxpos = n2
                     , ctxlang = deflangCtxt
                     , ctxmarkup = deffrmtCtxt
                     , ctxpats = pats
                     , ctxrs = Set.fromList rules
                     , ctxds = Set.fromList relations
                     , ctxpopus = udpops  -- the content is copied from p_pops
                     , ctxcds = allConceptDefs
                     , ctxks = identdefs
                     , ctxrrules = allRoleRules
                     , ctxRRels = allRoleRelations
                     , ctxreprs = representationOf contextInfo
                     , ctxvs = viewdefs
                     , ctxgs = map pClassify2aClassify p_gens
                     , ctxgenconcs = onlyUserConcepts (concGroups ++ map (:[]) (soloConcs contextInfo))
                     , ctxifcs = interfaces
                     , ctxps = purposes
                     , ctxmetas = p_metas
                     , ctxInfo = contextInfo 
                     }
      checkOtherAtomsInSessionConcept actx
      checkPurposes actx             -- Check whether all purposes refer to existing objects
      checkDanglingRulesInRuleRoles actx -- Check whether all rules in MAINTAIN statements are declared
      checkInterfaceCycles actx      -- Check that interface references are not cyclic
      checkMultipleDefaultViews actx -- Check whether each concept has at most one default view
      warnCaseProblems actx   -- Warn if there are problems with the casing of names of relations and/or concepts  
      return actx
  where
    concGroups = getGroups genLatticeIncomplete :: [[Type]]
    deflangCtxt = fromMaybe English $ ctxmLang `orElse` language opts
    deffrmtCtxt = fromMaybe ReST pandocf
    
    allGens = p_gens ++ concatMap pt_gns p_patterns
    allReprs = p_representations++concatMap pt_Reprs p_patterns
    g_contextInfo :: Guarded ContextInfo
    g_contextInfo
     = do let connectedConcepts = connect [] (map (Set.elems . concs) gns)
          typeMap <- mkTypeMap connectedConcepts allReprs
          let findR :: A_Concept -> TType
              findR cpt = fromMaybe
                            Object -- default representation is Object (sometimes called `ugly identifiers')
                            (lookup cpt typeMap)
          multitypologies <- traverse mkTypology connectedConcepts
          decls <- traverse (pDecl2aDecl Nothing deflangCtxt deffrmtCtxt) (p_relations ++ concatMap pt_dcs p_patterns)
          let declMap = Map.map groupOnTp (Map.fromListWith (++) [(name d,[EDcD d]) | d <- decls])
                where groupOnTp lst = Map.fromListWith const [(SignOrd$ sign d,d) | d <- lst]
          let allConcs = Set.fromList (map (aConcToType . source) decls ++ map (aConcToType . target) decls)  :: Set.Set Type
          return CI { ctxiGens = gns
                    , representationOf = findR
                    , multiKernels = multitypologies
                    , reprList = allReprs
                    , declDisambMap = declMap
                    , soloConcs = filter (not . isInSystem genLattice) (Set.toList allConcs)
                    , gens_efficient = genLattice
                    }
        where
          gns = map pClassify2aClassify allGens
          -- | function that creates a lookup table of concepts with a representation. 
          --   it is checked that concepts in the same conceptgroup share a common TType. 
          mkTypeMap :: [[A_Concept]] -> [Representation] -> Guarded [(A_Concept , TType)]
          mkTypeMap groups reprs 
            = f <$> traverse typeOfGroup groups
                <*> traverse typeOfSingle [c | c <- conceptsOfReprs, c `notElem` conceptsOfGroups]
            where 
              f :: [[(A_Concept,TType)]] -> [Maybe (A_Concept,TType,[Origin])] -> [(A_Concept , TType)]
              f typesOfGroups typesOfOthers
                  = concat typesOfGroups ++ map stripOrigin (catMaybes typesOfOthers)
              stripOrigin ::  (A_Concept,TType,[Origin]) -> (A_Concept,TType)
              stripOrigin (cpt,t,_) = (cpt,t)
              reprTrios :: [(A_Concept,TType,Origin)]
              reprTrios = L.nub $ concatMap toReprs reprs
                where toReprs :: Representation -> [(A_Concept,TType,Origin)]
                      toReprs r = [ (makeConcept str,reprdom r,origin r) | str <- NEL.toList $ reprcpts r]
              conceptsOfGroups :: [A_Concept]
              conceptsOfGroups = L.nub (concat groups)
              conceptsOfReprs :: [A_Concept]
              conceptsOfReprs = L.nub $ map fstOf3 reprTrios
                 where fstOf3 (cpt,_,_)=cpt
              typeOfSingle :: A_Concept -> Guarded (Maybe (A_Concept,TType,[Origin]))
              typeOfSingle cpt 
                = case filter ofCpt reprTrios of
                   [] -> pure Nothing
                   rs -> case L.nub (map getTType rs) of
                           []  -> fatal "Impossible empty list."
                           [t] -> pure ( Just (cpt,t, map getOrigin rs))
                           _   -> mkMultipleRepresentTypesError cpt lst
                                     where lst = [(t,o) | (_,t,o) <- rs]
                where ofCpt :: (A_Concept,TType,Origin) -> Bool
                      ofCpt (cpt',_,_) =  cpt == cpt'
                      getOrigin :: (A_Concept,TType,Origin) -> Origin
                      getOrigin (_,_,o) = o
              getTType :: (a,TType,b) -> TType
              getTType (_,t,_) = t
              typeOfGroup :: [A_Concept] -> Guarded [(A_Concept,TType)]
              typeOfGroup grp 
                = do singleTypes <- traverse typeOfSingle grp
                     let typeList = catMaybes singleTypes
                     case L.nub (map getTType typeList) of
                       []  -> pure []
                       [t] -> pure [(cpt,t) | cpt <- grp]
                       _   -> mkMultipleTypesInTypologyError typeList

          connect :: [[A_Concept]] -> [[A_Concept]] -> [[A_Concept]]
          connect typols gs = 
             case gs of
               []   -> typols
               x:xs -> connect (t:typols) rest
                 where 
                    (t,rest) = g x xs 
                    g a as = case L.partition (hasConceptsOf a) as of
                              (_,[])   -> (a,as)
                              (hs',hs) -> g (L.nub $ a ++ concat hs) hs'
                    hasConceptsOf :: [A_Concept] -> [A_Concept] -> Bool
                    hasConceptsOf a b = and [ x' `notElem` b | x' <- a]

          mkTypology :: [A_Concept] -> Guarded Typology
          mkTypology cs = 
            case filter (not . isSpecific) cs of
               []  -> -- there must be at least one cycle in the CLASSIFY statements.
                      case L.nub cycles of
                        []  -> fatal "No cycles found!"
                        x:xs -> mkCyclesInGensError (x NEL.:| xs)
                        where cycles = filter hasMultipleSpecifics $ getCycles [(g, f g) | g <- gns]
                                where
                                  f :: AClassify -> [AClassify]
                                  f g = [gn | gn <- gns
                                            , gn /= g
                                            , genspc g `elem` concs gn
                                        ]
                                  hasMultipleSpecifics :: [AClassify]-> Bool
                                  hasMultipleSpecifics gs = length (L.nub (map genspc gs)) > 1
               [r] -> pure  
                          Typology { tyroot = r
                                   , tyCpts = reverse . sortSpecific2Generic gns $ cs
                                   }
               rs -> mkMultipleRootsError rs $
                       case filter isInvolved gns of
                         []  -> fatal "No involved gens"
                         x:xs -> x NEL.:| xs
             where 
               isSpecific :: A_Concept -> Bool
               isSpecific cpt = cpt `elem` map genspc (filter (not . isTrivial) gns)
                 where 
                   isTrivial g =
                      case g of 
                        Isa{} -> gengen g == genspc g
                        IsE{} -> genrhs g == [genspc g]
               isInvolved :: AClassify -> Bool
               isInvolved gn = not . null $ concs gn `Set.intersection` Set.fromList cs

    p_interfaceAndDisambObjs :: DeclMap -> [(P_Interface, P_BoxItem (TermPrim, DisambPrim))]
    p_interfaceAndDisambObjs declMap = [ (ifc, disambiguate (termPrimDisAmb declMap) $ ifc_Obj ifc) | ifc <- p_interfaces ]
    
    -- story about genRules and genLattice
    -- the genRules is a list of equalities between concept sets, in which every set is interpreted as a conjunction of concepts
    -- the genLattice is the resulting optimized structure
    genRules :: [(Set.Set Type, Set.Set Type)]
    genRules = [ ( Set.fromList [ pConcToType . specific $ x]
                 , Set.fromList . NEL.toList . NEL.map pConcToType . generics $ x
                 )
               | x <- allGens
               ]

    completeRules = genRules ++
               [ ( Set.singleton (userConcept cpt), Set.fromList [BuiltIn (reprdom x), userConcept cpt] )
               | x <- p_representations++concatMap pt_Reprs p_patterns
               , cpt <- NEL.toList $ reprcpts x
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

    pClassify2aClassify :: PClassify -> AClassify
    pClassify2aClassify pg = 
          case NEL.tail (generics pg) of
            [] -> Isa{ genpos = origin pg
                     , gengen = pCpt2aCpt . NEL.head $ generics pg
                     , genspc = pCpt2aCpt $ specific pg
                     }
            _  -> IsE{ genpos = origin pg
                     , genrhs = NEL.toList . NEL.map pCpt2aCpt $ generics pg
                     , genspc = pCpt2aCpt $ specific pg
                     }

    userConcept :: String -> Type
    userConcept "ONE" = BuiltIn TypeOfOne
    userConcept x     = UserConcept x
    
    pPop2aPop :: ContextInfo -> P_Population -> Guarded Population
    pPop2aPop ci pop = 
     case pop of
       P_RelPopu{p_nmdr = nmdr, p_popps=aps, p_src = src, p_tgt = tgt}
         -> do dcl <- case p_mbSign nmdr of
                        Nothing -> findDeclLooselyTyped declMap nmdr (name nmdr) (makeConcept <$> src) (makeConcept <$> tgt)
                        _ -> namedRel2Decl declMap nmdr
                      
               aps' <- traverse (pAtomPair2aAtomPair (representationOf ci) dcl) aps
               src' <- maybeOverGuarded ((getAsConcept (origin pop) =<<) . isMoreGeneric (origin pop) dcl Src . userConcept) src
               tgt' <- maybeOverGuarded ((getAsConcept (origin pop) =<<) . isMoreGeneric (origin pop) dcl Tgt . userConcept) tgt
               return ARelPopu { popdcl = dcl
                               , popps  = Set.fromList aps'
                               , popsrc = fromMaybe (source dcl) src'
                               , poptgt = fromMaybe (target dcl) tgt'
                               }
       P_CptPopu{}
         -> let cpt = makeConcept (p_cnme pop) in  
            (\vals
              -> ACptPopu { popcpt = cpt
                          , popas  = vals
                          }
              ) <$> traverse (pAtomValue2aAtomValue (representationOf ci) cpt) (p_popas pop)
     where declMap = declDisambMap ci
    isMoreGeneric :: Origin -> Relation -> SrcOrTgt -> Type -> Guarded Type
    isMoreGeneric o dcl sourceOrTarget givenType
     = if givenType `elem` findExact genLattice (Atom (getConcept sourceOrTarget dcl) `Meet` Atom givenType)
       then pure givenType
       else mkTypeMismatchError o dcl sourceOrTarget givenType
               
    pObjDefDisamb2aObjDef :: ContextInfo -> P_BoxItem (TermPrim, DisambPrim) -> Guarded BoxItem
    pObjDefDisamb2aObjDef ci x = fmap fst (typecheckObjDef ci x)

    pViewDef2aViewDef :: ContextInfo -> P_ViewDef -> Guarded ViewDef
    pViewDef2aViewDef ci x = typecheckViewDef ci tpda
     where tpda = disambiguate (termPrimDisAmb (declDisambMap ci)) x

    typecheckViewDef :: ContextInfo -> P_ViewD (TermPrim, DisambPrim) -> Guarded ViewDef
    typecheckViewDef ci
       o@P_Vd { pos = orig
              , vd_lbl  = lbl   -- String
              , vd_cpt  = cpt   -- Concept
              , vd_isDefault = isDefault
              , vd_html = mHtml -- Html template
              , vd_ats  = pvs   -- view segments
              }
     = (\vdts
        -> Vd { vdpos  = orig
              , vdlbl  = lbl
              , vdcpt  = pCpt2aCpt cpt
              , vdIsDefault = isDefault
              , vdhtml = mHtml
              , vdats  = vdts
              })
       <$> traverse typeCheckViewSegment (zip [0..] pvs)
     where
       typeCheckViewSegment :: (Integer, P_ViewSegment (TermPrim, DisambPrim)) -> Guarded ViewSegment
       typeCheckViewSegment (seqNr, seg)
        = do payload <- typecheckPayload (vsm_load seg)
             return ViewSegment { vsmpos   = origin seg
                                , vsmlabel = vsm_labl seg
                                , vsmSeqNr = seqNr
                                , vsmLoad  = payload
                                }
         where 
          typecheckPayload :: P_ViewSegmtPayLoad (TermPrim, DisambPrim) -> Guarded ViewSegmentPayLoad
          typecheckPayload payload 
           = case payload of
              P_ViewExp term -> 
                 do (viewExpr,(srcBounded,_)) <- typecheckTerm ci term
                    case userList$toList$ findExact genLattice (flType$ lMeet c (source viewExpr)) of
                       []  -> mustBeOrdered (origin o) o (Src, source viewExpr, viewExpr)
                       r@(h:_) -> if srcBounded || c `elem` r then pure (ViewExp (addEpsilonLeft genLattice h viewExpr))
                             else mustBeBound (origin seg) [(Tgt,viewExpr)]
              P_ViewText str -> pure$ ViewText str
       c = mustBeConceptBecauseMath (pConcToType (vd_cpt o))
    
    isa :: Type -> Type -> Bool
    isa c1 c2 = c1 `elem` findExact genLattice (Atom c1 `Meet` Atom c2) -- shouldn't this Atom be called a Concept? SJC: Answer: we're using the constructor "Atom" in the lattice sense, not in the relation-algebra sense. c1 and c2 are indeed Concepts here
    isaC :: A_Concept -> A_Concept -> Bool
    isaC c1 c2 = aConcToType c1 `elem` findExact genLattice (Atom (aConcToType c1) `Meet` Atom (aConcToType c2))
    
    typecheckObjDef :: ContextInfo -> P_BoxItem (TermPrim, DisambPrim) -> Guarded (BoxItem, Bool)
    typecheckObjDef declMap objDef
      = case objDef of
          P_BxExpr { obj_nm = nm
                , pos = orig
                , obj_ctx = ctx
                , obj_crud = mCrud
                , obj_mView = mView
                , obj_msub = subs
                } -> do (objExpr,(srcBounded,tgtBounded)) <- typecheckTerm declMap ctx
                        checkCrud
                        crud <- pCruds2aCruds objExpr mCrud
                        maybeObj <- maybeOverGuarded (pSubi2aSubi declMap objExpr tgtBounded objDef) subs <* typeCheckViewAnnotation objExpr mView
                        case maybeObj of
                          Just (newExpr,subStructures) -> return (obj crud (newExpr,srcBounded) (Just subStructures))
                          Nothing                      -> return (obj crud (objExpr,srcBounded) Nothing)
            where      
              lookupView :: String -> Maybe P_ViewDef
              lookupView viewId = case [ vd | vd <- p_viewdefs, vd_lbl vd == viewId ] of
                                    []   -> Nothing
                                    vd:_ -> Just vd -- return the first one, if there are more, this is caught later on by uniqueness static check
                                
              checkCrud :: Guarded()
              checkCrud = 
                case (mCrud, subs) of
                  (Just _ , Just P_InterfaceRef{si_isLink=False} )
                      -> Errors . pure $ mkCrudForRefInterfaceError orig
                  _   -> pure()
              typeCheckViewAnnotation :: Expression -> Maybe String -> Guarded ()
              typeCheckViewAnnotation _       Nothing       = pure ()
              typeCheckViewAnnotation objExpr (Just viewId) =
                case lookupView viewId of 
                  Just vd -> let viewAnnCptStr = aConcToType $ target objExpr
                                 viewDefCptStr = pConcToType $ vd_cpt vd
                                 viewIsCompatible = viewAnnCptStr `isa` viewDefCptStr
                             in  if viewIsCompatible 
                                 then pure ()
                                 else Errors . pure $ 
                                        mkIncompatibleViewError objDef viewId viewAnnCptStr viewDefCptStr
                  Nothing -> Errors . pure $ mkUndeclaredError "view" objDef viewId
              obj crud (e,sr) s
                = ( BxExpr
                    ObjectDef { objnm = nm
                           , objpos = orig
                           , objExpression = e
                           , objcrud = crud
                           , objmView = mView
                           , objmsub = s
                           }, sr)
          P_BxTxt  { obj_nm  = nm
                , pos = orig
                , obj_txt = str
                } -> pure $ (BxTxt
                             BoxTxt { objnm = nm
                                    , objpos = orig
                                    , objtxt = str
                                    },True)

    pCruds2aCruds :: Expression -> Maybe P_Cruds -> Guarded Cruds
    pCruds2aCruds expr mCrud = 
       case mCrud of 
         Nothing -> mostLiberalCruds (Origin "Default for Cruds") ""
         Just pc@(P_Cruds org userCrudString )
             | (length . L.nub . map toUpper) userCrudString == length userCrudString &&
                all (`elem` "cCrRuUdD") userCrudString  
                         -> warnings pc $ mostLiberalCruds org userCrudString 
             | otherwise -> Errors . pure $ mkInvalidCRUDError org userCrudString
      where (defC, defR, defU, defD) = defaultCrud opts
            mostLiberalCruds :: Origin -> String -> Guarded Cruds
            mostLiberalCruds o str
             = pure Cruds { crudOrig = o
                          , crudC    = isFitForCrudC expr && f 'C' defC
                          , crudR    = isFitForCrudR expr && f 'R' defR
                          , crudU    = isFitForCrudU expr && f 'U' defU
                          , crudD    = isFitForCrudD expr && f 'D' defD
                          }
                   where
                     f :: Char -> Bool -> Bool 
                     f c def'
                      | toUpper c `elem` str = True
                      | toLower c `elem` str = False
                      | otherwise            = def'
            warnings :: P_Cruds -> Guarded Cruds -> Guarded Cruds
            warnings pc@(P_Cruds _ crd) aCruds = addWarnings warns aCruds
              where
                warns :: [Warning]
                warns = map (mkCrudWarning pc) $ 
                    [ 
                      [ "'C' was specified, but the expression "
                      , "  "++showA expr
                      , "doesn't allow for the creation of a new atom at its target concept ("++name (target expr)++") "
                      ] ++
                      [ "  HINT: You might want to use U(pdate), which updates the pair in the relation."
                      | isFitForCrudU expr, 'U' `notElem` crd
                      ]
                    | 'C' `elem` crd && not (isFitForCrudC expr)
                    ]++
                    [ [ "'R' was specified, but the expression "
                      , "  "++showA expr
                      , "doesn't allow for read of the pairs in that expression."
                      ]
                    | 'R' `elem` crd && not (isFitForCrudR expr)
                    ]++
                    [ [ "'U' was specified, but the expression "
                      , "  "++showA expr
                      , "doesn't allow to insert or delete pairs in it."
                      ]
                    | 'U' `elem` crd && not (isFitForCrudU expr)
                    ]++
                    [ [ "'D' was specified, but the expression "
                      , "  "++showA expr
                      , "doesn't allow for the deletion of an atom from its target concept ("++name (target expr)++") "
                      ]
                    | 'D' `elem` crd && not (isFitForCrudD expr)
                    ]
                    ++
                    [ [ "R(ead) is required to do U(pdate) or D(elete) "
                      , "however, you explicitly specified 'r'."
                      ]
                    | 'r' `elem` crd && ('U' `elem` crd || 'D' `elem` crd)
                    ]
    pSubi2aSubi :: ContextInfo
                -> Expression -- Expression of the surrounding
                -> Bool -- Whether the surrounding is bounded
                -> P_BoxItem a -- name of where the error occured!
                -> P_SubIfc (TermPrim, DisambPrim) -- Subinterface to check
                -> Guarded ( Expression -- In the case of a "Ref", we do not change the type of the subinterface with epsilons, this is to change the type of our surrounding instead. In the case of "Box", this is simply the original expression (in such a case, epsilons are added to the branches instead)
                           , SubInterface -- the subinterface
                           )
    pSubi2aSubi ci objExpr b o x
      = case x of
         P_InterfaceRef{si_str = ifcId} 
           ->  do (refIfcExpr,_) <- case lookupDisambIfcObj (declDisambMap ci) ifcId of
                                         Just disambObj -> typecheckTerm ci 
                                                                $ case disambObj of
                                                                             P_BxExpr{} -> obj_ctx disambObj -- term is type checked twice, but otherwise we need a more complicated type check method to access already-checked interfaces. TODO: hide possible duplicate errors in a nice way (that is: via CtxError)
                                                                             P_BxTxt {} -> fatal "TXT is not expected here."
                                         Nothing        -> Errors . pure $ mkUndeclaredError "interface" o ifcId
                  objExprEps <- typeCheckInterfaceRef o ifcId objExpr refIfcExpr
                  return (objExprEps,InterfaceRef{ siIsLink = si_isLink x
                                                 , siIfcId  = ifcId
                                                 }
                         )
         P_Box{}
           -> addWarnings warnings $
                       build <$> traverse (join . fmap fn . typecheckObjDef ci) l 
                             <*  uniqueNames l
                             <*  mustBeObject (target objExpr)
                  where l = si_box x
                        build :: [BoxItem] -> (Expression, SubInterface)
                        build lst = (objExpr,Box { siConcept = target objExpr
                                                 , siMClass  = si_class x
                                                 , siObjs    = lst
                                                 }
                                    )
                        fn :: (BoxItem, Bool) -> (Guarded BoxItem)
                        fn (BxExpr e,p) = fmap BxExpr $ matchWith (e,p)
                        fn (BxTxt t,_) = pure $ BxTxt t
                        mustBeObject :: A_Concept -> Guarded ()
                        mustBeObject cpt = case (representationOf ci) cpt of
                                             Object -> pure ()
                                             tt     -> Errors . pure $ mkSubInterfaceMustBeDefinedOnObject x cpt tt
     where matchWith :: (ObjectDef, Bool) -> (Guarded ObjectDef)
           matchWith (ojd,exprBound)
            = if b || exprBound then
                case userList$toList$ findExact genLattice (flType $ lMeet (target objExpr) (source . objExpression $ ojd)) of
                    [] -> mustBeOrderedLst x [(source (objExpression ojd),Src, aObjectDef2pObjectDef $ BxExpr ojd)]
                    (r:_) -> pure (ojd{objExpression=addEpsilonLeft genLattice r (objExpression ojd)})
              else mustBeBound (origin ojd) [(Src,objExpression ojd),(Tgt,objExpr)]
           warnings :: [Warning]
           warnings = [mkBOX_ROWSNH_Warning (origin x) | si_class x == Just "ROWSNH"] -- See issue #925
                    ++[mkNoBoxItemsWarning  (origin x) | null (si_box x)            ]
 
    typeCheckInterfaceRef :: P_BoxItem a -> String -> Expression -> Expression -> Guarded Expression
    typeCheckInterfaceRef objDef ifcRef objExpr ifcExpr = 
      let expTarget = target objExpr
          ifcSource = source ifcExpr
          refIsCompatible = expTarget `isaC` ifcSource || ifcSource `isaC` expTarget
      in  if refIsCompatible 
          then pure $ addEpsilonRight genLattice ifcSource objExpr 
          else Errors . pure $ mkIncompatibleInterfaceError objDef expTarget ifcSource ifcRef
    lookupDisambIfcObj :: DeclMap -> String -> Maybe (P_BoxItem (TermPrim, DisambPrim))
    lookupDisambIfcObj declMap ifcId =
      case [ disambObj | (vd,disambObj) <- p_interfaceAndDisambObjs declMap, ifc_Name vd == ifcId ] of
        []          -> Nothing
        disambObj:_ -> Just disambObj -- return the first one, if there are more, this is caught later on by uniqueness static check
    

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
        disambNamedRel (PNamedRel _ r Nothing)  = Map.elems $ findRels declMap r
        disambNamedRel (PNamedRel _ r (Just s)) = findRelsTyped declMap r $ pSign2aSign s

    pIfc2aIfc :: ContextInfo -> (P_Interface, P_BoxItem (TermPrim, DisambPrim)) -> Guarded Interface
    pIfc2aIfc declMap (pIfc, objDisamb) = 
       build $ pObjDefDisamb2aObjDef declMap objDisamb
      where 
         build :: Guarded BoxItem -> Guarded Interface
         build gb = 
           case gb of
             Errors x        -> Errors x
             Checked obj' ws -> 
                addWarnings ws $
                case obj' of
                  BxExpr o ->
                    case ttype . target . objExpression $ o of
                      Object -> 
                          pure Ifc { ifcIsAPI = ifc_IsAPI pIfc
                                  , ifcname = name pIfc 
                                  , ifcRoles = ifc_Roles pIfc
                                  , ifcObj = o
                                  , ifcControls = []  -- to be enriched in Adl2fSpec with rules to be checked
                                  , ifcPos = origin pIfc
                                  , ifcPrp = ifc_Prp pIfc
                                  }
                      tt -> Errors . pure
                            . mkInterfaceMustBeDefinedOnObject pIfc (target . objExpression $ o) $ tt
                  BxTxt _ -> fatal "Unexpected BxTxt"  --Interface should not have TXT only. it should have an expression object.     
         ttype :: A_Concept -> TType
         ttype = representationOf declMap

    pRoleRelation2aRoleRelation :: ContextInfo -> P_RoleRelation -> Guarded A_RoleRelation
    pRoleRelation2aRoleRelation ci prr
     = (\ ds' 
        -> RR { rrRoles = rr_Roles prr
              , rrRels  = ds'
              , rrPos   = origin prr
              }) <$> traverse (namedRel2Decl (declDisambMap ci)) (rr_Rels prr)
    pRoleRule2aRoleRule :: P_RoleRule -> A_RoleRule
    pRoleRule2aRoleRule prr
     = A_RoleRule { arRoles = mRoles prr
                  , arRules = mRules prr
                  , arPos   = origin prr
                  }
    
    pPat2aPat :: ContextInfo -> P_Pattern -> Guarded Pattern
    pPat2aPat ci ppat
     = f <$> traverse (pRul2aRul ci (Just $ name ppat)) (pt_rls ppat)
         <*> traverse (pIdentity2aIdentity ci (Just $ name ppat)) (pt_ids ppat) 
         <*> traverse (pPop2aPop ci) (pt_pop ppat)
         <*> traverse (pViewDef2aViewDef ci) (pt_vds ppat) 
         <*> traverse (pPurp2aPurp ci) (pt_xps ppat)
         <*> traverse (pDecl2aDecl (Just $ name ppat) deflangCtxt deffrmtCtxt) (pt_dcs ppat)
       where
        f rules' keys' pops' views' xpls relations
           = A_Pat { ptnm  = name ppat
                   , ptpos = origin ppat
                   , ptend = pt_end ppat
                   , ptrls = Set.fromList rules'
                   , ptgns = map pClassify2aClassify (pt_gns ppat)
                   , ptdcs = Set.fromList relations
                   , ptups = pops' 
                   , ptids = keys'
                   , ptvds = views'
                   , ptxps = xpls
                   }
    pRul2aRul :: ContextInfo -> Maybe String -- name of pattern the rule is defined in (if any)
              -> P_Rule TermPrim -> Guarded Rule
    pRul2aRul ci env = typeCheckRul ci env . disambiguate (termPrimDisAmb (declDisambMap ci))
    typeCheckRul :: ContextInfo -> 
                 Maybe String -- name of pattern the rule is defined in (if any)
              -> P_Rule (TermPrim, DisambPrim) -> Guarded Rule
    typeCheckRul ci env P_Ru { pos = orig
                             , rr_nm = nm
                             , rr_exp = expr
                             , rr_mean = meanings
                             , rr_msg = msgs
                             , rr_viol = viols
                             }
     = do (exp',_) <- typecheckTerm ci expr
          vls <- maybeOverGuarded (typeCheckPairView ci orig exp') viols
          return Ru { rrnm = nm
                    , formalExpression = exp'
                    , rrfps = orig
                    , rrmean = map (pMean2aMean deflangCtxt deffrmtCtxt) meanings
                    , rrmsg  = map (pMess2aMess deflangCtxt deffrmtCtxt) msgs
                    , rrviol = vls
                    , rrdcl = Nothing
                    , rrpat = env
                    , r_usr = UserDefined
                    , isSignal = not . null . filter (\x -> nm `elem` arRules x) $ allRoleRules 
                    }
    pIdentity2aIdentity ::
         ContextInfo -> Maybe String -- name of pattern the rule is defined in (if any)
      -> P_IdentDef -> Guarded IdentityDef
    pIdentity2aIdentity ci env pidt
     = case disambiguate (termPrimDisAmb (declDisambMap ci)) pidt of
           P_Id { ix_lbl = lbl
                , ix_ats = isegs
                } -> (\isegs' -> Id { idPos = orig
                                    , idLbl = lbl
                                    , idCpt = conc
                                    , idPat = env
                                    , identityAts = isegs'
                                    }) <$> traverse pIdentSegment2IdentSegment isegs
     where conc = pCpt2aCpt (ix_cpt pidt)
           orig = origin pidt
           pIdentSegment2IdentSegment :: P_IdentSegmnt (TermPrim, DisambPrim) -> Guarded IdentitySegment
           pIdentSegment2IdentSegment (P_IdentExp ojd) =
              do ob <- pObjDefDisamb2aObjDef ci ojd
                 case ob of
                   BxExpr o ->
                     case toList$ findExact genLattice $ aConcToType (source $ objExpression o) `lJoin` aConcToType conc of
                              [] -> mustBeOrdered orig (Src, origin ojd, objExpression o) pidt
                              _  -> pure $ IdentityExp o{objExpression = addEpsilonLeft genLattice conc (objExpression o)}
                   BxTxt t -> fatal $ "TXT is not expected in IDENT statements. ("++show (origin t)++")"
    typeCheckPairView :: ContextInfo -> Origin -> Expression -> PairView (Term (TermPrim, DisambPrim)) -> Guarded (PairView Expression)
    typeCheckPairView ci o x (PairView lst)
     = PairView <$> traverse (typeCheckPairViewSeg ci o x) lst
    typeCheckPairViewSeg :: ContextInfo -> Origin -> Expression -> PairViewSegment (Term (TermPrim, DisambPrim)) -> Guarded (PairViewSegment Expression)
    typeCheckPairViewSeg _ _ _ (PairViewText orig x) = pure (PairViewText orig x)
    typeCheckPairViewSeg ci o t (PairViewExp orig s x)
     = do (e,(b,_)) <- typecheckTerm ci x
          let tp = aConcToType (source e)
          case toList . findExact genLattice . lMeet tp $ getConcept s t of
                          [] -> mustBeOrdered o (Src, origin (fmap fst x), e) (s,t)
                          lst -> if b || elem (getConcept s t) lst then
                                    pure (PairViewExp orig s (addEpsilonLeft genLattice (getAConcept s t) e))
                                 else
                                    mustBeBound o [(Src, e)]
    pPurp2aPurp :: ContextInfo -> PPurpose -> Guarded Purpose
    pPurp2aPurp ci
                PRef2 { pos    = orig     -- :: Origin
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
       <$> pRefObj2aRefObj ci objref
    pRefObj2aRefObj :: ContextInfo -> PRef2Obj -> Guarded ExplObj
    pRefObj2aRefObj _       (PRef2ConceptDef  s ) = pure$ ExplConceptDef (lookupConceptDef s)
    pRefObj2aRefObj ci      (PRef2Relation tm)    = ExplRelation <$> namedRel2Decl (declDisambMap ci) tm
    pRefObj2aRefObj _       (PRef2Rule        s ) = pure$ ExplRule s
    pRefObj2aRefObj _       (PRef2IdentityDef s ) = pure$ ExplIdentityDef s
    pRefObj2aRefObj _       (PRef2ViewDef     s ) = pure$ ExplViewDef s
    pRefObj2aRefObj _       (PRef2Pattern     s ) = pure$ ExplPattern s
    pRefObj2aRefObj _       (PRef2Interface   s ) = pure$ ExplInterface s
    pRefObj2aRefObj _       (PRef2Context     s ) = pure$ ExplContext s
    lookupConceptDef :: String -> ConceptDef
    lookupConceptDef s
     = case filter (\cd -> name cd == s) allConceptDefs of
        []    -> Cd{pos=OriginUnknown, cdcpt=s, cddef="", cdref="", cdfrom=n1} 
        (x:_) -> x
    allConceptDefs :: [ConceptDef]
    allConceptDefs = p_conceptdefs++concatMap pt_cds p_patterns
    allRoleRules :: [A_RoleRule]
    allRoleRules = map pRoleRule2aRoleRule 
                      (p_roleRules ++ concatMap pt_RRuls p_patterns)

leastConcept :: Op1EqualitySystem Type -> A_Concept -> A_Concept -> A_Concept
leastConcept genLattice c str
     = case (aConcToType c `elem` leastConcepts, aConcToType str `elem` leastConcepts) of
         (True, _) -> c
         (_, True) -> str
         (_, _)    -> fatal ("Either "++name c++" or "++show str++" should be a subset of the other." )
       where
         leastConcepts = findExact genLattice (Atom (aConcToType c) `Meet` Atom (aConcToType str))

addEpsilonLeft,addEpsilonRight :: Op1EqualitySystem Type -> A_Concept -> Expression -> Expression
addEpsilonLeft genLattice a e
  = if a==source e then e else EEps (leastConcept genLattice (source e) a) (Sign a (source e)) .:. e
addEpsilonRight genLattice a e
  = if a==target e then e else e .:. EEps (leastConcept genLattice (target e) a) (Sign (target e) a)
addEpsilon :: Op1EqualitySystem Type -> A_Concept -> A_Concept -> Expression -> Expression
addEpsilon genLattice s t e
  = addEpsilonLeft genLattice s (addEpsilonRight genLattice t e)

typecheckTerm :: ContextInfo -> Term (TermPrim, DisambPrim) -> Guarded (Expression, (Bool, Bool))
typecheckTerm ci tct
 = case tct of
     Prim (t,v) -> join $ (\x -> case x of
        EMp1 s c -> const (x,(True,True)) <$> pAtomValue2aAtomValue (representationOf ci) c s
        _ -> return (x, case t of
                               PVee _ -> (False,False)
                               -- PI   _ -> (False,False) -- this line needs to be uncommented, but it causes too many problems in travis (scripts that turn out to be genuinely unbounded and ambiguous)
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
     PFlp _ a   -> (\(x,(s,t)) -> (EFlp x, (t,s))) <$> tt a
     PCpl _ a   -> (\(x,_) -> (ECpl x,(False,False))) <$> tt a
     PBrk _ e   -> first EBrk <$> tt e 
 where
  genLattice = gens_efficient ci
  o = origin (fmap fst tct)
  tt = typecheckTerm ci
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
     wrap (expr1,expr2) ((src,b1), (tgt,b2)) = (cbn (addEpsilon genLattice src tgt expr1) (addEpsilon genLattice src tgt expr2), (b1, b2))
  unary   cbn     tp e1      = wrap (fst e1) <$> deriv tp e1
    where
     wrap expr  ((src,b1), (tgt,b2))  = (cbn (addEpsilon genLattice src tgt expr), (b1, b2))
  binary' cbn preConcept tp side1 side2 e1 e2 = 
      do a <- deriv1 (fmap (resolve (e1,e2)) preConcept) 
         b <- deriv' tp (e1,e2)
         wrap (fst e1,fst e2) a b
    where
     wrap _ (_,False) ((_,b1), (_,b2))
      = mustBeBound o [(p,e) | (False,p,e)<-[(b1,side1,fst e1),(b2,side2,fst e2)]]
     wrap (expr1,expr2) (cpt,True) ((_,b1), (_,b2))
      = pure (cbn (lrDecide side1 expr1) (lrDecide side2 expr2), (b1, b2))
        where lrDecide side e = case side of Src -> addEpsilonLeft genLattice cpt e; Tgt -> addEpsilonRight genLattice cpt e
  deriv (t1,t2) es = (,) <$> deriv1 (fmap (resolve es) t1) <*> deriv1 (fmap (resolve es) t2)
  deriv1 :: TT (SrcOrTgt, (Expression, Bool)) -> Guarded (A_Concept, Bool)
  deriv1 x'
     = case x' of
        (MBE a@(p1,(e1,b1)) b@(p2,(e2,b2))) ->
             if (b1 && b2) || (getAConcept p1 e1 == getAConcept p2 e2) then (\x -> (x,b1||b2)) <$> getExactType lJoin (p1, e1) (p2, e2)
             else mustBeBound o [(p,e) | (p,(e,False))<-[a,b]]
        (MBG (p1,(e1,b1)) (p2,(e2,b2))) ->
             (\x -> (fst x,b1)) <$> getAndCheckType lJoin (p1, True, e1) (p2, b2, e2)
        (UNI (p1,(e1,b1)) (p2,(e2,b2))) ->
             (\x -> (fst x,b1 && b2)) <$> getAndCheckType lJoin (p1, b1, e1) (p2, b2, e2)
        (ISC (p1,(e1,b1)) (p2,(e2,b2))) ->
             (\(x,r) -> (x, (b1 && elem (getAConcept p1 e1) r) || (b2 && elem (getAConcept p2 e2) r) || (b1 && b2))
             ) <$> getAndCheckType lMeet (p1, b1, e1) (p2, b2, e2)
     where
      getExactType flf (p1,e1) (p2,e2)
       = case userList$toList$ findExact genLattice (flType$ flf (getAConcept p1 e1) (getAConcept p2 e2)) of
          [] -> mustBeOrdered o (p1,e1) (p2,e2)
          h:_ -> pure h
      getAndCheckType flf (p1,b1,e1) (p2,b2,e2)
       = case fmap (userList . toList)$toList$ findUpperbounds genLattice (flType$ flf (getAConcept p1 e1) (getAConcept p2 e2)) of -- note: we could have used GetOneGuarded, but this yields more specific error messages
          []  -> mustBeOrdered o (p1,e1) (p2,e2)
          [r@(h:_)]
              -> case (b1 || elem (getAConcept p1 e1) r,b2 || elem (getAConcept p2 e2) r ) of
                   (True,True) -> pure (h,r)
                   (a,b) -> mustBeBound o [(p,e) | (False,p,e)<-[(a,p1,e1),(b,p2,e2)]]
          lst -> mustBeOrderedConcLst o (p1,e1) (p2,e2) lst

pAtomPair2aAtomPair :: (A_Concept -> TType) -> Relation -> PAtomPair -> Guarded AAtomPair
pAtomPair2aAtomPair typ dcl pp = 
 mkAtomPair 
   <$> pAtomValue2aAtomValue typ (source dcl) (ppLeft  pp)
   <*> pAtomValue2aAtomValue typ (target dcl) (ppRight pp)

pAtomValue2aAtomValue :: (A_Concept -> TType) -> A_Concept -> PAtomValue -> Guarded AAtomValue
pAtomValue2aAtomValue typ cpt pav =
   case unsafePAtomVal2AtomValue ttyp (Just cpt) pav of
    Left msg -> Errors . pure $ mkIncompatibleAtomValueError pav msg
    Right av -> pure av
  where ttyp = typ cpt

pDecl2aDecl ::
     Maybe String   -- name of pattern the rule is defined in (if any)
  -> Lang           -- The default language
  -> PandocFormat   -- The default pandocFormat
  -> P_Relation -> Guarded Relation
pDecl2aDecl env defLanguage defFormat pd
 = let (prL:prM:prR:_) = dec_pragma pd ++ ["", "", ""]
       dcl = Relation
                 { decnm   = pack (dec_nm pd)
                 , decsgn  = decSign
                 , decprps = dec_prps pd
                 , decprps_calc = Nothing  --decprps_calc in an A_Context are still the user-defined only. prps are calculated in adl2fspec.
                 , decprL  = prL
                 , decprM  = prM
                 , decprR  = prR
                 , decMean = map (pMean2aMean defLanguage defFormat) (dec_Mean pd)
                 , decfpos = origin pd
                 , decusr  = True
                 , decpat  = env
                 , dechash = hash (dec_nm pd) `hashWithSalt` decSign
                 }
   in checkEndoProps >> pure dcl

 where
  decSign = pSign2aSign (dec_sign pd)
  checkEndoProps :: Guarded ()
  checkEndoProps
    | source decSign == target decSign
                = pure ()
    | Set.null xs
                = pure ()
    | otherwise = Errors . pure $ mkEndoPropertyError (origin pd) (Set.elems xs)
   where xs = Set.fromList [Prop,Sym,Asy,Trn,Rfx,Irf] `Set.intersection` dec_prps pd


pDisAmb2Expr :: (TermPrim, DisambPrim) -> Guarded Expression
pDisAmb2Expr (_,Known x) = pure x
pDisAmb2Expr (_,Rel [x]) = pure x
pDisAmb2Expr (o,dx)      = cannotDisambiguate o dx

pMean2aMean :: Lang           -- The default language
            -> PandocFormat   -- The default pandocFormat
            -> PMeaning -> Meaning
pMean2aMean defLanguage defFormat (PMeaning pmarkup)
 =  Meaning (pMarkup2aMarkup defLanguage defFormat pmarkup)
pMess2aMess :: Lang           -- The default language
            -> PandocFormat   -- The default pandocFormat
            -> PMessage -> Markup
pMess2aMess defLanguage defFormat (PMessage x) = pMarkup2aMarkup defLanguage defFormat x
pMarkup2aMarkup :: Lang           -- The default language
                -> PandocFormat   -- The default pandocFormat
                -> P_Markup -> Markup
pMarkup2aMarkup defLanguage defFormat
   P_Markup  { mLang   = ml
             , mFormat = mpdf
             , mString = str
             }
 = Markup { amLang = fromMaybe defLanguage ml -- The language is always defined; if not by the user, then by default.
          , amPandoc = string2Blocks (fromMaybe defFormat mpdf) str
          }

-- helpers for generating a lattice, not having to write `Atom' all the time
-- the l in lJoin and lMeet denotes the lattice.
lJoin,lMeet :: a -> a -> FreeLattice a
lJoin a b = Join (Atom a) (Atom b)
lMeet a b = Meet (Atom a) (Atom b)

flType :: FreeLattice A_Concept -> FreeLattice Type
flType = fmap aConcToType

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
  
getAConcept :: HasSignature a => SrcOrTgt -> a -> A_Concept
getAConcept Src = source
getAConcept Tgt = target
getConcept :: HasSignature a => SrcOrTgt -> a -> Type
getConcept Src = aConcToType . source
getConcept Tgt = aConcToType . target


-- | Left-biased choice on maybes
orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y

-- | getCycles returns a list of cycles in the edges list (each edge is a pair of a from-vertex
--   and a list of to-vertices)
getCycles :: Eq a => [(a, [a])] -> [[a]]
getCycles edges =
  let allVertices = L.nub . concat $ [ from : to | (from, to) <- edges ]
      keyFor v = fromMaybe (error "FATAL") $ L.elemIndex v allVertices
      graphEdges = [ (v, keyFor v , map keyFor vs)  | (v, vs) <- edges ]
  in  [ vs | CyclicSCC vs <- stronglyConnComp graphEdges ]
