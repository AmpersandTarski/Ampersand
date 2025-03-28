{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with fromMaybe" #-}

module Ampersand.ADL1.P2A_Converters
  ( pCtx2aCtx,
    pCpt2aCpt,
    ConceptMap,
    getConceptMap,
    pAtomValue2aAtomValue,
  )
where

import Ampersand.ADL1.Disambiguate (DisambPrim (..), disambiguate, orWhenEmpty, pCpt2aCpt)
-- used for type-checking
import Ampersand.ADL1.Lattices
import Ampersand.Basics hiding (conc, set)
import Ampersand.Classes
import Ampersand.Core.A2P_Converters
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec.ToFSpec.Populated (sortSpecific2Generic)
import Ampersand.Input.ADL1.CtxError
import Ampersand.Misc.HasClasses
import RIO.Char (toLower, toUpper)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import RIO.Time

pConcToType :: P_Concept -> Type
pConcToType P_ONE = BuiltIn TypeOfOne
pConcToType p = UserConcept (name p)

aConcToType :: A_Concept -> Type
aConcToType ONE = BuiltIn TypeOfOne
aConcToType p = UserConcept (fst . NE.head $ aliases p)

getAsConcept :: ConceptMap -> Origin -> Type -> Guarded A_Concept
getAsConcept cptMap o v = case typeOrConcept cptMap v of
  Right x -> unexpectedType o x
  Left x -> return x

userList :: ConceptMap -> [Type] -> [A_Concept]
userList fun = lefts . fmap (typeOrConcept fun)

mustBeConceptBecauseMath :: ConceptMap -> Type -> A_Concept
mustBeConceptBecauseMath cptMap tp =
  let fatalV :: a
      fatalV = fatal "A concept turned out to be a built-in type."
   in case getAsConcept cptMap fatalV tp of
        Checked v _ -> v
        _ -> fatalV

-- NOTE: Static checks like checkPurposes should ideally occur on the P-structure before type-checking, as it makes little
-- sense to do type checking when there are static errors. However, in Ampersand all collect functions (e.g. in ViewPoint)
-- only exist on the A-Structure, so we do it afterwards. Static purpose errors won't affect types, so in this case it is no problem.

-- Check whether all purposes refer to existing objects.
checkPurposes :: A_Context -> Guarded ()
checkPurposes ctx =
  let topLevelPurposes = ctxps ctx
      purposesInPatterns = concatMap ptxps (ctxpats ctx)
      allPurposes = topLevelPurposes <> purposesInPatterns
      danglingPurposes = filter (isDanglingPurpose ctx) allPurposes
   in case danglingPurposes of
        [] -> pure ()
        x : xs ->
          Errors
            $ mkDanglingPurposeError x
            NE.:| map mkDanglingPurposeError xs

-- Return True if the ExplObj in this Purpose does not exist.
isDanglingPurpose :: A_Context -> Purpose -> Bool
isDanglingPurpose ctx purp =
  case explObj purp of
    ExplConcept concDef -> let nm = name concDef in nm `notElem` map name (toList $ concs ctx)
    ExplRelation decl -> name decl `notElem` Set.map name (relsDefdIn ctx) -- is already covered by type checker
    ExplRule nm -> nm `notElem` map name (toList $ udefrules ctx)
    ExplIdentityDef nm -> nm `notElem` map name (identities ctx)
    ExplViewDef nm -> nm `notElem` map name (viewDefs ctx)
    ExplPattern nm -> nm `notElem` map name (ctxpats ctx)
    ExplInterface nm -> nm `notElem` map name (ctxifcs ctx)
    ExplContext nm ->
      ctxnm ctx
        /= nm
        && False -- HJO: This line is a workaround for the issue mentioned in https://github.com/AmpersandTarski/ampersand/issues/46
        -- TODO: fix this when we pick up working on multiple contexts.
        -- Check that interface references are not cyclic

checkInterfaceCycles :: A_Context -> Guarded ()
checkInterfaceCycles ctx =
  case interfaceCycles of
    [] -> return ()
    x : xs -> Errors $ fmap mkInterfaceRefCycleError (x NE.:| xs)
  where
    interfaceCycles :: [NE.NonEmpty Interface]
    interfaceCycles =
      map
        ( fmap lookupInterface
            . fromMaybe (fatal "Empty list of interfacenames is unexpected here.")
            . NE.nonEmpty
        )
        . getCycles
        $ refsPerInterface
    refsPerInterface :: [(Name, [Name])]
    refsPerInterface = [(name ifc, (map siIfcId . getDeepIfcRefs . ifcObj) ifc) | ifc <- ctxifcs ctx]
    lookupInterface :: Name -> Interface
    lookupInterface nm = case [ifc | ifc <- ctxifcs ctx, name ifc == nm] of
      [ifc] -> ifc
      _ -> fatal "Interface lookup returned zero or more than one result"

-- Check whether each concept has at most one default view
checkMultipleDefaultViews :: A_Context -> Guarded ()
checkMultipleDefaultViews ctx =
  case conceptsWithMultipleViews of
    [] -> return ()
    x : xs -> Errors $ fmap mkMultipleDefaultError (x NE.:| xs)
  where
    conceptsWithMultipleViews =
      filter (\x -> NE.length x > 1)
        . eqClass ((==) `on` vdcpt)
        . filter vdIsDefault
        $ ctxvs ctx

checkDanglingRulesInRuleRoles :: A_Context -> Guarded ()
checkDanglingRulesInRuleRoles ctx =
  case [ mkDanglingRefError "Rule" nm (arPos rr)
         | rr <- ctxrrules ctx,
           nm <- NE.toList $ arRules rr,
           nm `notElem` map name (toList $ allRules ctx)
       ] of
    [] -> return ()
    x : xs -> Errors (x NE.:| xs)

checkOtherAtomsInSessionConcept :: A_Context -> Guarded ()
checkOtherAtomsInSessionConcept ctx =
  case [ mkOtherAtomInSessionError atom
         | pop@ACptPopu {popcpt = cpt} <- ctxpopus ctx,
           isSESSION cpt,
           atom <- popas pop,
           -- SJC: I think we should not allow _SESSION in a POPULATION statement, as there is no current session at that time (_SESSION should only be allowed as Atom in expressions)
           not (_isPermittedSessionValue atom)
       ]
    <> [ mkOtherTupleInSessionError d pr
         | ARelPopu {popsrc = src, poptgt = tgt, popdcl = d, popps = ps} <- ctxpopus ctx,
           isSESSION src || isSESSION tgt,
           pr <- toList ps,
           (isSESSION src && not (_isPermittedSessionValue (apLeft pr)))
             || (isSESSION tgt && not (_isPermittedSessionValue (apRight pr)))
       ] of
    [] -> return ()
    x : xs -> Errors (x NE.:| xs)
  where
    _isPermittedSessionValue :: AAtomValue -> Bool
    _isPermittedSessionValue v@AAVString {} = aavtxt v == "_SESSION"
    _isPermittedSessionValue _ = False

warnCaseProblems :: A_Context -> Guarded ()
warnCaseProblems ctx = addWarnings warnings $ pure ()
  where
    warnings :: [Warning]
    warnings =
      warns (concs ctx)
        <> warns (relsDefdIn ctx)
    warns set =
      [ mkCaseProblemWarning x y
        | x <- toList set,
          y <- toList set,
          toUpperName x == toUpperName y,
          name x < name y
      ]
      where
        toUpperName = T.toUpper . fullName

pSign2aSign :: ConceptMap -> P_Sign -> Signature
pSign2aSign conceptmap (P_Sign src tgt) = Sign (conceptmap src) (pCpt2aCpt conceptmap tgt)

findRels :: DeclMap -> Name -> Map.Map Signature TExpression
findRels declMap x = Map.findWithDefault Map.empty x declMap -- get all relations with the same name as x

extractDecl :: TExpression -> Guarded TRelation
extractDecl (TEDcD _ r) = return r
extractDecl e = fatal $ "Expecting a declared relation, instead I found: " <> tshow e -- to fix: return an error via a (still to be made) function in CtxError

namedPRel2TRel :: ConceptMap -> DeclMap -> P_NamedRel -> Guarded TRelation
namedPRel2TRel _ declMap o@(PNamedRel _ r Nothing) = getOneExactly o (findDecls' declMap r) >>= extractDecl
namedPRel2TRel conceptmap declMap o@(PNamedRel _ r (Just s)) = getOneExactly o (findRelsTyped declMap r (pSign2aSign conceptmap s)) >>= extractDecl

findDecls' :: DeclMap -> Name -> [TExpression]
findDecls' declMap x = Map.elems (findRels declMap x)

findRelsLooselyTyped :: DeclMap -> Name -> Maybe A_Concept -> Maybe A_Concept -> [TExpression]
findRelsLooselyTyped declMap x (Just src) (Just tgt) =
  findRelsTyped declMap x (Sign src tgt)
    `orWhenEmpty` (findRelsLooselyTyped declMap x (Just src) Nothing `isct` findRelsLooselyTyped declMap x Nothing (Just tgt))
    `orWhenEmpty` (findRelsLooselyTyped declMap x (Just src) Nothing `unin` findRelsLooselyTyped declMap x Nothing (Just tgt))
    `orWhenEmpty` findDecls' declMap x
  where
    isct lsta lstb = [a | a <- lsta, a `elem` lstb]
    unin lsta lstb = L.nub (lsta <> lstb)
findRelsLooselyTyped declMap x Nothing Nothing = findDecls' declMap x
findRelsLooselyTyped declMap x (Just src) Nothing =
  [dcl | dcl <- findDecls' declMap x, source dcl == src]
    `orWhenEmpty` findDecls' declMap x
findRelsLooselyTyped declMap x Nothing (Just tgt) =
  [dcl | dcl <- findDecls' declMap x, target dcl == tgt]
    `orWhenEmpty` findDecls' declMap x

findDeclLooselyTyped ::
  DeclMap ->
  P_NamedRel ->
  Name ->
  Maybe A_Concept ->
  Maybe A_Concept ->
  Guarded TRelation
findDeclLooselyTyped declMap o x src tgt =
  getOneExactly (o, (src, tgt)) (findRelsLooselyTyped declMap x src tgt) >>= extractDecl

findRelsTyped :: DeclMap -> Name -> Signature -> [TExpression]
findRelsTyped declMap x tp = Map.findWithDefault [] tp (Map.map (: []) (findRels declMap x))

type DeclMap = Map.Map Name (Map.Map Signature TExpression)

getDeepIfcRefs :: ObjectDef  -> [SubInterface ]
getDeepIfcRefs objDef = case objmsub objDef of
  Nothing -> []
  Just si -> case si of
    InterfaceRef {} -> [si | not (siIsLink si)]
    Box {} -> concatMap getDeepIfcRefs [x | BxExpr x <- siObjs si]
getDeepTIfcRefs :: TObjectDef a -> [TSubInterface a]
getDeepTIfcRefs objDef = case tobjmsub objDef of
  Nothing -> []
  Just si -> case si of
    TInterfaceRef {} -> [si | not (tsiIsLink si)]
    TBox {} -> concatMap getDeepTIfcRefs [x | TBxExpr x <- tsiObjs si]

-- | this function contains all relation declarations and their given types.
getDeclMap :: P_Context -> Guarded DeclMap
getDeclMap ctx = do
  tRels <- allTRels ctx
  return $ Map.map groupOnTp (Map.fromListWith (<>) [(name d, [tExpressionOf d]) | d <- Set.toList tRels])
  where
    groupOnTp lst = Map.fromListWith const [(sign d, d) | d <- lst]
    tExpressionOf :: TRelation -> TExpression
    tExpressionOf d = TEDcD (Prim (PNamedR (namedRel d))) d
    namedRel :: TRelation -> P_NamedRel
    namedRel d =
      PNamedRel
        { p_nrnm = name d,
          p_mbSign = Just . aSign2pSign . sign $ d,
          pos = origin d
        }

allTRels :: P_Context -> Guarded (Set TRelation)
allTRels ctx = do
  trels <- traverse (pRel2tRel (getConceptMap ctx) Nothing (deflangCtxt ctx) (deffrmtCtxt ctx)) (ctx_ds ctx)
  prels <- traverse pRels (ctx_pats ctx)
  return . Set.fromList $ trels <> concat prels
  where
    pRels :: P_Pattern -> Guarded [TRelation]
    pRels p = traverse (pRel2tRel (getConceptMap ctx) (Just . name $ p) (deflangCtxt ctx) (deffrmtCtxt ctx)) (pt_dcs p)

deflangCtxt :: P_Context -> Lang
deflangCtxt = fromMaybe English . ctx_lang

deffrmtCtxt :: P_Context -> PandocFormat
deffrmtCtxt = fromMaybe ReST . ctx_markup

-- | pCtx2aCtx has three tasks:
-- 1. Disambiguate the structures.
--    Disambiguation means replacing every "TermPrim" (the parsed term) with the correct Expression (available through DisambPrim)
--    This is done by using the function "disambiguate" on the outer-most structure.
--    In order to do this, its data type must be polymorphic, as in "P_ViewSegmt a".
--    After parsing, the type has TermPrim for the type variable. In our example: "P_ViewSegmt TermPrim". Note that "type P_ViewSegment = P_ViewSegmt TermPrim".
--    After disambiguation, the type variable is (TermPrim, DisambPrim), as in "P_ViewSegmt (TermPrim, DisambPrim)"
-- 2. Typecheck the structures.
--    This changes the data-structure entirely, changing the P_ into the A_
--    A "Guarded" will be added on the outside, in order to catch both type errors and disambiguation errors.
--    Using the Applicative operations <$> and <*> causes these errors to be in parallel
-- 3. Check everything else on the A_-structure: interface references should not be cyclic, rules e.a. must have unique names, etc.
pCtx2aCtx ::
  (HasFSpecGenOpts env) =>
  env ->
  P_Context ->
  Guarded A_Context
pCtx2aCtx
  env
  ctx@PCtx
    { ctx_nm = n1,
      ctx_lbl = lbl,
      ctx_pos = n2,
      ctx_pats = p_patterns,
      ctx_rs = p_rules,
      ctx_ds = p_relations,
      ctx_cs = p_conceptdefs,
      ctx_ks = p_identdefs,
      ctx_rrules = p_roleRules,
      ctx_reprs = p_representations,
      ctx_vs = p_viewdefs,
      ctx_gs = p_gens,
      ctx_ifcs = p_interfaces,
      ctx_ps = p_purposes,
      ctx_pops = p_pops,
      ctx_metas = p_metas,
      ctx_enfs = p_enfs
    } =
    do
      contextInfo <- g_contextInfo -- the minimal amount of data needed to transform things from P-structure to A-structure.
      declMap <- getDeclMap ctx -- contains the relation declarations
      -- We start with the interfaces because we must harvest all concepts that need the technical type Object:
      tInterfaces <- traverse (pIfc2tIfc contextInfo) (p_interfaceAndDisambObjs declMap)
      --  Calculate the technical type of concepts. tTypeList has precisely one technical type for every concept in this context.
      ttypeInfo <- calcTechTypes contextInfo tInterfaces
      uniqueNames "pattern" p_patterns
      interfaces <- traverse (tIfc2aIfc ttypeInfo) tInterfaces
      pats <- traverse (pPat2aPat ttypeInfo contextInfo) p_patterns --  The patterns defined in this context
      uniqueNames "rule" $ p_rules <> concatMap pt_rls p_patterns
      rules <- traverse (pRul2aRul ttypeInfo contextInfo Nothing) p_rules --  All user defined rules in this context, but outside patterns
      uniqueNames "identity definition" $ p_identdefs <> concatMap pt_ids p_patterns
      identdefs <- traverse (pIdentity2aIdentity ttypeInfo contextInfo Nothing) p_identdefs --  The identity definitions defined in this context, outside the scope of patterns
      uniqueNames "view definition" $ p_viewdefs <> concatMap pt_vds p_patterns
      viewdefs <- traverse (pViewDef2aViewDef ttypeInfo contextInfo) p_viewdefs --  The view definitions defined in this context, outside the scope of patterns
      uniqueNames "interface" p_interfaces
      purposes <- traverse (pPurp2aPurp ttypeInfo contextInfo) p_purposes --  The purposes of objects defined in this context, outside the scope of patterns
      udpops <- traverse (pPop2aPop ttypeInfo contextInfo) p_pops --  [Population]
      relations <- traverse (pDecl2aDecl ttypeInfo cptMap Nothing (deflangCtxt ctx) (deffrmtCtxt ctx)) p_relations
      enforces' <- traverse (pEnforce2aEnforce ttypeInfo contextInfo Nothing) p_enfs
      let actx =
            ACtx
              { ctxnm = n1,
                ctxlbl = lbl,
                ctxpos = n2,
                ctxlang = deflangCtxt ctx,
                ctxmarkup = deffrmtCtxt ctx,
                ctxpats = pats,
                ctxrs = Set.fromList rules,
                ctxds = Set.fromList relations,
                ctxpopus = udpops, -- the content is copied from p_pops
                ctxcdsOutPats = allConceptDefsOutPats,
                ctxcds = allConceptDefs,
                ctxks = identdefs,
                ctxrrules = udefRoleRules',
                ctxreprs = ttypeInfo,
                ctxvs = viewdefs,
                ctxgs = mapMaybe pClassify2aClassify p_gens,
                ctxgenconcs = (fmap . userList) (getConceptMap ctx) (concGroups <> map (: []) (Set.toList $ soloConcs contextInfo)),
                ctxifcs = interfaces,
                ctxps = purposes,
                ctxmetas = p_metas,
                ctxInfo = contextInfo,
                ctxEnforces = enforces'
              }
      checkTTypeOfSubInterfaces
      checkOtherAtomsInSessionConcept actx
      checkPurposes actx -- Check whether all purposes refer to existing objects
      checkDanglingRulesInRuleRoles actx -- Check whether all rules in MAINTAIN statements are declared
      checkInterfaceCycles actx -- Check that interface references are not cyclic
      checkMultipleDefaultViews actx -- Check whether each concept has at most one default view
      warnCaseProblems actx -- Warn if there are problems with the casing of names of relations and/or concepts
      return actx
    where
      cptMap = getConceptMap ctx
      calcTechTypes :: ContextInfo -> [TInterface a] -> Guarded TTypeInfo
      calcTechTypes ci interfaces = do
        -- The TType of concepts is calculated based on the following rules:
        -- 1. Concepts that are explicitly declared in a REPRESENT statement must have the technical type as declared in the REPRESENT statement.
        -- 2. Concepts that are implicitly declared as key in (sub-)interfaces must have Object as their technical type.
        -- 3. All concepts in the same typology must have the same technical type.
        -- 4. Concepts for which the technical type cannot be derived by above rules must have the technical type Alphanumeric.
        let group1, group2 :: [(A_Concept, TType, Origin, Bool)]
            group1 =
              [ (cptMap cpt, reprdom repr, origin repr, True)
                | repr <- reprList ci,
                  cpt <- NE.toList . reprcpts $ repr
              ]
            group2 =
              L.nub
                $ [ ( target . tobjExpression . tifcObj $ ifc,
                      Object,
                      origin ifc,
                      False
                    )
                    | ifc <- interfaces
                  ]
                <> [ (tsiConcept si, Object, origin si, False)
                     | si <- concatMap (getDeepTIfcRefs . tifcObj) interfaces
                   ]
            extractConcept :: NonEmpty (A_Concept, TType, Origin, Bool) -> (A_Concept, NonEmpty (TType, Origin, Bool))
            extractConcept xs = (fst4 . NE.head $ xs, fmap removefst4 xs)
              where
                removefst4 :: (a, b, c, d) -> (b, c, d)
                removefst4 (_a, b, c, d) = (b, c, d)
        -- for all concepts of group1 and group2, check whether they have a unique technical type:
        group1and2Types <- (traverse (uncurry checkMultipleTTypesOfConcept . extractConcept) . NE.groupWith fst4) $ group1 <> group2
        let conceptsInTypology :: Typology -> [A_Concept]
            conceptsInTypology typology = tyroot typology : tyCpts typology
            checkTTypesOfTypology :: Typology -> Guarded [(A_Concept, TType)]
            checkTTypesOfTypology typology =
              case NE.groupWith snd4 . L.filter belongsToTypology $ group1 <> group2 of
                [] -> pure [] -- none of the concepts in the typology have a technical type
                [x] -> pure . map (,snd4 . NE.head $ x) $ conceptsInTypology typology -- all allready typed concepts in the typology have the same technical type
                h : tl -> mkMultipleTypesInTypologyError . join $ h NE.:| tl
              where
                belongsToTypology :: (A_Concept, TType, Origin, Bool) -> Bool
                belongsToTypology quadruple = fst4 quadruple `elem` conceptsInTypology typology
        group3Types <- traverse checkTTypesOfTypology . multiKernels $ ci
        let knownTypes = L.nub $ group1and2Types <> concat group3Types
        pure
          $ \c ->
            if c == ONE || tshow c == "SESSION"
              then Object
              else case [tt | (cpt, tt) <- knownTypes, cpt == c] of
                [tt] -> tt
                [] -> Alphanumeric
                _ -> fatal ("There are multiple technical types for concept " <> tshow c <> ".")

      concGroups = getGroups genLatticeIncomplete :: [[Type]]
      g_contextInfo :: Guarded ContextInfo
      g_contextInfo = do
        multitypologies <- traverse mkTypology connectConcepts
        tRels <- allTRels ctx
        declMap <- getDeclMap ctx
        let allConcepts = Set.unions [sources, targets, concs gns]
              where
                sources = Set.fromList . map source . Set.toList $ tRels
                targets = Set.fromList . map target . Set.toList $ tRels
        return
          ( CI
              { ctxiGens = gns,
                connectedConcepts = connectConcepts,
                multiKernels = multitypologies,
                reprList = p_representations <> concatMap pt_Reprs p_patterns,
                declDisambMap = declMap,
                soloConcs =
                  Set.filter (not . isInSystem genLattice)
                    . Set.fromList
                    . fmap aConcToType
                    . Set.toList
                    $ allConcepts,
                allConcs = allConcepts,
                gens_efficient = genLattice,
                defaultLang = deflangCtxt ctx,
                defaultFormat = deffrmtCtxt ctx
              }
          )
        where
          gns = mapMaybe pClassify2aClassify $ allGens ctx
          connectConcepts :: [[A_Concept]] -- a partitioning of all A_Concepts where every two connected concepts are in the same partition.
          connectConcepts = connect [] (map (toList . concs) gns)

          -- mkTypeMap :: [[A_Concept]] -> [Representation] -> Guarded [(A_Concept, TType)]
          -- mkTypeMap groups reprs =
          --   f
          --     <$> traverse typeOfGroup groups
          --     <*> traverse typeOfSingle [c | c <- conceptsOfReprs, c `notElem` conceptsOfGroups]
          --   where
          --     f :: [[(A_Concept, TType)]] -> [Maybe (A_Concept, TType, [Origin])] -> [(A_Concept, TType)]
          --     f typesOfGroups typesOfOthers =
          --       concat typesOfGroups <> [(cpt, t) | (cpt, t, _) <- catMaybes typesOfOthers]
          --     reprTrios :: [(A_Concept, TType, Origin)]
          --     reprTrios = nubTrios $ concatMap toReprs reprs
          --       where
          --         toReprs :: Representation -> [(A_Concept, TType, Origin)]
          --         toReprs r = [(pCpt2aCpt cptMap cpt, reprdom r, origin r) | cpt <- NE.toList $ reprcpts r]
          --         nubTrios :: [(A_Concept, TType, Origin)] -> [(A_Concept, TType, Origin)]
          --         nubTrios = map withNonFuzzyOrigin . NE.groupBy groupCondition
          --           where
          --             withNonFuzzyOrigin :: NE.NonEmpty (A_Concept, TType, Origin) -> (A_Concept, TType, Origin)
          --             withNonFuzzyOrigin xs = case NE.filter (not . isFuzzyOrigin . thd3) xs of
          --               [] -> NE.head xs
          --               h : _ -> h
          --             groupCondition :: (A_Concept, TType, Origin) -> (A_Concept, TType, Origin) -> Bool
          --             groupCondition (cptA, typA, _) (cptB, typB, _) = cptA == cptB && typA == typB
          --     conceptsOfGroups :: [A_Concept]
          --     conceptsOfGroups = L.nub (concat groups)
          --     conceptsOfReprs :: [A_Concept]
          --     conceptsOfReprs = L.nub $ map fstOf3 reprTrios
          --       where
          --         fstOf3 (cpt, _, _) = cpt
          --     typeOfSingle :: A_Concept -> Guarded (Maybe (A_Concept, TType, [Origin]))
          --     typeOfSingle cpt =
          --       case filter ofCpt reprTrios of
          --         [] -> pure Nothing
          --         rs -> case L.nub (map getTType rs) of
          --           [] -> fatal "Impossible empty list."
          --           [t] -> pure (Just (cpt, t, map getOrigin rs))
          --           _ -> checkMultipleTTypesOfConcept cpt lst
          --             where
          --               lst = [(t, o) | (_, t, o) <- rs]
          --       where
          --         ofCpt :: (A_Concept, TType, Origin) -> Bool
          --         ofCpt (cpt', _, _) = cpt == cpt'
          --         getOrigin :: (A_Concept, TType, Origin) -> Origin
          --         getOrigin (_, _, o) = o
          --     getTType :: (a, TType, b) -> TType
          --     getTType (_, t, _) = t
          --     typeOfGroup :: [A_Concept] -> Guarded [(A_Concept, TType)]
          --     typeOfGroup grp =
          --       do
          --         singleTypes <- traverse typeOfSingle grp
          --         let typeList = catMaybes singleTypes
          --         case L.nub (map getTType typeList) of
          --           [] -> pure []
          --           [t] -> pure [(cpt, t) | cpt <- grp]
          --           _ -> mkMultipleTypesInTypologyError typeList
          connect :: [[A_Concept]] -> [[A_Concept]] -> [[A_Concept]]
          connect typols gss =
            case gss of
              [] -> typols
              x : xs -> connect (t : typols) rest
                where
                  (t, rest) = g' x xs
                  g' a as = case L.partition (disjoint a) as of
                    (_, []) -> (a, as)
                    (hs', hs) -> g' (L.nub $ a <> concat hs) hs'
                  disjoint :: (Eq a) => [a] -> [a] -> Bool
                  disjoint ys = null . L.intersect ys

          mkTypology :: [A_Concept] -> Guarded Typology
          mkTypology cs =
            case filter (not . isSpecific) cs of
              [] -> fatal "Every typology must have at least one specific concept."
              -- When this fatal occurs, there is something wrong with detecting cycles in the p-structure.
              [r] ->
                pure
                  Typology
                    { tyroot = r,
                      tyCpts = reverse . sortSpecific2Generic gns $ cs
                    }
              rs -> mkMultipleRootsError rs
                $ case filter isInvolved gns of
                  [] -> fatal "No involved gens"
                  x : xs -> x NE.:| xs
            where
              isSpecific :: A_Concept -> Bool
              isSpecific cpt = cpt `elem` map genspc (filter (not . isTrivial) gns)
                where
                  isTrivial g =
                    case g of
                      Isa {} -> gengen g == genspc g
                      IsE {} -> genrhs g == genspc g NE.:| []
              isInvolved :: AClassify -> Bool
              isInvolved gn = not . null $ concs gn `Set.intersection` Set.fromList cs

      p_interfaceAndDisambObjs :: DeclMap -> [(P_Interface, P_BoxItem (TermPrim, DisambPrim))]
      p_interfaceAndDisambObjs declMap =
        [ (ifc, disambiguate cptMap (termPrimDisAmb cptMap declMap) $ ifc_Obj ifc)
          | ifc <- p_interfaces
        ]
      tIfc2aIfc :: TTypeInfo -> TInterface (TermPrim, DisambPrim) -> Guarded Interface
      tIfc2aIfc = undefined
      -- story about genRules and genLattice
      -- the genRules is a list of equalities between concept sets, in which every set is interpreted as a conjunction of concepts
      -- the genLattice is the resulting optimized structure
      genRules :: [(Set.Set Type, Set.Set Type)] -- SJ: Why not [(NE.NonEmpty Type, NE.NonEmpty Type)] ?
      genRules =
        [ ( Set.fromList [pConcToType . specific $ x],
            Set.fromList . NE.toList . NE.map pConcToType . generics $ x
          )
          | x <- allGens ctx
        ]

      completeRules =
        genRules
          <> [ (Set.singleton (userConcept cpt), Set.fromList [BuiltIn (reprdom x), userConcept cpt])
               | x <- p_representations <> concatMap pt_Reprs p_patterns,
                 cpt <- NE.toList $ reprcpts x
             ]
          <> [ ( Set.singleton RepresentSeparator,
                 Set.fromList
                   [ BuiltIn Alphanumeric,
                     BuiltIn BigAlphanumeric,
                     BuiltIn HugeAlphanumeric,
                     BuiltIn Password,
                     BuiltIn Binary,
                     BuiltIn BigBinary,
                     BuiltIn HugeBinary,
                     BuiltIn Date,
                     BuiltIn DateTime,
                     BuiltIn Boolean,
                     BuiltIn Integer,
                     BuiltIn Float,
                     -- , BuiltIn TypeOfOne -- not a valid way to represent something! Also treated differently in this code
                     BuiltIn Object,
                     RepresentSeparator
                   ]
               )
             ]
      genLatticeIncomplete :: Op1EqualitySystem Type -- used to derive the concept groups
      genLatticeIncomplete = optimize1 (foldr addEquality emptySystem genRules)
      genLattice :: Op1EqualitySystem Type
      genLattice = optimize1 (foldr addEquality emptySystem completeRules)

      pClassify2aClassify :: PClassify -> Maybe AClassify
      pClassify2aClassify pg =
        case NE.tail (generics pg) of
          [] -> case filter (/= specCpt) [pCpt2aCpt cptMap . NE.head $ generics pg] of
            [] -> Nothing
            h : _ ->
              Just
                Isa
                  { genpos = origin pg,
                    gengen = h,
                    genspc = specCpt
                  }
          _ -> case NE.filter (/= specCpt) . fmap (pCpt2aCpt cptMap) $ generics pg of
            [] -> Nothing
            h : tl ->
              Just
                IsE
                  { genpos = origin pg,
                    genrhs = h NE.:| tl,
                    genspc = specCpt
                  }
        where
          specCpt = pCpt2aCpt cptMap $ specific pg
      userConcept :: P_Concept -> Type
      userConcept P_ONE = BuiltIn TypeOfOne
      userConcept (PCpt nm) = UserConcept nm
      pPop2aPop :: TTypeInfo -> ContextInfo -> P_Population -> Guarded Population
      pPop2aPop ti ci pop =
        case pop of
          P_RelPopu {p_nmdr = nmdr, p_popps = aps, p_src = src, p_tgt = tgt} ->
            do
              trel <- case p_mbSign nmdr of
                Nothing -> findDeclLooselyTyped (declDisambMap ci) nmdr (name nmdr) (cptMap <$> src) (cptMap <$> tgt)
                _ -> namedPRel2TRel cptMap (declDisambMap ci) nmdr
              rel <- tRel2aRel ti trel
              aps' <- traverse (pAtomPair2aAtomPair ti trel) aps
              src' <- maybeOverGuarded (getAsConcept cptMap (origin pop) <=< (isMoreGeneric (origin pop) trel Src . userConcept)) src
              tgt' <- maybeOverGuarded (getAsConcept cptMap (origin pop) <=< (isMoreGeneric (origin pop) trel Tgt . userConcept)) tgt
              return
                ARelPopu
                  { popdcl = rel,
                    popps = Set.fromList aps',
                    popsrc = fromMaybe (source trel) src',
                    poptgt = fromMaybe (target trel) tgt'
                  }
          P_CptPopu {} ->
            let cpt = pCpt2aCpt cptMap (p_cpt pop)
             in ( \vals ->
                    ACptPopu
                      { popcpt = cpt,
                        popas = vals
                      }
                )
                  <$> traverse (pAtomValue2aAtomValue (ti cpt) cpt) (p_popas pop)
      isMoreGeneric :: Origin -> TRelation -> SrcOrTgt -> Type -> Guarded Type
      isMoreGeneric o dcl sourceOrTarget givenType =
        if givenType `elem` findExact genLattice (Atom (getConcept sourceOrTarget dcl) `Meet` Atom givenType)
          then pure givenType
          else mkTypeMismatchError o dcl sourceOrTarget givenType

      pBoxItemDisamb2TBoxItem :: ContextInfo -> P_BoxItem (TermPrim, DisambPrim) -> Guarded (TBoxItem (TermPrim, DisambPrim))
      pBoxItemDisamb2TBoxItem ci x = fmap fst (typecheckObjDef ci x)

      pViewDef2aViewDef :: TTypeInfo -> ContextInfo -> P_ViewDef -> Guarded ViewDef
      pViewDef2aViewDef ti ci x = typecheckViewDef ti ci tpda
        where
          tpda = disambiguate cptMap (termPrimDisAmb cptMap (declDisambMap ci)) x

      typecheckViewDef :: TTypeInfo -> ContextInfo -> P_ViewD (TermPrim, DisambPrim) -> Guarded ViewDef
      typecheckViewDef
        ti
        ci
        o@P_Vd
          { pos = orig,
            vd_nm = nm,
            vd_label = lbl',
            vd_cpt = cpt,
            vd_isDefault = isDefault,
            vd_html = mHtml,
            vd_ats = segmnts
          } =
          do
            segments <- traverse typeCheckViewSegment (zip [0 ..] segmnts)
            uniqueLables orig toLabel . filter hasLabel $ segments
            let avd =
                  Vd
                    { vdpos = orig,
                      vdname = nm,
                      vdlabel = lbl',
                      vdcpt = pCpt2aCpt cptMap cpt,
                      vdIsDefault = isDefault,
                      vdhtml = mHtml,
                      vdats = segments
                    }
            return avd
          where
            toLabel :: ViewSegment -> Text1
            toLabel vs = case vsmlabel vs of
              Nothing -> fatal "Segments without a label should have been filtered out here"
              Just x -> x
            hasLabel :: ViewSegment -> Bool
            hasLabel = isJust . vsmlabel
            typeCheckViewSegment :: (Integer, P_ViewSegment (TermPrim, DisambPrim)) -> Guarded ViewSegment
            typeCheckViewSegment (seqNr, seg) =
              do
                payload <- typecheckPayload (vsm_load seg)
                return
                  ViewSegment
                    { vsmpos = origin seg,
                      vsmlabel = vsm_labl seg,
                      vsmSeqNr = seqNr,
                      vsmLoad = payload
                    }
              where
                typecheckPayload :: P_ViewSegmtPayLoad (TermPrim, DisambPrim) -> Guarded ViewSegmentPayLoad
                typecheckPayload payload =
                  case payload of
                    P_ViewExp term ->
                      do
                        (tviewExpr, (srcBounded, _)) <- typecheckTerm cptMap ci term
                        viewExpr <- tExpr2aExpr ti tviewExpr
                        case userList cptMap . toList $ findExact genLattice (flType $ lMeet c (source viewExpr)) of
                          [] -> mustBeOrdered (origin o) o (Src, source viewExpr, viewExpr)
                          r@(h : _) -> do
                            if srcBounded || c `elem` r
                              then pure (ViewExp (addEpsilonLeft genLattice h viewExpr))
                              else mustBeBound (origin seg) [(Tgt, tviewExpr)]
                    P_ViewText str -> pure $ ViewText str
            c = mustBeConceptBecauseMath cptMap (pConcToType (vd_cpt o))

      isa :: Type -> Type -> Bool
      isa c1 c2 = c1 `elem` findExact genLattice (Atom c1 `Meet` Atom c2) -- shouldn't this Atom be called a Concept? SJC: Answer: we're using the constructor "Atom" in the lattice sense, not in the relation-algebra sense. c1 and c2 are indeed Concepts here
      isaC :: A_Concept -> A_Concept -> Bool
      isaC c1 c2 = aConcToType c1 `elem` findExact genLattice (Atom (aConcToType c1) `Meet` Atom (aConcToType c2))

      typecheckObjDef :: ContextInfo -> P_BoxItem (TermPrim, DisambPrim) -> Guarded (TBoxItem (TermPrim, DisambPrim), Bool)
      typecheckObjDef ci objDef =
        case objDef of
          P_BoxItemTerm
            { obj_PlainName = nm,
              obj_lbl = lbl',
              pos = orig,
              obj_term = objTerm,
              obj_crud = mCrud,
              obj_mView = mView,
              obj_msub = subs
            } -> do
              (objExpr, (srcBounded, tgtBounded)) <- typecheckTerm cptMap ci objTerm
              checkCrud
              crud <- pCruds2aCruds objExpr mCrud
              maybeObj <- maybeOverGuarded (pSubi2tSubi ci objExpr tgtBounded objDef) subs <* typeCheckViewAnnotation objExpr mView
              case maybeObj of
                Just (newExpr, subStructures) -> return (obj crud (newExpr, srcBounded) (Just subStructures))
                Nothing -> return (obj crud (objExpr, srcBounded) Nothing)
              where
                lookupView :: Name -> Maybe P_ViewDef
                lookupView viewId = case [vd | vd <- p_viewdefs, vd_nm vd == viewId] of
                  [] -> Nothing
                  vd : _ -> Just vd -- return the first one, if there are more, this is caught later on by uniqueness static check
                checkCrud :: Guarded ()
                checkCrud =
                  case (mCrud, subs) of
                    (Just _, Just P_InterfaceRef {si_isLink = False}) ->
                      Errors . pure $ mkCrudForRefInterfaceError orig
                    _ -> pure ()
                typeCheckViewAnnotation :: TExpression -> Maybe Name -> Guarded ()
                typeCheckViewAnnotation _ Nothing = pure ()
                typeCheckViewAnnotation objExpr (Just viewId) =
                  case lookupView viewId of
                    Just vd ->
                      let viewAnnCptStr = aConcToType $ target objExpr
                          viewDefCptStr = pConcToType $ vd_cpt vd
                          viewIsCompatible = viewAnnCptStr `isa` viewDefCptStr
                       in if viewIsCompatible
                            then pure ()
                            else
                              Errors
                                . pure
                                $ mkIncompatibleViewError objDef viewId viewAnnCptStr viewDefCptStr
                    Nothing -> Errors . pure $ mkUndeclaredError "view" objDef viewId
                obj :: Cruds -> (TExpression, b) -> Maybe (TSubInterface a) -> (TBoxItem a, b)
                obj crud (e, sr) s =
                  ( TBxExpr
                      TObjectDef
                        { tobjPlainName = nm,
                          tobjlbl = lbl',
                          tobjPos = orig,
                          tobjExpression = e,
                          tobjcrud = crud,
                          tobjmView = mView,
                          tobjmsub = s
                        },
                    sr
                  )
          P_BxTxt
            { obj_PlainName = nm,
              pos = orig,
              box_txt = str
            } ->
              pure
                ( TBxText
                    { tboxPlainName = nm,
                      tboxpos = orig,
                      tboxtxt = str
                    },
                  True
                )

      pCruds2aCruds :: TExpression -> Maybe P_Cruds -> Guarded Cruds
      pCruds2aCruds expr mCrud =
        case mCrud of
          Nothing -> mostLiberalCruds (Origin "Default for Cruds") Nothing
          Just pc@(P_Cruds org userCrud)
            | (length . L.nub . map toUpper) userCrudString
                == length userCrudString
                && all isValidChar userCrudString ->
                warnings pc $ mostLiberalCruds org (Just userCrud)
            | otherwise -> Errors . pure $ mkInvalidCRUDError org userCrud
            where
              userCrudString = T.unpack . text1ToText $ userCrud
        where
          isValidChar :: Char -> Bool
          isValidChar c = toUpper c `elem` ['C', 'R', 'U', 'D']
          (defC, defR, defU, defD) = view defaultCrudL env
          mostLiberalCruds :: Origin -> Maybe Text1 -> Guarded Cruds
          mostLiberalCruds o str =
            pure
              Cruds
                { crudOrig = o,
                  crudC = isFitForCrudC expr && f 'C' defC,
                  crudR = isFitForCrudR expr && f 'R' defR,
                  crudU = isFitForCrudU expr && f 'U' defU,
                  crudD = isFitForCrudD expr && f 'D' defD
                }
            where
              f :: Char -> Bool -> Bool
              f c def'
                | toUpper c `elem` T.unpack (maybe "" text1ToText str) = True
                | toLower c `elem` T.unpack (maybe "" text1ToText str) = False
                | otherwise = def'
          warnings :: P_Cruds -> Guarded Cruds -> Guarded Cruds
          warnings pc@(P_Cruds _ crd1) = addWarnings warns
            where
              crd = text1ToText crd1
              warns :: [Warning]
              warns =
                map (mkCrudWarning pc)
                  $ [ [ "'C' was specified, but the term ",
                        "  " <> showA expr,
                        "doesn't allow for the creation of a new atom at its target concept (" <> (fullName . target) expr <> ") "
                      ]
                        <> [ "  HINT: You might want to use U(pdate), which updates the pair in the relation."
                             | isFitForCrudU expr,
                               'U' `notElem` T.unpack crd
                           ]
                      | 'C' `elem` T.unpack crd && not (isFitForCrudC expr)
                    ]
                  <> [ [ "'R' was specified, but the term ",
                         "  " <> showA expr,
                         "doesn't allow for read of the pairs in that term."
                       ]
                       | 'R' `elem` T.unpack crd && not (isFitForCrudR expr)
                     ]
                  <> [ [ "'U' was specified, but the term ",
                         "  " <> showA expr,
                         "doesn't allow to insert or delete pairs in it."
                       ]
                       | 'U' `elem` T.unpack crd && not (isFitForCrudU expr)
                     ]
                  <> [ [ "'D' was specified, but the term ",
                         "  " <> showA expr,
                         "doesn't allow for the deletion of an atom from its target concept (" <> (fullName . target) expr <> ") "
                       ]
                       | 'D' `elem` T.unpack crd && not (isFitForCrudD expr)
                     ]
                  <> [ [ "R(ead) is required to do U(pdate) or D(elete) ",
                         "however, you explicitly specified 'r'."
                       ]
                       | 'r' `elem` T.unpack crd && ('U' `elem` T.unpack crd || 'D' `elem` T.unpack crd)
                     ]
      pSubi2tSubi ::
        ContextInfo ->
        TExpression -> -- Expression of the surrounding
        Bool -> -- Whether the surrounding is bounded
        P_BoxItem (TermPrim, DisambPrim) -> -- name of where the error occured!
        P_SubIfc (TermPrim, DisambPrim) -> -- Subinterface to check
        Guarded
          ( TExpression, -- In the case of a "Ref", we do not change the type of the subinterface with epsilons, this is to change the type of our surrounding instead. In the case of "Box", this is simply the original term (in such a case, epsilons are added to the branches instead)
            TSubInterface (TermPrim, DisambPrim) -- the subinterface
          )
      pSubi2tSubi ci objExpr b o x =
        case x of
          P_InterfaceRef {si_str = ifcId} ->
            do
              (refIfcExpr, _) <- case lookupDisambIfcObj (declDisambMap ci) ifcId of
                Just disambObj -> typecheckTerm cptMap ci
                  $ case disambObj of
                    P_BoxItemTerm {} -> obj_term disambObj -- term is type checked twice, but otherwise we need a more complicated type check method to access already-checked interfaces. TODO: hide possible duplicate errors in a nice way (that is: via CtxError)
                    P_BxTxt {} -> fatal "TXT is not expected here."
                Nothing -> Errors . pure $ mkUndeclaredError "interface" o ifcId
              objExprEps <- typeCheckInterfaceRef o ifcId objExpr refIfcExpr
              return
                ( objExprEps,
                  TInterfaceRef
                    { pos = origin x,
                      tsiConcept = target objExpr,
                      tsiIsLink = si_isLink x,
                      tsiIfcId = ifcId
                    }
                )
          P_Box {} ->
            addWarnings warnings
              $ build
              <$> traverse (fn <=< typecheckObjDef ci) l
              <* uniqueLables (origin x) tkkey (btKeys . si_header $ x)
              <* (uniqueLables (origin x) toNonEmptyLabel . filter hasLabel $ l) -- ensure that each label in a box has a unique name.
            where
              toNonEmptyLabel :: P_BoxItem a -> Text1
              toNonEmptyLabel bi = case obj_PlainName bi of
                Nothing -> fatal "all items without label should been filtered out here"
                Just labl -> labl
              hasLabel :: P_BoxItem a -> Bool
              hasLabel bi = case obj_PlainName bi of
                Nothing -> False
                Just _ -> True
              l :: [P_BoxItem (TermPrim, DisambPrim)]
              l = si_box x
              build :: [TBoxItem a] -> (TExpression, TSubInterface a)
              build lst =
                ( objExpr,
                  TBox
                    { pos = origin x,
                      tsiConcept = target objExpr,
                      tsiHeader = si_header x,
                      tsiObjs = lst
                    }
                )
              fn :: (TBoxItem (TermPrim, DisambPrim), Bool) -> Guarded (TBoxItem (TermPrim, DisambPrim))
              fn (boxitem, p) = case boxitem of
                TBxExpr {} -> TBxExpr <$> matchWith (tobjE boxitem, p)
                TBxText {} -> pure boxitem
        where
          matchWith :: (TObjectDef (TermPrim, DisambPrim), Bool) -> Guarded (TObjectDef (TermPrim, DisambPrim))
          matchWith (ojd, exprBound) =
            if b || exprBound
              then case userList cptMap . toList . findExact genLattice . flType . lMeet (target objExpr) . source . tobjExpression $ ojd of
                [] -> mustBeOrderedLst x [(source (tobjExpression ojd), Src, TBxExpr ojd)]
                (r : _) -> pure (ojd {tobjExpression = addEpsilonLeft genLattice r (tobjExpression ojd)})
              else mustBeBound (origin ojd) [(Src, tobjExpression ojd), (Tgt, objExpr)]
          warnings :: [Warning]
          warnings =
            [mkBoxRowsnhWarning (origin x) | toText1Unsafe "ROWSNH" == (btType . si_header $ x)] -- See issue #925
              <> [mkNoBoxItemsWarning (origin x) | null (si_box x)]
      checkTTypeOfSubInterfaces :: Guarded ()
      checkTTypeOfSubInterfaces = undefined

      --  mkInterfaceMustBeDefinedOnObject pIfc (target . objExpression $ o)
      --     <* mustBeObject (target objExpr) objExpr -- remove objExpr after diagnosis in issue #1537

      -- -- mustBeObject :: A_Concept -> Guarded ()
      -- mustBeObject cpt objExpr = case trace ("techTypeOf in mustBeObject "<>tshow cpt<>" ("<>tshow objExpr<>")\n"<>tshow ci) $ techTypeOf ci cpt of
      --   Object -> pure ()
      --   tt -> Errors . pure $ mkSubInterfaceMustBeDefinedOnObject x cpt tt

      typeCheckInterfaceRef :: P_BoxItem a -> Name -> TExpression -> TExpression -> Guarded TExpression
      typeCheckInterfaceRef objDef ifcRef objExpr ifcExpr =
        let expTarget = target objExpr
            ifcSource = source ifcExpr
            refIsCompatible = expTarget `isaC` ifcSource || ifcSource `isaC` expTarget
         in if refIsCompatible
              then pure $ addEpsilonRight genLattice ifcSource objExpr
              else Errors . pure $ mkIncompatibleInterfaceError objDef expTarget ifcSource ifcRef
      lookupDisambIfcObj :: DeclMap -> Name -> Maybe (P_BoxItem (TermPrim, DisambPrim))
      lookupDisambIfcObj declMap ifcId =
        case [disambObj | (vd, disambObj) <- p_interfaceAndDisambObjs declMap, ifc_Name vd == ifcId] of
          [] -> Nothing
          disambObj : _ -> Just disambObj -- return the first one, if there are more, this is caught later on by uniqueness static check

      -- this function helps in the disambiguation process:
      -- it adds a set of potential disambiguation outcomes to things that need to be disambiguated. For typed and untyped identities, singleton elements etc, this is immediate, but for relations we need to find it in the list of declarations.
      termPrimDisAmb :: ConceptMap -> DeclMap -> TermPrim -> (TermPrim, DisambPrim)
      termPrimDisAmb fun declMap tp =
        ( tp,
          case tp of
            PI _ -> Ident (Prim tp)
            Pid _ cpt -> Known (TEDcI (Prim tp) (pCpt2aCpt fun cpt))
            Patm _ s Nothing -> Mp1 (Prim tp) s
            Patm _ s (Just conspt) -> Known (TEMp1 (Prim tp) s (pCpt2aCpt fun conspt))
            PBin _ oper -> BinOper (Prim tp) oper
            PBind _ oper cpt -> Known (TEBin (Prim tp) oper (pCpt2aCpt fun cpt))
            PVee _ -> Vee (Prim tp)
            Pfull _ a b -> Known (TEDcV (Prim tp) (Sign (pCpt2aCpt fun a) (pCpt2aCpt fun b)))
            PNamedR nr -> Rel $ disambNamedRel nr
        )
        where
          disambNamedRel (PNamedRel _ r Nothing) = Map.elems $ findRels declMap r
          disambNamedRel (PNamedRel _ r (Just s)) = findRelsTyped declMap r $ pSign2aSign fun s

      pIfc2tIfc :: ContextInfo -> (P_Interface, P_BoxItem (TermPrim, DisambPrim)) -> Guarded (TInterface (TermPrim, DisambPrim))
      pIfc2tIfc ci (pIfc, objDisamb) = build <$> pBoxItemDisamb2TBoxItem ci objDisamb
        where
          build :: TBoxItem (TermPrim, DisambPrim) -> TInterface (TermPrim, DisambPrim)
          build boxItem = case boxItem of
            TBxExpr obj ->
              TInterface
                { tifcIsAPI = ifc_IsAPI pIfc,
                  tifcname = name pIfc,
                  tifclbl = mLabel pIfc,
                  tifcRoles = ifc_Roles pIfc,
                  tifcObj =
                    obj
                      { tobjPlainName = Just . fullName1 . name $ pIfc,
                        tobjlbl = mLabel pIfc
                      },
                  tifcConjuncts = [], -- to be enriched in Adl2fSpec with rules to be checked
                  tifcPos = origin pIfc,
                  tifcPurpose = ifc_Prp pIfc
                }
            TBxText {} -> fatal "Interface should not have TXT only. it should have a term object."
      pRoleRule2aRoleRule :: P_RoleRule -> A_RoleRule
      pRoleRule2aRoleRule prr =
        A_RoleRule
          { arRoles = mRoles prr,
            arRules = mRules prr,
            arPos = origin prr
          }

      pPat2aPat :: TTypeInfo -> ContextInfo -> P_Pattern -> Guarded Pattern
      pPat2aPat ti ci ppat =
        f
          <$> traverse (pRul2aRul ti ci (Just $ name ppat)) (pt_rls ppat)
          <*> traverse (pIdentity2aIdentity ti ci (Just $ name ppat)) (pt_ids ppat)
          <*> traverse (pPop2aPop ti ci) (pt_pop ppat)
          <*> traverse (pViewDef2aViewDef ti ci) (pt_vds ppat)
          <*> traverse (pPurp2aPurp ti ci) (pt_xps ppat)
          <*> traverse (pDecl2aDecl ti cptMap (Just $ name ppat) (deflangCtxt ctx) (deffrmtCtxt ctx)) (pt_dcs ppat)
          <*> pure (fmap (pConcDef2aConcDef cptMap (defaultLang ci) (defaultFormat ci)) (pt_cds ppat))
          <*> pure (fmap pRoleRule2aRoleRule (pt_RRuls ppat))
          <*> pure (pt_Reprs ppat)
          <*> traverse (pEnforce2aEnforce ti ci (Just $ name ppat)) (pt_enfs ppat)
        where
          f rules' keys' pops' views' xpls relations conceptdefs roleRules representations enforces' =
            A_Pat
              { ptnm = name ppat,
                ptlbl = mLabel ppat,
                ptpos = origin ppat,
                ptend = pt_end ppat,
                ptrls = Set.fromList rules',
                ptgns = mapMaybe pClassify2aClassify (pt_gns ppat),
                ptdcs = Set.fromList relations,
                ptrrs = roleRules,
                ptcds = conceptdefs,
                ptrps = representations,
                ptups = pops',
                ptids = keys',
                ptvds = views',
                ptxps = xpls,
                ptenfs = enforces'
              }
      pRul2aRul ::
        TTypeInfo ->
        ContextInfo ->
        Maybe Name -> -- name that identifies the pattern it is defined in (if any), just for documentation purposes.
        P_Rule TermPrim ->
        Guarded Rule
      pRul2aRul ti ci mPat = typeCheckRul ti ci mPat . disambiguate cptMap (termPrimDisAmb cptMap (declDisambMap ci))
      typeCheckRul ::
        TTypeInfo ->
        ContextInfo ->
        Maybe Name -> -- name that identifies the pattern it is defined in (if any), just for documentation purposes.
        P_Rule (TermPrim, DisambPrim) ->
        Guarded Rule
      typeCheckRul
        ti
        ci
        mPat
        P_Rule
          { pos = orig,
            rr_nm = nm,
            rr_lbl = lbl',
            rr_exp = expr,
            rr_mean = meanings,
            rr_msg = msgs,
            rr_viol = viols
          } =
          do
            (texp', _) <- typecheckTerm cptMap ci expr
            exp' <- tExpr2aExpr ti texp'
            vls <- maybeOverGuarded (typeCheckPairView ti ci orig exp') viols
            return
              Rule
                { rrnm = nm,
                  rrlbl = lbl',
                  formalExpression = exp',
                  rrfps = orig,
                  rrmean = map (pMean2aMean (deflangCtxt ctx) (deffrmtCtxt ctx)) meanings,
                  rrmsg = map (pMess2aMess (deflangCtxt ctx) (deffrmtCtxt ctx)) msgs,
                  rrviol = vls,
                  rrpat = mPat,
                  rrkind = UserDefined
                }
      pEnforce2aEnforce ::
        TTypeInfo ->
        ContextInfo ->
        Maybe Name -> -- name that identifies the pattern it is defined in (if any), just for documentation purposes.
        P_Enforce TermPrim ->
        Guarded AEnforce
      pEnforce2aEnforce ti ci mPat = typeCheckEnforce ti ci mPat . disambiguate cptMap (termPrimDisAmb cptMap (declDisambMap ci))
      typeCheckEnforce ::
        TTypeInfo ->
        ContextInfo ->
        Maybe Name -> -- name that identifies the pattern it is defined in (if any), just for documentation purposes.
        P_Enforce (TermPrim, DisambPrim) ->
        Guarded AEnforce
      typeCheckEnforce
        ti
        ci
        mPat
        P_Enforce
          { pos = pos',
            penfRel = pRel,
            penfOp = oper,
            penfExpr = x
          } =
          case pRel of
            (_, Known (TEDcD _ trel)) ->
              do
                rel <- tRel2aRel ti trel
                (expr, (_srcBounded, _tgtBounded)) <- typecheckTerm cptMap ci x
                -- SJC: the following two error messages can occur in parallel
                --      thanks to 'ApplicativeDo', however, we can write the following
                --      sequential-looking code that suggests checking src before tgt.
                --      ApplicativeDo should translate this with a <*> instead.
                let srcOk = source expr `isaC` source rel
                unless srcOk $ mustBeOrdered pos' (Src, expr) (Src, rel)
                let tgtOk = target expr `isaC` target rel
                unless tgtOk $ mustBeOrdered pos' (Tgt, expr) (Tgt, rel)
                expr' <-
                  tExpr2aExpr ti
                    . addEpsilonLeft genLattice (source rel)
                    $ addEpsilonRight genLattice (target rel) expr
                return
                  AEnforce
                    { pos = pos',
                      enfRel = rel,
                      enfOp = oper,
                      enfExpr = expr',
                      enfPatName = mPat,
                      enfRules = enforce2Rules rel expr'
                    }
            (o, dx) -> cannotDisambiguate o dx
          where
            enforce2Rules :: Relation -> Expression -> [Rule]
            enforce2Rules rel expr =
              case oper of
                IsSuperSet {} -> [insPair]
                IsSubSet {} -> [delPair]
                IsSameSet {} -> [insPair, delPair]
              where
                insPair = mkRule "InsPair" (EInc (expr, bindedRel))
                delPair = mkRule "DelPair" (EInc (bindedRel, expr))
                bindedRel = EDcD rel
                mkRule command fExpr =
                  Rule
                    { rrnm =
                        mkName
                          RuleName
                          ( ( case toNamePart $ "Compute" <> (tshow . abs . hash $ lbl') of
                                Nothing -> fatal "Not a proper NamePart."
                                Just np -> np
                            )
                              NE.:| []
                          ),
                      rrlbl = Just (Label lbl'),
                      formalExpression = fExpr,
                      rrfps = pos',
                      rrmean = [],
                      rrmsg = [],
                      rrviol =
                        Just
                          . PairView
                          $ PairViewText pos' ("{EX} " <> command <> ";" <> fullName rel <> ";" <> fullName (source rel) <> ";")
                          NE.:| [ PairViewExp pos' Src (EDcI (source rel)),
                                  PairViewText pos' $ ";" <> fullName (target rel) <> ";",
                                  PairViewExp pos' Tgt (EDcI (target rel))
                                ],
                      rrpat = mPat,
                      rrkind = Enforce
                    }
                  where
                    lbl' :: Text
                    lbl' = "Compute " <> tshow rel <> " using " <> command
      pIdentity2aIdentity ::
        TTypeInfo ->
        ContextInfo ->
        Maybe Name -> -- name that identifies the pattern it is defined in (if any), just for documentation purposes.
        P_IdentDef ->
        Guarded IdentityRule
      pIdentity2aIdentity ti ci mPat pidt =
        case disambiguate cptMap (termPrimDisAmb cptMap (declDisambMap ci)) pidt of
          P_Id
            { ix_name = nm,
              ix_label = lbl',
              ix_ats = isegs
            } ->
              ( \isegs' ->
                  Id
                    { idPos = orig,
                      idName = nm,
                      idlabel = lbl',
                      idCpt = conc,
                      idPat = mPat,
                      identityAts = isegs'
                    }
              )
                <$> traverse pIdentSegment2IdentSegment isegs
        where
          conc = pCpt2aCpt cptMap (ix_cpt pidt)
          orig = origin pidt
          pIdentSegment2IdentSegment :: P_IdentSegmnt (TermPrim, DisambPrim) -> Guarded IdentitySegment
          pIdentSegment2IdentSegment (P_IdentExp ojd) =
            do
              tboxitem <- pBoxItemDisamb2TBoxItem ci ojd
              boxitem <- tBoxItem2aBoxItem ti tboxitem
              case boxitem of
                BxExpr {objE = o} ->
                  case toList . findExact genLattice $ aConcToType (source $ objExpression o) `lJoin` aConcToType conc of
                    [] -> mustBeOrdered orig (Src, origin ojd, objExpression o) pidt
                    _ -> pure $ IdentityExp o {objExpression = addEpsilonLeft genLattice conc (objExpression o)}
                BxText {} -> fatal $ "TXT is not expected in IDENT statements. (" <> tshow (origin boxitem) <> ")"
      typeCheckPairView :: TTypeInfo -> ContextInfo -> Origin -> Expression -> PairView (Term (TermPrim, DisambPrim)) -> Guarded (PairView Expression)
      typeCheckPairView ti ci o x (PairView lst) =
        PairView <$> traverse (typeCheckPairViewSeg ti ci o x) lst
      typeCheckPairViewSeg :: TTypeInfo -> ContextInfo -> Origin -> Expression -> PairViewSegment (Term (TermPrim, DisambPrim)) -> Guarded (PairViewSegment Expression)
      typeCheckPairViewSeg ti ci o t pv =
        case pv of
          (PairViewText orig x) -> pure (PairViewText orig x)
          (PairViewExp orig s x) -> do
            (tExpr, (b, _)) <- typecheckTerm cptMap ci x
            e <- tExpr2aExpr ti tExpr
            let tp = aConcToType (source e)
            case toList . findExact genLattice . lMeet tp $ getConcept s t of
              [] -> mustBeOrdered o (Src, origin (fmap fst x), e) (s, t)
              lst ->
                if b || elem (getConcept s t) lst
                  then pure (PairViewExp orig s (addEpsilonLeft genLattice (getAConcept s t) e))
                  else mustBeBound o [(Src, tExpr)]
      pPurp2aPurp :: TTypeInfo -> ContextInfo -> PPurpose -> Guarded Purpose
      pPurp2aPurp
        ti
        ci
        PPurpose
          { pos = orig, -- :: Origin
            pexObj = objref, -- :: PRefObj
            pexMarkup = pmarkup, -- :: P_Markup
            pexRefIDs = refIds -- :: [Text]
          } =
          ( \obj ->
              Expl
                { explPos = orig,
                  explObj = obj,
                  explMarkup = pMarkup2aMarkup (deflangCtxt ctx) (deffrmtCtxt ctx) pmarkup,
                  explRefIds = refIds
                }
          )
            <$> pRefObj2aRefObj ti ci objref
      pRefObj2aRefObj :: TTypeInfo -> ContextInfo -> PRef2Obj -> Guarded ExplObj
      pRefObj2aRefObj ti ci refference =
        case refference of
          PRef2ConceptDef s -> pure $ ExplConcept (pCpt2aCpt cptMap $ mkPConcept s)
          PRef2Relation tm -> do
            tRelation <- namedPRel2TRel cptMap (declDisambMap ci) tm
            rel <- tRel2aRel ti tRelation
            return $ ExplRelation rel
          PRef2Rule s -> pure $ ExplRule s
          PRef2IdentityDef s -> pure $ ExplIdentityDef s
          PRef2ViewDef s -> pure $ ExplViewDef s
          PRef2Pattern s -> pure $ ExplPattern s
          PRef2Interface s -> pure $ ExplInterface s
          PRef2Context s -> pure $ ExplContext s
      allConceptDefsOutPats :: [AConceptDef]
      allConceptDefsOutPats = map (pConcDef2aConcDef cptMap (deflangCtxt ctx) (deffrmtCtxt ctx)) p_conceptdefs
      allConceptDefs :: [AConceptDef]
      allConceptDefs = map (pConcDef2aConcDef cptMap (deflangCtxt ctx) (deffrmtCtxt ctx)) (p_conceptdefs <> concatMap pt_cds p_patterns)
      udefRoleRules' :: [A_RoleRule]
      udefRoleRules' =
        map
          pRoleRule2aRoleRule
          (p_roleRules <> concatMap pt_RRuls p_patterns)

tRel2aRel :: TTypeInfo -> TRelation -> Guarded Relation
tRel2aRel ti trel = do
  dflts <- mapM (pReldefault2aReldefaults ti) . L.nub $ tdecDefaults trel :: Guarded [ARelDefault]
  return
    Relation
      { decusr = tdecusr trel,
        decsgn = tdecsgn trel,
        decprps = tdecprps trel,
        decpr = tdecpr trel,
        decpat = tdecpat trel,
        decnm = tdecnm trel,
        declabel = tdeclabel trel,
        dechash = tdechash trel,
        decfpos = tdecfpos trel,
        decMean = tdecMean trel,
        decDefaults = dflts
      }
  where
    Sign src' tgt' = tdecsgn trel
    pReldefault2aReldefaults :: TTypeInfo -> PRelationDefault -> Guarded ARelDefault
    pReldefault2aReldefaults ti' x = case x of
      PDefAtom st vals ->
        ARelDefaultAtom st
          <$> traverse
            ( case st of
                Src -> pAtomValue2aAtomValue ( ti' src') src'
                Tgt -> pAtomValue2aAtomValue ( ti' tgt') tgt'
            )
            vals
      PDefEvalPHP st txt -> pure $ ARelDefaultEvalPHP st txt

leastConcept :: Op1EqualitySystem Type -> A_Concept -> A_Concept -> A_Concept
leastConcept genLattice c c' =
  case (aConcToType c `elem` leastConcepts, aConcToType c' `elem` leastConcepts) of
    (True, _) -> c
    (_, True) -> c'
    (_, _) -> fatal ("Either " <> fullName c <> " or " <> fullName c' <> " should be a subset of the other.")
  where
    leastConcepts = findExact genLattice (Atom (aConcToType c) `Meet` Atom (aConcToType c'))

addEpsilonLeft, addEpsilonRight :: (ExpressionLike a) => Op1EqualitySystem Type -> A_Concept -> a -> a
addEpsilonLeft genLattice a e =
  if a == source e
    then e
    else
      eEps
        (fatal "This Term TermPrim should never be exposed.")
        (leastConcept genLattice (source e) a)
        (Sign a (source e))
        .:. e
addEpsilonRight genLattice a e =
  if a == target e
    then e
    else
      e
        .:. eEps
          (fatal "This Term TermPrim should never be exposed.")
          (leastConcept genLattice (target e) a)
          (Sign (target e) a)

addEpsilon :: Op1EqualitySystem Type -> A_Concept -> A_Concept -> TExpression -> TExpression
addEpsilon genLattice s t e =
  addEpsilonLeft genLattice s (addEpsilonRight genLattice t e)

typecheckTerm :: ConceptMap -> ContextInfo -> Term (TermPrim, DisambPrim) -> Guarded (TExpression, (Bool, Bool))
typecheckTerm cptMap ci tct =
  case tct of
    Prim (t, v) ->
      ( \x -> case x of
          TEMp1 {} -> pure (x, (True, True))
          TEBin {} -> pure (x, (True, True))
          _ ->
            return
              ( x,
                case t of
                  PVee _ -> (False, False)
                  _ -> (True, True)
              )
      )
        =<< pDisAmb2Expr (t, v)
    PEqu _ a b -> join $ binary (.==.) (MBE (Src, fst) (Src, snd), MBE (Tgt, fst) (Tgt, snd)) <$> tt a <*> tt b
    PInc _ a b -> join $ binary (.|-.) (MBG (Src, snd) (Src, fst), MBG (Tgt, snd) (Tgt, fst)) <$> tt a <*> tt b
    PIsc _ a b -> join $ binary (./\.) (ISC (Src, fst) (Src, snd), ISC (Tgt, fst) (Tgt, snd)) <$> tt a <*> tt b
    PUni _ a b -> join $ binary (.\/.) (UNI (Src, fst) (Src, snd), UNI (Tgt, fst) (Tgt, snd)) <$> tt a <*> tt b
    PDif _ a b -> join $ binary (.-.) (MBG (Src, fst) (Src, snd), MBG (Tgt, fst) (Tgt, snd)) <$> tt a <*> tt b
    PLrs _ a b -> join $ binary' (./.) (MBE (Tgt, snd) (Tgt, fst)) ((Src, fst), (Src, snd)) Tgt Tgt <$> tt a <*> tt b
    PRrs _ a b -> join $ binary' (.\.) (MBE (Src, fst) (Src, snd)) ((Tgt, fst), (Tgt, snd)) Src Src <$> tt a <*> tt b
    PDia _ a b -> join $ binary' (.<>.) (ISC (Tgt, fst) (Src, snd)) ((Src, fst), (Tgt, snd)) Tgt Src <$> tt a <*> tt b -- MBE would have been correct, but too restrictive
    PCps _ a b -> join $ binary' (.:.) (ISC (Tgt, fst) (Src, snd)) ((Src, fst), (Tgt, snd)) Tgt Src <$> tt a <*> tt b
    PRad _ a b -> join $ binary' (.!.) (MBE (Tgt, fst) (Src, snd)) ((Src, fst), (Tgt, snd)) Tgt Src <$> tt a <*> tt b -- Using MBE instead of ISC allows the programmer to use De Morgan
    PPrd _ a b -> (\(x, (s, _)) (y, (_, t)) -> (x .*. y, (s, t))) <$> tt a <*> tt b
    PKl0 _ a -> unary TEKl0 (UNI (Src, id) (Tgt, id), UNI (Src, id) (Tgt, id)) =<< tt a
    PKl1 _ a -> unary TEKl1 (UNI (Src, id) (Tgt, id), UNI (Src, id) (Tgt, id)) =<< tt a
    PFlp _ a -> (\(x, (s, t)) -> (TEFlp x, (t, s))) <$> tt a
    PCpl _ a -> (\(x, _) -> (TECpl x, (False, False))) <$> tt a
    PBrk _ e -> first TEBrk <$> tt e
  where
    --    tTypOf :: A_Concept -> TType
    --    tTypOf = techTypeOf ci
    genLattice = gens_efficient ci
    o = origin (fmap fst tct)
    tt = typecheckTerm cptMap ci
    -- SJC: Here is what binary, binary' and unary do:
    -- (1) Create a term, the combinator for this is given by its first argument
    -- (2) Fill in the corresponding type-checked terms to that term
    -- (3) For binary' only: fill in the intermediate concept too
    -- (4) Fill in the type of the new term
    -- For steps (3) and (4), you can use the `TT' data type to specify the new type, and what checks should occur:
    -- If you don't know what to use, try MBE: it is the strictest form.
    -- In the steps (3) and (4), different type errors may arise:
    -- If the type does not exist, this yields a type error.
    -- Some types may be generalized, while others may not.
    -- When a type may be generalized, that means that the value of the term does not change if the type becomes larger
    -- When a type may not be generalized:
    --   the type so far is actually just an estimate
    --   it must be bound by the context to something smaller, or something as big
    --   a way to do this, is by using (V[type] /\ thingToBeBound)
    -- More details about generalizable types can be found by looking at "deriv1".
    binary ::
      (TExpression -> TExpression -> TExpression) -> -- combinator
      ( TT
          ( SrcOrTgt,
            ( (TExpression, (Bool, Bool)),
              (TExpression, (Bool, Bool))
            ) ->
            (TExpression, (Bool, Bool))
          ),
        TT
          ( SrcOrTgt,
            ( (TExpression, (Bool, Bool)),
              (TExpression, (Bool, Bool))
            ) ->
            (TExpression, (Bool, Bool))
          )
      ) -> -- simple instruction on how to derive the type
      (TExpression, (Bool, Bool)) ->
      (TExpression, (Bool, Bool)) -> -- expressions to feed into the combinator after translation
      Guarded (TExpression, (Bool, Bool))
    binary cbn tp e1 e2 = wrap (fst e1, fst e2) <$> deriv tp (e1, e2)
      where
        wrap (expr1, expr2) ((src, b1), (tgt, b2)) = (cbn (addEpsilon genLattice src tgt expr1) (addEpsilon genLattice src tgt expr2), (b1, b2))
    unary cbn tp e1 = wrap (fst e1) <$> deriv tp e1
      where
        wrap expr ((src, b1), (tgt, b2)) = (cbn (addEpsilon genLattice src tgt expr), (b1, b2))
    binary' cbn preConcept tp side1 side2 e1 e2 =
      do
        a <- deriv1 (fmap (resolve (e1, e2)) preConcept)
        b <- deriv' tp (e1, e2)
        wrap (fst e1, fst e2) a b
      where
        wrap _ (_, False) ((_, b1), (_, b2)) =
          mustBeBound o [(p, e) | (False, p, e) <- [(b1, side1, fst e1), (b2, side2, fst e2)]]
        wrap (expr1, expr2) (cpt, True) ((_, b1), (_, b2)) =
          pure (cbn (lrDecide side1 expr1) (lrDecide side2 expr2), (b1, b2))
          where
            lrDecide side e = case side of Src -> addEpsilonLeft genLattice cpt e; Tgt -> addEpsilonRight genLattice cpt e
    deriv (t1, t2) es = (,) <$> deriv1 (fmap (resolve es) t1) <*> deriv1 (fmap (resolve es) t2)
    deriv1 :: TT (SrcOrTgt, (TExpression, Bool)) -> Guarded (A_Concept, Bool)
    deriv1 x' =
      case x' of
        (MBE a@(p1, (e1, b1)) b@(p2, (e2, b2))) ->
          if (b1 && b2) || (getAConcept p1 e1 == getAConcept p2 e2)
            then (,b1 || b2) <$> getExactType lJoin (p1, e1) (p2, e2)
            else mustBeBound o [(p, e) | (p, (e, False)) <- [a, b]]
        (MBG (p1, (e1, b1)) (p2, (e2, b2))) ->
          (\x -> (fst x, b1)) <$> getAndCheckType lJoin (p1, True, e1) (p2, b2, e2)
        (UNI (p1, (e1, b1)) (p2, (e2, b2))) ->
          (\x -> (fst x, b1 && b2)) <$> getAndCheckType lJoin (p1, b1, e1) (p2, b2, e2)
        (ISC (p1, (e1, b1)) (p2, (e2, b2))) ->
          ( \(x, r) -> (x, (b1 && elem (getAConcept p1 e1) r) || (b2 && elem (getAConcept p2 e2) r) || (b1 && b2))
          )
            <$> getAndCheckType lMeet (p1, b1, e1) (p2, b2, e2)
      where
        getExactType flf (p1, e1) (p2, e2) =
          case userList cptMap . toList . findExact genLattice . flType $ flf (getAConcept p1 e1) (getAConcept p2 e2) of
            [] -> mustBeOrdered o (p1, e1) (p2, e2)
            h : _ -> pure h
        getAndCheckType flf (p1, b1, e1) (p2, b2, e2) =
          case userList cptMap . toList <$> (toList . findUpperbounds genLattice . flType $ flf (getAConcept p1 e1) (getAConcept p2 e2)) of -- note: we could have used GetOneGuarded, but this yields more specific error messages
            [] -> mustBeOrdered o (p1, e1) (p2, e2)
            [r@(h : _)] ->
              case (b1 || elem (getAConcept p1 e1) r, b2 || elem (getAConcept p2 e2) r) of
                (True, True) -> pure (h, r)
                (a, b) -> mustBeBound o [(p, e) | (False, p, e) <- [(a, p1, e1), (b, p2, e2)]]
            lst -> mustBeOrderedConcLst o (p1, e1) (p2, e2) lst

pAtomPair2aAtomPair :: (A_Concept -> TType) -> TRelation -> PAtomPair -> Guarded AAtomPair
pAtomPair2aAtomPair tTypOf dcl pp =
  mkAtomPair
    <$> pAtomValue2aAtomValue (tTypOf (source dcl)) (source dcl) (ppLeft pp)
    <*> pAtomValue2aAtomValue (tTypOf (target dcl)) (target dcl) (ppRight pp)

pAtomValue2aAtomValue :: TType -> A_Concept -> PAtomValue -> Guarded AAtomValue
pAtomValue2aAtomValue typ cpt pav =
  case pav of
    PSingleton o str mval ->
      case typ of
        Alphanumeric -> pure (AAVString (abs . hash $ str) typ str)
        BigAlphanumeric -> pure (AAVString (abs . hash $ str) typ str)
        HugeAlphanumeric -> pure (AAVString (abs . hash $ str) typ str)
        Password -> pure (AAVString (abs . hash $ str) typ str)
        Object -> pure (AAVString (abs . hash $ str) typ str)
        _ -> case mval of
          Nothing -> Errors . pure $ mkIncompatibleAtomValueError pav (message o str)
          Just x -> pAtomValue2aAtomValue typ cpt x
    ScriptString o str ->
      case typ of
        Alphanumeric -> pure (AAVString (abs . hash $ str) typ str)
        BigAlphanumeric -> pure (AAVString (abs . hash $ str) typ str)
        HugeAlphanumeric -> pure (AAVString (abs . hash $ str) typ str)
        Password -> pure (AAVString (abs . hash $ str) typ str)
        Binary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        BigBinary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        HugeBinary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        Date -> Errors . pure $ mkIncompatibleAtomValueError pav (message o str)
        DateTime -> Errors . pure $ mkIncompatibleAtomValueError pav (message o str)
        Boolean -> Errors . pure $ mkIncompatibleAtomValueError pav (message o str)
        Integer -> Errors . pure $ mkIncompatibleAtomValueError pav (message o str)
        Float -> Errors . pure $ mkIncompatibleAtomValueError pav (message o str)
        TypeOfOne -> Errors . pure $ mkIncompatibleAtomValueError pav "ONE has a population of it's own, that cannot be modified"
        Object -> pure (AAVString (abs . hash $ str) typ str)
    XlsxString o str ->
      case typ of
        Alphanumeric -> pure (AAVString (abs . hash $ str) typ str)
        BigAlphanumeric -> pure (AAVString (abs . hash $ str) typ str)
        HugeAlphanumeric -> pure (AAVString (abs . hash $ str) typ str)
        Password -> pure (AAVString (abs . hash $ str) typ str)
        Binary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        BigBinary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        HugeBinary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        Date -> Errors . pure $ mkIncompatibleAtomValueError pav (message o str)
        DateTime -> Errors . pure $ mkIncompatibleAtomValueError pav (message o str)
        Boolean ->
          let table =
                [ ("TRUE", True),
                  ("FALSE", False),
                  ("YES", True),
                  ("NO", False),
                  ("WAAR", True),
                  ("ONWAAR", False),
                  ("JA", True),
                  ("NEE", False),
                  ("WEL", True),
                  ("NIET", False)
                ]
           in case lookup (T.toUpper str) table of
                Just b -> pure (AAVBoolean typ b)
                Nothing -> Errors . pure $ mkIncompatibleAtomValueError pav $ "permitted Booleans: " <> (tshow . fmap (camelCase . fst) $ table)
          where
            camelCase :: Text -> Text
            camelCase txt = case T.uncons txt of
              Nothing -> mempty
              Just (h, tl) -> T.cons (toUpper h) (T.toLower tl)
        Integer -> case readMaybe . T.unpack $ str of
          Just i -> pure (AAVInteger typ i)
          Nothing -> Errors . pure $ mkIncompatibleAtomValueError pav (message o str)
        Float -> case readMaybe . T.unpack $ str of
          Just r -> pure (AAVFloat typ r)
          Nothing -> Errors . pure $ mkIncompatibleAtomValueError pav (message o str)
        TypeOfOne -> Errors . pure $ mkIncompatibleAtomValueError pav "ONE has a population of it's own, that cannot be modified"
        Object -> pure (AAVString (abs . hash $ str) typ str)
    ScriptInt o i ->
      case typ of
        Alphanumeric -> Errors . pure $ mkIncompatibleAtomValueError pav (message o i)
        BigAlphanumeric -> Errors . pure $ mkIncompatibleAtomValueError pav (message o i)
        HugeAlphanumeric -> Errors . pure $ mkIncompatibleAtomValueError pav (message o i)
        Password -> Errors . pure $ mkIncompatibleAtomValueError pav (message o i)
        Binary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary ca)not be populated in an ADL script"
        BigBinary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        HugeBinary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        Date -> Errors . pure $ mkIncompatibleAtomValueError pav (message o i)
        DateTime -> Errors . pure $ mkIncompatibleAtomValueError pav (message o i)
        Boolean -> Errors . pure $ mkIncompatibleAtomValueError pav (message o i)
        Integer -> pure (AAVInteger typ i)
        Float -> pure (AAVFloat typ (fromInteger i)) -- must convert, because `34.000` is lexed as Integer
        TypeOfOne -> Errors . pure $ mkIncompatibleAtomValueError pav "ONE has a population of it's own, that cannot be modified"
        Object -> Errors . pure $ mkIncompatibleAtomValueError pav (message o i)
    ScriptFloat o x ->
      case typ of
        Alphanumeric -> Errors . pure $ mkIncompatibleAtomValueError pav (message o x)
        BigAlphanumeric -> Errors . pure $ mkIncompatibleAtomValueError pav (message o x)
        HugeAlphanumeric -> Errors . pure $ mkIncompatibleAtomValueError pav (message o x)
        Password -> Errors . pure $ mkIncompatibleAtomValueError pav (message o x)
        Binary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        BigBinary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        HugeBinary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        Date -> Errors . pure $ mkIncompatibleAtomValueError pav (message o x)
        DateTime -> Errors . pure $ mkIncompatibleAtomValueError pav (message o x)
        Boolean -> Errors . pure $ mkIncompatibleAtomValueError pav (message o x)
        Integer -> Errors . pure $ mkIncompatibleAtomValueError pav (message o x)
        Float -> pure (AAVFloat typ x)
        TypeOfOne -> Errors . pure $ mkIncompatibleAtomValueError pav "ONE has a population of it's own, that cannot be modified"
        Object -> Errors . pure $ mkIncompatibleAtomValueError pav (message o x)
    XlsxDouble o d ->
      case typ of
        Alphanumeric -> pure $ relaxXLSXInput d
        BigAlphanumeric -> pure $ relaxXLSXInput d
        HugeAlphanumeric -> pure $ relaxXLSXInput d
        Password -> pure $ relaxXLSXInput d
        Binary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        BigBinary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        HugeBinary -> Errors . pure $ mkIncompatibleAtomValueError pav "Binary cannot be populated in an ADL script"
        Date ->
          pure
            AAVDate
              { aavtyp = typ,
                aadateDay = addDays (floor d) dayZeroExcel
              }
        DateTime ->
          pure
            $ AAVDateTime
              { aavtyp = typ,
                aadatetime =
                  roundBySeconds
                    $ UTCTime
                      (addDays daysSinceZero dayZeroExcel)
                      (picosecondsToDiffTime . floor $ fractionOfDay * picosecondsPerDay)
              }
          where
            picosecondsPerDay = 24 * 60 * 60 * 1000000000000
            (daysSinceZero, fractionOfDay) = properFraction d
        Boolean -> Errors . pure $ mkIncompatibleAtomValueError pav (message o d)
        Integer ->
          if frac == 0
            then pure (AAVInteger typ int)
            else Errors . pure $ mkIncompatibleAtomValueError pav (message o d)
          where
            (int, frac) = properFraction d
        Float -> pure (AAVFloat typ d)
        TypeOfOne -> Errors . pure $ mkIncompatibleAtomValueError pav "ONE has a population of it's own, that cannot be modified"
        Object -> pure $ relaxXLSXInput d
    ComnBool o b ->
      if typ == Boolean
        then pure (AAVBoolean typ b)
        else Errors . pure $ mkIncompatibleAtomValueError pav (message o b)
    ScriptDate o x ->
      if typ == Date
        then pure (AAVDate typ x)
        else Errors . pure $ mkIncompatibleAtomValueError pav (message o x)
    ScriptDateTime o x ->
      if typ == DateTime
        then pure $ AAVDateTime typ (roundBySeconds x)
        else Errors . pure $ mkIncompatibleAtomValueError pav (message o x)
  where
    roundBySeconds :: UTCTime -> UTCTime
    roundBySeconds x = x {utctDayTime = rounded (utctDayTime x)}
      where
        -- Rounding is needed, to maximize the number of databases
        -- on wich this runs. (MySQL 5.5 only knows seconds)
        picosecondsInASecond = 1000000000000
        rounded :: DiffTime -> DiffTime
        rounded = picosecondsToDiffTime . quot picosecondsInASecond . diffTimeToPicoseconds

    relaxXLSXInput :: Double -> AAtomValue
    relaxXLSXInput v = AAVString (hash v) typ . neat . tshow $ v
      where
        neat :: Text -> Text
        neat s
          | onlyZeroes dotAndAfter = beforeDot
          | otherwise = s
          where
            (beforeDot, dotAndAfter) = T.span (/= '.') s
            onlyZeroes s' = case T.uncons s' of
              Nothing -> True
              Just ('.', afterDot) -> T.all (== '0') afterDot
              _ -> False
    message :: (Show x) => Origin -> x -> Text
    message orig x =
      T.intercalate "\n    "
        $ [ "Representation mismatch",
            "Found: `" <> tshow x <> "` (" <> tshow orig <> "),",
            "as representation of an atom in concept `" <> text1ToText (fullName1 cpt) <> "`.",
            "However, the representation-type of that concept is " <> implicitly,
            "defined as " <> tshow typ <> ". The found value does not match that type."
          ]
        <> example
      where
        implicitly = if typ == Object then "(implicitly) " else ""
        example :: [Text]
        example = case typ of
          Alphanumeric -> ["ALPHANUMERIC types are texts (max 255 chars) surrounded with double quotes (\"-characters)."]
          BigAlphanumeric -> ["BIGALPHANUMERIC types are texts (max 64k chars) surrounded with double quotes (\"-characters)."]
          Boolean -> ["BOOLEAN types can have the value TRUE or FALSE (without surrounding quotes)."]
          Date -> ["DATE types are defined by ISO8601, e.g. 2013-07-04 (without surrounding quotes)."]
          DateTime -> ["DATETIME types follow ISO 8601 format, e.g. 2013-07-04T11:11:11+00:00 or 2015-06-03T13:21:58Z (without surrounding quotes)."]
          Float -> ["FLOAT type are floating point numbers. There should be a dot character (.) in it."]
          HugeAlphanumeric -> ["HUGEALPHANUMERIC types are texts (max 16M chars) surrounded with double quotes (\"-characters)."]
          Integer -> ["INTEGER types are decimal numbers (max 20 positions), e.g. 4711 or -4711 (without surrounding quotes)"]
          Password -> ["PASSWORD types are texts (max 255 chars) surrounded with double quotes (\"-characters)."]
          Object -> ["OBJECT types are non-scalar atoms represented by an identifier (max 255 chars) surrounded with double quotes (\"-characters)."]
          _ -> fatal $ "There is no example denotational syntax for a value of type `" <> tshow typ <> "`."
    dayZeroExcel = addDays (-2) (fromGregorian 1900 1 1) -- Excel documentation tells that counting starts a jan 1st, however, that isn't totally true.

pDecl2aDecl ::
  TTypeInfo ->
  ConceptMap ->
  Maybe Name -> -- name that identifies the pattern it is defined in (if any), just for documentation purposes.
  Lang -> -- The default language
  PandocFormat -> -- The default pandocFormat
  P_Relation ->
  Guarded Relation
pDecl2aDecl ti cptMap maybePat defLanguage defFormat pd =
  do
    checkEndoProps
    -- propLists <- mapM pProp2aProps . Set.toList $ dec_prps pd
    dflts <- mapM (pReldefault2aReldefaults ti) . L.nub $ dec_defaults pd :: Guarded [ARelDefault]
    return
      Relation
        { decnm = dec_nm pd,
          decsgn = decSign,
          declabel = dec_label pd,
          decprps = Set.fromList . concatMap pProp2aProps . Set.toList $ dec_prps pd,
          decDefaults = dflts,
          decpr = dec_pragma pd,
          decMean = map (pMean2aMean defLanguage defFormat) (dec_Mean pd),
          decfpos = origin pd,
          decusr = True,
          decpat = maybePat,
          dechash = hash (dec_nm pd) `hashWithSalt` decSign
        }
  where
    pReldefault2aReldefaults :: TTypeInfo -> PRelationDefault -> Guarded ARelDefault
    pReldefault2aReldefaults ti' x = case x of
      PDefAtom st vals ->
        ARelDefaultAtom st
          <$> traverse
            ( case st of
                Src -> pAtomValue2aAtomValue (ti' (source decSign)) (source decSign)
                Tgt -> pAtomValue2aAtomValue (ti' (target decSign)) (target decSign)
            )
            vals
      PDefEvalPHP st txt -> pure $ ARelDefaultEvalPHP st txt
    pProp2aProps :: PProp -> [AProp]
    pProp2aProps p = case p of
      P_Uni -> [Uni]
      P_Inj -> [Inj]
      P_Map -> [Uni, Tot]
      P_Sur -> [Sur]
      P_Tot -> [Tot]
      P_Bij -> [Inj, Sur]
      P_Sym -> [Sym]
      P_Asy -> [Asy]
      P_Prop -> [Sym, Asy]
      P_Trn -> [Trn]
      P_Rfx -> [Rfx]
      P_Irf -> [Irf]

    decSign = pSign2aSign cptMap (dec_sign pd)
    checkEndoProps :: Guarded ()
    checkEndoProps
      | source decSign == target decSign =
          pure ()
      | null xs =
          pure ()
      | otherwise = Errors . pure $ mkEndoPropertyError (origin pd) (Set.toList xs)
      where
        xs = Set.filter isEndoProp $ dec_prps pd
        isEndoProp :: PProp -> Bool
        isEndoProp p = p `elem` [P_Prop, P_Sym, P_Asy, P_Trn, P_Rfx, P_Irf]

pDisAmb2Expr :: (TermPrim, DisambPrim) -> Guarded TExpression
pDisAmb2Expr (_, Known x) = pure x
pDisAmb2Expr (_, Rel [x]) = pure x
pDisAmb2Expr (o, dx) = cannotDisambiguate o dx

pConcDef2aConcDef ::
  ConceptMap ->
  Lang -> -- The default language
  PandocFormat -> -- The default pandocFormatPConceptDef
  PConceptDef ->
  AConceptDef
pConcDef2aConcDef cptMap defLanguage defFormat pCd =
  AConceptDef
    { pos = origin pCd,
      acdcpt = pCpt2aCpt cptMap (PCpt {p_cptnm = name pCd}),
      acdname = name pCd,
      acdlabel = cdlbl pCd,
      acddef2 = pCDDef2Mean defLanguage defFormat $ cddef2 pCd,
      acdmean = map (pMean2aMean defLanguage defFormat) (cdmean pCd),
      acdfrom = cdfrom pCd
    }

pCDDef2Mean ::
  Lang -> -- The default language
  PandocFormat -> -- The default pandocFormat
  PCDDef ->
  Meaning
pCDDef2Mean defLanguage defFormat x = case x of
  PCDDefLegacy defStr refStr ->
    Meaning
      Markup
        { amLang = defLanguage,
          amPandoc = string2Blocks defFormat (defStr <> if T.null refStr then mempty else "[" <> refStr <> "]")
        }
  PCDDefNew m -> pMean2aMean defLanguage defFormat m

pMean2aMean ::
  Lang -> -- The default language
  PandocFormat -> -- The default pandocFormat
  PMeaning ->
  Meaning
pMean2aMean defLanguage defFormat (PMeaning pmarkup) =
  Meaning (pMarkup2aMarkup defLanguage defFormat pmarkup)

pMess2aMess ::
  Lang -> -- The default language
  PandocFormat -> -- The default pandocFormat
  PMessage ->
  Markup
pMess2aMess defLanguage defFormat (PMessage x) = pMarkup2aMarkup defLanguage defFormat x

pMarkup2aMarkup ::
  Lang -> -- The default language
  PandocFormat -> -- The default pandocFormat
  P_Markup ->
  Markup
pMarkup2aMarkup
  defLanguage
  defFormat
  P_Markup
    { mLang = ml,
      mFormat = mpdf,
      mString = str
    } =
    Markup
      { amLang = fromMaybe defLanguage ml, -- The language is always defined; if not by the user, then by default.
        amPandoc = string2Blocks (fromMaybe defFormat mpdf) str
      }

-- helpers for generating a lattice, not having to write `Atom' all the time
-- the l in lJoin and lMeet denotes the lattice.
lJoin, lMeet :: a -> a -> FreeLattice a
lJoin a b = Join (Atom a) (Atom b)
lMeet a b = Meet (Atom a) (Atom b)

flType :: FreeLattice A_Concept -> FreeLattice Type
flType = fmap aConcToType

-- intended for finding the right term on terms like (Src,fst)
resolve :: t -> (SrcOrTgt, t -> (t1, (t2, t2))) -> (SrcOrTgt, (t1, t2))
resolve es (p, f) =
  case (p, f es) of
    (Src, (e, (b, _))) -> (Src, (e, b))
    (Tgt, (e, (_, b))) -> (Tgt, (e, b))

maybeOverGuarded :: (t -> Guarded a) -> Maybe t -> Guarded (Maybe a)
maybeOverGuarded _ Nothing = pure Nothing
maybeOverGuarded f (Just x) = Just <$> f x

data TT a -- (In order of increasing strictness. If you are unsure which to pick: just use MBE, it'll usually work fine)
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

deriv' ::
  (Applicative f) =>
  ((SrcOrTgt, t -> (TExpression, (Bool, Bool))), (SrcOrTgt, t -> (TExpression, (Bool, Bool)))) ->
  t ->
  f ((Type, Bool), (Type, Bool))
deriv' (a, b) es =
  let (sourceOrTarget1, (e1, t1)) = resolve es a
      (sourceOrTarget2, (e2, t2)) = resolve es b
   in pure ((getConcept sourceOrTarget1 e1, t1), (getConcept sourceOrTarget2 e2, t2))

instance Functor TT where
  fmap f (UNI a b) = UNI (f a) (f b)
  fmap f (ISC a b) = ISC (f a) (f b)
  fmap f (MBE a b) = MBE (f a) (f b)
  fmap f (MBG a b) = MBG (f a) (f b)

getAConcept :: (HasSignature a) => SrcOrTgt -> a -> A_Concept
getAConcept Src = source
getAConcept Tgt = target

getConcept :: (HasSignature a) => SrcOrTgt -> a -> Type
getConcept Src = aConcToType . source
getConcept Tgt = aConcToType . target

tBoxItem2aBoxItem :: TTypeInfo -> TBoxItem (TermPrim, DisambPrim) -> Guarded BoxItem
tBoxItem2aBoxItem ti tbi =
  case tbi of
    TBxExpr obj ->
      BxExpr <$> tObjDef2aObjDef ti obj
    --        (expr, (b, _)) <- typecheckTerm (tiContext ti) (objExpression o)
    --        pure $ BxExpr o {objExpression = expr}
    TBxText {} ->
      pure
        $ BxText
          { boxPlainName = tboxPlainName tbi,
            boxpos = tboxpos tbi,
            boxtxt = tboxtxt tbi
          }

tObjDef2aObjDef ::
  TTypeInfo ->
  TObjectDef (TermPrim, DisambPrim) ->
  Guarded ObjectDef
tObjDef2aObjDef ti obj = do
  msub <- maybeOverGuarded (tSubi2aSubi ti) (tobjmsub obj)
  expr <- tExpr2aExpr ti (tobjExpression obj)
  return
    ObjectDef
      { objmsub = msub,
        objmView = tobjmView obj,
        objlbl = tobjlbl obj,
        objcrud = tobjcrud obj,
        objPos = tobjPos obj,
        objPlainName = tobjPlainName obj,
        objExpression = expr
      }

tSubi2aSubi ::
  TTypeInfo ->
  TSubInterface (TermPrim, DisambPrim) ->
  Guarded SubInterface
tSubi2aSubi _ti _subi = undefined

tExpr2aExpr :: TTypeInfo -> TExpression -> Guarded Expression
tExpr2aExpr ti expr = case expr of
  (TEEqu (l, r)) -> do
    l' <- tExpr2aExpr ti l
    r' <- tExpr2aExpr ti r
    return $ EEqu (l', r')
  (TEInc (l, r)) -> do
    l' <- tExpr2aExpr ti l
    r' <- tExpr2aExpr ti r
    return $ EInc (l', r')
  (TEIsc (l, r)) -> do
    l' <- tExpr2aExpr ti l
    r' <- tExpr2aExpr ti r
    return $ EIsc (l', r')
  (TEUni (l, r)) -> do
    l' <- tExpr2aExpr ti l
    r' <- tExpr2aExpr ti r
    return $ EUni (l', r')
  (TEDif (l, r)) -> do
    l' <- tExpr2aExpr ti l
    r' <- tExpr2aExpr ti r
    return $ EDif (l', r')
  (TELrs (l, r)) -> do
    l' <- tExpr2aExpr ti l
    r' <- tExpr2aExpr ti r
    return $ ELrs (l', r')
  (TERrs (l, r)) -> do
    l' <- tExpr2aExpr ti l
    r' <- tExpr2aExpr ti r
    return $ ERrs (l', r')
  (TEDia (l, r)) -> do
    l' <- tExpr2aExpr ti l
    r' <- tExpr2aExpr ti r
    return $ EDia (l', r')
  (TECps (l, r)) -> do
    l' <- tExpr2aExpr ti l
    r' <- tExpr2aExpr ti r
    return $ ECps (l', r')
  (TERad (l, r)) -> do
    l' <- tExpr2aExpr ti l
    r' <- tExpr2aExpr ti r
    return $ ERad (l', r')
  (TEPrd (l, r)) -> do
    l' <- tExpr2aExpr ti l
    r' <- tExpr2aExpr ti r
    return $ EPrd (l', r')
  (TEKl0 e) -> do
    e' <- tExpr2aExpr ti e
    return $ EKl0 e'
  (TEKl1 e) -> do
    e' <- tExpr2aExpr ti e
    return $ EKl1 e'
  (TEFlp e) -> do
    e' <- tExpr2aExpr ti e
    return $ EFlp e'
  (TECpl e) -> do
    e' <- tExpr2aExpr ti e
    return $ ECpl e'
  (TEBrk e) -> do
    e' <- tExpr2aExpr ti e
    return $ EBrk e'
  (TEDcD _ trel) -> EDcD <$> tRel2aRel ti trel
  (TEDcI _ cpt) -> pure $ EDcI cpt
  (TEBin tp oper cpt) ->
    if isValidOperator
      then pure $ EBin oper cpt
      else Errors . pure $ mkOperatorError (origin tp) oper cpt typ
    where
      typ = ti cpt
      isValidOperator :: Bool
      isValidOperator =
        case oper of
          LessThan -> hasORD typ
          GreaterThan -> hasORD typ
          LessThanOrEqual -> hasEQ typ && hasORD typ
          GreaterThanOrEqual -> hasEQ typ && hasORD typ
  (TEEps _ cpt sgn) -> pure $ EEps cpt sgn
  (TEDcV _ sgn) -> pure $ EDcV sgn
  (TEMp1 _ val cpt) -> do
    aval <- pAtomValue2aAtomValue (ti cpt) cpt val
    pure $ EMp1 aval cpt
  where
    hasEQ, hasORD :: TType -> Bool
    hasEQ Float = True -- This must hold as long as I is valid on a concept with TTYPE Float
    hasEQ _ = True
    hasORD ttyp = case ttyp of
      Alphanumeric -> True
      BigAlphanumeric -> True
      HugeAlphanumeric -> True
      Password -> False
      Binary -> False
      BigBinary -> False
      HugeBinary -> False
      Date -> True
      DateTime -> True
      Boolean -> False
      Integer -> True
      Float -> True
      Object -> False
      TypeOfOne -> True

pRel2tRel ::
  ConceptMap ->
  Maybe Name -> -- name that identifies the pattern it is defined in (if any), just for documentation purposes.
  Lang -> -- The default language
  PandocFormat -> -- The default pandocFormat
  P_Relation ->
  Guarded TRelation
pRel2tRel cptMap maybePatName defLanguage defFormat pd = do
  checkEndoProps
  return
    TRelation
      { tdecnm = dec_nm pd,
        tdecsgn = decSign,
        tdeclabel = dec_label pd,
        tdecprps = Set.fromList . concatMap pProp2aProps . Set.toList $ dec_prps pd,
        tdecDefaults = dec_defaults pd,
        tdecpr = dec_pragma pd,
        tdecMean = map (pMean2aMean defLanguage defFormat) (dec_Mean pd),
        tdecfpos = origin pd,
        tdecusr = True,
        tdecpat = maybePatName,
        tdechash = hash (dec_nm pd) `hashWithSalt` decSign
      }
  where
    pReldefault2aReldefaults :: TTypeInfo -> PRelationDefault -> Guarded ARelDefault
    pReldefault2aReldefaults ti x = case x of
      PDefAtom st vals ->
        ARelDefaultAtom st
          <$> traverse
            ( case st of
                Src -> pAtomValue2aAtomValue (ti (source decSign)) (source decSign)
                Tgt -> pAtomValue2aAtomValue (ti (target decSign)) (target decSign)
            )
            vals
      PDefEvalPHP st txt -> pure $ ARelDefaultEvalPHP st txt
    pProp2aProps :: PProp -> [AProp]
    pProp2aProps p = case p of
      P_Uni -> [Uni]
      P_Inj -> [Inj]
      P_Map -> [Uni, Tot]
      P_Sur -> [Sur]
      P_Tot -> [Tot]
      P_Bij -> [Inj, Sur]
      P_Sym -> [Sym]
      P_Asy -> [Asy]
      P_Prop -> [Sym, Asy]
      P_Trn -> [Trn]
      P_Rfx -> [Rfx]
      P_Irf -> [Irf]

    decSign = pSign2aSign cptMap (dec_sign pd)
    checkEndoProps :: Guarded ()
    checkEndoProps
      | source decSign == target decSign =
          pure ()
      | null xs =
          pure ()
      | otherwise = Errors . pure $ mkEndoPropertyError (origin pd) (Set.toList xs)
      where
        xs = Set.filter isEndoProp $ dec_prps pd
        isEndoProp :: PProp -> Bool
        isEndoProp p = p `elem` [P_Prop, P_Sym, P_Asy, P_Trn, P_Rfx, P_Irf]
