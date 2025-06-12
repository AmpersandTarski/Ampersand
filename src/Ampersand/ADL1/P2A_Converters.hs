{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE TupleSections #-}

module Ampersand.ADL1.P2A_Converters
  ( pCtx2aCtx,
    pSign2aSign,
    ConceptMap,
    findRels,
    findRelsTyped
  )
where

import Ampersand.ADL1.Expression
import Ampersand.ADL1.Lattices
import Ampersand.Basics hiding (conc, set, guard)
import Ampersand.Classes
import Ampersand.Core.A2P_Converters (aConcept2pConcept)
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowAStruct
import Ampersand.Core.ShowPStruct (showP)
import Ampersand.FSpec.ToFSpec.Populated (sortSpecific2Generic)
import Ampersand.Input.ADL1.CtxError
import Ampersand.Misc.HasClasses
import Algebra.Graph.AdjacencyMap
import Data.Tuple.Extra ({-fst3,-} snd3, thd3)
import RIO.Char (toLower, toUpper)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

pConcToType :: P_Concept -> Type
pConcToType P_ONE = BuiltIn TypeOfOne
pConcToType p = UserConcept (name p)

aConcToType :: A_Concept -> Type
aConcToType ONE = BuiltIn TypeOfOne
aConcToType p = UserConcept (fst . NE.head $ aliases p)

getAsConcept :: ContextInfo -> Origin -> Type -> Guarded A_Concept
getAsConcept ci o v = case typeOrConcept (conceptMap ci) v of
  Right x -> unexpectedType o x
  Left x -> return x

userList :: ConceptMap -> [Type] -> [A_Concept]
userList fun = lefts . fmap (typeOrConcept fun)


-- NOTE: Static checks like checkPurposes should ideally occur on the P-structure before type-checking, as it makes little
-- sense to do type checking when there are static errors. However, in Ampersand all collect functions (e.g. in ViewPoint)
-- only exist on the A-Structure, so we do it afterwards. Static purpose errors won't affect types, so in this case it is no problem.

-- Check whether all purposes refer to existing objects.
checkPurposes :: A_Context -> Guarded ()
checkPurposes ctx =
  case filter (isDanglingPurpose ctx) (ctxps ctx <> concatMap ptxps (ctxpats ctx)) of
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

warnUnusedConcepts :: A_Context -> Guarded ()
warnUnusedConcepts ctx = addWarnings warnings $ pure ()
  where
    warnings :: [Warning]
    warnings =
      [ mkUnusedCptDefWarning cDef
        | cDef <- L.nub $ ctxcds ctx <> concatMap ptcds (ctxpats ctx),
          acdcpt cDef `notElem` (concs . relsDefdIn $ ctx)
      ]

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
    refsPerInterface = [(name ifc, getDeepIfcRefs $ ifcObj ifc) | ifc <- ctxifcs ctx]
    getDeepIfcRefs :: ObjectDef -> [Name]
    getDeepIfcRefs obj = case objmsub obj of
      Nothing -> []
      Just si -> case si of
        InterfaceRef {} -> [siIfcId si | not (siIsLink si)]
        Box {} -> concatMap getDeepIfcRefs [x | BxExpr x <- siObjs si]
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
           isSESSION src && not (_isPermittedSessionValue (apLeft pr))
             || isSESSION tgt && not (_isPermittedSessionValue (apRight pr))
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
pSign2aSign pCpt2aCpt (P_Sign src tgt) = Sign (pCpt2aCpt src) (pCpt2aCpt tgt)

findRels :: DeclMap -> Name -> Map.Map SignOrd Relation
findRels declMap x = Map.findWithDefault Map.empty x declMap -- get all relations with the same name as x

namedRel2Decl :: ConceptMap -> DeclMap -> P_NamedRel -> Guarded Relation
namedRel2Decl ci declMap (PNamedRel o r mSgn)
 = case decls of
    [dcl] -> pure dcl
    []    -> (Errors . return . CTXE o) ("Undefined relation named: "<>tshow r)
    ds    -> (Errors . return . CTXE o) ("Ambiguous relation named: "<>tshow r<>"\n"<>tshow ds)
   where
     decls = case mSgn of
               Nothing -> findDecls declMap r
               Just s  -> findRelsTyped declMap r (pSign2aSign ci s)

findDecls :: DeclMap -> Name -> [Relation]
findDecls declMap x = Map.elems (findRels declMap x)

findRelsTyped :: DeclMap -> Name -> Signature -> [Relation]
findRelsTyped declMap x tp = Map.findWithDefault [] (SignOrd tp) (Map.map (: []) (findRels declMap x))

type DeclMap = Map.Map Name (Map.Map SignOrd Relation)

onlyUserConcepts :: ContextInfo -> [[Type]] -> [[A_Concept]]
onlyUserConcepts = fmap . userList . conceptMap

-- | pCtx2aCtx has three tasks:
-- 1. Disambiguate the structures.
--    Disambiguation means replacing every "TermPrim" (the parsed term) with the correct Expression (available through DisambPrim)
--    This is done by using the function "disambiguate" on the outer-most structure.
--    In order to do this, its data type must be polymorphic, as in "P_ViewSegmt a".
--    After parsing, the type has TermPrim for the type variable. In our example: "P_ViewSegmt TermPrim". Note that "type P_ViewSegment = P_ViewSegmt TermPrim".
--    After disambiguation, the type variable is TermPrim, as in "P_ViewSegmt TermPrim"
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
  PCtx
    { ctx_nm = n1,
      ctx_lbl = lbl,
      ctx_pos = n2,
      ctx_lang = ctxmLang,
      ctx_markup = pandocf,
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
      contextInfoPre <- g_contextInfo -- the minimal amount of data needed to transform things from P-structure to A-structure.
      -- aReprs contains all concepts that have TTypes given in REPRESENT statements and in Interfaces (i.e. Objects)
      aReprs <- traverse (pRepr2aRepr contextInfoPre) (p_representations <> concatMap pt_Reprs p_patterns) :: Guarded [A_Representation] --  The representations defined in this context
      -- allReprs contains all concepts and every concept has precisely one TType
      allReps <- makeComplete contextInfoPre aReprs
      let contextInfo = contextInfoPre {representationOf = defaultTType allReps}
      --  uniqueNames "pattern" p_patterns   -- Unclear why this restriction was in place. So I removed it
      pats <- traverse (pPat2aPat contextInfo) p_patterns --  The patterns defined in this context
      uniqueNames "rule" $ p_rules <> concatMap pt_rls p_patterns
      rules <- traverse (pRul2aRul contextInfo Nothing) p_rules --  All user defined rules in this context, but outside patterns
      uniqueNames "identity definition" $ p_identdefs <> concatMap pt_ids p_patterns
      identdefs <- traverse (pIdentity2aIdentity contextInfo Nothing) p_identdefs --  The identity definitions defined in this context, outside the scope of patterns
      uniqueNames "view definition" $ p_viewdefs <> concatMap pt_vds p_patterns
      viewdefs <- traverse (pViewDef2aViewDef contextInfo) p_viewdefs --  The view definitions defined in this context, outside the scope of patterns
      uniqueNames "interface" p_interfaces
      interfaces <- traverse (pIfc2aIfc contextInfo) p_interfaces
      purposes <- traverse (pPurp2aPurp contextInfo) p_purposes --  The purposes of objects defined in this context, outside the scope of patterns
      udpops <- traverse (pPop2aPop contextInfo) p_pops --  [Population]
      relations <- -- the following trace statement is kept in comment for possible further work on contextInfo (March 31st, 2025).
      -- trace ("\ncontextInfo = "<>tshow contextInfo<>"\n\nallConcepts contextInfo = "<>tshow (allConcepts contextInfo)<>"\np_representations = "<>tshow p_representations<>"\naReprs = "<>tshow aReprs<>"\nmultiKernels = "<>tshow (multiKernels contextInfo)<>"\nallReps = "<>tshow allReps) $
        traverse (pDecl2aDecl (representationOf contextInfo) pCpt2aCpt Nothing deflangCtxt deffrmtCtxt) p_relations
      enforces' <- traverse (pEnforce2aEnforce contextInfo Nothing) p_enfs
      let actx =
            ACtx
              { ctxnm = n1,
                ctxlbl = lbl,
                ctxpos = n2,
                ctxlang = deflangCtxt,
                ctxmarkup = deffrmtCtxt,
                ctxpats = pats,
                ctxrs = Set.fromList rules,
                ctxds = Set.fromList relations,
                ctxpopus = udpops, -- the content is copied from p_pops
                ctxcdsOutPats = allConceptDefsOutPats contextInfo,
                ctxcds = allConceptDefs contextInfo,
                ctxks = identdefs,
                ctxrrules = udefRoleRules',
                ctxvs = viewdefs,
                ctxgs = mapMaybe pClassify2aClassify p_gens,
                ctxgenconcs = onlyUserConcepts contextInfo (concGroups <> map (: []) (Set.toList $ soloConcs contextInfo)),
                ctxifcs = interfaces,
                ctxps = purposes,
                ctxmetas = p_metas,
                ctxInfo = contextInfo,
                ctxEnforces = enforces'
              }
      checkOtherAtomsInSessionConcept actx
      checkPurposes actx -- Check whether all purposes refer to existing objects
      checkDanglingRulesInRuleRoles actx -- Check whether all rules in MAINTAIN statements are declared
      checkInterfaceCycles actx -- Check that interface references are not cyclic
      checkMultipleDefaultViews actx -- Check whether each concept has at most one default view
      warnCaseProblems actx -- Warn if there are problems with the casing of names of relations and/or concepts
      warnUnusedConcepts actx -- Warn if there are concepts defined that are not used in relations
      return actx
    where
      makeComplete :: ContextInfo -> [A_Representation] -> Guarded [A_Representation]
      makeComplete contextInfo aReprs = {- trace ("\nttypeAnalysis"<>tshow ttypeAnalysis) -} checkDuplicates
        where
          -- \| ttypeAnalysis exposes duplicate TTypes, so we can make error messages
          ttypeAnalysis :: [([A_Concept], [(TType, [Origin])])]
          ttypeAnalysis =
            [ (typolConcs, case ttOrigPairs of [] -> [(Alphanumeric, [])]; _ -> ttOrigPairs)
              | typolConcs <- typolSets <> [[c] | c <- Set.toList (allConcepts contextInfo `Set.union` ttypedConcepts), c `notElem` concat typolSets],
                let ttOrigPairs = ttPairs typolConcs
            ]
            where
              -- \| To ensure that all concepts that will be Object are treated as declared objects, we compute ttypedConcepts. Without it,
              ttypedConcepts :: Set.Set A_Concept
              ttypedConcepts = (Set.fromList . concat) [(NE.toList . aReprFrom) aRepr | aRepr <- aReprs]
              typolSets = map tyCpts (multiKernels contextInfo)
              ttPairs :: [A_Concept] -> [(TType, [Origin])]
              ttPairs typology =
                [ (t, [origin aRepr | aRepr <- NE.toList cl])
                  | cl <- eqCl aReprTo [aRepr | aRepr <- aReprs, not . null $ NE.toList (aReprFrom aRepr) `L.intersect` typology],
                    t <- L.nub [aReprTo aRepr | aRepr <- NE.toList cl]
                ]
          checkDuplicates :: Guarded [A_Representation]
          checkDuplicates =
            case [(cs, tts) | (cs, tts@(_ : _ : _)) <- ttypeAnalysis] of
              [] -> pure [Arepr os (c :| cs) t | (c : cs, [(t, os)]) <- ttypeAnalysis]
              errs -> traverse mkMultipleRepresentTypesError errs
      defaultTType :: [A_Representation] -> A_Concept -> TType
      defaultTType aReprs c =
        if c == ONE || show c == "SESSION"
          then Object
          else case L.nub [aReprTo aRepr | aRepr <- L.nub aReprs, c `elem` NE.toList (aReprFrom aRepr)] of
            [] -> Alphanumeric
            [t] -> t
            _ -> fatal "Multiple representations for a single concept"
      concGroups = getGroups genLatticeIncomplete :: [[Type]]
      deflangCtxt = fromMaybe English ctxmLang
      deffrmtCtxt = fromMaybe ReST pandocf
      alleGens :: [PClassify]
      alleGens = p_gens <> concatMap pt_gns p_patterns
      allReprs :: [P_Representation]
      allReprs = p_representations <> concatMap pt_Reprs p_patterns
      g_contextInfo :: Guarded ContextInfo
      g_contextInfo = do
        -- The reason for having monadic syntax ("do") is that g_contextInfo is Guarded
        typeMap <- mkTypeMap connectedConcepts allReprs -- This is error free if every partition refers to precisely one technical type.
        -- > SJ:  It seems to mee that `multitypologies` can be implemented more concisely and more maintainably by using a transitive closure algorithm (Warshall).
        --        Also, `connectedConcepts` is not used in the result, so is avoidable when using a transitive closure approach.
        multitypologies <- traverse mkTypology connectedConcepts -- SJ: why `traverse` instead of `map`? Does this have to do with guarded as well?
        let reprOf cpt =
              fromMaybe
                Alphanumeric -- See issue #1537
                (lookup cpt typeMap)
        decls <- traverse (pDecl2aDecl reprOf pCpt2aCpt Nothing deflangCtxt deffrmtCtxt) (p_relations <> concatMap pt_dcs p_patterns)
        let declMap = Map.map groupOnTp (Map.fromListWith (<>) [(name d, [d]) | d <- decls])  :: Map Name (Map SignOrd Relation)
              where
                groupOnTp lst = Map.fromListWith const [(SignOrd $ sign d, d) | d <- lst]
        let allConcs = Set.fromList (map aConcToType (map source decls <> map target decls)) :: Set.Set Type
        return
          CI
            { ctxiGens = gns,
              representationOf = reprOf,
              multiKernels = multitypologies,
              reprList = allReprs,
              declarationsMap = declMap,
              soloConcs = Set.filter (not . isInSystem genLattice) allConcs,
              allConcepts = concs decls `Set.union` concs gns `Set.union` concs allConcDefs,
              allGens = gns,
              gens_efficient = genLattice,
              conceptMap = pCpt2aCpt,
              defaultLang = deflangCtxt,
              defaultFormat = deffrmtCtxt
            }
        where
          allConcDefs :: Set.Set AConceptDef
          allConcDefs = Set.fromList (map (pConcDef2aConcDef pCpt2aCpt deflangCtxt deffrmtCtxt) (p_conceptdefs <> concatMap pt_cds p_patterns))

          gns :: [AClassify]
          gns = mapMaybe pClassify2aClassify alleGens

          -- | We make concept graphs with Algebra.Graph.AdjacencyMap.
          -- See https://github.com/snowleopard/alga-paper for documentation and https://www.youtube.com/watch?v=EdQGLewU-8k for motivation.
          -- AdjacencyMap is an instance of the Graph type class. It is especially designed (and also efficient) for graphs with closures, such as our concept graph.
          -- postcondition: makeGraph is the transitive closure of gns.

          connectedConcepts :: [[A_Concept]] -- a partitioning of all A_Concepts where every two connected concepts are in the same partition.
          connectedConcepts = connect' [] (map (toList . concs) gns)

          mkTypeMap :: [[A_Concept]] -> [P_Representation] -> Guarded [(A_Concept, TType)]
          mkTypeMap groups reprs =
            f
              <$> traverse typeOfGroup groups
              <*> traverse typeOfSingle [c | c <- conceptsOfReprs, c `notElem` conceptsOfGroups]
            where
              f :: [[(A_Concept, TType)]] -> [Maybe (A_Concept, TType, [Origin])] -> [(A_Concept, TType)]
              f typesOfGroups typesOfOthers =
                concat typesOfGroups <> map stripOrigin (catMaybes typesOfOthers)
              stripOrigin :: (A_Concept, TType, [Origin]) -> (A_Concept, TType)
              stripOrigin (cpt, t, _) = (cpt, t)
              reprTrios :: [(A_Concept, TType, Origin)]
              reprTrios = nubTrios $ concatMap toReprs reprs
                where
                  toReprs :: P_Representation -> [(A_Concept, TType, Origin)]
                  toReprs r@Repr {} = [(pCpt2aCpt cpt, reprdom r, origin r) | cpt <- NE.toList $ reprcpts r]
                  toReprs ImplicitRepr {} = []
                  nubTrios :: [(A_Concept, TType, Origin)] -> [(A_Concept, TType, Origin)]
                  nubTrios = map withNonFuzzyOrigin . NE.groupBy groupCondition
                    where
                      withNonFuzzyOrigin :: NE.NonEmpty (A_Concept, TType, Origin) -> (A_Concept, TType, Origin)
                      withNonFuzzyOrigin xs = case NE.filter (not . isFuzzyOrigin . thdOf3) xs of
                        [] -> NE.head xs
                        h : _ -> h
                      groupCondition :: (A_Concept, TType, Origin) -> (A_Concept, TType, Origin) -> Bool
                      groupCondition (cptA, typA, _) (cptB, typB, _) = cptA == cptB && typA == typB
                      thdOf3 (_, _, x) = x
              conceptsOfGroups :: [A_Concept]
              conceptsOfGroups = L.nub (concat groups)
              conceptsOfReprs :: [A_Concept]
              conceptsOfReprs = L.nub $ map fstOf3 reprTrios
                where
                  fstOf3 (cpt, _, _) = cpt
              typeOfSingle :: A_Concept -> Guarded (Maybe (A_Concept, TType, [Origin]))
              typeOfSingle cpt =
                case filter ofCpt reprTrios of
                  [] -> pure Nothing
                  rs -> case L.nub (map getTType rs) of
                    [] -> fatal "Impossible empty list."
                    [t] -> pure (Just (cpt, t, map getOrigin rs))
                    _ -> mkMultipleRepresentTypesError ([cpt], lst)
                      where
                        lst = [(snd3 (NE.head cl), fmap thd3 (NE.toList cl)) | cl <- eqCl snd3 rs]
                where
                  ofCpt :: (A_Concept, TType, Origin) -> Bool
                  ofCpt (cpt', _, _) = cpt == cpt'
                  getOrigin :: (A_Concept, TType, Origin) -> Origin
                  getOrigin (_, _, o) = o
              getTType :: (a, TType, b) -> TType
              getTType (_, t, _) = t
              typeOfGroup :: [A_Concept] -> Guarded [(A_Concept, TType)]
              typeOfGroup grp =
                do
                  singleTypes <- traverse typeOfSingle grp
                  let typeList = catMaybes singleTypes
                  case L.nub (map getTType typeList) of
                    [] -> pure []
                    [t] -> pure [(cpt, t) | cpt <- grp]
                    _ -> mkMultipleTypesInTypologyError typeList
          connect' :: [[A_Concept]] -> [[A_Concept]] -> [[A_Concept]]
          connect' typols gss =
            case gss of
              [] -> typols
              x : xs -> connect' (t : typols) rest
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

      pCpt2aCpt :: ConceptMap
      pCpt2aCpt = makeConceptMap (p_conceptdefs <> concatMap pt_cds p_patterns) (p_gens <> concatMap pt_gns p_patterns)

      -- story about genRules and genLattice
      -- the genRules is a list of equalities between concept sets, in which every set is interpreted as a conjunction of concepts
      -- the genLattice is the resulting optimized structure
      genRules :: [(Set.Set Type, Set.Set Type)] -- SJ: Why not [(NE.NonEmpty Type, NE.NonEmpty Type)] ?
      genRules =
        [ ( Set.fromList [pConcToType (specific x)],
            Set.fromList . NE.toList . NE.map pConcToType . generics $ x
          )
          | x <- alleGens
        ]

      completeTypePairs :: [(Set Type, Set Type)]
      completeTypePairs =
        genRules
          <> [ (Set.singleton (userConcept cpt), Set.fromList [BuiltIn (reprdom x), userConcept cpt])
               | x@Repr {} <- p_representations <> concatMap pt_Reprs p_patterns,
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
      genLattice = optimize1 (foldr addEquality emptySystem completeTypePairs)

      pClassify2aClassify :: PClassify -> Maybe AClassify
      pClassify2aClassify pg =
        case NE.tail (generics pg) of
          [] -> case filter (/= specCpt) [pCpt2aCpt . NE.head $ generics pg] of
            [] -> Nothing
            h : _ ->
              Just
                Isa
                  { genpos = origin pg,
                    gengen = h,
                    genspc = specCpt
                  }
          _ -> case NE.filter (/= specCpt) . fmap pCpt2aCpt $ generics pg of
            [] -> Nothing
            h : tl ->
              Just
                IsE
                  { genpos = origin pg,
                    genrhs = h NE.:| tl,
                    genspc = specCpt
                  }
        where
          specCpt = pCpt2aCpt (specific pg)

      userConcept :: P_Concept -> Type
      userConcept P_ONE = BuiltIn TypeOfOne
      userConcept (PCpt nm) = UserConcept nm

      pRepr2aRepr :: ContextInfo -> P_Representation -> Guarded A_Representation
      pRepr2aRepr ci repr@Repr {} = pure Arepr {origins = [origin repr], aReprFrom = fmap (conceptMap ci) (reprcpts repr), aReprTo = reprdom repr}
      pRepr2aRepr ci repr@ImplicitRepr {} =
        do
          expr <- (term2Expr ci . reprTerm) repr
          return (Arepr [origin repr] (target expr :| []) Object)

      pPop2aPop :: ContextInfo -> P_Population -> Guarded Population
      pPop2aPop ci pop =
        case pop of
          P_RelPopu {p_nmdr = nmdr, p_popps = aps, p_src = src, p_tgt = tgt} ->
            do
              expr <- term2Expr ci (Prim (PNamedR nmdr))
              let dcl = case expr of
                         EDcD d -> d
                         _ -> fatal ("Expected a relation in a population, but got "<>tshow expr<>".")
              aps' <- traverse (pAtomPair2aAtomPair (representationOf ci) dcl) aps
              src' <- maybeOverGuarded (getAsConcept ci (origin pop) <=< isMoreGeneric (origin pop) dcl Src . userConcept) src
              tgt' <- maybeOverGuarded (getAsConcept ci (origin pop) <=< isMoreGeneric (origin pop) dcl Tgt . userConcept) tgt
              return
                ARelPopu
                  { popdcl = dcl,
                    popps = Set.fromList aps',
                    popsrc = fromMaybe (source dcl) src',
                    poptgt = fromMaybe (target dcl) tgt'
                  }
          P_CptPopu {} ->
            let cpt = pCpt2aCpt (p_cpt pop)
             in ( \vals ->
                    ACptPopu
                      { popcpt = cpt,
                        popas = vals
                      }
                )
                  <$> traverse (pAtomValue2aAtomValue (representationOf ci) cpt) (p_popas pop)

      isMoreGeneric :: Origin -> Relation -> SrcOrTgt -> Type -> Guarded Type
      isMoreGeneric o dcl sourceOrTarget givenType =
        if givenType `elem` findExact genLattice (Atom (getConcept sourceOrTarget dcl) `Meet` Atom givenType)
          then pure givenType
          else mkTypeMismatchError o dcl sourceOrTarget givenType

      pViewDef2aViewDef :: ContextInfo -> P_ViewDef -> Guarded ViewDef
      pViewDef2aViewDef ci
        P_Vd{ pos = orig,
              vd_nm = nm,
              vd_label = lbl',
              vd_cpt = cpt,
              vd_isDefault = isDefault,
              vd_html = mHtml,
              vd_ats = segmnts
            } = do segments <- traverse typeCheckViewSegment (zip [0 ..] segmnts)
                   uniqueLables orig toLabel . filter hasLabel $ segments
                   return
                         Vd
                           { vdpos = orig,
                             vdname = nm,
                             vdlabel = lbl',
                             vdcpt = conceptMap ci cpt,
                             vdIsDefault = isDefault,
                             vdhtml = mHtml,
                             vdats = segments
                           }
        where
          toLabel :: ViewSegment -> Text1
          toLabel vs = case vsmlabel vs of
            Nothing -> fatal "Segments without a label should have been filtered out here"
            Just x -> x
          hasLabel :: ViewSegment -> Bool
          hasLabel = isJust . vsmlabel
          typeCheckViewSegment :: (Integer, P_ViewSegment TermPrim) -> Guarded ViewSegment
          typeCheckViewSegment (seqNr, seg) =
           do payload <- typecheckPayload (vsm_load seg)
              return
                ViewSegment
                  { vsmpos = origin seg,
                    vsmlabel = vsm_labl seg,
                    vsmSeqNr = seqNr,
                    vsmLoad = payload
                  }
            where
              typecheckPayload :: P_ViewSegmtPayLoad TermPrim -> Guarded ViewSegmentPayLoad
              typecheckPayload payload =
                case payload of
                  P_ViewExp term ->
                    do
                      viewExpr <- term2Expr ci (PCps orig (Prim (Pid orig cpt)) term)
                      pure (ViewExp viewExpr)
                  P_ViewText str -> pure $ ViewText str

      isa :: Type -> Type -> Bool
      isa c1 c2 = c1 `elem` findExact genLattice (Atom c1 `Meet` Atom c2) -- shouldn't this Atom be called a Concept? SJC: Answer: we're using the constructor "Atom" in the lattice sense, not in the relation-algebra sense. c1 and c2 are indeed Concepts here

      pBoxItem2aBoxItem :: ContextInfo -> P_BoxItem TermPrim -> Guarded BoxItem
      pBoxItem2aBoxItem contextInfo objDef =
        case objDef of
          P_BoxItemTerm
            { obj_PlainName = nm,
              obj_lbl = lbl',
              pos = orig,
              obj_term = term,
              obj_crud = mCrud,
              obj_mView = mView,
              obj_msub = subs
            } -> do
              objExpr <- term2Expr contextInfo term
              let tgtConcept = (aConcept2pConcept . target) objExpr
              checkCrud
              typeCheckViewAnnotation objExpr mView
              crud <- pCruds2aCruds objExpr mCrud
              s <- case subs of
                    Just sub@P_Box{} -> traverse (pBoxItem2aBoxItem contextInfo . hinge tgtConcept) (si_box sub)
                    Just P_InterfaceRef {si_str = str, si_isLink = isLink} ->
                      case lookupView str of
                        Just _  -> pure [BxExpr (ObjectDef nm lbl' orig objExpr crud mView (Just InterfaceRef{pos=orig, siIsLink = isLink, siIfcId = str}))]
                        Nothing -> Errors . pure $ mkUndeclaredError "view" objDef str
                    Nothing  -> pure []
              return
               (BxExpr ObjectDef
                        { objPlainName  = nm,
                          objlbl        = lbl',
                          objPos        = orig,
                          objExpression = objExpr,
                          objcrud       = crud,
                          objmView      = mView,
                          objmsub       = case subs of
                                           Nothing -> Nothing
                                           Just sub -> Just Box{ pos = orig,
                                                                 siConcept = target objExpr,
                                                                 siHeader = si_header sub,
                                                                 siObjs = s
                                                               }
                        })
              where
                -- | hinge inserts the target of the enveloping box expression to ensure that the sub-boxes are properly typed.
                hinge :: P_Concept -> P_BoxItem TermPrim -> P_BoxItem TermPrim
                hinge c pbi = pbi{ obj_term = PCps (origin pbi) (Prim (Pid (origin pbi) c)) (obj_term pbi) }
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
                typeCheckViewAnnotation :: Expression -> Maybe Name -> Guarded ()
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
          P_BxTxt
            { obj_PlainName = nm,
              pos = orig,
              box_txt = str
            } ->
              pure
                ( BxText
                    { boxPlainName = nm,
                      boxpos = orig,
                      boxtxt = str
                    }
                )

      pCruds2aCruds :: Expression -> Maybe P_Cruds -> Guarded Cruds
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

      pIfc2aIfc :: ContextInfo -> P_Interface -> Guarded Interface
      pIfc2aIfc contextInfo pIfc =
        do
          pBox <- pBoxItem2aBoxItem contextInfo (ifc_Obj pIfc)
          objDef <- case pBox of
                      BxExpr{} -> pure (objE pBox)
                      _ -> (Errors . return . CTXE (origin pIfc)) "TXT is not expected here."
          let objExpr = objExpression objDef
              ifcSource = source objExpr
              ifcSourceType = representationOf contextInfo ifcSource
          if ifcSourceType==Object
            then return
                   Ifc
                     { ifcIsAPI = ifc_IsAPI pIfc,
                       ifcname = name pIfc,
                       ifclbl = mLabel pIfc,
                       ifcRoles = ifc_Roles pIfc,
                       ifcObj =
                         objDef
                           { objPlainName = Just . fullName1 . name $ pIfc,
                             objlbl = mLabel pIfc
                           },
                       ifcPos = origin pIfc,
                       ifcPurpose = ifc_Prp pIfc
                     }
            else Errors . pure . CTXE (origin pIfc) . T.intercalate "\n  " $
                   [ "The TYPE of the concept for which an INTERFACE is defined must be OBJECT.",
                     "The TYPE of the concept `" <> (text1ToText . showWithAliases) ifcSource <> "`, for interface `" <> fullName pIfc <> "`, however is " <> tshow ifcSourceType <> "."
                   ]


      -- pIfc2aIfc :: ContextInfo -> (P_Interface, P_BoxItem (TermPrim, DisambPrim)) -> Guarded Interface
      -- pIfc2aIfc contextInfo (pIfc, objDisamb) =
      --   build $ pBoxItemDisamb2BoxItem contextInfo objDisamb
      --   where
      --     build :: Guarded BoxItem -> Guarded Interface
      --     build gb =
      --       case gb of
      --         Errors x -> Errors x
      --         Checked obj' ws ->
      --           addWarnings ws
      --             $ case obj' of
      --               BxExpr o ->
      --                 case ttype . target . objExpression $ o of
      --                   Object ->
      --                     pure
      --                       Ifc
      --                         { ifcIsAPI = ifc_IsAPI pIfc,
      --                           ifcname = name pIfc,
      --                           ifclbl = mLabel pIfc,
      --                           ifcRoles = ifc_Roles pIfc,
      --                           ifcObj =
      --                             o
      --                               { objPlainName = Just . fullName1 . name $ pIfc,
      --                                 objlbl = mLabel pIfc
      --                               },
      --                           ifcPos = origin pIfc,
      --                           ifcPurpose = ifc_Prp pIfc
      --                         }
      --                   tt ->
      --                     Errors
      --                       . pure
      --                       . mkInterfaceMustBeDefinedOnObject pIfc (target . objExpression $ o)
      --                       $ tt
      --               BxText {} -> fatal "Unexpected BxTxt" -- Interface should not have TXT only. it should have a term object.
      --     ttype :: A_Concept -> TType
      --     ttype = representationOf contextInfo

      pRoleRule2aRoleRule :: P_RoleRule -> A_RoleRule
      pRoleRule2aRoleRule prr =
        A_RoleRule
          { arRoles = mRoles prr,
            arRules = mRules prr,
            arPos = origin prr
          }

      pPat2aPat :: ContextInfo -> P_Pattern -> Guarded Pattern
      pPat2aPat ci ppat =
        f
          <$> traverse (pRul2aRul ci (Just $ label ppat)) (pt_rls ppat)
          <*> traverse (pIdentity2aIdentity ci (Just $ label ppat)) (pt_ids ppat)
          <*> traverse (pPop2aPop ci) (pt_pop ppat)
          <*> traverse (pViewDef2aViewDef ci) (pt_vds ppat)
          <*> traverse (pPurp2aPurp ci) (pt_xps ppat)
          <*> traverse (pDecl2aDecl (representationOf ci) (conceptMap ci) (Just $ label ppat) deflangCtxt deffrmtCtxt) (pt_dcs ppat)
          <*> pure (fmap (pConcDef2aConcDef (conceptMap ci) (defaultLang ci) (defaultFormat ci)) (pt_cds ppat))
          <*> pure (fmap pRoleRule2aRoleRule (pt_RRuls ppat))
          <*> pure (pt_Reprs ppat)
          <*> traverse (pEnforce2aEnforce ci (Just $ label ppat)) (pt_enfs ppat)
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
        ContextInfo ->
        Maybe Text -> -- name of pattern the rule is defined in (if any), just for documentation purposes.
        P_Rule TermPrim ->
        Guarded Rule
      pRul2aRul
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
            exp' <- term2Expr ci expr
            vls <- maybeOverGuarded (typeCheckPairView ci orig exp') viols
            return
              Rule
                { rrnm = nm,
                  rrlbl = lbl',
                  formalExpression = exp',
                  rrfps = orig,
                  rrmean = map (pMean2aMean deflangCtxt deffrmtCtxt) meanings,
                  rrmsg = map (pMess2aMess deflangCtxt deffrmtCtxt) msgs,
                  rrviol = vls,
                  rrpat = mPat,
                  rrkind = UserDefined
                }
      -- | The AEnforce calls the PHP-ExecEngine in the rrviol field of mkRule in the where-part, to enforce this rule.
      pEnforce2aEnforce ::
        ContextInfo ->
        Maybe Text -> -- name of pattern the enforcement rule is defined in (if any), just for documentation purposes.
        P_Enforce TermPrim ->
        Guarded AEnforce
      pEnforce2aEnforce
        ci
        mPat
        P_Enforce
          { pos = pos',
            penfRel = pRel,
            penfOp = oper,
            penfExpr = x
          } = case oper of
                IsSuperSet {} ->
                  do xpr <- term2Expr ci (PInc pos' x (Prim pRel))
                     case xpr of
                       EInc (expr,EDcD rel) -> return (toAEnforce rel expr)
                       _ -> fatal "Alternative 1 in pEnforce2aEnforce."
                IsSubSet {} ->
                  do xpr <- term2Expr ci (PInc pos' (Prim pRel) x)
                     case xpr of
                       EInc (EDcD rel,expr) -> return (toAEnforce rel expr)
                       _ -> fatal "Alternative 2 in pEnforce2aEnforce."
                IsSameSet {} ->
                  do xpr <- term2Expr ci (PEqu pos' (Prim pRel) x)
                     case xpr of
                       EEqu (EDcD rel,expr) -> return (toAEnforce rel expr)
                       _ -> fatal "Alternative 3 in pEnforce2aEnforce."
          where
            toAEnforce :: Relation -> Expression -> AEnforce
            toAEnforce rel expr
             = AEnforce
                 { pos = pos',
                   enfRel = rel,
                   enfOp = oper,
                   enfExpr = expr,
                   enfPatName = mPat,
                   enfRules = case oper of
                               IsSuperSet {} -> [insPair]
                               IsSubSet {} -> [delPair]
                               IsSameSet {} -> [insPair, delPair]
                 }
              where
                insPair = mkRule "InsPair" (EInc (expr, EDcD rel))
                delPair = mkRule "DelPair" (EInc (EDcD rel, expr))
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
        ContextInfo ->
        Maybe Text -> -- name of pattern the rule is defined in (if any), just for documentation purposes.
        P_IdentDef ->
        Guarded IdentityRule
      pIdentity2aIdentity ci mPat pidt =
        do isegs <- traverse (term2Expr ci) (ix_ats pidt)
           return ( Id
                    { idPos = origin pidt,
                      idName = ix_name pidt,
                      idlabel = ix_label pidt,
                      idCpt = pCpt2aCpt (ix_cpt pidt),
                      idPat = mPat,
                      identityAts = isegs
                    }
                 )

      typeCheckPairView :: ContextInfo -> Origin -> Expression -> PairView (Term TermPrim) -> Guarded (PairView Expression)
      typeCheckPairView ci o x (PairView lst) =
        PairView <$> traverse (typeCheckPairViewSeg ci o x) lst
      typeCheckPairViewSeg :: ContextInfo -> Origin -> Expression -> PairViewSegment (Term TermPrim) -> Guarded (PairViewSegment Expression)
      typeCheckPairViewSeg _ _ _ (PairViewText orig x) = pure (PairViewText orig x)
      typeCheckPairViewSeg ci o expr (PairViewExp orig s x) =
        do
          let src = (aConcept2pConcept . source) expr
              tgt = (aConcept2pConcept . target) expr
          e <- term2Expr ci (case s of
                              Src -> PCps o (Prim (Pid o src)) x
                              Tgt -> PCps o (Prim (Pid o tgt)) x
                            )
          return (PairViewExp orig s e)
      pPurp2aPurp :: ContextInfo -> PPurpose -> Guarded Purpose
      pPurp2aPurp
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
                  explMarkup = pMarkup2aMarkup deflangCtxt deffrmtCtxt pmarkup,
                  explRefIds = refIds
                }
          )
            <$> pRefObj2aRefObj ci objref
      pRefObj2aRefObj :: ContextInfo -> PRef2Obj -> Guarded ExplObj
      pRefObj2aRefObj ci (PRef2ConceptDef s) = (pure . ExplConcept . conceptMap ci . mkPConcept) s
      pRefObj2aRefObj ci (PRef2Relation tm) = ExplRelation <$> namedRel2Decl (conceptMap ci) (declarationsMap ci) tm
      pRefObj2aRefObj _ (PRef2Rule s) = pure $ ExplRule s
      pRefObj2aRefObj _ (PRef2IdentityDef s) = pure $ ExplIdentityDef s
      pRefObj2aRefObj _ (PRef2ViewDef s) = pure $ ExplViewDef s
      pRefObj2aRefObj _ (PRef2Pattern s) = pure $ ExplPattern s
      pRefObj2aRefObj _ (PRef2Interface s) = pure $ ExplInterface s
      pRefObj2aRefObj _ (PRef2Context s) = pure $ ExplContext s
      allConceptDefsOutPats :: ContextInfo -> [AConceptDef]
      allConceptDefsOutPats ci = map (pConcDef2aConcDef (conceptMap ci) deflangCtxt deffrmtCtxt) p_conceptdefs
      allConceptDefs :: ContextInfo -> [AConceptDef]
      allConceptDefs ci = map (pConcDef2aConcDef (conceptMap ci) deflangCtxt deffrmtCtxt) (p_conceptdefs <> concatMap pt_cds p_patterns)
      udefRoleRules' :: [A_RoleRule]
      udefRoleRules' =
        map
          pRoleRule2aRoleRule
          (p_roleRules <> concatMap pt_RRuls p_patterns)


signatures :: ContextInfo -> Term TermPrim -> Guarded [Signature]
signatures contextInfo trm = case trm of
  Prim (PI _)              ->                       pure [Sign c c| c<-conceptList]
  Prim (Pid _ c)           -> let c'=pCpt2aCpt c in pure [Sign c' c']
  Prim (Patm _ _ (Just c)) -> let c'=pCpt2aCpt c in pure [Sign c' c']
  Prim (Patm _ _  Nothing) ->                       pure [Sign c c| c<-conceptList]
  Prim (PVee _)            ->                       pure [Sign src tgt | src<-conceptList, tgt<-conceptList ]
  Prim (Pfull _ src tgt)   ->                       pure [Sign (pCpt2aCpt src) (pCpt2aCpt tgt)]
  Prim (PBin _ _)          ->                       pure [Sign c c| c<-conceptList]
  Prim (PBind _ _ c)       -> let c'=pCpt2aCpt c in pure [Sign c' c']
  Prim (PNamedR rel)       -> let sgns :: Maybe P_Sign -> [Signature]
                                  sgns (Just sgn) = (map sign . findRelsTyped (declarationsMap contextInfo) (name rel) . pSign2aSign pCpt2aCpt) sgn
                                  sgns Nothing    = (fmap sign . findDecls (declarationsMap contextInfo) . name) rel
                              in  case sgns (p_mbSign rel) of
                                               [] -> (Errors . return . CTXE (origin trm)) ("No signature found for relation "<> tshow rel)
                                               ss -> pure ss
  PEqu o a b -> msgPeri o "equation" a b
  PInc o a b -> msgPeri o "inclusion" a b
  PIsc o a b -> msgPeri o "intersection" a b
  PUni o a b -> msgPeri o "union" a b
  PDif o a b -> msgPeri o "difference" a b
  PLrs o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign (source sgn_a) (source sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just True<-[leq conceptGraph (target sgn_b) (target sgn_a)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the target concepts of the two sides of the left residual.\n  The target of: "<>displayLeft sgna a<>"should be equal (or more generic) than the target of "<>displayRight sgnb b<>".")
                    ss -> return ss
  PRrs o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign (target sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just True<-[leq conceptGraph (source sgn_a) (source sgn_b)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the source concepts of the two sides of the right residual.\n  The source of: "<>displayLeft sgna a<>"should be equal (or more specific) than the source of "<>displayRight sgnb b<>".")
                    ss -> return ss
  PDia o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, target sgn_a == source sgn_b ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the signatures of the two sides of the diamond.\n  The target of: "<>displayLeft sgna a<>"should be equal to the source of "<>displayRight sgnb b<>".")
                    ss -> return ss
  PCps o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just _between<-[glb conceptGraph (target sgn_a) (source sgn_b)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the signatures of the two sides of the composition.\n  The target of "<>displayLeft sgna a<>"should be equal to (or share a concept with) the source of "<>displayRight sgnb b<>".")
                    ss -> return ss
  PRad o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just _between<-[lub conceptGraph (target sgn_a) (source sgn_b)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the signatures of the two sides of the relative addition.\n  The target of: "<>displayLeft sgna a<>"should be equal to (or share a concept with) the source of "<>displayRight sgnb b<>".")
                    ss -> return ss
  PPrd _ a b -> do sgna <- signats a; sgnb <- signats b
                   return [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb ]
  PKl0 _ e   -> signats e
  PKl1 _ e   -> signats e
  PFlp _ e   -> fmap flp (signats e)
  PCpl _ e   -> signats e
  PBrk _ e   -> signats e
  where
    displayLeft sgns expr =
      showP expr<>case sgns of
                    [sgn] -> ", which is "<>tshow (target sgn)<>", "
                    []    -> " is undefined because "<>showP expr<>" is untypable, but it"
                    _     -> ", which can be any of "<>tshow (map target sgns)<>", "
    displayRight sgns expr =
      showP expr<>case sgns of
                    [sgn] -> ", which is "<>tshow (source sgn)
                    []    -> " is undefined because "<>showP expr<>" is untypable"
                    _     -> ", which can be any of "<>tshow (map source sgns)
    -- | msgPeri generates a type error message for equations, inclusions, unions, and intersects.
    msgPeri :: Origin -> Text -> Term TermPrim -> Term TermPrim -> Guarded [Signature]
    msgPeri o kind a b =
      do sgna <- signats a; sgnb <- signats b
         let sgnsSrc = [ src | sgn_a<-sgna, sgn_b<-sgnb, Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)] ]
             sgnsTgt = [ tgt | sgn_a<-sgna, sgn_b<-sgnb, Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)] ]
         case [ Sign src tgt | sgn_a<-sgna, sgn_b<-sgnb, Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)] ] of
          []  -> case (sgnsSrc, sgnsTgt) of
                  ([],_:_) -> (Errors . return . CTXE o) ("Cannot match the source concepts of the two sides of the "<>kind<>".\n   The source of "<>showP a<>" is "<>showSgns (map source sgna)<>"\n   The source of "<>showP b<>" is "<>showSgns (map source sgnb))
                  (_:_,[]) -> (Errors . return . CTXE o) ("Cannot match the target concepts of the two sides of the "<>kind<>".\n   The target of "<>showP a<>" is "<>showSgns (map target sgna)<>"\n   The target of "<>showP b<>" is "<>showSgns (map target sgnb))
                  _        -> (Errors . return . CTXE o) ("Cannot match the signatures at both sides of the "<>kind<>".\n   On the left side, "<>showP a<>" is "<>showSgns sgna<>"\n   On the right side, "<>showP b<>" is "<>showSgns sgnb)
                 where
                   showSgns :: Show a => [a] -> Text
                   showSgns sgns = case sgns of
                                    [] ->     "untyped"
                                    [sgn] ->  tshow sgn
                                    sgn:ss -> (T.intercalate ", " . map tshow) ss<>", or "<>tshow sgn
          sgns -> return sgns

    conceptList :: [A_Concept]
    conceptList = (vertexList . makeGraph . allGens) contextInfo
    pCpt2aCpt = conceptMap contextInfo
    conceptGraph :: AdjacencyMap A_Concept
    conceptGraph = overlay (makeGraph (allGens contextInfo)) (vertices (Set.toList (allConcepts contextInfo)))
    signats = signatures contextInfo

dereference :: ContextInfo -> [Signature] -> TermPrim -> Guarded Expression
dereference contextInfo sgns trmprim
  = case trmprim of
      PI _           -> guard [ EDcI c | Sign c c'<-sgns, c==c']
      Pid _ c        -> guard [ EDcI (pCpt2aCpt c) | Sign s t<-sgns, s==t, pCpt2aCpt c==s]
      Patm _ av mc   -> case mc of
                         Just c  -> guard [ EMp1 av s | Sign s t<-sgns, s==t, pCpt2aCpt c==s]
                         Nothing -> guard [ EMp1 av s | Sign s t<-sgns, s==t]
      PVee _         -> guard [ EDcV sgn | sgn<-sgns]
      Pfull _ s t    -> guard [ EDcV sgn | sgn<-sgns, Just _src<-[source sgn `grLwB` pCpt2aCpt s], Just _tgt<-[target sgn `grLwB` pCpt2aCpt t]]
      PBin _ oper    -> guard [ EBin oper s | Sign s t<-sgns, s==t]
      PBind _ oper c -> guard [ EBin oper s | Sign s t<-sgns, s==t, pCpt2aCpt c==s]
      PNamedR rel    -> guard [ EDcD decl | sgn<-sgns, decl<-rels rel, Just _src<-[source sgn `grLwB` source decl], Just _tgt<-[target sgn `grLwB` target decl]]
    where
      guard :: [Expression] -> Guarded Expression
      guard []     = Errors . return $ CTXE (origin trmprim) ("Cannot derive a signature for "<>showP trmprim<>".")
      guard [expr] = pure expr
      guard exprs  = Errors . return $ CTXE (origin trmprim) ("Ambiguous "<>showP trmprim<>". Please specify "<>if length exprs>4 then "the type explicitly." else "one of: "<>T.intercalate ", " (map (tshow . source) exprs)<>".")
      rels :: P_NamedRel -> [Relation]
      rels rel = case p_mbSign rel of
                  Just sg -> (findRelsTyped (declarationsMap contextInfo) (name rel) . pSign2aSign pCpt2aCpt) sg
                  Nothing -> (findDecls (declarationsMap contextInfo) . name) rel
      conceptGraph :: AdjacencyMap A_Concept
      conceptGraph = overlay (makeGraph (allGens contextInfo)) (vertices (Set.toList (allConcepts contextInfo)))
      grLwB = glb conceptGraph
      -- lsUpB = glb conceptGraph
      pCpt2aCpt = conceptMap contextInfo

term2Expr :: ContextInfo -> Term TermPrim -> Guarded Expression
term2Expr contextInfo trm = do
  sgns <- signatures contextInfo trm
  case trm of
    Prim tp -> dereference contextInfo sgns tp
    _ -> mapTermStructure sgns trm
  where
    mapTermStructure :: [Signature] -> Term TermPrim -> Guarded Expression
    mapTermStructure sgns term =
      -- Handle the base case separately since it needs signatures
      case term of
        Prim tp -> dereference contextInfo sgns tp
        PEqu _ a b -> EEqu <$> processPair a b
        PInc _ a b -> EInc <$> processPair a b
        PIsc _ a b -> EIsc <$> processPair a b
        PUni _ a b -> EUni <$> processPair a b
        PDif _ a b -> EDif <$> processPair a b
        PLrs _ a b -> ELrs <$> processPair a b
        PRrs _ a b -> ERrs <$> processPair a b
        PDia _ a b -> EDia <$> processPair a b
        PCps _ a b -> ECps <$> processPair a b
        PRad _ a b -> ERad <$> processPair a b
        PPrd _ a b -> EPrd <$> processPair a b
        PKl0 _ e -> EKl0 <$> term2Expr contextInfo e
        PKl1 _ e -> EKl1 <$> term2Expr contextInfo e
        PFlp _ e -> EFlp <$> term2Expr contextInfo e
        PCpl _ e -> ECpl <$> term2Expr contextInfo e
        PBrk _ e -> term2Expr contextInfo e
    processPair a b = (,) <$> term2Expr contextInfo a <*> term2Expr contextInfo b


pAtomPair2aAtomPair :: (A_Concept -> TType) -> Relation -> PAtomPair -> Guarded AAtomPair
pAtomPair2aAtomPair typ dcl pp =
  mkAtomPair
    <$> pAtomValue2aAtomValue typ (source dcl) (ppLeft pp)
    <*> pAtomValue2aAtomValue typ (target dcl) (ppRight pp)

pAtomValue2aAtomValue :: (A_Concept -> TType) -> A_Concept -> PAtomValue -> Guarded AAtomValue
pAtomValue2aAtomValue typ cpt pav =
  case unsafePAtomVal2AtomValue ttyp (Just cpt) pav of
    Left msg -> Errors . pure $ mkIncompatibleAtomValueError pav msg
    Right av -> pure av
  where
    ttyp = typ cpt

pDecl2aDecl ::
  (A_Concept -> TType) ->
  ConceptMap ->
  Maybe Text -> -- label of pattern the rule is defined in (if any), just for documentation purposes
  Lang -> -- The default language
  PandocFormat -> -- The default pandocFormat
  P_Relation ->
  Guarded Relation
pDecl2aDecl typ cptMap maybePatLabel defLanguage defFormat pd =
  do
    checkEndoProps
    -- propLists <- mapM pProp2aProps . Set.toList $ dec_prps pd
    dflts <- mapM pReldefault2aReldefaults . L.nub $ dec_defaults pd
    return
      Relation
        { decnm = dec_nm pd,
          decsgn = decSign,
          declabel = dec_label pd,
          decprps = Set.fromList . concatMap pProp2aProps . Set.toList $ dec_prps pd,
          decDefaults = Set.fromList dflts,
          decpr = dec_pragma pd,
          decMean = map (pMean2aMean defLanguage defFormat) (dec_Mean pd),
          decfpos = origin pd,
          decusr = True,
          decpat = maybePatLabel,
          dechash = hash (dec_nm pd) `hashWithSalt` decSign
        }
  where
    pReldefault2aReldefaults :: PRelationDefault -> Guarded ARelDefault
    pReldefault2aReldefaults x = case x of
      PDefAtom st vals ->
        ARelDefaultAtom st
          <$> traverse
            ( pAtomValue2aAtomValue
                typ
                ( case st of
                    Src -> source decSign
                    Tgt -> target decSign
                )
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

pConcDef2aConcDef ::
  ConceptMap ->
  Lang -> -- The default language
  PandocFormat -> -- The default pandocFormatPConceptDef
  PConceptDef ->
  AConceptDef
pConcDef2aConcDef pCpt2aCpt defLanguage defFormat pCd =
  AConceptDef
    { pos = origin pCd,
      acdcpt = pCpt2aCpt (PCpt {p_cptnm = name pCd}),
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

maybeOverGuarded :: (t -> Guarded a) -> Maybe t -> Guarded (Maybe a)
maybeOverGuarded _ Nothing = pure Nothing
maybeOverGuarded f (Just x) = Just <$> f x

getConcept :: (HasSignature a) => SrcOrTgt -> a -> Type
getConcept Src = aConcToType . source
getConcept Tgt = aConcToType . target
