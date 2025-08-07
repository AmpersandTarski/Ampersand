{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
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
import Ampersand.Basics hiding (conc, set, guard, join)
import Ampersand.Classes
import Ampersand.Core.A2P_Converters (aConcept2pConcept)
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowAStruct
import Ampersand.Core.ShowPStruct (showP)
import Ampersand.FSpec.ToFSpec.Populated (sortSpecific2Generic)
import Ampersand.Input.ADL1.CtxError
import Ampersand.Misc.HasClasses
import Ampersand.Runners (logLevel)
import Ampersand.Types.Config (HasRunner, runnerL)
import Algebra.Graph.AdjacencyMap
import Data.Tuple.Extra (fst3, snd3, thd3)
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
aConcToType p = (usc . fmap fst . Set.toList . aliases) p
 where usc (nm:_) = UserConcept nm; usc _ = fatal "Not a proper A_Concept"

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
  (HasFSpecGenOpts env, HasRunner env) =>
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
              conceptGraph = makeGraph gns,
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
          expr <- (term2Expr env ci . reprTerm) repr
          return (Arepr [origin repr] (target expr :| []) Object)

      pPop2aPop :: ContextInfo -> P_Population -> Guarded Population
      pPop2aPop ci pop =
        case pop of
          P_RelPopu {p_nmdr = nmdr, p_popps = aps, p_src = src, p_tgt = tgt} ->
            do
              expr <- term2Expr env ci (Prim (PNamedR nmdr))
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
                      viewExpr <- term2Expr env ci (PCps orig (Prim (Pid orig cpt)) term)
                      pure (ViewExp viewExpr)
                  P_ViewText str -> pure $ ViewText str

      isa :: Type -> Type -> Bool
      isa c1 c2 = c1 `elem` findExact genLattice (Atom c1 `Meet` Atom c2) -- shouldn't this Atom be called a Concept? SJC: Answer: we're using the constructor "Atom" in the lattice sense, not in the relation-algebra sense. c1 and c2 are indeed Concepts here

      -- | pSubIfc2aSubIfc takes the target of its object-expression (i.e. tgtConcept) and composes it with the object-expression of every sub-interface.
      -- Thus, the type checker can ensure that all box-items in the interface are properly typed.
      -- pSubIfc2aSubIfc :: ContextInfo -> A_Concept -> P_SubIfc TermPrim -> Guarded SubInterface
      pSubIfc2aSubIfc :: ContextInfo -> A_Concept -> P_SubIfc TermPrim -> Guarded SubInterface
      pSubIfc2aSubIfc contextInfo tgtConcept sub =
        case sub of
          P_Box{} -> do subBoxes <- mapM (pBoxItem2aBoxItem contextInfo (Just tgtConcept)) (si_box sub)
                        return (Box{ pos = origin sub, siConcept = tgtConcept, siHeader = si_header sub, siObjs = subBoxes})
          P_InterfaceRef
            { pos       = orig,
              si_isLink = isLink,
              si_str    = ifcName
            } -> do srcIfc <- getInterface -- get the source atom of the interface that is referenced, to check for compatibility.
                    return (InterfaceRef { pos       = orig,
                                           siIsLink  = isLink,
                                           siIfcId   = ifcName,
                                           siConcept = srcIfc
                                         })
        where
          -- | prefixWithI inserts the target of the enveloping box expression to ensure that the sub-boxes are properly typed.
          getInterface :: Guarded A_Concept
          getInterface
           = case filter ((==si_str sub).name) p_interfaces of
               [] -> (Errors . return . CTXE (origin sub)) ("Interface " <> (tshow.si_str) sub <> " not found")
               [pIfc] -> do expr <- (term2Expr env contextInfo . obj_term . ifc_Obj) pIfc
                            let srcCpt = source expr
                            case leq (conceptGraph contextInfo) tgtConcept srcCpt of
                              Just True  -> return srcCpt
                              Just False -> (Errors . return . CTXE (origin sub)) ("Interface " <> (tshow.name) pIfc <> " from " <> (tshow.origin) pIfc <> " is incompatible.\n   It requires CLASSIFY " <> tshow tgtConcept <> " ISA " <> tshow srcCpt)
                              Nothing    -> (Errors . return . CTXE (origin sub)) ("Interface " <> (tshow.name) pIfc <> " from " <> (tshow.origin) pIfc <> " is incompatible.\n   Its concept is " <> tshow srcCpt <> " but I expected " <> tshow tgtConcept <> ".")
               _ -> (Errors . return . CTXE (origin sub)) ("Multiple interfaces with name " <> (tshow.si_str) sub <> " found")

      -- | mCpt is the A_Concept that links the objExpression of this BoxItem to the objExpressions of its subBoxes.
      --   It is "Maybe" because on the top level, in pIfc2aIfc, there is no concept to link to. 
      pBoxItem2aBoxItem :: ContextInfo -> Maybe A_Concept -> P_BoxItem TermPrim -> Guarded BoxItem
      pBoxItem2aBoxItem contextInfo mCpt pBoxItem =
        case pBoxItem of
          P_BoxItemTerm
            { obj_PlainName = nm,
              obj_lbl = lbl',
              pos = orig,
              obj_term = term,
              obj_crud = mCrud,
              obj_mView = mView,
              obj_msub = p_msub
            } -> do
              objExpr <- term2Expr env contextInfo term
              -- The following strange construction gives a type error if subinterfaces don't match with their parent's target concept.
              -- This ensures that checkCrud will give the correct error messages because it works on objExpr.
              expr <- case mCpt of
                           Just tgtConcept -> term2Expr env contextInfo (PCps (origin term) (Prim (Pid (origin term) (aConcept2pConcept tgtConcept))) term)
                           Nothing         -> pure objExpr
              a_msub <- traverse (pSubIfc2aSubIfc contextInfo (target objExpr)) p_msub
              checkCrud
              typeCheckViewAnnotation objExpr mView
              crud <- pCruds2aCruds objExpr mCrud
              return (BxExpr ObjectDef{ objPlainName  = nm,
                                        objlbl        = lbl',
                                        objPos        = orig,
                                        objExpression = expr,
                                        objcrud       = crud,
                                        objmView      = mView,
                                        objmsub       = a_msub
                                      })
              where
                lookupView :: Name -> Maybe P_ViewDef
                lookupView viewId = case [vd | vd <- p_viewdefs, vd_nm vd == viewId] of
                  [] -> Nothing
                  vd : _ -> Just vd -- return the first one, if there are more, this is caught later on by uniqueness static check
                checkCrud :: Guarded ()
                checkCrud =
                  case (mCrud, p_msub) of
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
                                $ mkIncompatibleViewError pBoxItem viewId viewAnnCptStr viewDefCptStr
                    Nothing -> Errors . pure $ mkUndeclaredError "view" pBoxItem viewId
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
          pBox <- pBoxItem2aBoxItem contextInfo Nothing (ifc_Obj pIfc)
          boxItem <- case pBox of
                      BxExpr{} -> pure (objE pBox)
                      _ -> (Errors . return . CTXE (origin pIfc)) "TXT is not expected here."
          let objExpr = objExpression boxItem
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
                         boxItem
                           { objPlainName = Just . fullName1 . name $ pIfc,
                             objlbl = mLabel pIfc
                           },
                       ifcPos = origin pIfc,
                       ifcPurpose = ifc_Prp pIfc
                     }
            else Errors . pure . CTXE (origin pIfc) . T.intercalate "\n  " $
                   [ "The TYPE of the concept for which an INTERFACE is defined must be OBJECT.",
                     "However, the TYPE of the concept `" <> (text1ToText . showWithAliases) ifcSource <> "` for interface `" <> fullName pIfc <> "` is " <> tshow ifcSourceType <> "."
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
            exp' <- term2Expr env ci expr
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
                  do xpr <- term2Expr env ci (PInc pos' x (Prim pRel))
                     case xpr of
                       EInc (expr,EDcD rel) -> return (toAEnforce rel expr)
                       _ -> fatal "Alternative 1 in pEnforce2aEnforce."
                IsSubSet {} ->
                  do xpr <- term2Expr env ci (PInc pos' (Prim pRel) x)
                     case xpr of
                       EInc (EDcD rel,expr) -> return (toAEnforce rel expr)
                       _ -> fatal "Alternative 2 in pEnforce2aEnforce."
                IsSameSet {} ->
                  do xpr <- term2Expr env ci (PEqu pos' (Prim pRel) x)
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
                        case try2Name RuleName $ "Compute" <> (tshow . abs . hash $ lbl') of
                          Left err -> fatal $ "Not a proper Name: " <> err
                          Right (nm, _) -> nm,
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
        do isegs <- traverse (term2Expr env ci) (ix_ats pidt)
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
          e <- term2Expr env ci (case s of
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

data OpTree a
  = STbinary  { lSigns  :: OpTree a
              , rSigns  :: OpTree a
              , opSigns :: [a]
              }
  | STnullary { opSigns :: [a] }
  deriving (Eq, Functor)

instance (Show a) => Show (OpTree a) where
  show = T.unpack . showOpTreeStructure
    where
      showOpTreeStructure :: (Show a) => OpTree a -> Text
      showOpTreeStructure opTree = T.intercalate "\n" (showIndented "   " opTree)

      showIndented :: (Show a) => Text -> OpTree a -> [Text]
      showIndented indent (STnullary ss) =
        [indent <> T.intercalate ", " (fmap tshow ss)]
      showIndented indent (STbinary l r ss) =
        (indent <> T.intercalate ", " (fmap tshow ss)) :
        showIndented (indent <> "   ") l <>
        showIndented (indent <> "   ") r

{-
showOpTree formats the OpTree structure in a readable way.
Cline built it on Aug 6th, 2025.
Personally, I would never have put in the effort myself but it was too much fun to let Cline do it.)
Example output when compiling testing/Travis/testcases/prototype/shouldSucceed/try17.adl:
Signatures yields:
[X*Y]       └── result3 [X*Y]=result1 [X*Course];manages [Activity*Y]
[X*Y]           ├── result3 [X*Y]
[X*Y]           └── result1 [X*Course];manages [Activity*Y]
[X*Course]          ├── result1 [X*Course]
[Course*Y]          └── manages [Activity*Y]
This function .
-}
showOpTree :: OpTree (Expression, Signature) -> Text
showOpTree opTree = T.intercalate "\n" (map T.concat (L.transpose [sigTexts, paddings, treeLines, exprTexts]))
  where
    -- Step 1: Extract all signatures as [Text]
    sigTexts = extractSignatures opTree

    -- Step 2: Compute maximum length and create padding list
    maxSigWidth = foldr (max . T.length) 0 sigTexts
    paddings = map (\sigText -> T.replicate (maxSigWidth - T.length sigText + 1) " ") sigTexts

    -- Step 3: Create recursive tree of expressions as [Text]  
    treeLines = extractTreeLines opTree "" True
    exprTexts = extractExpressions opTree

    extractSignatures :: OpTree (Expression, Signature) -> [Text]
    extractSignatures (STnullary ss) = [T.intercalate ", " (map (tshow . snd) ss)]
    extractSignatures (STbinary l r ss) = [T.intercalate ", " (map (tshow . snd) ss)] <> extractSignatures l <> extractSignatures r

    extractExpressions :: OpTree (Expression, Signature) -> [Text]
    extractExpressions (STnullary ss) = [T.intercalate ", " (map (showA . fst) ss)]
    extractExpressions (STbinary l r ss) = [T.intercalate ", " (map (showA . fst) ss)] <> extractExpressions l <> extractExpressions r

    extractTreeLines :: OpTree (Expression, Signature) -> Text -> Bool -> [Text]
    extractTreeLines (STnullary _) prefix isLast =
      [if T.null prefix then "" else prefix <> (if isLast then "└── " else "├── ")]
    extractTreeLines (STbinary l r _) prefix isLast =
      [if T.null prefix then "" else prefix <> (if isLast then "└── " else "├── ")] <>
      extractTreeLines l newPrefix False <>
      extractTreeLines r newPrefix True
      where
        newPrefix = prefix <> (if isLast then "    " else "│   ")

instance (Flippable a) => Flippable (OpTree a) where
  flp (STbinary a b ss) = STbinary (flp b) (flp a) (flp ss)
  flp (STnullary ss) = STnullary (flp ss)

-- | signatures constructs a tree with all possible (expression, signature) pairs in each node of the tree.
--   It is used by the function term2Expr to weed out these pairs down to one for every (sub-)term,
--   to establish a unique signature for the term and all of its subterms.
--   Post:
--    - Every set of (expression, signature) pairs in the OpTree is not empty.
--    - Every signature is consistent with the type rules of the term it corresponds to.
-- Helper function for creating verbose type error messages
mkVerboseTypeError :: (HasRunner env) => env -> Origin -> Text -> OpTree (Expression, Signature) -> Guarded a
mkVerboseTypeError env origin baseMsg opTree =
  Errors . return $ CTXE origin $
    if logLevel (view runnerL env) <= LevelInfo
    then baseMsg <> "\n\nType analysis:\n" <> showOpTree opTree
    else baseMsg

-- Helper function for creating type mismatch errors with optional suggestions
mkVerboseTypeMismatchError :: (HasRunner env) => env -> Origin -> Text -> Maybe Text -> OpTree (Expression, Signature) -> Guarded a
mkVerboseTypeMismatchError env origin baseMsg maybeSuggestions opTree =
  let fullMsg = baseMsg <> fromMaybe "" maybeSuggestions
  in mkVerboseTypeError env origin fullMsg opTree

signatures :: (HasFSpecGenOpts env, HasRunner env) => env -> ContextInfo -> Term TermPrim -> Guarded (OpTree (Expression, Signature))
signatures env contextInfo trm = case trm of
  Prim (PI _)               ->                       pure (STnullary [(EDcI anyCpt, ISgn anyCpt)])
  PCpl _ e@(Prim (PI _))    ->                       signats e
  Prim (Pid _ c)            -> let c'=pCpt2aCpt c in pure (STnullary [(EDcI c', ISgn c')])
  Prim (Patm _ av (Just c)) -> let c'=pCpt2aCpt c in pure (STnullary [(EMp1 av c', ISgn c')])
  Prim (Patm _ av Nothing)  ->                       pure (STnullary [(EMp1 av anyCpt, ISgn anyCpt)])
  PCpl _ (Prim (Patm _ av Nothing)) ->               pure (STnullary [(ECpl (EMp1 av anyCpt), ISgn anyCpt)])
  Prim (PVee _)             -> let sgn = Sign anyCpt anyCpt in                   pure (STnullary [(EDcV sgn, sgn)])
  PCpl _ (Prim (PVee _))    -> let sgn = Sign anyCpt anyCpt in                   pure (STnullary [(ECpl (EDcV sgn), sgn)])
  Prim (Pfull _ src tgt)    -> let sgn = Sign (pCpt2aCpt src) (pCpt2aCpt tgt) in pure (STnullary [(EDcV sgn, sgn)])
  Prim (PBin _ oper)        ->                       pure (STnullary [(EBin oper anyCpt, ISgn anyCpt)])
  PCpl _ (Prim (PBin _ oper )) ->                    pure (STnullary [(ECpl (EBin oper anyCpt), ISgn anyCpt)])
  Prim (PBind _ oper c)     -> let c'=pCpt2aCpt c in pure (STnullary [(EBin oper c', ISgn c')])
  Prim (PNamedR rel)        -> let rels :: P_NamedRel -> [Relation]
                                   rels r = case p_mbSign r of
                                     Just sg -> (findRelsTyped (declarationsMap contextInfo) (name r) . pSign2aSign pCpt2aCpt) sg
                                     Nothing -> (findDecls (declarationsMap contextInfo) . name) r
                               in  case rels rel of
                                                [] -> (Errors . return . CTXE (origin trm)) ("Undeclared relation "<> tshow rel)
                                                ds -> pure (STnullary [(EDcD d, sign d) | d <- ds])
  PEqu o a b -> checkPeri  o "equation"          EEqu a b       meet true  -- "PEqu" "meet" -- extra parameters for tracing purpose: opStr mjString
  PInc o a b -> checkPeri  o "inclusion"         EInc a b       meet true  -- "PInc" "meet"
  PIsc o a b -> checkPeri  o "intersection"      EIsc a b       meet true  -- "PIsc" "meet"
  PUni o a b -> checkPeri  o "union"             EUni a b       join true  -- "PUni" "join"
  PDif o a b -> checkPeri  o "difference"        EDif a b       join true  -- "PDif" "join"
  PCps o a b -> checkIntra o "composition"       ECps a b       meet true  -- "PCps" "meet"
  PRad o a b -> checkIntra o "relative addition" ERad a b       join true  -- "PRad" "join"
  PLrs o a b -> checkIntra o "left residual"     ELrs a (flp b) join isGeq -- "PLrs" "join"
  PRrs o a b -> checkIntra o "right residual"    ERrs (flp a) b join isLeq -- "PRrs" "join"
  PDia o a b -> checkIntra o "diamond"           EDia a b       meet true  -- "PDia" "meet"
  PPrd _ a b -> do sgnaTree <- signats a; sgnbTree <- signats b
                   let pairsa = opSigns sgnaTree; pairsb = opSigns sgnbTree
                   return (STbinary sgnaTree sgnbTree [(EPrd (expr_a, expr_b), Sign (source sgn_a) (target sgn_b)) | (expr_a, sgn_a)<-pairsa, (expr_b, sgn_b)<-pairsb ])
  PFlp _ e   -> signats (flp e)
  PKl0 _ e   -> signats e
  PKl1 _ e   -> signats e
  PCpl _ e   -> signats e
  PBrk _ e   -> signats e
  where
    true, isGeq, isLeq :: A_Concept -> A_Concept -> Bool
    true  _ _  = True
    isGeq c c' = Just True == leq conceptsGraph c' c
    isLeq c c' = Just True == leq conceptsGraph c c'
    checkIntra, checkPeri
      :: {- o          -} Origin
      -> {- kind       -} Text
      -> {- combinator -} ((Expression, Expression) -> Expression)
      -> {- a          -} Term TermPrim
      -> {- b          -} Term TermPrim
      -> {- meetORjoin -} (AdjacencyMap A_Concept -> A_Concept -> A_Concept -> Maybe A_Concept)
      -> {- compare    -} (A_Concept -> A_Concept -> Bool)
      -- extra parameters for tracing purpose:
      -- -> {- opStr      -} Text
      -- -> {- mjString   -} Text
      -> Guarded (OpTree (Expression, Signature))
    checkIntra o kind combinator a b meetORjoin cmpare = -- extra parameters for tracing purpose: opStr mjString 
      do sgnaTree <- signats a; sgnbTree <- signats b
         let pairsa = opSigns sgnaTree; pairsb = opSigns sgnbTree
             sgnsa = map snd pairsa; sgnsb = map snd pairsb
         let trees = -- trace ("\n"<>opStr<>" ("<>tshow o<>") ("<>showP a<>") ("<>showP b<>")\n   sgnsa: "<>tshow sgnsa<>"\n   sgnsb: "<>tshow sgnsb) $
                     [ -- trace ("Between "<>showP a<>" and  "<>showP b<>" ("<>mjString<>"): "<>tshow between<>"\n   "<>tshow (Sign srca tgtb, Sign srca left, Sign right tgtb))
                       ((combinator (expr_a, expr_b), Sign srca tgtb), (expr_a, Sign srca left), (expr_b, Sign right tgtb))
                     | (expr_a, Sign srca tgta)<-pairsa, (expr_b, Sign srcb tgtb)<-pairsb -- , trace ("\n  cmpare tgta srcb = cmpare "<>tshow tgta<>" "<>tshow srcb<>" = "<>tshow (cmpare tgta srcb)) True
                     , cmpare tgta srcb, Just between<-[meetORjoin conceptsGraph tgta srcb]
                     , Just left<-[meet conceptsGraph between tgta] , Just right<-[meet conceptsGraph srcb between]
                     ] <>
                     [ -- trace ("Between "<>showP a<>" and  "<>showP b<>" ("<>mjString<>"): "<>tshow between<>"\n   "<>tshow (Sign srca right, Sign srca left, ISgn right))
                       ((combinator (expr_a, expr_b), Sign srca right), (expr_a, Sign srca left), (expr_b, ISgn right))
                     | (expr_a, Sign srca tgta)<-pairsa, (expr_b, ISgn cptb)<-pairsb -- , trace ("\n  cmpare tgta cptb = cmpare "<>tshow tgta<>" "<>tshow cptb<>" = "<>tshow (cmpare tgta cptb)) True
                     , cmpare tgta cptb, Just between<-[meetORjoin conceptsGraph tgta cptb]
                     , Just left<-[meet conceptsGraph between tgta] , Just right<-[meet conceptsGraph cptb between]
                     ] <>
                     [ -- trace ("Between "<>showP a<>" and  "<>showP b<>" ("<>mjString<>"): "<>tshow between<>"\n   "<>tshow (Sign left tgtb, ISgn left, Sign right tgtb))
                       ((combinator (expr_a, expr_b), Sign left tgtb), (expr_a, ISgn left), (expr_b, Sign right tgtb))
                     | (expr_a, ISgn cpta)<-pairsa, (expr_b, Sign srcb tgtb)<-pairsb -- , trace ("\n  cmpare cpta srcb = cmpare "<>tshow cpta<>" "<>tshow srcb<>" = "<>tshow (cmpare cpta srcb)) True
                     , cmpare cpta srcb, Just between<-[meetORjoin conceptsGraph cpta srcb]
                     , Just left<-[meet conceptsGraph between cpta] , Just right<-[meet conceptsGraph srcb between]
                     ] <>
                     [ -- trace ("Between "<>showP a<>" and  "<>showP b<>" ("<>mjString<>"): "<>tshow between<>"\n   "<>tshow (Sign left right, ISgn left, ISgn right))
                       ((combinator (expr_a, expr_b), Sign left right), (expr_a, ISgn left), (expr_b, ISgn right))
                     | (expr_a, ISgn cpta)<-pairsa, (expr_b, ISgn cptb)<-pairsb -- , trace ("\n  cmpare cpta cptb = cmpare "<>tshow cpta<>" "<>tshow cptb<>" = "<>tshow (cmpare cpta cptb)) True
                     , cmpare cpta cptb, Just between<-[meetORjoin conceptsGraph cpta cptb]
                     , Just left<-[meet conceptsGraph between cpta] , Just right<-[meet conceptsGraph cptb between]
                     ]
         case trees of
          []  -> let baseMsg = "Cannot match the signatures of the two sides of the " <> kind <> "." <> diagnosis sgnsa sgnsb
                     errorExprs = [(combinator (expr_a, expr_b), Sign (source sgn_a) (target sgn_b)) | (expr_a, sgn_a)<-pairsa, (expr_b, sgn_b)<-pairsb ]
                     opTree = STbinary sgnaTree sgnbTree errorExprs
                 in mkVerboseTypeError env o baseMsg opTree
          [(pair_result, pairL, pairR)] -> return (STbinary sgnaTree{opSigns=[pairL]} sgnbTree{opSigns=[pairR]} [pair_result])
          triplesigns  -> let baseMsg = "Ambiguous signatures of the two sides of the composition of " <> showP a <> " and " <> showP b
                              suggestions = Just $ ". You might mean one of: " <> T.concat [ "\n    -   " <> showP a <> tshow (snd pairA) <> " ; " <> showP b <> tshow (snd pairB) | (_,pairA,pairB)<-triplesigns]
                              opTree = STbinary sgnaTree sgnbTree (map fst3 triplesigns)
                          in mkVerboseTypeMismatchError env o baseMsg suggestions opTree
        where
          diagnosis sgnsa sgnsb
           = case (kind, sgnsa==sgnsb) of
              ("composition"      , eq) -> "\n  The target of "<>displayLeft (map target sgnsa) a<>"should "<>(if eq then "match" else "be equal to (or share a concept with)")<>" the source of "<>displayRight (map source sgnsb) b<>"."
              ("relative addition", eq) -> "\n  The target of "<>displayLeft (map target sgnsa) a<>"should match "<>(if eq then "" else "with")<>" the source of "<>displayRight (map source sgnsb) b<>"."
              ("left residual"    , eq) -> "\n  The target of "<>displayLeft (map target sgnsa) a<>"should "<>(if eq then "match" else "be equal to or more generic than ")<>" the target of "<>displayRight (map target sgnsb) b<>"."
              ("right residual"   , eq) -> "\n  The source of "<>displayLeft (map source sgnsa) a<>"should "<>(if eq then "match" else "be equal to or more specific than")<>" the source of "<>displayRight (map source sgnsb) b<>"."
              ("diamond"          , eq) -> "\n  The target of "<>displayLeft (map target sgnsa) a<>"should "<>(if eq then "match" else "be equal to (or share a concept with)")<>" the source of "<>displayRight (map source sgnsb) b<>"."
              _ -> fatal ("Unknown kind of operation in diagnosis: "<>kind)
          displayLeft cpts expr =
            showP expr<>case cpts of
                          [cpt] -> ", which is "<>tshow cpt<>", "
                          []    -> " is undefined because "<>showP expr<>" is untypable, but it "
                          _     -> ", which can be any of "<>tshow cpts<>", "
          displayRight cpts expr =
            showP expr<>case cpts of
                          [cpt] -> ", which is "<>tshow cpt
                          []    -> " is undefined because "<>showP expr<>" is untypable"
                          _     -> ", which can be any of "<>tshow cpts

    -- | checkPeri generates a type error message for equations, inclusions, unions, intersects, and difference.
    checkPeri o kind combinator a b meetORjoin _ = -- extra parameters for tracing purpose: opStr mjString 
      do sgnaTree <- signats a; sgnbTree <- signats b
         let sgnsa = opSigns sgnaTree; sgnsb = opSigns sgnbTree
             conceptsSrc = L.nub [ src | sgn_a<-fmap snd sgnsa, sgn_b<-fmap snd sgnsb, Just src<-[meetORjoin conceptsGraph (source sgn_a) (source sgn_b)] ]
             conceptsTgt = L.nub [ tgt | sgn_a<-fmap snd sgnsa, sgn_b<-fmap snd sgnsb, Just tgt<-[meetORjoin conceptsGraph (target sgn_a) (target sgn_b)] ]
         case -- trace ("\n"<>opStr<>" ("<>tshow o<>") ("<>showP a<>") ("<>showP b<>")\n   sgnsa: "<>tshow sgnsa<>"\n   sgnsb: "<>tshow sgnsb) $
              [ -- trace (mjString<>" on "<>showP a<>" and  "<>showP b<>" yields: "<>tshow (Sign src tgt))
                ((combinator (expr_a, expr_b), Sign src tgt), (expr_a, Sign src tgt), (expr_b, Sign src tgt))
              | (expr_a, sgn_a)<-sgnsa, (expr_b, sgn_b)<-sgnsb -- , trace (mjString<>" "<>tshow (source sgn_a)<>" "<>tshow (source sgn_b)<>" yields "<>tshow (meetORjoin conceptsGraph (source sgn_a) (source sgn_b))<>" and "<>mjString<>" "<>tshow (target sgn_a)<>" "<>tshow (target sgn_b)<>" yields "<>tshow (meetORjoin conceptsGraph (target sgn_a) (target sgn_b))) True
              , Just src<-[meetORjoin conceptsGraph (source sgn_a) (source sgn_b)], Just tgt<-[meetORjoin conceptsGraph (target sgn_a) (target sgn_b)] ] of
          []  -> let errorExprs = [(combinator (expr_a, expr_b), Sign (source sgn_a) (target sgn_b)) | (expr_a, sgn_a)<-sgnsa, (expr_b, sgn_b)<-sgnsb ]
                     opTree = STbinary sgnaTree sgnbTree errorExprs
                 in case (conceptsSrc, conceptsTgt) of
                  ([],[])  -> let baseMsg = "Cannot match the source concepts on both sides of the "<>kind<>")\n   sgnsa: "<>tshow (map snd sgnsa)<>"\n   sgnsb: "<>tshow (map snd sgnsb)
                              in mkVerboseTypeError env o baseMsg opTree
                  ([],_:_) -> let baseMsg = "Cannot match the source concepts on the left side of the "<>kind<>".\n   The source of "<>showP a<>" is "<>showSgns (map (source . snd) sgnsa)<>"\n   The source of "<>showP b<>" is "<>showSgns (map (source . snd) sgnsb)
                              in mkVerboseTypeError env o baseMsg opTree
                  (_:_,[]) -> let baseMsg = "Cannot match the target concepts of the right side of the "<>kind<>".\n   The target of "<>showP a<>" is "<>showSgns (map (target . snd) sgnsa)<>"\n   The target of "<>showP b<>" is "<>showSgns (map (target . snd) sgnsb)
                              in mkVerboseTypeError env o baseMsg opTree
                  _        -> let baseMsg = "Cannot match the signatures at either side of the "<>kind<>".\n   sgnsa: "<>tshow (map snd sgnsa)<>"\n   sgnsb: "<>tshow (map snd sgnsb)
                              in mkVerboseTypeError env o baseMsg opTree
                 where
                   showSgns :: Show a => [a] -> Text
                   showSgns sgns = case sgns of
                                    [] ->     "untyped"
                                    [sgn] ->  tshow sgn
                                    sgn:ss -> (T.intercalate ", " . map tshow) ss<>", or "<>tshow sgn
          [(pair_result, pairL, pairR)] -> return (STbinary sgnaTree{opSigns=[pairL]} sgnbTree{opSigns=[pairR]} [pair_result])
          triplesigns -> let baseMsg = "Ambiguous signatures at either side of the "<>kind<>".\n   You might mean one of: "<>T.concat [ "\n    -   "<>showP a<>tshow (snd pairA)<>" ; "<>showP b<>tshow (snd pairB) | (_,pairA,pairB)<-triplesigns]
                             errorExprs = [(combinator (expr_a, expr_b), Sign (source sgn_a) (target sgn_b)) | (expr_a, sgn_a)<-sgnsa, (expr_b, sgn_b)<-sgnsb ]
                             opTree = STbinary sgnaTree sgnbTree errorExprs
                         in mkVerboseTypeError env o baseMsg opTree

    pCpt2aCpt = conceptMap contextInfo
    conceptsGraph = typeGraph contextInfo
    signats = signatures env contextInfo

typeGraph :: ContextInfo -> AdjacencyMap A_Concept
typeGraph contextInfo = L.foldr overlay empty [initialGraph, anyEdges, oneGraph] -- add the edges for the ANY concept to the initial graph
  where
    initialGraph = conceptGraph contextInfo
    anyEdges = edges [ (c, anyCpt) | c<-Set.toList (allConcepts contextInfo)]
    oneGraph = vertices [ ONE]
--  typologies = meetSubsets initialGraph


anyCpt :: A_Concept
anyCpt = (PlainConcept . Set.fromList)
            [(case try2Name ConceptName "_ANY" of
                Left err -> fatal $ "Not a proper concept name: _ANY. " <> err
                Right (nm, _) -> nm
            , Nothing)]

termPrim2Expr :: ContextInfo -> [Signature] -> TermPrim -> Guarded Expression
termPrim2Expr contextInfo sgns trmprim
  = case trmprim of
      PI _           -> guard ([ EDcI cpt | ISgn cpt<-sgns]<>[ EDcI src | Sign src tgt<-sgns, src==tgt ])
      Pid _ _c       -> guard ([ EDcI cpt | ISgn cpt<-sgns]<>[ EDcI src | Sign src tgt<-sgns, src==tgt ])
      Patm _ av mc   -> case mc of
                         Just c  -> guard ([ EMp1 av cpt | ISgn cpt<-sgns, pCpt2aCpt c==cpt]<>[ EMp1 av src | Sign src tgt<-sgns, src==tgt ])
                         Nothing -> guard ([ EMp1 av cpt | ISgn cpt<-sgns]                  <>[ EMp1 av src | Sign src tgt<-sgns, src==tgt ])
      PVee _         -> guard [ EDcV sgn | sgn<-sgns]
      Pfull _ s t    -> guard [ EDcV sgn | sgn<-sgns, Just _src<-[source sgn `grLwB` pCpt2aCpt s], Just _tgt<-[target sgn `grLwB` pCpt2aCpt t]]
      PBin _ oper    -> guard ([ EBin oper cpt | ISgn cpt<-sgns]                  <>[ EBin oper src | Sign src tgt<-sgns, src==tgt ])
      PBind _ oper c -> guard ([ EBin oper cpt | ISgn cpt<-sgns, pCpt2aCpt c==cpt]<>[ EBin oper src | Sign src tgt<-sgns, src==tgt ])
      PNamedR rel    -> guard [ EDcD decl | sgn<-sgns, decl<-rels rel, Just _src<-[source sgn `grLwB` source decl], Just _tgt<-[target sgn `grLwB` target decl]]
    where
      guard :: [Expression] -> Guarded Expression
      guard []     = Errors . return $ CTXE (origin trmprim) ("Can derive no signature for "<>showP trmprim<>".")
      guard [expr] | source expr == anyCpt && target expr == anyCpt = Errors . return $ CTXE (origin trmprim) ("Cannot derive a signature for "<>showP trmprim<>".")
      guard [expr] | source expr == anyCpt                          = Errors . return $ CTXE (origin trmprim) ("Cannot derive a signature for "<>showP trmprim<>"[ANY*"<>tshow (target expr)<>"]")
      guard [expr] |                          target expr == anyCpt = Errors . return $ CTXE (origin trmprim) ("Cannot derive a signature for "<>showP trmprim<>"["<>tshow (source expr)<>"*ANY]")
      guard [expr] = pure expr
      guard exprs  = Errors . return $ CTXE (origin trmprim) ("Ambiguous "<>showP trmprim<>". You should specify the type explicitly"<>if length exprs>4 then "" else ", for instance one of: "<>T.intercalate ", " (map (tshow . sign) exprs)<>".")
      rels :: P_NamedRel -> [Relation]
      rels rel = case p_mbSign rel of
                  Just sg -> (findRelsTyped (declarationsMap contextInfo) (name rel) . pSign2aSign pCpt2aCpt) sg
                  Nothing -> (findDecls (declarationsMap contextInfo) . name) rel
      conceptsGraph = typeGraph contextInfo
      grLwB = meet conceptsGraph
      -- lsUpB = meet conceptGraph
      pCpt2aCpt = conceptMap contextInfo

term2Expr :: (HasFSpecGenOpts env, HasRunner env) => env -> ContextInfo -> Term TermPrim -> Guarded Expression
term2Expr env contextInfo term
  = do sgnTree <- signatures env contextInfo term
       trace ("\nsignatures yields:\n"<>showOpTree sgnTree) $ t2e sgnTree term
  where
    t2e :: OpTree (Expression, Signature) -> Term TermPrim -> Guarded Expression
    t2e sgnTree trm =
      trace ("---   "<>(T.intercalate ", " . fmap (showA . fst) . opSigns) sgnTree)$
      case (trm, sgnTree) of
        (Prim tp   , STnullary pairs)            -> -- trace ("termPrim2Expr "<>tshow tp<>" with pairs: "<>tshow pairs) $
                                                     termPrim2Expr contextInfo (map snd pairs) tp
        (trm, STbinary stLeft stRight pairs@(_:_:_)) -> let baseMsg = "Ambiguous term " <> showP trm <> " might be one of: " <> T.intercalate ", " (map (tshow . snd) pairs) <> ".\n  Please specify the signature explicitly."
                                                            opTree = STbinary stLeft stRight pairs
                                                        in mkVerboseTypeError env (origin trm) baseMsg opTree
        (PEqu _ a b, STbinary stLeft stRight pairs)
          -> EEqu <$> ((,) <$> t2e stLeft a <*> t2e stRight b)
        (PInc _ a b, STbinary stLeft stRight pairs)
          -> EInc <$> ((,) <$> t2e stLeft{opSigns = pairs} a <*> t2e stRight{opSigns = pairs} b)
        (PIsc _ a b, STbinary stLeft stRight pairs)
          -> EIsc <$> ((,) <$> t2e stLeft a <*> t2e stRight b)
        (PUni _ a b, STbinary stLeft stRight pairs)
          -> EUni <$> ((,) <$> t2e stLeft a <*> t2e stRight b)
        (PDif _ a b, STbinary stLeft stRight pairs)
          -> EDif <$> ((,) <$> t2e stLeft a <*> t2e stRight b)
        (PLrs _ a b, STbinary stLeft stRight [(_, sgn)])
          -> ELrs <$> ((,) <$> t2e stLeft{opSigns = leftPairs} a <*> t2e stRight{opSigns = rightPairs} b)
             where
               -- Note: For left residual (a/b), the right operand b is flipped in signature generation
               -- So stRight already contains flipped signatures. We need to extract them for type checking.
               rightFlippedSigns = [(expr, flp sig) | (expr, sig) <- opSigns stRight]
               triples = [ trace ("On the right hand side of "<>showP a<>" and  "<>showP b<>" (join): "<>tshow between<>"\n   "<>tshow (Sign srca tgtb, Sign srca between, Sign between tgtb))
                           (Sign srca tgtb, Sign srca between, Sign between tgtb)
                         | (_, Sign srca tgta)<-opSigns stLeft, (_, Sign srcb tgtb)<-rightFlippedSigns
                         , Just between<-[join conceptsGraph tgta srcb]
                         ]
               leftPairs = L.nubBy ((==) `on` snd) [(expr, snd3 trip) | (expr, _) <- opSigns stLeft, trip <- triples]
               rightPairs = L.nubBy ((==) `on` snd) [(expr, thd3 trip) | (expr, _) <- rightFlippedSigns, trip <- triples]
        (PRrs _ a b, STbinary stLeft stRight [(_, sgn)])
          -> ERrs <$> ((,) <$> t2e stLeft{opSigns = leftPairs} a <*> t2e stRight{opSigns = rightPairs} b)
             where
               -- Note: For right residual (a\b), the left operand a is flipped in signature generation
               -- So stLeft already contains flipped signatures. We need to extract them for type checking.
               leftFlippedSigns = [(expr, flp sig) | (expr, sig) <- opSigns stLeft]
               triples = [ trace ("On the left hand side of "<>showP a<>" and  "<>showP b<>" (join): "<>tshow between<>"\n   "<>tshow (Sign srca tgtb, Sign srca between, Sign between tgtb))
                           (Sign srca tgtb, Sign srca between, Sign between tgtb)
                         | (_, Sign srca tgta)<-leftFlippedSigns, (_, Sign srcb tgtb)<-opSigns stRight
                         , Just between<-[join conceptsGraph tgta srcb]
                         ]
               leftPairs = L.nubBy ((==) `on` snd) [(expr, snd3 trip) | (expr, _) <- opSigns stLeft, trip <- triples]
               rightPairs = L.nubBy ((==) `on` snd) [(expr, thd3 trip) | (expr, _) <- opSigns stRight, trip <- triples]
        (PCps _ a b, STbinary stLeft stRight [(_, sgn)]) -> ECps <$> ((,) <$> t2e stLeft{opSigns = leftPairs} a <*> t2e stRight{opSigns = rightPairs} b)
                                                         where
                                                           triples = [ trace ("Between "<>showP a<>" and  "<>showP b<>" (meet): "<>tshow between<>"\n   "<>tshow (Sign srca tgtb, Sign srca between, Sign between tgtb))
                                                                       (Sign srca tgtb, Sign srca between, Sign between tgtb)
                                                                     | (_, Sign srca tgta)<-opSigns stLeft, (_, Sign srcb tgtb)<-opSigns stRight
                                                                     , Just between<-[meet conceptsGraph tgta srcb]
                                                                     ]
                                                           leftPairs = L.nubBy ((==) `on` snd) [(expr, snd3 trip) | (expr, _) <- opSigns stLeft, trip <- triples]
                                                           rightPairs = L.nubBy ((==) `on` snd) [(expr, thd3 trip) | (expr, _) <- opSigns stRight, trip <- triples]
        (PRad _ a b, STbinary stLeft stRight pairs)  -> ERad <$> ((,) <$> t2e stLeft a <*> t2e stRight b)
        (PPrd _ a b, STbinary stLeft stRight pairs)  -> EPrd <$> ((,) <$> t2e stLeft a <*> t2e stRight b)
        (PKl0 _ e  , _)                             -> EKl0 <$> t2e sgnTree e
        (PKl1 _ e  , _)                             -> EKl1 <$> t2e sgnTree e
        (PFlp _ e  , _)                             -> EFlp <$> t2e sgnTree (flp e)
        (PCpl _ e  , _)                             -> ECpl <$> t2e sgnTree e
        (PBrk _ e  , _)                             -> t2e sgnTree e
        _ -> fatal ("Software error: term2Expr encountered an unexpected term: " <> tshow trm <> " and opTree: " <> tshow sgnTree)
    conceptsGraph = trace ("conceptsGraph: "<>tshow (typeGraph contextInfo)) (typeGraph contextInfo)

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
