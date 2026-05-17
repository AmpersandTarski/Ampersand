{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# HLINT ignore "Use catMaybes" #-}
{-# HLINT ignore "Eta reduce" #-}
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
import Ampersand.Basics hiding (conc, set, guard, join)
import Ampersand.Classes
import Ampersand.Classes.ConceptStructure (PConceptStructure(..))
import Ampersand.Core.AbstractSyntaxTree hiding (Guarded(..), Errors, Warning, CTXE, CtxError, Checked)
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowAStruct
import Ampersand.Core.ShowPStruct (showP)
import Ampersand.Input.ADL1.CtxError
import Ampersand.Misc.HasClasses
import Ampersand.Runners (logLevel)
import Ampersand.Types.Config (HasRunner, runnerL)
import Algebra.Graph.AdjacencyMap
import Data.Tuple.Extra (fst3, snd3)
import RIO.Char (toLower, toUpper)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

-- NOTE: Static checks like checkPurposes should ideally occur on the P-structure before type-checking, as it makes little
-- sense to do type checking when there are static errors. However, in Ampersand all collect functions (e.g. in ViewPoint)
-- only exist on the A-Structure, so we do it afterwards. Static purpose errors won't affect types, so in this case it is no problem.

-- Check whether all purposes refer to existing objects.
checkPurposes :: A_Context -> Guarded ()
checkPurposes ctx =
  case filter (isDanglingPurpose ctx) (ctxps ctx <> concatMap ptxps (ctxpats ctx)) of
        [] -> pure ()
        x : xs -> Errors . fmap mkDanglingPurposeError $ x NE.:| xs

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
  case [ mkDanglingRefError "Rule" (arRule rr) (arPos rr)
         | rr <- Set.toList (ctxrrules ctx),
           arRule rr `notElem` map name (toList $ allRules ctx)
       ] of
    [] -> return ()
    x : xs -> Errors (x NE.:| xs)

-- Phase 2: Validate interface reference compatibility
-- This is called after all interfaces have been type-checked
validateInterfaceRefs :: A_Context -> Guarded ()
validateInterfaceRefs ctx =
  case concatMap checkInterface (ctxifcs ctx) of
    [] -> pure ()
    x : xs -> Errors (x NE.:| xs)
  where
    checkInterface :: Interface -> [CtxError]
    checkInterface ifc = checkObjectDef ifc (ifcObj ifc)

    checkObjectDef :: Interface ->  ObjectDef -> [CtxError]
    checkObjectDef ifc obj =
      maybe [] checkSubInterface (objmsub obj)
      where
        parentExpr = objExpression obj
        checkSubInterface :: SubInterface -> [CtxError]
        checkSubInterface subIfc =
          case subIfc of
            Box { siObjs = boxItems } ->
              concatMap recur boxItems
              where
                recur :: BoxItem -> [CtxError]
                recur (BxExpr subObj) = checkObjectDef ifc subObj
                recur (BxText {}) = []

            InterfaceRef {pos = refOrigin, siIfcId = refName} ->
              -- Look up the referenced interface
              case filter ((== refName) . name) (ctxifcs ctx) of
                [] -> fatal ("Interface " <> tshow refName <> " not found. This should have been caught when checking the interfaces.")
                [refIfc] ->
                  let refConcept = source (objExpression (ifcObj refIfc))
                      parentConcept = target parentExpr
                  -- Check if expectedConcept (what we're passing) >= refConcept (what interface expects)
                  -- This ensures we're passing something at least as specific as what the interface needs
                  in case refConcept `geq` parentConcept of
                       Just True  -> [] -- parentConcept is more general or equal - OK
                       Just False -> case parentConcept `geq` refConcept of -- refConcept `join` parentConcept of
                                       Just True  -> [mkInterfaceRefNarrowerError refOrigin refName parentExpr refConcept]
                                       _          -> [mkIncompatibleInterfaceError refOrigin parentConcept refConcept refName]
                       Nothing    -> [mkIncompatibleInterfaceError refOrigin parentConcept refConcept refName]
                _ -> fatal ("Multiple interfaces with name " <> tshow refName <> ". This should have been caught when checking the interfaces.")

-- | Check that all singleton atom values in expressions (EMp1 nodes) are
--   compatible with the representation type of their concept.
--   This prevents a fatal crash later in 'safePSingleton2AAtomVal'.
checkSingletonAtomValues :: A_Context -> Guarded ()
checkSingletonAtomValues ctx =
  case [ mkSingletonRepresentationError pav cpt typ
       | EMp1 pav cpt <- toList (expressionsIn ctx)
       , let typ = reprType ci cpt
       , Left _ <- [unsafePAtomVal2AtomValue typ (Just cpt) pav]
       ] of
    []     -> pure ()
    x : xs -> Errors (x NE.:| xs)
  where
    ci = ctxInfo ctx

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

pSign2aSign :: (P_Concept -> Guarded A_Concept) -> P_Sign -> Guarded Signature
pSign2aSign pCpt2aCpt (P_Sign src tgt) = Sign <$> pCpt2aCpt src <*> pCpt2aCpt tgt

findRels :: DeclMap -> Name -> Map.Map SignOrd Relation
findRels declMap x = Map.findWithDefault Map.empty x declMap -- get all relations with the same name as x

namedRel2Decl :: ConceptMap -> DeclMap -> P_NamedRel -> Guarded Relation
namedRel2Decl pCpt2aCpt declMap rel@(PNamedRel o r mSgn)
 = do
    decls <- case mSgn of
               Nothing -> pure (findDecls declMap r)
               Just s  -> do sgn <- pSign2aSign (pCpt2aCpt o) s
                             pure (findRelsTyped declMap r sgn)
    case decls of
      [dcl] -> pure dcl
      []    -> (Errors . return . CTXE o) ("Undefined relation named: "<>showP rel)
      ds    -> (Errors . return . CTXE o) ("Ambiguous relation named: "<>showP rel<>"\n"<>tshow ds)

findDecls :: DeclMap -> Name -> [Relation]
findDecls declMap x = Map.elems (findRels declMap x)

findRelsTyped :: DeclMap -> Name -> Signature -> [Relation]
findRelsTyped declMap x tp = let result = Map.findWithDefault [] (SignOrd tp) (Map.map (: []) (findRels declMap x))
                             in  -- trace ("1. findRelsTyped (x: "<>tshow x<>") (tp: "<>tshow tp<>") = "<>tshow result)
                                  result

type DeclMap = Map.Map Name (Map.Map SignOrd Relation)

-- | Partition REPRESENT statements into valid and invalid based on concept existence
partitionValidRepresentations :: Set.Set P_Concept -> [P_Representation] -> ([P_Representation], [P_Representation])
partitionValidRepresentations pCpts reprs =
  L.partition (\(Repr _ cpts _) -> all (`Set.member` pCpts) (NE.toList cpts)) reprs

-- | pCpt2aCpt converts a P_Concept to A_Concept using the concept table
-- Note that:      type ConceptMap = Origin -> P_Concept -> Guarded A_Concept
-- pCpt2aCpt is a lookup function that raises a fatal error if the concept
-- has not been seen before (which should never happen if typologies are constructed correctly)
makePCpt2ACpt :: [Typology] -> ConceptMap
makePCpt2ACpt typologies = pCpt2aCpt
  where
    pCpt2aCpt :: Origin -> P_Concept -> Guarded A_Concept
    pCpt2aCpt _ P_ONE = pure ONE
    pCpt2aCpt o (PCpt nm)
      | nm == nameOfONE = fatal "De naam ONE had vertaald moeten worden naar P_ONE"-- Prevent ONE from being looked up as PlainConcept
      | otherwise =
          case Map.lookup nm conceptTable of
            Just aCpt -> pure aCpt
            Nothing -> Errors . return . CTXE o $
                       ("Concept " <> fullName nm <> " does not occur in any concept definition, relation or rule.")
    -- | Build a concept table from typologies
    -- This creates a lookup map from concept names to A_Concepts
    conceptTable :: Map.Map Name A_Concept
    conceptTable = -- trace ("CONCEPT TABLE: " <> tshow (Map.keys conceptTableMap))
                   conceptTableMap
      where
        conceptTableMap = Map.fromList $
          [ (nm, PlainConcept
                   { aliases = if null aliasSet then fatal "Empty alias set in concept table" else aliasSet
                   , typology = typo
                   })
          | typo <- typologies
          , aliasSet <- vertexList (tyGrph typo)
          , nm <- Set.toList aliasSet
          ] -- ONE is already present in typologies.

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
pCtx2aCtx :: (HasFSpecGenOpts env, HasRunner env) => env -> P_Context -> Guarded A_Context
pCtx2aCtx env
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
      contextInfoPre <- g_contextInfo
      identdefs <- traverse (pIdentity2aIdentity contextInfoPre Nothing) p_identdefs --  The identity definitions defined in this context, outside the scope of patterns
      viewdefs <- traverse (pViewDef2aViewDef contextInfoPre) p_viewdefs --  The view definitions defined in this context, outside the scope of patterns
      -- Filter REPRESENT statements before trying to convert them
      let allRepresentations = p_representations <> concatMap pt_Reprs p_patterns
          (validReprs, invalidReprs) = partitionValidRepresentations pCpts allRepresentations
      -- Issue warnings for orphaned REPRESENT statements
      addWarnings (map mkOrphanedRepresentWarning invalidReprs) $ pure ()
      interfaces <- traverse (pIfc2aIfc contextInfoPre) p_interfaces
      -- Only process valid REPRESENT statements
      explicitReprs <- traverse (\pRepr -> do
                          aCpts <- traverse (conceptMap contextInfoPre (origin pRepr)) (reprcpts pRepr)
                          pure Arepr {aReprFrom = aCpts, aReprTo = reprdom pRepr})
                        validReprs
      let objectReprs = getObjReprs interfaces viewdefs identdefs ++ [ Arepr (ONE NE.:| []) Object ] -- Ensure that ONE also gets an OBJECT representation
      checkDuplicateReprTypes (explicitReprs <> objectReprs)
      let contextInfo = contextInfoPre {reprType = reprTypeDefaults (explicitReprs <> objectReprs)}
      allAConcepts <- traverse (conceptMap contextInfo (Origin "allAConcepts")) (Set.toList (allPConcepts contextInfo))
      classifies <- traverse (pClassify2aClassify contextInfo) p_gens --  The specialization statements defined in this context, outside the scope of patterns
      pats <- traverse (pPat2aPat contextInfo) p_patterns --  The patterns defined in this context
      rules <- traverse (pRul2aRul contextInfo Nothing) p_rules --  All user defined rules in this context, but outside patterns
      purposes <- traverse (pPurp2aPurp contextInfo) p_purposes --  The purposes of objects defined in this context, outside the scope of patterns
      udpops <- traverse (pPop2aPop contextInfo) p_pops --  [Population], user-defined
      relations <- traverse (pDecl2aDecl (reprType contextInfo) (conceptMap contextInfo) Nothing deflangCtxt deffrmtCtxt) p_relations
      enforces' <- traverse (pEnforce2aEnforce contextInfo Nothing) p_enfs
      conceptDefsOutPats <- allConceptDefsOutPats contextInfo
      allConceptDefs' <- allConceptDefs contextInfo
      --  uniqueNames "pattern" p_patterns   -- Unclear why this restriction was in place. So I removed it
      uniqueNames "rule" $ p_rules <> concatMap pt_rls p_patterns
      uniqueNames "identity definition" $ p_identdefs <> concatMap pt_ids p_patterns
      uniqueNames "view definition" $ p_viewdefs <> concatMap pt_vds p_patterns
      uniqueNames "interface" p_interfaces
      let actx =
          --  trace ("\n🔍 DEBUG allAConcepts created. Count: " <> tshow (length allAConcepts) <> 
          --            "\n  Concepts with their aliases:" <>
          --            T.concat ["\n    - " <> tshow cpt <> " -> aliases: " <> 
          --                     (case cpt of 
          --                       PlainConcept{aliases=als} -> tshow (Set.toList als)
          --                       ONE -> "[special ONE]"
          --                       _ -> "[complex type]")
          --                     | cpt <- allAConcepts]) $
            ACtx
              { ctxnm = n1,
                ctxlbl = lbl,
                ctxpos = n2,
                ctxlang = deflangCtxt,
                ctxmarkup = deffrmtCtxt,
                ctxpats = pats,
                ctxcs = allAConcepts,
                ctxrs = Set.fromList rules,
                ctxds = Set.fromList relations,
                ctxpopus = udpops, -- the content is copied from p_pops
                ctxcdsOutPats = conceptDefsOutPats,
                ctxcds = allConceptDefs',
                ctxks = identdefs,
                ctxrrules =
                  Set.unions
                    . map
                      pRoleRule2aRoleRule
                    $ p_roleRules
                    <> concatMap pt_RRuls p_patterns,
                ctxvs = viewdefs,
                ctxgs = classifies,
                ctxReprType = reprType contextInfo,
                ctxifcs = interfaces,
                ctxps = purposes,
                ctxmetas = p_metas,
                ctxInfo = contextInfo,
                ctxEnforces = enforces'
              }
      checkSingletonAtomValues actx  -- Check singleton atom values match their concept's representation type
      checkOtherAtomsInSessionConcept actx
      checkPurposes actx -- Check whether all purposes refer to existing objects
      checkDanglingRulesInRuleRoles actx -- Check whether all rules in MAINTAIN statements are declared
      checkInterfaceCycles actx -- Check that interface references are not cyclic
      checkMultipleDefaultViews actx -- Check whether each concept has at most one default view
      validateInterfaceRefs actx -- Phase 2: Validate interface reference compatibility
      warnCaseProblems actx -- Warn if there are problems with the casing of names of relations and/or concepts
      warnUnusedConcepts actx -- Warn if there are concepts defined that are not used in relations
      return actx
    where
      -- Early filtering: get all P_Concepts that exist in the schema (before type checking)
      pCpts :: Set.Set P_Concept
      pCpts = pConcs (p_conceptdefs <> concatMap pt_cds p_patterns) `Set.union`
              pConcs (p_relations <> concatMap pt_dcs p_patterns)   `Set.union`
              pConcs (p_gens <> concatMap pt_gns p_patterns)        `Set.union`
              pConcs (p_rules <> concatMap pt_rls p_patterns)       `Set.union`
              Set.fromList (concatMap (NE.toList . reprcpts) (p_representations <> concatMap pt_Reprs p_patterns)) `Set.union`
              Set.fromList ([P_ONE] <> [PCpt nameOfSESSION | not (null p_interfaces)])

      -- | Extract object representations from interfaces, views, and identities
      -- All these concepts must be represented as Object type
      getObjReprs :: [Interface] -> [ViewDef] -> [IdentityRule] -> [A_Representation]
      getObjReprs interfaces viewdefs identdefs =
        case Set.toList (getInterfaceConcepts interfaces `Set.union` getViewConcepts viewdefs `Set.union` getIdentityConcepts identdefs) of
             [] -> []
             cpt:cpts -> -- trace ("8. getObjReprs "<>T.intercalate " " (NE.toList (fmap showA (cpt :| cpts))))
                         [ Arepr (cpt :| cpts) Object ]
        where
          -- | Extract all concepts from interfaces (source + all box targets recursively)
          getInterfaceConcepts :: [Interface] -> Set.Set A_Concept
          getInterfaceConcepts ifcs = Set.fromList [ source (objExpression (ifcObj ifc)) | ifc <- ifcs ] `Set.union`
                                      Set.unions [ getObjectConcepts (ifcObj ifc) | ifc <- ifcs ]

          -- | Recursively extract all target concepts from box expressions
          getObjectConcepts :: ObjectDef -> Set.Set A_Concept
          getObjectConcepts obj =
            case objmsub obj of
              Nothing -> Set.empty  -- No BOX = endpoint, target doesn't need OBJECT representation
              Just si -> Set.singleton (target (objExpression obj)) `Set.union` getSubInterfaceConcepts si

          -- | Extract concepts from sub-interfaces
          getSubInterfaceConcepts :: SubInterface -> Set.Set A_Concept
          getSubInterfaceConcepts (Box {siObjs = boxItems}) = Set.unions (map getBoxItemConcepts boxItems)
          getSubInterfaceConcepts (InterfaceRef {siIfcId = refId}) =
            -- Look up the referenced interface and get its actual source concept
            case filter ((== refId) . name) interfaces of
              [refIfc] -> Set.singleton (source (objExpression (ifcObj refIfc)))
              _ -> fatal ("Manufacturing error in the Ampersand compiler: there should be exactly one interface with name " <> tshow refId <> ", but none or multiple were found.")

          -- | Extract concepts from box items
          getBoxItemConcepts :: BoxItem -> Set.Set A_Concept
          getBoxItemConcepts (BxExpr obj) = getObjectConcepts obj
          getBoxItemConcepts (BxText {}) = Set.empty

          -- | Extract concepts from views
          getViewConcepts :: [ViewDef] -> Set.Set A_Concept
          getViewConcepts vds = Set.fromList (map vdcpt vds)

          -- | Extract concepts from identities
          getIdentityConcepts :: [IdentityRule] -> Set.Set A_Concept
          getIdentityConcepts ids = Set.fromList (map idCpt ids)

      -- Create the final representation function
      reprTypeDefaults :: [A_Representation] -> A_Concept -> TType
      reprTypeDefaults representationPairs cpt =
        if cpt == ONE || show cpt == "SESSION"  || cpt == topCpt || cpt == botCpt
          then Object
          else Map.findWithDefault Alphanumeric cpt reprTypeMap
        where
          reprTypeMap :: Map.Map A_Concept TType
          reprTypeMap = Map.fromList
            [ (c, aReprTo aRepr)
            | aRepr <- L.nub representationPairs
            , c <- NE.toList (aReprFrom aRepr)
            ]

      -- | Check for duplicate TTypes assigned to the same concept
      checkDuplicateReprTypes :: [A_Representation] -> Guarded ()
      checkDuplicateReprTypes reprs =
        case [ NE.toList cl | cl <- (eqCl fst . L.nub) [ (c, aReprTo r) | r <- reprs, c <- NE.toList (aReprFrom r)], length cl>1] of
          []   -> pure ()
          x:xs -> Errors (fmap mkDuplicateReprTypeError (x :| xs))
        where
          mkDuplicateReprTypeError :: [(A_Concept,TType)] -> CtxError
          mkDuplicateReprTypeError pairs =
            CTXE (Origin "representation check") $
              "Concept " <> (T.intercalate ", " . map showWithAliases . L.nub . map fst) pairs <>
              " has multiple representation types: " <>
              T.intercalate ", " (map (tshow . snd) pairs)

      deflangCtxt = fromMaybe English ctxmLang
      deffrmtCtxt = fromMaybe ReST pandocf
      -- allReprs contains P_Concept - TType bindings.
      allReprs :: [P_Representation]
      allReprs = p_representations             <> -- P_Concept - TType bindings from REPRESENT statements outside of patterns
                 concatMap pt_Reprs p_patterns <> -- P_Concept - TType bindings from REPRESENT statements inside patterns
                 identReprs                    <> -- implicit P_Concept - Object bindings for IDENT concepts 
                 viewReprs                        -- implicit P_Concept - Object bindings for VIEW concepts
        where
          -- In the process of parsing, we cannot create representations for box concepts yet. because we do not know the target concept of the box term.
          -- The type checker needs to determine the target concept of the box term first.

          -- Create explicit representations for IDENT concepts
          identReprs :: [P_Representation]
          identReprs = [Repr (origin ident) (ix_cpt ident :| []) Object
                       | ident <- p_identdefs <> concatMap pt_ids p_patterns]

          -- Create explicit representations for VIEW concepts  
          viewReprs :: [P_Representation]
          viewReprs = [Repr (origin viewDef) (vd_cpt viewDef :| []) Object
                      | viewDef <- p_viewdefs <> concatMap pt_vds p_patterns]

      g_contextInfo :: Guarded ContextInfo
      g_contextInfo = do
        typols <- (makeTypologies . makeAliasGraph . makePGraph (p_gens <> concatMap pt_gns p_patterns)) pCpts
        let pCpt2aCpt = makePCpt2ACpt typols
        -- Filter REPRESENT statements to only those with existing concepts
        let allRepresentations = p_representations <> concatMap pt_Reprs p_patterns
            (validReprs, _invalidReprs) = partitionValidRepresentations pCpts allRepresentations
        reprs <- traverse (pRepr2aRepr pCpt2aCpt) validReprs
        let reprOf :: A_Concept -> TType
            reprOf cpt =
              if cpt == ONE || show cpt == "SESSION"
                then Object
                else case [aReprTo r | r <- L.nub reprs, cpt `elem` aReprFrom r] of
                       [t] -> t
                       []  -> Alphanumeric
                       ts  -> fatal $ "Multiple representations found for concept " <> showWithAliases cpt <> ": " <> tshow ts <> ". This should not happen as all concepts should have only one representation assigned."
        -- Fix for bug: Excel-imported relations (in p_relations / ctx_ds) have dec_prps = {}
        -- while the same relation declared in an ADL pattern (pt_dcs) may have dec_prps = {UNI, ...}.
        -- By applying mergeRels first, the union of dec_prps from both sources is computed,
        -- so that ADL-declared properties are not silently overwritten by the Excel import.
        -- mergeRels is the same function used by mergeContexts in ParseTree.hs — no duplicate logic.
        -- See docs/isuni-trace-analysis.md for the full bug analysis and proof.
        let mergedPRelations = mergeRels (p_relations <> concatMap pt_dcs p_patterns)
        decls <- traverse (pDecl2aDecl reprOf pCpt2aCpt Nothing deflangCtxt deffrmtCtxt) mergedPRelations
        let declMap = Map.map groupOnTp (Map.fromListWith (<>) [(name d, [d]) | d <- decls])  :: Map Name (Map SignOrd Relation)
              where
                groupOnTp lst = Map.fromListWith const [(SignOrd $ sign d, d) | d <- lst]
          -- An edge `(A,B)` in `dagGraph` means that the atoms of `A` are a subset of the atoms of `B`. We say: \"`A` is more specific than `B`\", which is a partial ordering relation on A_Concepts.
          -- This partial ordering relation allows us to freely mix concepts from lattice theory (e.g. lattice, join, meet) and graph theory (vertex, edge) in our language.
          -- The process of making typologies starts with the initialGraph :: AdjacencyMap P_Concept:
          -- * `dagGraph = graph2dag initialGraph` ensures dagGraph is a DAG, and therefore acyclic.
          -- * `wccs = wcComponents dagGraph` computes the weakly connected components of dagGraph. Every wcc will become a typology
          -- * `map createTypology completeWCCs` gives us the typologies.
        return CI{ reprType = reprOf
                 , multiKernels = typols
                 , reprList = allReprs
                 , declarationsMap = declMap
                --  , soloConcs = Set.filter (not . isInSystem genLattice) allConcs
                 , allPConcepts = pCpts
                 , conceptMap = pCpt2aCpt
                 , defaultLang = deflangCtxt
                 , defaultFormat = deffrmtCtxt
                 }

      -- story about genRules and genLattice
      -- the genRules is a list of equalities between concept sets, in which every set is interpreted as a conjunction of concepts
      -- the genLattice is the resulting optimized structure

      -- TODO: The definition of PClassify (in ParseTree) does not support IsE, so the original IS statement is likely not reproduced correctly in all cases.
      pClassify2aClassify :: ContextInfo -> PClassify -> Guarded AClassify
      pClassify2aClassify ci pg = case NE.tail (generics pg) of
        [] -> (Isa (origin pg) <$> pCpt2aCpt (NE.head (generics pg))) <*> pCpt2aCpt (specific pg)
        _ -> do
          genGenerics <- traverse pCpt2aCpt (generics pg)
          genSpec <- pCpt2aCpt (specific pg)
          pure IsE
            { genpos = origin pg,
              genrhs = genGenerics,
              genspc = genSpec
            }
        where
          pCpt2aCpt = conceptMap ci (origin pg)

      pPop2aPop :: ContextInfo -> P_Population -> Guarded Population
      pPop2aPop ci pop =
        case pop of
          P_RelPopu {p_nmdr = nmdr, p_popps = aps} ->
            do
              expr <- term2Expr env ci Nothing (Prim (PNamedR nmdr))
              let dcl = case expr of
                         EDcD d -> d
                         _ -> fatal ("Expected a relation in a population, but got "<>tshow expr<>".")
              aps' <- traverse (pAtomPair2aAtomPair (reprType ci) dcl) aps
              return
                ARelPopu
                  { popdcl = dcl,
                    popps = Set.fromList aps',
                    popsrc = source dcl,
                    poptgt = target dcl
                  }
          P_CptPopu {} -> do
            validatePConceptsInSchema ci (origin pop) pop ("POPULATION for " <> fullName (p_cpt pop))
            cpt <- conceptMap ci (origin pop) (p_cpt pop)
            vals <- traverse (pAtomValue2aAtomValue (reprType ci) cpt) (p_popas pop)
            pure ACptPopu
                  { popcpt = cpt,
                    popas = vals
                  }

      pViewDef2aViewDef :: ContextInfo -> P_ViewDef -> Guarded ViewDef
      pViewDef2aViewDef ci
        pvd@P_Vd{ pos = orig,
              vd_nm = nm,
              vd_label = lbl',
              vd_cpt = cpt,
              vd_isDefault = isDefault,
              vd_html = mHtml,
              vd_ats = segmnts
            } = do
                   -- Validate all concepts in the view definition
                   validatePConceptsInSchema ci orig pvd ("VIEW " <> fullName nm)
                   viewConcept <- conceptMap ci orig cpt
                   segments <- traverse (typeCheckViewSegment viewConcept) (zip [0 ..] segmnts)
                   uniqueLabels orig toLabel (filter hasLabel segments) "VIEW statement"
                   return
                         Vd
                           { vdpos = orig,
                             vdname = nm,
                             vdlabel = lbl',
                             vdcpt = viewConcept,
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

          typeCheckViewSegment :: A_Concept -> (Integer, P_ViewSegment TermPrim) -> Guarded ViewSegment
          typeCheckViewSegment viewConcept (seqNr, seg) = do
            payload <- typeCheckPayload (origin seg) (vsm_load seg)
            return ViewSegment
              { vsmpos = origin seg,
                vsmlabel = vsm_labl seg,
                vsmSeqNr = seqNr,
                vsmLoad = payload
              }
            where
              typeCheckPayload :: Origin -> P_ViewSegmtPayLoad TermPrim -> Guarded ViewSegmentPayLoad
              typeCheckPayload origPL (P_ViewExp term) = do
                xpr <- term2Expr env ci (Just viewConcept) term
                case geq (source xpr) viewConcept of
                  Just True  -> pure (ViewExp xpr)
                  Just False -> Errors . pure $ mkRelationTooNarrowForViewError origPL xpr viewConcept
                  Nothing    -> Errors . pure $ mkViewExpressionMismatchError origPL xpr viewConcept
              typeCheckPayload _ (P_ViewText str) = pure $ ViewText str

      -- | pSubIfc2aSubIfc uses the box concept from the parent box to constrain type-checking of sub-interfaces.
      pSubIfc2aSubIfc :: ContextInfo -> A_Concept -> P_SubIfc TermPrim -> Guarded SubInterface
      pSubIfc2aSubIfc ci boxConcept sub =
        case sub of
          P_Box{} -> do subBoxes <- mapM (pBoxItem2aBoxItem ci (Just boxConcept)) (si_box sub)
                        uniqueLabels (origin sub) toLabel (filter hasLabel subBoxes) "BOX"
                        return (Box{ pos = origin sub, siConcept = boxConcept, siHeader = si_header sub, siObjs = subBoxes})
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
          toLabel :: BoxItem -> Text1
          toLabel (BxExpr obj) = case objPlainName obj of
            Nothing -> fatal "Box items without a plain name should have been filtered out here"
            Just nm -> nm
          toLabel (BxText {}) = fatal "BxText items should have been filtered out"

          hasLabel :: BoxItem -> Bool
          hasLabel (BxExpr obj) = isJust (objPlainName obj)
          hasLabel (BxText {}) = False

          -- | getInterface checks that the referenced interface exists.
          -- Compatibility checking is deferred to Phase 2 (validateInterfaceRefs) to avoid cyclic references in the type checker.
          getInterface :: Guarded A_Concept
          getInterface
           = case filter ((==si_str sub).name) p_interfaces of
               [] -> (Errors . return . CTXE (origin sub)) ("Interface " <> (tshow.si_str) sub <> " not found")
               [_pIfc] ->
                 -- Phase 1: Just check existence, return topCpt as placeholder
                 -- The actual compatibility check happens in Phase 2 after all interfaces are typed
                 pure topCpt
               _ -> (Errors . return . CTXE (origin sub)) ("Multiple interfaces with name " <> (tshow.si_str) sub <> " found")

      -- | mBoxConcept is the A_Concept from the parent box, used to constrain type-checking of relations.
      --   It is "Maybe" because on the top level, in pIfc2aIfc, there is no parent box concept.
      --   It is "A_Concept" it can be the target concept of an expression, so it cannot be a P_Concept. 
      pBoxItem2aBoxItem :: ContextInfo -> Maybe A_Concept -> P_BoxItem TermPrim -> Guarded BoxItem
      pBoxItem2aBoxItem ci mBoxConcept pBoxItem =
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
              -- The result must be more generic than or equal to the mBoxConcept.
              objExpr <- term2Expr env ci mBoxConcept term
              a_msub <- if target objExpr == topCpt || target objExpr == botCpt
                        then pure Nothing  -- Skip sub-interface processing for wildcard types
                        else traverse (pSubIfc2aSubIfc ci (target objExpr)) p_msub
              checkCrud
              typeCheckViewAnnotation objExpr mView
              crud <- pCruds2aCruds objExpr mCrud
              return (BxExpr ObjectDef{ objPlainName  = nm,
                                        objlbl        = lbl',
                                        objPos        = orig,
                                        objExpression = objExpr,
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
                    Just vd -> do
                      viewConcept <- conceptMap ci (origin pBoxItem) (vd_cpt vd)
                      -- Check both directions to distinguish between "too specific" and "incompatible"
                      case (viewConcept `geq` target objExpr, target objExpr `join` viewConcept) of
                            (Just True, _)    -> pure ()  -- viewConcept is more general or equal: OK
                            (Just False, _)   -> Errors . pure $ mkViewTooSpecificError (origin pBoxItem) vd objExpr  -- target is more general: too specific
                            (_, Just joinCpt) -> Errors . pure $ mkViewIncompatibleError (origin pBoxItem) joinCpt vd objExpr  -- Different typologies: incompatible
                            (_, Nothing) -> Errors . pure $ mkViewIncomparableError (origin pBoxItem) vd objExpr
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
      pIfc2aIfc ci pIfc =
        -- Validate concepts first - if this fails, return immediately without processing box items
        case validatePConceptsInSchema ci (origin pIfc) pIfc ("INTERFACE " <> fullName pIfc) of
          Errors err -> Errors err
          Checked () warnings -> addWarnings warnings $ do
            -- Only proceed with type-checking if validation succeeded
            pBox <- pBoxItem2aBoxItem ci Nothing (ifc_Obj pIfc)
            boxItem <- case pBox of
                          BxExpr{} -> pure (objE pBox)
                          _ -> (Errors . return . CTXE (origin pIfc)) "TXT is not expected here."
            -- Check for duplicate labels in the top-level box
            case objmsub boxItem of
              Just (Box { siObjs = subBoxes }) -> uniqueLabels (origin pIfc) toLabel (filter hasLabel subBoxes) "BOX"
              _ -> pure ()
            -- Note: We don't check the TType here because it hasn't been determined yet.
            -- The TType (Object) will be assigned automatically by getObjReprs after all interfaces are type-checked.
            return
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
        where
          toLabel :: BoxItem -> Text1
          toLabel (BxExpr obj) = case objPlainName obj of
            Nothing -> fatal "Box items without a plain name should have been filtered out here"
            Just nm -> nm
          toLabel (BxText {}) = fatal "BxText items should have been filtered out"

          hasLabel :: BoxItem -> Bool
          hasLabel (BxExpr obj) = isJust (objPlainName obj)
          hasLabel (BxText {}) = False


      pRoleRule2aRoleRule :: P_RoleRule -> Set A_RoleRule
      pRoleRule2aRoleRule prr =
        Set.fromList
          [ A_RoleRule
              { arRole = rol,
                arRule = rul,
                arPos = origin prr
              }
            | rol <- NE.toList (mRoles prr),
              rul <- NE.toList (mRules prr)
          ]

      pPat2aPat :: ContextInfo -> P_Pattern -> Guarded Pattern
      pPat2aPat ci ppat =
        f
          <$> traverse (pRul2aRul ci (Just $ label ppat)) (pt_rls ppat)
          <*> traverse (pIdentity2aIdentity ci (Just $ label ppat)) (pt_ids ppat)
          <*> traverse (pPop2aPop ci) (pt_pop ppat)
          <*> traverse (pViewDef2aViewDef ci) (pt_vds ppat)
          <*> traverse (pPurp2aPurp ci) (pt_xps ppat)
          <*> traverse (pDecl2aDecl (reprType ci) (conceptMap ci) (Just $ label ppat) deflangCtxt deffrmtCtxt) (pt_dcs ppat)
          <*> traverse (pConcDef2aConcDef (conceptMap ci) (defaultLang ci) (defaultFormat ci)) (pt_cds ppat)
          <*> pure (Set.unions . map pRoleRule2aRoleRule . pt_RRuls $ ppat)
          <*> pure (pt_Reprs ppat)
          <*> traverse (pClassify2aClassify ci) (pt_gns ppat)
          <*> traverse (pEnforce2aEnforce ci (Just $ label ppat)) (pt_enfs ppat)
        where
          f rules' keys' pops' views' purps' relations conceptdefs roleRules representations gns' enforces' =
            A_Pat
              { ptnm   = name ppat,
                ptlbl  = mLabel ppat,
                ptpos  = origin ppat,
                ptend  = pt_end ppat,
                ptrls  = Set.fromList rules',
                ptgns  = gns',
                ptdcs  = Set.fromList relations,
                ptrrs  = roleRules,
                ptcds  = conceptdefs,
                ptrps  = representations,
                ptups  = pops',
                ptids  = keys',
                ptvds  = views',
                ptxps  = purps',
                ptenfs = enforces'
              }

      typeCheckPairView :: ContextInfo -> Expression -> PairView (Term TermPrim) -> Guarded (PairView Expression)
      typeCheckPairView ci expr (PairView lst) =
        PairView <$> traverse typeCheckPairViewSeg lst
        where
          typeCheckPairViewSeg (PairViewText segOrigin x)  = pure (PairViewText segOrigin x)
          typeCheckPairViewSeg (PairViewExp segOrigin s x) =
            do let requiredConcept = (case s of Src->source; Tgt->target) expr
               e <- term2Expr env ci (Just requiredConcept) x
               return (PairViewExp segOrigin s e)

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
            exp' <- term2Expr env ci Nothing expr
            vls <- maybeOverGuarded (typeCheckPairView ci exp') viols
            return
              Rule
                { rrnm = nm,
                  rrlbl = lbl',
                  -- According to Ampersand semantics, a rule without |- or = 
                  -- (i.e., "RULE name : expr") should be equivalent to "V |- expr"
                  -- This means the expression must hold for all pairs (i.e., must equal V)
                  formalExpression = case exp' of
                                       EInc _ -> exp'  -- Already has |-
                                       EEqu _ -> exp'  -- Already has =
                                       _      -> EInc (EDcV (sign exp'), exp'),
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
            penfFlipped = isFlped,
            penfOp = oper,
            penfExpr = x
          } = case oper of
                IsSuperSet {} ->
                  do xpr <- term2Expr env ci Nothing (PInc pos' x relTerm)
                     case xpr of
                       EInc (expr,EDcD rel) -> return (toAEnforce rel expr)
                       EInc (expr,EFlp (EDcD rel)) -> return (toAEnforce rel (flp expr))
                       _ -> fatal "Alternative 1 in pEnforce2aEnforce."
                IsSubSet {} ->
                  do xpr <- term2Expr env ci Nothing (PInc pos' relTerm x)
                     case xpr of
                       EInc (EDcD rel,expr) -> return (toAEnforce rel expr)
                       EInc (EFlp (EDcD rel),expr) -> return (toAEnforce rel (flp expr))
                       _ -> fatal "Alternative 2 in pEnforce2aEnforce."
                IsSameSet {} ->
                  do xpr <- term2Expr env ci Nothing (PEqu pos' relTerm x)
                     case xpr of
                       EEqu (EDcD rel,expr) -> return (toAEnforce rel expr)
                       EInc (EFlp (EDcD rel),expr) -> return (toAEnforce rel (flp expr))
                       _ -> fatal "Alternative 3 in pEnforce2aEnforce."
          where
            relTerm = if isFlped then Prim (flp pRel) else Prim pRel
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
        do let orig = origin pidt
           cpt <- conceptMap ci orig (ix_cpt pidt)
           validatePConceptsInSchema ci orig pidt ("IDENT " <> fullName (ix_name pidt))
           isegs <- traverse (term2Expr env ci (Just cpt)) (ix_ats pidt)
           return ( Id
                    { idPos = orig,
                      idName = ix_name pidt,
                      idlabel = ix_label pidt,
                      idCpt = cpt,
                      idPat = mPat,
                      identityAts = isegs
                    }
                  )

      pPurp2aPurp :: ContextInfo -> PPurpose -> Guarded Purpose
      pPurp2aPurp ci ppurp@PPurpose{ pos = purpOrigin,    -- :: Origin
                                     pexObj = objref,     -- :: PRefObj
                                     pexMarkup = pmarkup, -- :: P_Markup
                                     pexRefIDs = refIds   -- :: [Text]
                                   }
       = do
          -- Validate concepts in PURPOSE (only validates PRef2ConceptDef)
          validatePConceptsInSchema ci purpOrigin ppurp "PURPOSE"
          obj <- pRefObj2aRefObj objref
          pure Expl
                { explPos = purpOrigin,
                  explObj = obj,
                  explMarkup = pMarkup2aMarkup deflangCtxt deffrmtCtxt pmarkup,
                  explRefIds = refIds
                }
          where
            pRefObj2aRefObj :: PRef2Obj -> Guarded ExplObj
            pRefObj2aRefObj (PRef2ConceptDef s) = ExplConcept <$> conceptMap ci purpOrigin (mkPConcept s)
            pRefObj2aRefObj (PRef2Relation tm) = ExplRelation <$> namedRel2Decl (conceptMap ci) (declarationsMap ci) tm
            pRefObj2aRefObj (PRef2Rule s) = pure $ ExplRule s
            pRefObj2aRefObj (PRef2IdentityDef s) = pure $ ExplIdentityDef s
            pRefObj2aRefObj (PRef2ViewDef s) = pure $ ExplViewDef s
            pRefObj2aRefObj (PRef2Pattern s) = pure $ ExplPattern s
            pRefObj2aRefObj (PRef2Interface s) = pure $ ExplInterface s
            pRefObj2aRefObj (PRef2Context s) = pure $ ExplContext s

      allConceptDefsOutPats :: ContextInfo -> Guarded [AConceptDef]
      allConceptDefsOutPats ci = traverse (pConcDef2aConcDef (conceptMap ci) deflangCtxt deffrmtCtxt) p_conceptdefs
      allConceptDefs :: ContextInfo -> Guarded [AConceptDef]
      allConceptDefs ci = traverse (pConcDef2aConcDef (conceptMap ci) deflangCtxt deffrmtCtxt) (p_conceptdefs <> concatMap pt_cds p_patterns)

data OpTree a
  = STbinary  (OpTree a) (OpTree a) [a]
  | STunary   (OpTree a) [a]
  | STnullary [a]
  deriving (Functor, Foldable, Traversable)

opSigns :: OpTree a -> [a]
opSigns (STbinary _ _ ss) = ss
opSigns (STunary _ ss)    = ss
opSigns (STnullary ss)    = ss

assignOpSigns :: [a] -> OpTree a -> OpTree a
assignOpSigns ss (STbinary l r _) = STbinary l r ss
assignOpSigns ss (STunary e _) = STunary e ss
assignOpSigns ss (STnullary _) = STnullary ss

-- showTriple :: (Expression, Signature, Term TermPrim) -> Text
-- showTriple (expr, sgn, trm) = "("<>showA expr<>", "<>tshow sgn<>", "<>showP trm<>")"

-- Uncomment showGuardedOpTree for certain trace statements. (Search for showGuardedOpTree to find out which ones.)
-- showGuardedOpTree :: Guarded (OpTree (Expression, Signature, Term TermPrim)) -> Text
-- showGuardedOpTree (Errors errs) = 
--   "Errors:\n" <> T.intercalate "\n" (map tshow (NE.toList errs))
-- showGuardedOpTree (Checked opTree []) = showOpTree opTree
-- showGuardedOpTree (Checked opTree warnings) = 
--   "Checked (with " <> tshow (length warnings) <> " warning(s)):\n" <> 
--   showOpTree opTree <> "\n\nWarnings:\n" <> 
--   T.intercalate "\n" (map tshow warnings)

instance (Show a) => Show (OpTree a) where
  show = T.unpack . showOpTreeStructure
    where
      showOpTreeStructure :: (Show a) => OpTree a -> Text
      showOpTreeStructure opTree = T.intercalate "\n" (showIndented "   " opTree)

      showIndented :: (Show a) => Text -> OpTree a -> [Text]
      showIndented indent (STnullary ss) =
        [indent <> T.intercalate ", " (fmap tshow ss)]
      showIndented indent (STunary t ss) =
        (indent <> T.intercalate ", " (fmap tshow ss)) :
        showIndented (indent <> "   ") t
      showIndented indent (STbinary l r ss) =
        (indent <> T.intercalate ", " (fmap tshow ss)) :
        showIndented (indent <> "   ") l <>
        showIndented (indent <> "   ") r

{-
showOpTree formats the OpTree structure in a readable way.
Cline built it on Aug 6th, 2025.
Personally, I would not have put in the effort myself but it was too much fun to let Cline do it.)
Example output when compiling testing/Travis/testcases/prototype/shouldSucceed/try17.adl:
[Student*Subject] shouldPass[Student*Subject]=enrolled[Student*Course];requires[Course*Subject]
[Student*Subject]     ├── shouldPass[Student*Subject]
[Student*Subject]     └── enrolled[Student*Course];requires[Course*Subject]
[Student*Course]          ├── enrolled[Student*Course]
[Course*Subject]          └── requires[Course*Subject]
-}
showOpTree :: OpTree (Expression, Signature, Term TermPrim) -> Text
showOpTree opTree =
  let outputLines = map T.concat (L.transpose [sigTexts, paddings, treeLines, exprTexts])
  in if length outputLines == 1
     then ""
     else T.intercalate "\n" outputLines
  where
    -- Step 1: Extract all signatures as [Text]
    sigTexts = extractSignatures opTree

    -- Step 2: Compute maximum length and create padding list as [Text]
    paddings = map (\sigText -> T.replicate (maxWidth - T.length sigText + 1) " ") sigTexts
     where maxWidth = foldr (max . T.length) 0 sigTexts

    -- Step 3: Create recursive tree of expressions as [Text]  
    treeLines = extractTreeLines opTree "" True
    exprTexts = extractExpressions opTree

    extractSignatures :: OpTree (Expression, Signature, Term TermPrim) -> [Text]
    extractSignatures (STnullary ss) = [T.intercalate ", " (map (tshow . snd3) ss)]
    extractSignatures (STunary t ss) = [T.intercalate ", " (map (tshow . snd3) ss)] <> extractSignatures t
    extractSignatures (STbinary l r ss) = [T.intercalate ", " (map (tshow . snd3) ss)] <> extractSignatures l <> extractSignatures r

    extractExpressions :: OpTree (Expression, Signature, Term TermPrim) -> [Text]
    extractExpressions (STnullary ss) = [T.intercalate ", " (map (showA . fst3) ss)]
    extractExpressions (STunary t ss) = [T.intercalate ", " (map (showA . fst3) ss)] <> extractExpressions t
    extractExpressions (STbinary l r ss) = [T.intercalate ", " (map (showA . fst3) ss)] <> extractExpressions l <> extractExpressions r

    extractTreeLines :: OpTree (Expression, Signature, Term TermPrim) -> Text -> Bool -> [Text]
    extractTreeLines (STnullary _) prefix isLast =
      [if T.null prefix then "" else prefix <> (if isLast then "└── " else "├── ")]
    extractTreeLines (STunary t _) prefix isLast =
      [if T.null prefix then "" else prefix <> (if isLast then "└── " else "├── ")] <>
      extractTreeLines t (prefix <> if isLast then "    " else "│   ") True
    extractTreeLines (STbinary l r _) prefix isLast =
      [if T.null prefix then "" else prefix <> (if isLast then "└── " else "├── ")] <>
      extractTreeLines l newPrefix False <>
      extractTreeLines r newPrefix True
      where
        newPrefix = prefix <> (if isLast then "    " else "│   ")

instance (Flippable a) => Flippable (OpTree a) where
  flp (STbinary a b ss) = STbinary (flp b) (flp a) (flp ss)
  flp (STunary t ss) = STunary (flp t) (flp ss)
  flp (STnullary ss) = STnullary (flp ss)

instance (Flippable a, Flippable b) => Flippable (a,b) where
  flp (e,s) = (flp e, flp s)

instance (Flippable a, Flippable b, Flippable c) => Flippable (a,b,c) where
  flp (e,s,trm) = (flp e, flp s, flp trm)

-- Helper function for creating verbose type error messages
mkVerboseTypeError :: (HasRunner env) => env -> Origin -> OpTree (Expression, Signature, Term TermPrim) -> Text -> Guarded a
mkVerboseTypeError env errorOrigin opTree baseMsg =
  Errors . return $ CTXE errorOrigin $
    if logLevel (view runnerL env) <= LevelInfo
    then let treeOutput = showOpTree opTree
         in if T.null treeOutput
            then baseMsg
            else baseMsg <> "\n\nType analysis:\n" <> treeOutput
    else baseMsg

-- | Validate that P_Concepts in a P-structure are in the schema
-- SESSION is always implicitly declared, so it's exempt from validation
validatePConceptsInSchema :: PConceptStructure a => ContextInfo -> Origin -> a -> Text -> Guarded ()
validatePConceptsInSchema ci orig pStruct contextName =
  case [ pc | pc <- Set.toList (pConcs pStruct), fullName (name pc) /= "SESSION", fullName (name pc) /= "ONE", pc `Set.notMember` allPConcepts ci] of
    [] -> pure ()
    c:cs -> Errors . fmap (\pc -> mkConceptNotInSchemaError orig (name pc) contextName) $ c NE.:| cs

-- | Check if a signature contains the TOP concept
-- containsTOP :: Signature -> Bool
-- containsTOP (Sign src tgt) = src==topCpt || tgt==topCpt
-- containsTOP (ISgn cpt)     = cpt==topCpt

-- | Check if a signature contains the BOT concept
-- containsBOT :: Signature -> Bool
-- containsBOT (Sign src tgt) = src==botCpt || tgt==botCpt
-- containsBOT (ISgn cpt)     = cpt==botCpt

-- | Compute the 'between' concept from a binary expression.
-- For compositions e1;e2, the between concept is meet(target(e1), source(e2)).
-- For left residual e1/e2, the between concept is meet(target(e1), target(e2)).
-- For right residual e1\e2, the between concept is meet(source(e1), source(e2)).
-- Returns Nothing if the concepts are incompatible (no meet/join exists).
-- This prevents botCpt from entering the system where it might be looked up.
getBetweenConcept :: Expression -> Maybe A_Concept
getBetweenConcept (ECps (e1, e2)) = meet (target e1) (source e2)
getBetweenConcept (EDia (e1, e2)) = join (target e1) (source e2)
getBetweenConcept (ERad (e1, e2)) = join (target e1) (source e2)
getBetweenConcept (ELrs (e1, e2)) = meet (target e1) (target e2)
getBetweenConcept (ERrs (e1, e2)) = meet (source e1) (source e2)
getBetweenConcept e = fatal ("getBetweenConcept: pattern match failure on expression "<>showA e<>"\nThis is a bug in the compiler.")

-- | Refine an expression by replacing TOP with more specific concepts, capped with a target signature. Only narrow, never widen.
-- Pre: targetSig contains no TOP or BOT concepts (i.e. it is concrete).
-- Calling refineANY targetSig once gives the same result as calling it more often; it is an idempotent function.
-- This function is used both during type inference and as a final cleanup step.
-- It only narrows types (makes them more specific), never widens them.
refineANY :: Signature -> Expression -> Expression
refineANY targetSig expr
 = -- trace ("refineANY (" <> tshow targetSig <> ") (" <> showA expr <> ") yields " <> showA refinedExpr)
   refinedExpr
  where
    -- For heterogeneous signatures (where src and tgt have no meet),
    -- we cannot refine narrowable expressions like I, V, atoms, etc.
    -- In such cases, mIcpt will be Nothing
    mIcpt = case targetSig of
              ISgn cpt     -> Just cpt
              Sign src tgt -> meet src tgt
    refinedExpr = case expr of
      -- Nullary operations:
      -- geq returns Just True if first arg is more generic than the second arg
      EDcI cpt        -> if cpt==topCpt
                         then maybe expr EDcI mIcpt  -- Cannot refine for heterogeneous signature
                         else expr
      -- For EMp1: only narrow, never widen. If expr's concept is already narrower than target, keep it.
      EMp1 av cpt     -> if cpt==topCpt
                         then case mIcpt of
                                Just iCpt -> EMp1 av iCpt
                                Nothing   -> expr  -- Cannot refine for heterogeneous signature
                         else expr
      EDcD _          -> expr -- Declarations are concrete already
      EBin oper sgn   -> case (source sgn==topCpt, target sgn==topCpt) of
                           (True , True ) -> EBin oper targetSig
                           (True , False) -> EBin oper (source targetSig `Sign` target sgn)
                           (False, True ) -> EBin oper (source sgn `Sign` target targetSig)
                           (False, False) -> EBin oper sgn
      EDcV sgn        -> case (source sgn==topCpt, target sgn==topCpt) of
                           (True , True ) -> EDcV targetSig
                           (True , False) -> EDcV (source targetSig `Sign` target sgn)
                           (False, True ) -> EDcV (source sgn `Sign` target targetSig)
                           (False, False) -> EDcV sgn
      -- Binary operations:
      EEqu (e1, e2)   -> EEqu (refineANY targetSig e1, refineANY targetSig e2)
      EInc (e1, e2)   -> EInc (refineANY targetSig e1, refineANY targetSig e2)
      EUni (e1, e2)   -> EUni (refineANY targetSig e1, refineANY targetSig e2)
      EIsc (e1, e2)   -> EIsc (refineANY targetSig e1, refineANY targetSig e2)
      EDif (e1, e2)   -> EDif (refineANY targetSig e1, refineANY targetSig e2)
      ECps (e1, e2)   -> case getBetweenConcept expr of
                           Just between -> ECps (refineANY (Sign (source targetSig) between) e1, refineANY (Sign between (target targetSig)) e2)
                           Nothing -> expr  -- Can't refine - concepts are incompatible
      ELrs (e1, e2)   -> case getBetweenConcept expr of
                           Just between -> ELrs (refineANY (Sign (source targetSig) between) e1, refineANY (Sign (target targetSig) between) e2)
                           Nothing -> expr
      ERrs (e1, e2)   -> case getBetweenConcept expr of
                           Just between -> ERrs (refineANY (Sign between (source targetSig)) e1, refineANY (Sign between (target targetSig)) e2)
                           Nothing -> expr
      EDia (e1, e2)   -> case getBetweenConcept expr of
                           Just between -> EDia (refineANY (Sign (source targetSig) between) e1, refineANY (Sign between (target targetSig)) e2)
                           Nothing -> expr
      ERad (e1, e2)   -> case getBetweenConcept expr of
                           Just between -> ERad (refineANY (Sign (source targetSig) between) e1, refineANY (Sign between (target targetSig)) e2)
                           Nothing -> expr
      EPrd (e1, e2)   -> EPrd (refineANY (Sign (source targetSig) (target e1)) e1, refineANY (Sign (source e2) (target targetSig)) e2)
      -- Unary operations:
      EFlp e          -> EFlp (refineANY (flp targetSig) e)
      ECpl e          -> ECpl (refineANY targetSig e)
      EBrk e          -> EBrk (refineANY targetSig e)
      EKl0 e          -> EKl0 (refineANY targetSig e)
      EKl1 e          -> EKl1 (refineANY targetSig e)

-- | term2Expr converts a Term to an Expression, ensuring that the Expression and each of its subexpressions has precisely one concrete signature.
--   (A signature is concrete if it does not contain TOP or BOT.)
--   If it cannot do that, it produces relevant error messages the user can understand.
--   Thus, term2Expr ensures that the expression has a well-defined population, as specified in the documentation.
--   As a consequence, the expression can be translated to SQL unambiguously and a database can realize those semantics.
term2Expr :: (HasRunner env) => env -> ContextInfo -> Maybe A_Concept -> Term TermPrim -> Guarded Expression
term2Expr env ci mConstraintCpt term
  = do sgnTree <- -- trace ("🔍 term2Expr called with constraint=" <> tshow mConstraintCpt <> ", term=" <> showP term <> " at " <> tshow (origin term)) $
                  signatures (case mConstraintCpt of Just cpt -> Just (Src, cpt); Nothing -> Nothing) term
       checkAmbiguities sgnTree
       case -- trace (showOpTree sgnTree)
            sgnTree of
         STnullary    [(expr, sig, _)]
           | isConcreteSignature sig -> pure expr  -- Single expression - already reduced, return it
           | otherwise -> -- trace ("\nterm2Expr (mConstraintSig="<>tshow mConstraintSig<>") ("<>showP term<>"):\n"<>showOpTree sgnTree) $
                           mkVerboseTypeError env (origin term) sgnTree ("Cannot determine a concrete type for nullary term " <> showP term <> ". The inferred signature contains " <> tshow sig)
         STunary _    [(_, sig, _)]
           | not (isConcreteSignature sig) -> mkVerboseTypeError env (origin term) sgnTree ("Cannot determine a concrete type for unary term " <> showP term <> ". The inferred signature contains " <> tshow sig)
         STbinary _ _ [(_, sig, _)]
           | not (isConcreteSignature sig) -> mkVerboseTypeError env (origin term) sgnTree ("Cannot determine a concrete type for binary term " <> showP term <> ". The inferred signature contains " <> tshow sig)
         STnullary    []      -> fatal "Empty triples list in STnullary"
         STunary _    []      -> fatal "Empty triples list in STunary"
         STbinary _ _ []      -> fatal "Empty triples list in STbinary"
         STnullary    (_:_:_) -> fatal ("term2Expr: pattern match failure in STnullary. Function signatures yields:\n"<>showOpTree sgnTree<>"\nThis is a bug in the compiler.")
         STunary _    (_:_:_) -> fatal ("term2Expr: pattern match failure in STunary. Function signatures yields:\n"<>showOpTree sgnTree<>"\nThis is a bug in the compiler.")
         STbinary _ _ (_:_:_) -> fatal ("term2Expr: pattern match failure in STbinary. Function signatures yields:\n"<>showOpTree sgnTree<>"\nThis is a bug in the compiler.")
         _ -> -- trace ("term2Expr:\n"<>showOpTree sgnTree <> " at " <> tshow (origin term)) $
               do finalExpr <- rootExpression sgnTree
                  if isConcreteSignature (sign finalExpr)
                    then pure finalExpr
                    else Errors . pure $ CTXE (origin term) $
                      "Cannot determine a concrete type for expression " <> showP term <> ". " <>
                      "The inferred signature " <> tshow (sign finalExpr) <> " contains TOP or BOT. " <>
                      "Please add an explicit signature."
  where
    rootExpression sgnTree =
       case sgnTree of
         STnullary    [ (expr, _, _) ] -> pure expr
         STunary _    [ (expr, _, _) ] -> pure expr
         STbinary _ _ [ (expr, _, _) ] -> pure expr
         _                          -> fatal ("term2Expr: pattern match failure in rootExpression. Function signatures yields:\n"<>showOpTree sgnTree<>"\nThis is a bug in the compiler.")

    checkAmbiguities :: OpTree (Expression, Signature, Term TermPrim) -> Guarded ()
    checkAmbiguities opTree =
      case findAmbiguities True opTree of
        [] -> pure ()
        err:errs -> Errors (err NE.:| errs)
      where
        findAmbiguities :: Bool -> OpTree (Expression, Signature, Term TermPrim) -> [CtxError]
        findAmbiguities isRoot node =
          let triples = opSigns node
              sigs = map (\(_, s, _) -> s) triples
              uniqueSigs = L.nub sigs
          in case triples of
              [_] -> []  -- Only one triple → no ambiguity here
              []  -> fatal "findAmbiguities: empty triples list. This is a bug in the compiler."
              _   -> if length sigs == length uniqueSigs
                     then [makeAmbiguityError isRoot triples uniqueSigs]
                     else case node of
                         STnullary _ -> []  -- Leaf nodes cannot have underlying ambiguities
                         STunary child _ -> findAmbiguities False child
                         STbinary l r _ -> findAmbiguities False l <> findAmbiguities False r

        makeAmbiguityError :: Bool -> [(Expression, Signature, Term TermPrim)] -> [Signature] -> CtxError
        makeAmbiguityError isRoot triples uniqueSigs =
          let trm = case triples of
                      (_, _, t):_ -> t
                      []          -> fatal "makeAmbiguityError: empty triples list"
              -- Group expressions by signature
              exprsBySig = [ (sig, [expr | (expr, s, _) <- triples, s == sig])
                           | sig <- uniqueSigs ]
          in CTXE (origin trm) $
               "Ambiguity in "<>(if isRoot then "" else "sub")<>"expression " <> showP trm <> ".\n" <>
               "This "<>(if isRoot then "" else "sub")<>"expression can have signatures " <>
               T.intercalate " or " (map tshow uniqueSigs) <>
               ", depending on the choice for either\n  " <>
               T.intercalate "\nor\n  "
                 [ showA expr
                 | (_, expr:_) <- exprsBySig ]

    -- | The signatures function computes all possible type signatures for a term within a constraint.
    --   It yields an OpTree with the exact recursive structure of the term.
    --   In every node there is a nonempty list of triples (Expression, Signature, Term)
    --   that details the (sub-)expression of that node.
    --   If signatures cannot construct at least one valid triple in each node, it returns a type error.
    --   Nullary terms correspond to Prim terms and they end the recursion.
    --
    --   mConstrSig::Signature is use for type-checking expressions in boxes (interfaces/APIs), IDENT statements, pairviews or views,
    --   to communicate the expected signature from the context. The signature must remain within mConstrSig.

    signatures :: Maybe (SrcOrTgt, A_Concept) -> Term TermPrim -> Guarded (OpTree (Expression, Signature, Term TermPrim))
    signatures mConstraint trm = do
      opTree <- -- trace ("4. signatures ("<>tshow mConstraint<>") ("<>showP trm<>") in "<>tshow (origin trm)<>" yields:")
                resultTerm
      let refinedOpTree = -- trace ("   resultTerm:  "<>showOpTree opTree) $
                          fmap (\(expr, sgn, t) -> (refineANY sgn expr, sgn, t)) opTree
      -- trace ("   refinedOpTree: " <> showOpTree refinedOpTree <> "\n   constrain refinedOpTree: " <> showGuardedOpTree (constrain refinedOpTree)) $
      constrain refinedOpTree
      where
        -- | resultPrim yields all possible (expression, signature, term) triples for a Prim term.
        -- Filter relations to keep only those with signatures wider or equal to constraint
        resultPrim :: TermPrim -> Guarded (OpTree (Expression, Signature, Term TermPrim))
        resultPrim trmPrim
         = case trmPrim of
             PI _               -> pure (STnullary [(EDcI topCpt, ISgn topCpt, Prim trmPrim)])
             Pid o c            -> do c' <- conceptMap ci o c
                                      pure (STnullary [(EDcI c', ISgn c', Prim trmPrim)])
             Patm _ av Nothing  -> pure (STnullary [(EMp1 av topCpt, ISgn topCpt, Prim trmPrim)])
             Patm o av (Just c) -> do c' <- conceptMap ci o c
                                      pure (STnullary [(EMp1 av c', ISgn c', Prim trmPrim)])
             PVee _             -> let sgn = Sign topCpt topCpt in pure (STnullary [(EDcV sgn, sgn, Prim trmPrim)])
             Pfull o src tgt    -> do src' <- conceptMap ci o src
                                      tgt' <- conceptMap ci o tgt
                                      let sgn = Sign src' tgt'
                                      pure (STnullary [(EDcV sgn, sgn, Prim trmPrim)])
             PBin _ oper        -> let x = topCpt      in pure (STnullary [(EBin oper (Sign x x), Sign x x, Prim trmPrim)])
             PBind o oper c     -> do x <- conceptMap ci o c
                                      pure (STnullary [(EBin oper (Sign x x), Sign x x, Prim trmPrim)])
             PFlipped t         -> do sgnTree <- signatures (fmap flp mConstraint) (Prim t)
                                      pure (flp sgnTree)
             PNamedR rel        -> do relsList <- case p_mbSign rel of
                                        Just sg -> do sgn <- pSign2aSign (conceptMap ci (origin trmPrim)) sg
                                                      pure (findRelsTyped (declarationsMap ci) (name rel) sgn)
                                        Nothing -> pure (findDecls (declarationsMap ci) (name rel))
                                      -- ( if tshow (name rel) == "productnaam"
                                      --       then trace ("DEBUG relsList for productnaam: ["
                                      --                <> T.intercalate ", " (map (tshow . sign) relsList)
                                      --                <> "]")
                                      --       else id
                                      --   ) $
                                      case relsList of
                                          [] -> Errors . return . CTXE (origin trmPrim) $ "Undeclared relation " <> showP trmPrim
                                          ds -> pure (STnullary [(EDcD d, sign d, Prim trmPrim) | d <- ds])

        -- | resultTerm yields all possible (expression, signature, term) triples for a term.
        resultTerm :: Guarded (OpTree (Expression, Signature, Term TermPrim))
        resultTerm
         = case trm of
            Prim trmPrim -> resultPrim trmPrim
            -- Intra-type operations: decompose constraint
            PCps o a b -> checkIntra o "composition"       ECps (PCps o) a b "PCps"
            PRad o a b -> checkIntra o "relative addition" ERad (PRad o) a b "PRad"
            PDia o a b -> checkIntra o "diamond"           EDia (PDia o) a b "PDia"
            PLrs o a b -> checkPLrs  o "left residual"     ELrs (PLrs o) a b "PLrs"
            PRrs o a b -> checkPRrs  o "right residual"    ERrs (PRrs o) a b "PRrs"
            PPrd o a b -> do let lConstraint = case mConstraint of
                                                  Just (Src, _) -> mConstraint
                                                  _             -> Nothing
                                 rConstraint = case mConstraint of
                                                  Just (Tgt, _) -> mConstraint
                                                  _             -> Nothing
                             sgnaTree <- signatures lConstraint a
                             sgnbTree <- signatures rConstraint b
                             -- For PPrd, combine refined expressions directly, avoiding calls to checkIntra and makeTriples
                             let triples = [ let expr = EPrd (expr_a, expr_b)
                                                in (expr, sign expr, PPrd o trm_a trm_b)
                                           | (expr_a, _sgn_a, trm_a) <- opSigns sgnaTree
                                           , (expr_b, _sgn_b, trm_b) <- opSigns sgnbTree
                                           ]
                             return (STbinary sgnaTree sgnbTree triples)
            PFlp _ e   -> do -- When flipping, swap the constraint using the Flippable instance
                             sgnTree <- signatures (fmap flp mConstraint) e
                             return (STunary sgnTree [(EFlp expr, flp sgn, trm) | (expr, sgn, _)<-opSigns sgnTree])
            PKl0 o e   -> do sgnTree <- signatures mConstraint e
                             let endoSigns = [(EKl0 expr, sgn, trm) | (expr, sgn, _)<-opSigns sgnTree, source sgn == target sgn]
                             case endoSigns of
                               [] -> mkVerboseTypeError env o sgnTree ("Kleene star (*) requires an endomorphic relation (source must equal target).\n  Expression: " <> showP e <> "\n  Available signatures: " <> T.intercalate ", " [tshow sgn | (_, sgn, _) <- opSigns sgnTree])
                               _  -> return (STunary sgnTree endoSigns)
            PKl1 o e   -> do sgnTree <- signatures mConstraint e
                             let endoSigns = [(EKl1 expr, sgn, trm) | (expr, sgn, _)<-opSigns sgnTree, source sgn == target sgn]
                             case endoSigns of
                               [] -> mkVerboseTypeError env o sgnTree ("Kleene plus (+) requires an endomorphic relation (source must equal target).\n  Expression: " <> showP e <> "\n  Available signatures: " <> T.intercalate ", " [tshow sgn | (_, sgn, _) <- opSigns sgnTree])
                               _  -> return (STunary sgnTree endoSigns)
            PCpl _ e   -> do sgnTree <- signatures mConstraint e
                             return (STunary sgnTree [(ECpl expr, sgn, trm) | (expr, sgn, _)<-opSigns sgnTree])
            PBrk _ e   -> do sgnTree <- signatures mConstraint e
                             return (STunary sgnTree [(EBrk expr, sgn, trm) | (expr, sgn, _)<-opSigns sgnTree])
            -- Inter-type operations: both operands get the same constraint
            -- For union, use join (least upper bound) to find the common supertype
            -- For other operations (intersection, equation, inclusion, difference), use meet (greatest lower bound)
            PInc o a b -> checkPeri  o "inclusion"    Join EInc (PInc o) a b
            PEqu o a b -> checkPeri  o "equation"     Join EEqu (PEqu o) a b
            PIsc o a b -> checkPeri  o "intersection" Meet EIsc (PIsc o) a b
            PUni o a b -> checkPeri  o "union"        Join EUni (PUni o) a b
            PDif o a b -> checkPeri  o "difference"   Join EDif (PDif o) a b

        errorsPeri o kind joinOrMeet combinator pCombinator a b sgnaTree sgnbTree sgnsa sgnsb
         = let errorOpTree = STbinary sgnaTree sgnbTree [(combinator (expr_a, expr_b), sgn_a, pCombinator trm_a trm_b) | (expr_a, sgn_a, trm_a)<-sgnsa, (expr_b, _, trm_b)<-sgnsb ]
               conceptsSrc = L.nub [ src | (_,sgn_a,_)<-sgnsa, (_,sgn_b,_)<-sgnsb, Just src<-[joinOrMeet (source sgn_a) (source sgn_b)] ]
               conceptsTgt = L.nub [ tgt | (_,sgn_a,_)<-sgnsa, (_,sgn_b,_)<-sgnsb, Just tgt<-[joinOrMeet (target sgn_a) (target sgn_b)] ]
               -- | getRootSig extracts the unique Signature from an OpTree that has exactly 1 triple.
               -- Returns Nothing if the tree has 0 or more than 1 triple (e.g. when the expression is ambiguous).
               -- In the Nothing case, the ISgn special-case error messages cannot be used, and we fall through
               -- to the general error handler below.
               getRootSig opTree = case opSigns opTree of
                                   [(_, sgn, _)] -> Just sgn
                                   _             -> Nothing
               -- Check for ISgn types to provide better error messages
           in case (getRootSig sgnaTree, getRootSig sgnbTree) of
                -- Special case: when one or both sides have ISgn (identity) types
                (Just (ISgn cptA), Just (Sign srcB tgtB)) | cptA==botCpt ->
                      mkVerboseTypeError env o errorOpTree
                        ("Cannot assign a type to "<>showP a<>" because " <> tshow srcB <> " and " <> tshow tgtB <> " are not equal.")
                (Just (Sign srcA tgtA), Just (ISgn cptB)) | cptB==botCpt ->
                      mkVerboseTypeError env o errorOpTree
                        ("Cannot assign a type to "<>showP b<>" because " <> tshow srcA <> " and " <> tshow tgtA <> " are not equal.")
                _ -> case (conceptsSrc, conceptsTgt) of
                       ([],[])  -> mkVerboseTypeError env o errorOpTree ("Cannot match the source concepts nor the target concepts in the "<>kind<>")\n   on the left hand side: "<>showSgns (map snd3 sgnsa)<>"\n   on the right hand side: "<>showSgns (map snd3 sgnsb))
                       ([],_:_) -> mkVerboseTypeError env o errorOpTree ("Cannot match the source concepts on the left side of the "<>kind<>".\n   The source of "<>showP a<>" is "<>showSgns (map (source . snd3) sgnsa)<>"\n   The source of "<>showP b<>" is "<>showSgns (map (source . snd3) sgnsb))
                       (_:_,[]) -> mkVerboseTypeError env o errorOpTree ("Cannot match the target concepts of the right side of the "<>kind<>".\n   The target of "<>showP a<>" is "<>showSgns (map (target . snd3) sgnsa)<>"\n   The target of "<>showP b<>" is "<>showSgns (map (target . snd3) sgnsb))
                       _        -> mkVerboseTypeError env o errorOpTree ("Cannot match the signatures at either side of the "<>kind<>".\n   on the left hand side: "<>showSgns (map snd3 sgnsa)<>"\n   on the right hand side: "<>showSgns (map snd3 sgnsb))
           where
             showSgns :: Show a => [a] -> Text
             showSgns sgns = case sgns of
                              [] ->     "untyped"
                              [sgn] ->  tshow sgn
                              sgn:ss -> (T.intercalate ", " . map tshow) ss<>", or "<>tshow sgn

        -- | checkPeri generates a type error message for equations, inclusions, unions, intersects, and difference.
        -- The meetORjoin parameter determines how to combine concept types:
        -- - For union, use join (least upper bound) to find the common supertype
        -- - For other operations (intersection, equation, inclusion, difference), use meet (greatest lower bound)

        checkPeri :: Origin -> Text -> MeetOrJoin -> ((Expression, Expression) -> Expression) -> (Term TermPrim -> Term TermPrim -> Term TermPrim) -> Term TermPrim -> Term TermPrim -> Guarded (OpTree (Expression, Signature, Term TermPrim))
        checkPeri o kind moj combinator pCombinator a b =
          do
            sgnaTree <- signatures mConstraint a
            sgnbTree <- signatures mConstraint b
            let triplesa = opSigns sgnaTree
                triplesb = opSigns sgnbTree
                triples = makeTriples triplesa triplesb
            case triples of
              []  -> errorsPeri o kind joinOrMeet combinator pCombinator a b sgnaTree sgnbTree triplesa triplesb
              _ -> -- trace ("\n17. checkPeri yields:\n"<>showOpTree (STbinary sgnaTree sgnbTree triples)) $
                   return (STbinary sgnaTree sgnbTree triples)
            where
              joinOrMeet :: A_Concept -> A_Concept -> Maybe A_Concept
              joinOrMeet = case moj of Join -> join; Meet -> meet

              -- | makeTriples constructs all possible (Expression, Signature, Term) triples for binary peri operations (equations, unions, etc.).
              -- This filters based on whether joinOrMeetSig succeeds for the operand signatures.
              makeTriples :: [(Expression, Signature, Term TermPrim)]
                          -> [(Expression, Signature, Term TermPrim)]
                          -> [(Expression, Signature, Term TermPrim)]
              makeTriples triplesa triplesb =
                [ (refineANY resSign (combinator (expr_a, expr_b)), resSign, pCombinator trm_a trm_b)
                | (expr_a, sgn_a, trm_a) <- triplesa
                , (expr_b, sgn_b, trm_b) <- triplesb
                , Just resSign <- [joinOrMeetSig sgn_a sgn_b]
                ]

              joinOrMeetSig :: Signature -> Signature -> Maybe Signature
              joinOrMeetSig sgn_a sgn_b =
                case (sgn_a, moj, sgn_b) of
                  (ISgn c, Join, ISgn c')
                    | c == topCpt || c' == topCpt -> ISgn  <$>  c `meet` c'
                    | otherwise                   -> ISgn  <$>  c `join` c'
                  (ISgn c, Meet, ISgn c')         -> ISgn  <$>  c `meet` c'
                  (ISgn c, _, Sign s t)           -> deduplicate moj c s t
                  (Sign s t, _, ISgn c)           -> deduplicate moj c s t
                  (Sign s t, _ , Sign s' t') ->
                    case (topCpt == s, topCpt == t, moj, topCpt == s', topCpt == t') of
                      (True , True ,  _  , False, False) -> Just sgn_b
                      (False, False,  _  , True , True ) -> Just sgn_a
                      (False, False, Join, False, False) -> Sign  <$>  s `join` s'  <*>  t `join` t'
                      (False,   _  , Join, False,   _  ) -> Sign  <$>  s `join` s'  <*>  t `meet` t'
                      (  _  , False, Join,   _  , False) -> Sign  <$>  s `meet` s'  <*>  t `join` t'
                      _                                  -> Sign  <$>  s `meet` s'  <*>  t `meet` t'
                 where
                   deduplicate Join c s t
                     | topCpt == s && topCpt == t = Just (ISgn c)
                     | c == topCpt                = ISgn  <$>  s `join` t
                     | topCpt == s                = Sign c  <$>  c `join` t   -- This is equal to Sign  <*>  Just c  <$>  c `join` t
                     | topCpt == t                = Sign  <$>  c `join` s  <*>  Just c
                     | otherwise                  = Sign  <$>  c `join` s  <*>  c `join` t   -- Normaal geval, zonder topCpt
                   deduplicate Meet c s t         = do mSrc <- c `meet` s
                                                       mTgt <- c `meet` t
                                                       mBoth <- mSrc `meet` mTgt
                                                       return (ISgn mBoth)

        checkIntra, checkPRrs, checkPLrs
          :: {- o           -} Origin
          -> {- kind        -} Text   -- e.g., "composition", "left residual", ...
          -> {- combinator  -} ((Expression, Expression) -> Expression)
          -> {- pCombinator -} (Term TermPrim -> Term TermPrim -> Term TermPrim)
          -> {- a           -} Term TermPrim
          -> {- b           -} Term TermPrim
          -- extra parameters for tracing purpose:
          -> {- opStr       -} Text -- for tracing purpose
          -> Guarded (OpTree (Expression, Signature, Term TermPrim))
        checkIntra o kind combinator pCombinator a b _opStr = -- activate the last parameter opStr when using the trace statements: 
          do let lConstraint = case mConstraint of
                                  Just (Src, _) -> mConstraint
                                  _             -> Nothing
             let rConstraint = case mConstraint of
                                  Just (Tgt, _) -> mConstraint
                                  _             -> Nothing
             sgnaTree <- signatures lConstraint a; sgnbTree <- signatures rConstraint b
             let triplesa = opSigns sgnaTree; triplesb = opSigns sgnbTree
                 triples = makeTriples triplesa triplesb
                 sgnsa = fmap (\(_,s,_) -> s) triplesa; sgnsb = fmap (\(_,s,_) -> s) triplesb
             -- trace ("10. makeTriples on "<>tshow o<>" yields "<>tshow (length triples)<>" triples: "<>T.intercalate ", " (map showTriple triples)) $
             case triples of
               []  -> let errorExprs = [(combinator (expr_a, expr_b), Sign (source sgn_a) (target sgn_b), pCombinator trm_a trm_b) | (expr_a, sgn_a, trm_a)<-triplesa, (expr_b, sgn_b, trm_b)<-triplesb ]
                          opTree = STbinary sgnaTree sgnbTree errorExprs
                      in mkVerboseTypeError env o opTree ("Cannot match the signatures on the left and right of the " <> kind <> "." <> diagnosis kind a b sgnsa sgnsb)
               _ -> -- trace ("\n11. checkIntra yields: \n"<>showOpTree (STbinary sgnaTree sgnbTree triples)) $
                    return (STbinary sgnaTree sgnbTree triples)
          where
            -- | makeTriples constructs all possible (Expression, Signature, Term) triples for binary operations.
            makeTriples :: [(Expression, Signature, Term TermPrim)]
                        -> [(Expression, Signature, Term TermPrim)]
                        -> [(Expression, Signature, Term TermPrim)]
            makeTriples triplesa triplesb
             = -- trace ("\n12. makeTriples called with:\n  triplesa signatures: "<>T.intercalate ", " (map showTriple triplesa)<>"\n  triplesb signatures: "<>T.intercalate ", " (map showTriple triplesb)) $
               [ -- trace ("\n13. list comprehension with:\n  between: "<>tshow between) $
                 let result_expr = refineANY (Sign srca tgtb) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, Sign srca tgta, trm_a)<-triplesa, (expr_b, Sign srcb tgtb, trm_b)<-triplesb
               , Just _between<-[meet tgta srcb]
               ] <>
               [ let result_expr = refineANY (Sign srca between) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, Sign srca tgta, trm_a)<-triplesa, (expr_b, ISgn cptb, trm_b)<-triplesb
               , Just between<-[meet tgta cptb]
               ] <>
               [ let result_expr = refineANY (Sign between tgtb) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, ISgn cpta, trm_a)<-triplesa, (expr_b, Sign srcb tgtb, trm_b)<-triplesb
               , Just between<-[meet cpta srcb]
               ] <>
               [ let result_expr = refineANY (ISgn between) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, ISgn cpta, trm_a)<-triplesa, (expr_b, ISgn cptb, trm_b)<-triplesb
               , Just between<-[meet cpta cptb]
               ]

        checkPRrs o kind combinator pCombinator a b _opStr = -- activate the last parameter opStr when using the trace statements: 
          do let lConstraint = case mConstraint of
                                  Just (Src, c) -> Just (Tgt, c)
                                  _             -> Nothing
             let rConstraint = case mConstraint of
                                  Just (Tgt, _) -> mConstraint
                                  _             -> Nothing
             sgnaTree <- signatures lConstraint a; sgnbTree <- signatures rConstraint b
             let triplesa = opSigns sgnaTree; triplesb = opSigns sgnbTree
                 triples = makeTriples triplesa triplesb
                 sgnsa = fmap (\(_,s,_) -> s) triplesa; sgnsb = fmap (\(_,s,_) -> s) triplesb
             -- trace ("10. makeTriples on "<>tshow o<>" yields "<>tshow (length triples)<>" triples: "<>T.intercalate ", " (map showTriple triples)) $
             case triples of
               []  -> let errorExprs = [(combinator (expr_a, expr_b), Sign (source sgn_a) (target sgn_b), pCombinator trm_a trm_b) | (expr_a, sgn_a, trm_a)<-triplesa, (expr_b, sgn_b, trm_b)<-triplesb ]
                          opTree = STbinary sgnaTree sgnbTree errorExprs
                      in mkVerboseTypeError env o opTree ("Cannot match the signatures on the left and right of the " <> kind <> "." <> diagnosis kind a b sgnsa sgnsb)
               _ -> -- trace ("\n11. checkIntra yields: \n"<>showOpTree (STbinary sgnaTree sgnbTree triples)) $
                    return (STbinary sgnaTree sgnbTree triples)
          where
            -- | makeTriples constructs all possible (Expression, Signature, Term) triples for binary operations.
            makeTriples :: [(Expression, Signature, Term TermPrim)]
                        -> [(Expression, Signature, Term TermPrim)]
                        -> [(Expression, Signature, Term TermPrim)]
            makeTriples triplesa triplesb
             = -- trace ("\n12. makeTriples called with:\n  triplesa signatures: "<>T.intercalate ", " (map showTriple triplesa)<>"\n  triplesb signatures: "<>T.intercalate ", " (map showTriple triplesb)) $
               [ -- trace ("\n13. list comprehension with:\n  between: "<>tshow between) $
                 let result_expr = refineANY (Sign tgta tgtb) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, Sign srca tgta, trm_a)<-triplesa, (expr_b, Sign srcb tgtb, trm_b)<-triplesb
               , Just _between<-[meet srca srcb]
               ] <>
               [ let result_expr = refineANY (Sign tgta between) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, Sign srca tgta, trm_a)<-triplesa, (expr_b, ISgn cptb, trm_b)<-triplesb
               , Just between<-[meet srca cptb]
               ] <>
               [ let result_expr = refineANY (Sign between tgtb) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, ISgn cpta, trm_a)<-triplesa, (expr_b, Sign srcb tgtb, trm_b)<-triplesb
               , Just between<-[meet cpta srcb]
               ] <>
               [ let result_expr = refineANY (ISgn between) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, ISgn cpta, trm_a)<-triplesa, (expr_b, ISgn cptb, trm_b)<-triplesb
               , Just between<-[meet cpta cptb]
               ]

        checkPLrs o kind combinator pCombinator a b _opStr = -- activate the last parameter opStr when using the trace statements: 
          do let lConstraint = case mConstraint of
                                  Just (Src, _) -> mConstraint
                                  _             -> Nothing
             let rConstraint = case mConstraint of
                                  Just (Tgt, c) -> Just (Src, c)
                                  _             -> Nothing
             sgnaTree <- signatures lConstraint a; sgnbTree <- signatures rConstraint b
             let triplesa = opSigns sgnaTree; triplesb = opSigns sgnbTree
                 triples = makeTriples triplesa triplesb
                 sgnsa = fmap (\(_,s,_) -> s) triplesa; sgnsb = fmap (\(_,s,_) -> s) triplesb
             -- trace ("10. makeTriples on "<>tshow o<>" yields "<>tshow (length triples)<>" triples: "<>T.intercalate ", " (map showTriple triples)) $
             case triples of
               []  -> let errorExprs = [(combinator (expr_a, expr_b), Sign (source sgn_a) (source sgn_b), pCombinator trm_a trm_b) | (expr_a, sgn_a, trm_a)<-triplesa, (expr_b, sgn_b, trm_b)<-triplesb ]
                          opTree = STbinary sgnaTree sgnbTree errorExprs
                      in mkVerboseTypeError env o opTree ("Cannot match the signatures on the left and right of the " <> kind <> "." <> diagnosis kind a b sgnsa sgnsb)
               _ -> -- trace ("\n11. checkIntra yields: \n"<>showOpTree (STbinary sgnaTree sgnbTree triples)) $
                    return (STbinary sgnaTree sgnbTree triples)
          where
            -- | makeTriples constructs all possible (Expression, Signature, Term) triples for binary operations.
            makeTriples :: [(Expression, Signature, Term TermPrim)]
                        -> [(Expression, Signature, Term TermPrim)]
                        -> [(Expression, Signature, Term TermPrim)]
            makeTriples triplesa triplesb
             = -- trace ("\n12. makeTriples called with:\n  triplesa signatures: "<>T.intercalate ", " (map showTriple triplesa)<>"\n  triplesb signatures: "<>T.intercalate ", " (map showTriple triplesb)) $
               [ -- trace ("\n13. list comprehension with:\n  between: "<>tshow between) $
                 let result_expr = refineANY (Sign srca srcb) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, Sign srca tgta, trm_a)<-triplesa, (expr_b, Sign srcb tgtb, trm_b)<-triplesb
               , Just _between<-[meet tgta tgtb]
               ] <>
               [ let result_expr = refineANY (Sign srca between) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, Sign srca tgta, trm_a)<-triplesa, (expr_b, ISgn cptb, trm_b)<-triplesb
               , Just between<-[meet tgta cptb]
               ] <>
               [ let result_expr = refineANY (Sign between srcb) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, ISgn cpta, trm_a)<-triplesa, (expr_b, Sign srcb tgtb, trm_b)<-triplesb
               , Just between<-[meet cpta tgtb]
               ] <>
               [ let result_expr = refineANY (ISgn between) (combinator (expr_a, expr_b))
                 in (result_expr, sign result_expr, pCombinator trm_a trm_b)
               | (expr_a, ISgn cpta, trm_a)<-triplesa, (expr_b, ISgn cptb, trm_b)<-triplesb
               , Just between<-[meet cpta cptb]
               ]

        diagnosis kind a b sgnsa sgnsb
         = case (kind, sgnsa==sgnsb) of
            ("composition"      , eq) -> "\n  The target of "<>displayLeft (map target sgnsa) a<>"should "<>(if eq then "match" else "be equal to (or share a concept with)")<>" the source of "<>displayRight (map source sgnsb) b<>"."
            ("relative addition", eq) -> "\n  The target of "<>displayLeft (map target sgnsa) a<>"should match "<>(if eq then "" else "with")<>" the source of "<>displayRight (map source sgnsb) b<>"."
            ("left residual"    , eq) -> "\n  The target of "<>displayLeft (map target sgnsa) a<>"should "<>(if eq then "match" else "be equal to or more generic than" )<>" the target of "<>displayRight (map target sgnsb) (flp b)<>"."
            ("right residual"   , eq) -> "\n  The source of "<>displayLeft (map source sgnsa) (flp a)<>"should "<>(if eq then "match" else "be equal to or more specific than")<>" the source of "<>displayRight (map source sgnsb) b<>"."
            ("diamond"          , eq) -> "\n  The target of "<>displayLeft (map target sgnsa) a<>"should "<>(if eq then "match" else "be equal to (or share a concept with)")<>" the source of "<>displayRight (map source sgnsb) b<>"."
            _ -> fatal ("Unknown kind of operation in diagnosis: "<>kind)
          where
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

        -- | Check for ambiguous subexpressions in the OpTree
        -- An ambiguity exists when a node has multiple triples with different signatures
        -- If all (of more than one) triples have the same signature, there's an underlying ambiguity in a subexpression
        constrain :: OpTree (Expression, Signature, Term TermPrim) -> Guarded (OpTree (Expression, Signature, Term TermPrim))
        constrain opTree =
          case mConstraint of
            Nothing -> pure opTree
            Just (sot, limit) -> -- ensure that the signatures in opTree are equal or wider than the constraint
              case [ case expr of
                       -- For narrowable expressions (I, V, atoms, bind), compute the meet
                       EDcI _      -> (EDcI (source sgn'), sgn', t)
                       EDcV _      -> (EDcV sgn', sgn', t)
                       EMp1 av _   -> (EMp1 av (source sgn'), sgn', t)
                       EBin oper _ -> (EBin oper sgn', sgn', t)
                       _           -> (expr, sgn, t)
                   | (expr, sgn, t) <- opSigns opTree
                   -- Apply contravariance principle (see docs/CompilationProcess.md)
                   -- For narrowable expressions (I, V, atoms, bind): use wider signatures (contravariance)
                   -- For declared relations: they must match exactly or be wider than the constraint
                   , (case sot of Src -> source; Tgt -> target) expr `geq` limit  == Just True
                   , let sgn' = case (sot, sgn) of
                                 (Src, ISgn _)     | source expr==topCpt -> ISgn limit
                                 (Src, Sign _ tgt) | source expr==topCpt -> Sign limit tgt
                                 (Tgt, ISgn _)     | target expr==topCpt -> ISgn limit
                                 (Tgt, Sign src _) | target expr==topCpt -> Sign src limit
                                 _                                       -> sgn
                   ] of
                [] -> case errs of
                        (err:errors) -> Errors (err NE.:| errors)
                        [] -> fatal "No errors found"
                      where
                        errs = [mkConstraintError (origin t) sot t expr limit | (expr, _sgn, t) <- opSigns opTree]
                filteredTriples -> pure (assignOpSigns filteredTriples opTree)
          where
            mkConstraintError o sot t expr limit =
              CTXE o $
                "The term " <> showP t <> " has " <> (case sot of Src -> "source"; Tgt -> "target") <> " " <> tshow cpt <> ", which is "<>diagnosticString<>" " <> tshow limit <> ".\n" <>
                "  Use a term with a " <> (case sot of Src -> "source"; Tgt -> "target") <> " equal to or wider than " <> tshow limit <> "."
              where
                cpt = (case sot of Src -> source; Tgt -> target) expr
                diagnosticString = if limit `geq` (case sot of Src -> source; Tgt -> target) expr == Just True then "narrower than" else "incompatible with"

instance HasSignature (OpTree (Expression, Signature, Term TermPrim)) where
  sign sgnTree = case sgnTree of
                 STnullary    [(_, sgn, _)] -> sgn
                 STunary _    [(_, sgn, _)] -> sgn
                 STbinary _ _ [(_, sgn, _)] -> sgn
                 _                          -> fatal ("HasSignature.sign: pattern match failure.\n"<>showOpTree sgnTree<>"\nThis is a bug in the compiler.")

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
pDecl2aDecl typ pCpt2aCpt maybePatLabel defLanguage defFormat pd =
  do
    decSign <- pSign2aSign (pCpt2aCpt (origin pd)) (dec_sign pd)
    -- Note: We don't validate concepts here because RELATION statements implicitly declare their signature concepts
    checkEndoProps decSign
    -- propLists <- mapM pProp2aProps . Set.toList $ dec_prps pd
    dflts <- mapM (pReldefault2aReldefaults decSign) . L.nub $ dec_defaults pd
    return ( -- trace ("19. pd =  " <> tshow pd) $
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
        })
  where
    pReldefault2aReldefaults :: (HasSignature a) => a -> PRelationDefault -> Guarded ARelDefault
    pReldefault2aReldefaults decSign x = case x of
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

    checkEndoProps :: (HasSignature a) => a -> Guarded ()
    checkEndoProps decSign
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
  Guarded AConceptDef
pConcDef2aConcDef pCpt2aCpt defLanguage defFormat pCd =
  do
    cpt <- pCpt2aCpt (origin pCd) (PCpt {p_cptnm = name pCd})
    return $ AConceptDef
      { pos = origin pCd,
        acdcpt = cpt,
        acdname = name pCd,
        acdlabel = cdlbl pCd,
        acddef2 = pCDDef2Mean defLanguage defFormat $ cddef2 pCd,
        acdmean = map (pMean2aMean defLanguage defFormat) (cdmean pCd),
        acdfrom = cdfrom pCd
      }

pRepr2aRepr :: ConceptMap -> P_Representation -> Guarded A_Representation
pRepr2aRepr pCpt2aCpt (Repr orig cpts ttype) = do
  aCpts <- traverse (pCpt2aCpt orig) cpts
  pure Arepr
    { aReprFrom = aCpts,
      aReprTo = ttype
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
