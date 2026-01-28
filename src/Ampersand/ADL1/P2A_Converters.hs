{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# HLINT ignore "Use catMaybes" #-}
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
import Ampersand.Core.A2P_Converters (aConcept2pConcept)
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
          ] <> [ (nameOfONE, ONE) ] -- Add SESSION concept manually

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
      interfaces <- traverse (pIfc2aIfc contextInfoPre) p_interfaces
      explicitReprs <- traverse (\pRepr -> do
                          aCpts <- traverse (conceptMap contextInfoPre (origin pRepr)) (reprcpts pRepr)
                          pure Arepr {aReprFrom = aCpts, aReprTo = reprdom pRepr})
                        (p_representations <> concatMap pt_Reprs p_patterns)
      let objectReprs = getObjReprs interfaces viewdefs identdefs
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
      checkOtherAtomsInSessionConcept actx
      checkPurposes actx -- Check whether all purposes refer to existing objects
      checkDanglingRulesInRuleRoles actx -- Check whether all rules in MAINTAIN statements are declared
      checkInterfaceCycles actx -- Check that interface references are not cyclic
      checkMultipleDefaultViews actx -- Check whether each concept has at most one default view
      warnCaseProblems actx -- Warn if there are problems with the casing of names of relations and/or concepts
      warnUnusedConcepts actx -- Warn if there are concepts defined that are not used in relations
      return actx
    where

      -- | Extract object representations from interfaces, views, and identities
      -- All these concepts must be represented as Object type
      getObjReprs :: [Interface] -> [ViewDef] -> [IdentityRule] -> [A_Representation]
      getObjReprs interfaces viewdefs identdefs =
        case Set.toList (getInterfaceConcepts interfaces `Set.union` getViewConcepts viewdefs `Set.union` getIdentityConcepts identdefs) of
             [] -> []
             cpt:cpts ->  [ Arepr (cpt :| cpts) Object ]
        where
          -- | Extract all concepts from interfaces (source + all box targets recursively)
          getInterfaceConcepts :: [Interface] -> Set.Set A_Concept
          getInterfaceConcepts ifcs = Set.fromList [ source (objExpression (ifcObj ifc)) | ifc <- ifcs ] `Set.union`
                                      Set.unions [ getObjectConcepts (ifcObj ifc) | ifc <- ifcs ]

          -- | Recursively extract all target concepts from box expressions
          getObjectConcepts :: ObjectDef -> Set.Set A_Concept
          getObjectConcepts obj =
            Set.singleton (target (objExpression obj)) `Set.union` maybe Set.empty getSubInterfaceConcepts (objmsub obj)

          -- | Extract concepts from sub-interfaces
          getSubInterfaceConcepts :: SubInterface -> Set.Set A_Concept
          getSubInterfaceConcepts (Box {siObjs = boxItems}) = Set.unions (map getBoxItemConcepts boxItems)
          getSubInterfaceConcepts (InterfaceRef {siConcept = cpt}) = Set.singleton cpt

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
        if cpt == ONE || show cpt == "SESSION"
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
        let pCpts = pConcs (p_conceptdefs <> concatMap pt_cds p_patterns) `Set.union`
                    pConcs (p_relations <> concatMap pt_dcs p_patterns)   `Set.union`
                    pConcs (p_gens <> concatMap pt_gns p_patterns)        `Set.union`
                    pConcs (p_rules <> concatMap pt_rls p_patterns)       `Set.union`
                    Set.fromList ([P_ONE] <> [PCpt nameOfSESSION | not (null p_interfaces)])
        typols <- (makeTypologies . makeAliasGraph . makePGraph (p_gens <> concatMap pt_gns p_patterns)) pCpts
        let pCpt2aCpt = makePCpt2ACpt typols
        reprs <- traverse (pRepr2aRepr pCpt2aCpt) (p_representations <> concatMap pt_Reprs p_patterns)
        let reprOf :: A_Concept -> TType
            reprOf cpt =
              if cpt == ONE || show cpt == "SESSION"
                then Alphanumeric
                else case [aReprTo r | r <- L.nub reprs, cpt `elem` aReprFrom r] of
                       [t] -> t
                       []  -> Alphanumeric
                       ts  -> fatal $ "Multiple representations found for concept " <> showWithAliases cpt <> ": " <> tshow ts <> ". This should not happen as all concepts should have only one representation assigned."
        decls <- traverse (pDecl2aDecl reprOf pCpt2aCpt Nothing deflangCtxt deffrmtCtxt) (p_relations <> concatMap pt_dcs p_patterns)
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
        [] -> Isa
               <$> pure (origin pg)
               <*> pCpt2aCpt (NE.head (generics pg))
               <*> pCpt2aCpt (specific pg)
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
                   segments <- traverse typeCheckViewSegment (zip [0 ..] segmnts)
                   uniqueLabels orig toLabel (filter hasLabel segments) "VIEW statement"
                   viewConcept <- conceptMap ci orig cpt
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
          typeCheckViewSegment :: (Integer, P_ViewSegment TermPrim) -> Guarded ViewSegment
          typeCheckViewSegment (seqNr, seg) =
           do payload <- typecheckPayload (origin seg) (vsm_load seg)
              return
                ViewSegment
                  { vsmpos = origin seg,
                    vsmlabel = vsm_labl seg,
                    vsmSeqNr = seqNr,
                    vsmLoad = payload
                  }
          typecheckPayload :: Origin -> P_ViewSegmtPayLoad TermPrim -> Guarded ViewSegmentPayLoad
          typecheckPayload origPL payload =
                case payload of
                  P_ViewExp term ->
                    do
                      -- Constrain the term to have source=viewConcept and target=BOT (serves as a wildcard here)
                      viewConcept <- conceptMap ci origPL cpt
                      xpr <- term2Expr env ci (Just (Sign viewConcept topCpt)) term
                      case geq viewConcept (source xpr) of
                        Just True  -> pure (ViewExp xpr)
                        Just False -> Errors . pure $ mkRelationTooNarrowForViewError origPL xpr viewConcept
                        Nothing    -> Errors . pure $ mkViewExpressionMismatchError orig xpr viewConcept
                  P_ViewText str -> pure $ ViewText str

      -- | pSubIfc2aSubIfc uses the box concept from the parent box to constrain type-checking of sub-interfaces.
      pSubIfc2aSubIfc :: ContextInfo -> P_Concept -> P_SubIfc TermPrim -> Guarded SubInterface
      pSubIfc2aSubIfc ci boxConcept sub =
        case sub of
          P_Box{} -> do let pCpt2aCpt = conceptMap ci (origin sub)
                        boxAConcept <- pCpt2aCpt boxConcept
                        subBoxes <- mapM (pBoxItem2aBoxItem ci (Just boxConcept)) (si_box sub)
                        uniqueLabels (origin sub) toLabel (filter hasLabel subBoxes) "BOX"
                        return (Box{ pos = origin sub, siConcept = boxAConcept, siHeader = si_header sub, siObjs = subBoxes})
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

          -- | getInterface retrieves the interface concept and checks compatibility with the box concept.
          getInterface :: Guarded A_Concept
          getInterface
           = case filter ((==si_str sub).name) p_interfaces of
               [] -> (Errors . return . CTXE (origin sub)) ("Interface " <> (tshow.si_str) sub <> " not found")
               [pIfc] -> do
                 let pCpt2aCpt = conceptMap ci (origin pIfc)
                 -- First check if the referenced interface has undeclared concepts - if so, skip type-checking
                 validatePConceptsInSchema ci (origin pIfc) pIfc ("INTERFACE " <> fullName pIfc)
                 -- Convert boxConcept to A_Concept for use in constraint
                 boxAConcept <- pCpt2aCpt boxConcept
                 -- Only proceed if validation succeeded
                 expr <- (term2Expr env ci (Just (Sign boxAConcept topCpt)) . obj_term . ifc_Obj) pIfc
                 let srcCpt = source expr
                 if isConcreteSignature (ISgn boxAConcept)
                   then case geq srcCpt boxAConcept of
                     Just True  -> return srcCpt
                     Just False -> (Errors . return . CTXE (origin sub)) ("Interface " <> (tshow.name) pIfc <> " from " <> (tshow.origin) pIfc <> " is incompatible.\n   It requires CLASSIFY " <> tshow srcCpt <> " ISA " <> tshow boxAConcept)
                     Nothing    -> (Errors . return . CTXE (origin sub)) ("Interface " <> (tshow.name) pIfc <> " from " <> (tshow.origin) pIfc <> " is incompatible.\n   Its concept is " <> tshow srcCpt <> " but I expected " <> tshow boxAConcept <> ".")
                   else fatal ("Unexpected: boxAConcept == "<>tshow boxAConcept<>" in interface "<>tshow (name pIfc) )
               _ -> (Errors . return . CTXE (origin sub)) ("Multiple interfaces with name " <> (tshow.si_str) sub <> " found")

      -- | mBoxConcept is the P_Concept from the parent box, used to constrain type-checking of relations.
      --   It is "Maybe" because on the top level, in pIfc2aIfc, there is no parent box concept. 
      pBoxItem2aBoxItem :: ContextInfo -> Maybe P_Concept -> P_BoxItem TermPrim -> Guarded BoxItem
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
              -- The mConstraint causes a type error if subinterfaces don't match with their parent's target concept.
              -- This ensures that checkCrud will give the correct error messages because it works on objExpr.
              -- The result must be more generic than or equal to the mConstraint.
              mConstraint <- case mBoxConcept of
                    Just boxConcept -> do src <- conceptMap ci (origin pBoxItem) boxConcept
                                          pure (Just (Sign src topCpt))
                    Nothing -> pure Nothing
              objExpr <- term2Expr env ci mConstraint term
              a_msub <- traverse (pSubIfc2aSubIfc ci (aConcept2pConcept (target objExpr))) p_msub
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
                      case viewConcept `geq` target objExpr of
                            Just True  -> pure ()
                            Just False -> Errors . pure $ mkViewTooSpecificError (origin pBoxItem) vd objExpr
                            Nothing    -> Errors . pure $ mkViewExpressionMismatchError (origin pBoxItem) objExpr viewConcept
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
            let objExpr = objExpression boxItem
                ifcSource = source objExpr
                ifcSourceType = reprType ci ifcSource
            if ifcSourceType==Object || ifcSource == topCpt
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
                         "However, the TYPE of the concept `" <> showWithAliases ifcSource <> "` for interface `" <> fullName pIfc <> "` is " <> tshow ifcSourceType <> "."
                       ]
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
            vls <- maybeOverGuarded (typeCheckPairView ci orig exp') viols
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
               pCpt2aCpt = conceptMap ci orig
           cpt <- pCpt2aCpt (ix_cpt pidt)
           validatePConceptsInSchema ci orig pidt ("IDENT " <> fullName (ix_name pidt))
           let mConstraint = Just (Sign cpt topCpt)
           isegs <- traverse (term2Expr env ci mConstraint) (ix_ats pidt)
           return ( Id
                    { idPos = orig,
                      idName = ix_name pidt,
                      idlabel = ix_label pidt,
                      idCpt = cpt,
                      idPat = mPat,
                      identityAts = isegs
                    }
                 )

      typeCheckPairView :: ContextInfo -> Origin -> Expression -> PairView (Term TermPrim) -> Guarded (PairView Expression)
      typeCheckPairView ci o x (PairView lst) =
        PairView <$> traverse (typeCheckPairViewSeg ci o x) lst
      typeCheckPairViewSeg :: ContextInfo -> Origin -> Expression -> PairViewSegment (Term TermPrim) -> Guarded (PairViewSegment Expression)
      typeCheckPairViewSeg _ _ _ (PairViewText segOrigin x) = pure (PairViewText segOrigin x)
      typeCheckPairViewSeg ci o expr (PairViewExp segOrigin s x) =
        do
          let pCpt2aCpt = conceptMap ci o
          srcConcept <- pCpt2aCpt (aConcept2pConcept (source expr))
          tgtConcept <- pCpt2aCpt (aConcept2pConcept (target expr))
          let src = (aConcept2pConcept . source) expr; srcConstraint = Just (Sign srcConcept topCpt)
              tgt = (aConcept2pConcept . target) expr; tgtConstraint = Just (Sign tgtConcept topCpt)
          e <- case s of
                 Src -> term2Expr env ci srcConstraint (PCps o (Prim (Pid o src)) x)
                 Tgt -> term2Expr env ci tgtConstraint (PCps o (Prim (Pid o tgt)) x)
          return (PairViewExp segOrigin s e)
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

-- Uncomment showTriple and showGuardedOpTree for certain trace statements. (Search for showTriple or showGuardedOpTree to find out which ones.)
showTriple :: (Expression, Signature, Term TermPrim) -> Text
showTriple (expr, sgn, trm) = "("<>showA expr<>", "<>tshow sgn<>", "<>showP trm<>")"

showGuardedOpTree :: Guarded (OpTree (Expression, Signature, Term TermPrim)) -> Text
showGuardedOpTree (Errors errs) = 
  "Errors:\n" <> T.intercalate "\n" (map tshow (NE.toList errs))
showGuardedOpTree (Checked opTree []) = showOpTree opTree
showGuardedOpTree (Checked opTree warnings) = 
  "Checked (with " <> tshow (length warnings) <> " warning(s)):\n" <> 
  showOpTree opTree <> "\n\nWarnings:\n" <> 
  T.intercalate "\n" (map tshow warnings)

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
showOpTree opTree = T.intercalate "\n" (map T.concat (L.transpose [sigTexts, paddings, treeLines, exprTexts]))
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

instance (Flippable a, Flippable b, Flippable c) => Flippable (a,b,c) where
  flp (e,s,trm) = (flp e, flp s, flp trm)

-- | signatures constructs a tree with all possible (expression, signature) pairs in each node of the tree.
--   It is used by the function term2Expr to weed out these pairs down to one for every (sub-)term,
--   to establish a unique signature for the term and all of its subterms.
--   Post:
--    - Every set of (expression, signature) pairs in the OpTree is not empty.
--    - Every signature is consistent with the type rules of the term it corresponds to.
-- Helper function for creating verbose type error messages
mkVerboseTypeError :: (HasRunner env) => env -> Origin -> OpTree (Expression, Signature, Term TermPrim) -> Text -> Guarded a
mkVerboseTypeError env errorOrigin opTree baseMsg =
  Errors . return $ CTXE errorOrigin $
    if logLevel (view runnerL env) <= LevelInfo
    then baseMsg <> "\n\nType analysis:\n" <> showOpTree opTree
    else baseMsg

-- Helper function for creating type mismatch errors with optional suggestions
mkVerboseTypeMismatchError :: (HasRunner env) => env -> Origin -> Text -> Maybe Text -> OpTree (Expression, Signature, Term TermPrim) -> Guarded a
mkVerboseTypeMismatchError env errorOrigin baseMsg maybeSuggestions opTree =
  mkVerboseTypeError env errorOrigin opTree (baseMsg <> fromMaybe "" maybeSuggestions)

-- obsolete:
-- | BoxConstraint tracks whether we should match source or target when filtering relations in a box context.
-- This becomes important when dealing with flipped relations, where we need to match the target instead of source.
-- data BoxConstraint = MatchSource P_Concept 
--                    | MatchTarget P_Concept

-- instance Flippable BoxConstraint where
--   flp (MatchSource c) = MatchTarget c
--   flp (MatchTarget c) = MatchSource c

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
-- This handles cases where the subexpressions are concrete declarations
-- whose signatures weren't changed by refineANY.
getBetweenConcept :: Expression -> A_Concept
getBetweenConcept (ECps (e1, e2)) = fromMaybe botCpt (meet (target e1) (source e2))
getBetweenConcept (EDia (e1, e2)) = fromMaybe botCpt (join (target e1) (source e2))
getBetweenConcept (ERad (e1, e2)) = fromMaybe botCpt (join (target e1) (source e2))
getBetweenConcept (ELrs (e1, e2)) = fromMaybe botCpt (meet (target e1) (target e2))
getBetweenConcept (ERrs (e1, e2)) = fromMaybe botCpt (meet (source e1) (source e2))
getBetweenConcept (EPrd (_ , _ )) = topCpt  -- Product doesn't have a between concept constraint
getBetweenConcept e = fatal ("getBetweenConcept: pattern match failure on expression "<>showA e<>"\nThis is a bug in the compiler.")

-- | Refine an expression by replacing TOP with more specific concepts, capped with a target signature. Only narrow, never widen.
-- Pre: targetSig contains no TOP or BOT concepts (i.e. it is concrete).
-- Calling refineANY targetSig once gives the same result as calling it more often; it is an idempotent function.
-- This function is used both during type inference and as a final cleanup step.
-- It only narrows types (makes them more specific), never widens them.
refineANY :: Signature -> Expression -> Expression
refineANY targetSig expr
 = trace ("refineANY (" <> tshow targetSig <> ") (" <> showA expr <> ") yields " <> showA refinedExpr)
   refinedExpr
  where
    iCpt = case targetSig of
             ISgn cpt     -> cpt
             Sign src tgt -> case meet src tgt of
                               Just m  -> m
                               Nothing -> fatal ("refineANY: cannot compute meet of source expr " <> tshow (source expr) <> " and targetSig source " <> tshow src <> " for expr " <> showA expr)
    refinedExpr = case expr of
      -- Nullary operations:
      -- geq returns Just True if first arg is more generic than the second arg
      EDcI cpt        -> if cpt==botCpt then EDcI iCpt else expr
      -- For EMp1: only narrow, never widen. If expr's concept is already narrower than target, keep it.
      EMp1 av cpt     -> if cpt==botCpt then EMp1 av iCpt else expr
      EDcD _          -> expr -- Declarations are concrete already
      EBin oper sgn   -> case (source sgn==botCpt, target sgn==botCpt) of
                           (True , True ) -> EBin oper targetSig
                           (True , False) -> EBin oper (source targetSig `Sign` target sgn)
                           (False, True ) -> EBin oper (source sgn `Sign` target targetSig)
                           (False, False) -> EBin oper sgn
      EDcV sgn        -> case (source sgn==botCpt, target sgn==botCpt) of
                           (True , True ) -> EDcV targetSig
                           (True , False) -> EDcV (source targetSig `Sign` target sgn)
                           (False, True ) -> EDcV (source sgn `Sign` target targetSig)
                           (False, False) -> EDcV sgn
      -- Binary operations:
      EEqu (e1, e2)   -> EEqu (refineANY targetSig e1, refineANY targetSig e2)
      EInc (e1, e2)   -> EInc (refineANY targetSig e1, refineANY targetSig e2)
      EPrd (e1, e2)   -> EPrd (refineANY targetSig e1, refineANY targetSig e2)
      EUni (e1, e2)   -> EUni (refineANY targetSig e1, refineANY targetSig e2)
      EIsc (e1, e2)   -> EIsc (refineANY targetSig e1, refineANY targetSig e2)
      EDif (e1, e2)   -> EDif (refineANY targetSig e1, refineANY targetSig e2)
      ECps (e1, e2)   -> ECps (refineANY (Sign (source targetSig) between) e1, refineANY (Sign between (target targetSig)) e2)
                            where between = getBetweenConcept expr
      ELrs (e1, e2)   -> ELrs (refineANY (Sign (source targetSig) between) e1, refineANY (Sign (target targetSig) between) e2)
                            where between = getBetweenConcept expr
      ERrs (e1, e2)   -> ERrs (refineANY (Sign between (source targetSig)) e1, refineANY (Sign between (target targetSig)) e2)
                            where between = getBetweenConcept expr
      EDia (e1, e2)   -> EDia (refineANY (Sign (source targetSig) between) e1, refineANY (Sign between (target targetSig)) e2)
                            where between = getBetweenConcept expr
      ERad (e1, e2)   -> ERad (refineANY (Sign (source targetSig) between) e1, refineANY (Sign between (target targetSig)) e2)
                            where between = getBetweenConcept expr
      -- Unary operations:
      EFlp e          -> EFlp (refineANY (flp targetSig) e)
      ECpl e          -> ECpl (refineANY targetSig e)
      EBrk e          -> EBrk (refineANY targetSig e)
      EKl0 e          -> EKl0 (refineANY targetSig e)
      EKl1 e          -> EKl1 (refineANY targetSig e)

-- | Compute all possible type signatures for a term within a constraint.
--   
-- | The signatures function yields an OpTree with the exact recursive structure of the term.
--   In every node there is a nonempty list of triples (Expression, Signature, Term)
--   that details the (sub-)expression of that node.
--   If signatures cannot construct at least one valid triple in each node, it returns a type error.
--   Nullary terms correspond to Prim terms and they end the recursion.
--
--   When type-checking expressions in boxes (interfaces/APIs) or views, a constraint signature provides context
--   that can help disambiguate relation names. The constraint filters relation candidates to keep only those
--   whose signatures are wider or equal to the constraint.

signatures :: (HasFSpecGenOpts env, HasRunner env) => env -> ContextInfo -> Maybe Signature -> Term TermPrim -> Guarded (OpTree (Expression, Signature, Term TermPrim))
signatures env ci mConstraintSig trm = do
  opTree <- trace ("4. signatures ("<>tshow mConstraintSig<>") ("<>showP trm<>") in "<>tshow (origin trm)<>" yields:")
            resultTerm
  let refinedOpTree = trace ("   resultTerm:  "<>showOpTree opTree) $
                      fmap (\(expr, sgn, t) -> (refineANY sgn expr, sgn, t)) opTree
  trace ("   refinedOpTree: "<>showOpTree refinedOpTree) $
   constrain refinedOpTree
    where
      constrain :: OpTree (Expression, Signature, Term TermPrim) -> Guarded (OpTree (Expression, Signature, Term TermPrim))
      constrain opTree =
        case mConstraintSig of
          Nothing -> pure opTree
          Just constraintSig ->
            case [ (expr, sgn, trm) | (expr, sgn, trm) <- opSigns opTree, withinSig sgn == Just True ] of
              [] -> mkVerboseTypeMismatchError env (origin trm)
                     ("No matching signatures for term " <> showP trm <> " within the constraint signature " <> tshow constraintSig <> ".")
                     (Just ("Expected a signature that is wider or equal to the constraint signature " <> tshow constraintSig <> "."))
                     opTree
              filteredTriples  -> pure (assignOpSigns filteredTriples opTree)
        where
          -- withinSig checks if a signature, sgn, is narrower (i.e. more specific) or equal to mConstraintSig, for the purpose checking box items.
          -- Since every concept is narrower or equal to topCpt, the constraint is always satisfied if it is topCpt.
          withinSig :: Signature -> Maybe Bool
          withinSig sgn = f mConstraintSig
            where
              f :: Maybe Signature -> Maybe Bool
              f _mConstraintSig@Nothing                                = Just True
              f (Just (Sign src tgt)) | src == topCpt && tgt == topCpt = Just True
              f (Just (Sign src tgt)) | src == topCpt                  = geq tgt (target sgn)
              f (Just (Sign src tgt)) | tgt == topCpt                  = geq src (source sgn)
              f (Just (Sign src tgt))                                  = (&&) <$> geq src (source sgn) <*> geq tgt (target sgn)
              f (Just (ISgn cpt)    ) | cpt == topCpt                  = Just True
              f (Just (ISgn cpt)    )                                  = geq cpt (source sgn)


      -- | resultPrim yields all possible (expression, signature, term) triples for a Prim term.
                                    -- Filter relations to keep only those with signatures wider or equal to constraint
      resultPrim :: TermPrim -> Guarded (OpTree (Expression, Signature, Term TermPrim))
      resultPrim trmPrim
       = case trmPrim of
           PI _               -> pure (STnullary [(EDcI topCpt, ISgn topCpt, Prim trmPrim)])
           Pid o c            -> do c' <- pCpt2aCpt o c
                                    pure (STnullary [(EDcI c', ISgn c', Prim trmPrim)])
           Patm _ av Nothing  -> pure (STnullary [(EMp1 av topCpt, ISgn topCpt, Prim trmPrim)])
           Patm o av (Just c) -> do c' <- pCpt2aCpt o c
                                    pure (STnullary [(EMp1 av c', ISgn c', Prim trmPrim)])
           PVee _             -> let sgn = Sign topCpt topCpt                   in pure (STnullary [(EDcV sgn, sgn, Prim trmPrim)])
           Pfull o src tgt    -> do src' <- pCpt2aCpt o src
                                    tgt' <- pCpt2aCpt o tgt
                                    let sgn = Sign src' tgt'
                                    pure (STnullary [(EDcV sgn, sgn, Prim trmPrim)])
           PBin _ oper        -> let x = topCpt      in pure (STnullary [(EBin oper (Sign x x), Sign x x, Prim trmPrim)])
           PBind o oper c     -> do x <- pCpt2aCpt o c
                                    pure (STnullary [(EBin oper (Sign x x), Sign x x, Prim trmPrim)])
           PFlipped t         -> do sgnTree <- signats (fmap flp mConstraintSig) (Prim t)
                                    pure (flp sgnTree)
           PNamedR rel        -> do relsList <- case p_mbSign rel of
                                      Just sg -> do sgn <- pSign2aSign (pCpt2aCpt (origin trmPrim)) sg
                                                    pure (findRelsTyped (declarationsMap ci) (name rel) sgn)
                                      Nothing -> pure (findDecls (declarationsMap ci) (name rel))
                                    case relsList of
                                       [] -> Errors . return . CTXE (origin trmPrim) $
                                             case (relsList, mConstraintSig) of
                                                ([d], Just (Sign constraintSrc constraintTgt)) -> ("There is no match for relation "<>showP rel<>" because ") <> T.intercalate " and "
                                                       ([ "its source concept "<>tshow (source (sign d))<>" should be "<>tshow constraintSrc<>" (or more generic)"
                                                        | Just False <- [geq (source (sign d)) constraintSrc]] <>
                                                        [ "its source concept "<>tshow (source (sign d))<>" is unrelated to "<>tshow constraintSrc
                                                        | Nothing <- [geq (source (sign d)) constraintSrc]] <>
                                                        [ "its target concept "<>tshow (target (sign d))<>" should be "<>tshow constraintTgt<>" (or more generic)"
                                                        | Just False <- [geq (target (sign d)) constraintTgt]] <>
                                                        [ "its target concept "<>tshow (target (sign d))<>" is unrelated to "<>tshow constraintTgt
                                                        | Nothing <- [geq (target (sign d)) constraintTgt]])<>"."
                                                ([],_)  -> "Undeclared relation "<> showP trmPrim
                                                _   -> "None of the relations: " <> T.intercalate ", " [showA d | d <- relsList] <> " match on " <>showP rel<>"."
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
          PLrs o a b -> case checkIntra o "left residual" ECps (PCps o) a (flp b) "PLrs" of
                          Checked (STbinary trL trR trs) _ -> pure (STbinary trL (flp trR) (map flpRight trs))
                                                              where
                                                                flpRight (ECps (el,er), sgn, PCps o' pl pr) = (ELrs (el,flp er), sgn, PLrs o' pl (flp pr))
                                                                flpRight _ = fatal ("Unexpected triple in a pattern of a PLrs term at "<> tshow o)
                          Checked _ _ -> fatal "Unexpected non-binary OpTree in PLrs"
                          Errors errs -> Errors errs -- TODO: check error messages for flipping the right operand
          PRrs o a b -> case checkIntra o "right residual" ECps (PCps o) (flp a) b "PRrs" of
                          Checked (STbinary trL trR trs) _ -> pure (STbinary (flp trL) trR (map flpLeft trs))
                                                              where
                                                                flpLeft (ECps (el,er), sgn, PCps o' pl pr) = (ERrs (flp el, er), sgn, PRrs o' (flp pl) pr)
                                                                flpLeft _ = fatal ("Unexpected triple in a pattern of a PRrs term at "<> tshow o)
                          Checked _ _ -> fatal "Unexpected non-binary OpTree in PRrs"
                          Errors errs -> Errors errs -- TODO: check error messages for flipping the left operand
          PPrd o a b -> do sgnaTree <- signats mConstraintSig a; sgnbTree <- signats mConstraintSig b
                           return (STbinary sgnaTree sgnbTree [ (EPrd (expr_a, expr_b), Sign (source sgn_a) (target sgn_b), PPrd o trm_a trm_b)
                                                              | (expr_a, sgn_a, trm_a)<-opSigns sgnaTree, (expr_b, sgn_b, trm_b)<-opSigns sgnbTree])
          PFlp _ e   -> do -- When flipping, swap the constraint using the Flippable instance
                           sgnTree <- signats (flp <$> mConstraintSig) e
                           return (STunary sgnTree [(EFlp expr, flp sgn, trm) | (expr, sgn, _)<-opSigns sgnTree])
          PKl0 o e   -> do sgnTree <- signats mConstraintSig e
                           let endoSigns = [(EKl0 expr, sgn, trm) | (expr, sgn, _)<-opSigns sgnTree, source sgn == target sgn]
                           case endoSigns of
                             [] -> mkVerboseTypeError env o sgnTree ("Kleene star (*) requires an endomorphic relation (source must equal target).\n  Expression: " <> showP e <> "\n  Available signatures: " <> T.intercalate ", " [tshow sgn | (_, sgn, _) <- opSigns sgnTree])
                             _  -> return (STunary sgnTree endoSigns)
          PKl1 o e   -> do sgnTree <- signats mConstraintSig e
                           let endoSigns = [(EKl1 expr, sgn, trm) | (expr, sgn, _)<-opSigns sgnTree, source sgn == target sgn]
                           case endoSigns of
                             [] -> mkVerboseTypeError env o sgnTree ("Kleene plus (+) requires an endomorphic relation (source must equal target).\n  Expression: " <> showP e <> "\n  Available signatures: " <> T.intercalate ", " [tshow sgn | (_, sgn, _) <- opSigns sgnTree])
                             _  -> return (STunary sgnTree endoSigns)
          PCpl _ e   -> do sgnTree <- signats mConstraintSig e
                           return (STunary sgnTree [(ECpl expr, sgn, trm) | (expr, sgn, _)<-opSigns sgnTree])
          PBrk _ e   -> do sgnTree <- signats mConstraintSig e
                           return (STunary sgnTree [(EBrk expr, sgn, trm) | (expr, sgn, _)<-opSigns sgnTree])
          -- Inter-type operations: both operands get the same constraint
          -- For union, use join (least upper bound) to find the common supertype
          -- For other operations (intersection, equation, inclusion, difference), use meet (greatest lower bound)
          PInc o a b -> checkIncl  o "inclusion"    EInc (PInc o) a b
          PEqu o a b -> checkEqtn  o "equation"     EEqu (PEqu o) a b
          PIsc o a b -> checkIsct  o "intersection" EIsc (PIsc o) a b
          PUni o a b -> checkUnin  o "union"        EUni (PUni o) a b
          PDif o a b -> checkUnin  o "difference"   EDif (PDif o) a b

      errorsInter o kind combinator pCombinator a b sgnaTree sgnbTree sgnsa sgnsb
       = let errorOpTree = STbinary sgnaTree sgnbTree [(combinator (expr_a, expr_b), sgn_a, pCombinator trm_a trm_b) | (expr_a, sgn_a, trm_a)<-sgnsa, (expr_b, _, trm_b)<-sgnsb ]
             conceptsSrc = L.nub [ src | (_,sgn_a,_)<-sgnsa, (_,sgn_b,_)<-sgnsb, Just src<-[join (source sgn_a) (source sgn_b)] ]
             conceptsTgt = L.nub [ tgt | (_,sgn_a,_)<-sgnsa, (_,sgn_b,_)<-sgnsb, Just tgt<-[join (target sgn_a) (target sgn_b)] ]
             getRootSig opTree = case opTree of
                                 STbinary _ _ [(_, sgn, _)] -> sgn
                                 STunary _    [(_, sgn, _)] -> sgn
                                 STnullary    [(_, sgn, _)] -> sgn
                                 _ -> Sign botCpt botCpt  -- fallback
             -- Check for ISgn types to provide better error messages
         in case (getRootSig sgnaTree, getRootSig sgnbTree) of
              -- Special case: when one or both sides have ISgn (identity) types
              (ISgn cptA, Sign srcB tgtB) | cptA==botCpt ->
                    mkVerboseTypeError env o errorOpTree
                      ("Cannot assign a type to "<>showP a<>" because " <> tshow srcB <> " and " <> tshow tgtB <> " are not equal.")
              (Sign srcA tgtA, ISgn cptB) | cptB==botCpt ->
                    mkVerboseTypeError env o errorOpTree
                      ("Cannot assign a type to "<>showP b<>" because " <> tshow srcA <> " and " <> tshow tgtA <> " are not equal.")
              _ -> case (conceptsSrc, conceptsTgt) of
                     ([],[])  -> mkVerboseTypeError env o errorOpTree ("Cannot match the source concepts nor the target concepts in the "<>kind<>")\n   sgnsa: "<>tshow (map snd3 sgnsa)<>"\n   sgnsb: "<>tshow (map snd3 sgnsb))
                     ([],_:_) -> mkVerboseTypeError env o errorOpTree ("Cannot match the source concepts on the left side of the "<>kind<>".\n   The source of "<>showP a<>" is "<>showSgns (map (source . snd3) sgnsa)<>"\n   The source of "<>showP b<>" is "<>showSgns (map (source . snd3) sgnsb))
                     (_:_,[]) -> mkVerboseTypeError env o errorOpTree ("Cannot match the target concepts of the right side of the "<>kind<>".\n   The target of "<>showP a<>" is "<>showSgns (map (target . snd3) sgnsa)<>"\n   The target of "<>showP b<>" is "<>showSgns (map (target . snd3) sgnsb))
                     _        -> mkVerboseTypeError env o errorOpTree ("Cannot match the signatures at either side of the "<>kind<>".\n   sgnsa: "<>tshow (map snd3 sgnsa)<>"\n   sgnsb: "<>tshow (map snd3 sgnsb))
         where
           showSgns :: Show a => [a] -> Text
           showSgns sgns = case sgns of
                            [] ->     "untyped"
                            [sgn] ->  tshow sgn
                            sgn:ss -> (T.intercalate ", " . map tshow) ss<>", or "<>tshow sgn

      checkIncl :: Origin -> Text -> ((Expression, Expression) -> Expression) -> (Term TermPrim -> Term TermPrim -> Term TermPrim) -> Term TermPrim -> Term TermPrim -> Guarded (OpTree (Expression, Signature, Term TermPrim))
      checkIncl o kind combinator pCombinator a b =
        do
          sgnaTree <- signats mConstraintSig a
          sgnbTree <- signats mConstraintSig b
          let sgnsa = opSigns sgnaTree
              sgnsb = opSigns sgnbTree
          case [ (combinator (expr_a, expr_b), sgn_a, pCombinator trm_a trm_b)
               | (expr_a, sgn_a, trm_a)<-sgnsa, (expr_b, sgn_b, trm_b)<-sgnsb
               , Just True <- [geqSig sgn_a sgn_b]
               ] of
            []  -> errorsInter o kind combinator pCombinator a b sgnaTree sgnbTree sgnsa sgnsb
            [res] -> -- trace ("\n17. Checkperi yields:\n"<>showOpTree (STbinary sgnaTree sgnbTree [res]))
                     return (STbinary sgnaTree sgnbTree [res])
            results -> let baseMsg = "Ambiguous signatures at either side of the "<>kind<>".\n   You might mean one of: "<>T.concat [ "\n    -   "<>showP a<>" ; "<>showP b<>" with result " <> tshow sig | (_,sig,_)<-results]
                           opTree = STbinary sgnaTree sgnbTree results
                       in mkVerboseTypeError env o opTree baseMsg

      checkEqtn :: Origin -> Text -> ((Expression, Expression) -> Expression) -> (Term TermPrim -> Term TermPrim -> Term TermPrim) -> Term TermPrim -> Term TermPrim -> Guarded (OpTree (Expression, Signature, Term TermPrim))
      checkEqtn o kind combinator pCombinator a b =
        do
          sgnaTree <- signats mConstraintSig a
          sgnbTree <- signats mConstraintSig b
          let sgnsa = opSigns sgnaTree
              sgnsb = opSigns sgnbTree
          case [ (refinedExpr, sign refinedExpr, pCombinator trm_a trm_b)
               | (expr_a, sgn_a, trm_a)<-sgnsa, (expr_b, sgn_b, trm_b)<-sgnsb
               , Just combinedSgn <- ([meetSig sgn_a sgn_b | not (isConcreteSignature sgn_a && isConcreteSignature sgn_b)]<>
                                      [joinSig sgn_a sgn_b | isConcreteSignature sgn_a && isConcreteSignature sgn_b])
               , let refinedExpr = refineANY combinedSgn (combinator (refineANY combinedSgn expr_a, refineANY combinedSgn expr_b))
               ] of
            []  -> errorsInter o kind combinator pCombinator a b sgnaTree sgnbTree sgnsa sgnsb
            [res] -> return (STbinary sgnaTree sgnbTree [res])
            results -> let baseMsg = "Ambiguous signatures at either side of the "<>kind<>".\n   You might mean one of: "<>T.concat [ "\n    -   "<>showP a<>" ; "<>showP b<>" with result " <> tshow sig | (_,sig,_)<-results]
                           opTree = STbinary sgnaTree sgnbTree results
                       in mkVerboseTypeError env o opTree baseMsg

      -- | checkPeri generates a type error message for equations, inclusions, unions, intersects, and difference.
      -- The meetORjoin parameter determines how to combine concept types:
      -- - For union, use join (least upper bound) to find the common supertype
      -- - For other operations (intersection, equation, inclusion, difference), use meet (greatest lower bound)
      checkIsct :: Origin -> Text -> ((Expression, Expression) -> Expression) -> (Term TermPrim -> Term TermPrim -> Term TermPrim) -> Term TermPrim -> Term TermPrim -> Guarded (OpTree (Expression, Signature, Term TermPrim))
      checkIsct o kind combinator pCombinator a b =
        do
          sgnaTree <- signats mConstraintSig a
          sgnbTree <- signats mConstraintSig b
          let sgnsa = opSigns sgnaTree
              sgnsb = opSigns sgnbTree
          case [ (combinator (expr_a, expr_b), sgn, pCombinator trm_a trm_b)
               | (expr_a, sgn_a, trm_a)<-sgnsa, (expr_b, sgn_b, trm_b)<-sgnsb
               , Just sgn<-[meetSig sgn_a sgn_b]] of
            []  -> let errorExprs = [(combinator (expr_a, expr_b), Sign (source sgn_a) (target sgn_b), pCombinator trm_a trm_b) | (expr_a, sgn_a, trm_a)<-sgnsa, (expr_b, sgn_b, trm_b)<-sgnsb ]
                       opTree = STbinary sgnaTree sgnbTree errorExprs
                       conceptsTgt = L.nub [ tgt | (_,sgn_a,_)<-sgnsa, (_,sgn_b,_)<-sgnsb, Just tgt<-[meet (target sgn_a) (target sgn_b)] ]
                       conceptsSrc = L.nub [ src | (_,sgn_a,_)<-sgnsa, (_,sgn_b,_)<-sgnsb, Just src<-[meet (source sgn_a) (source sgn_b)] ]
                   in case (conceptsSrc, conceptsTgt) of
                    ([],[])  -> mkVerboseTypeError env o opTree ("Cannot match the concepts on both sides of the "<>kind<>")\n   sgnsa: "<>tshow (map snd3 sgnsa)<>"\n   sgnsb: "<>tshow (map snd3 sgnsb))
                    ([],_:_) -> mkVerboseTypeError env o opTree ("Cannot match the source concepts on the left side of the "<>kind<>".\n   The source of "<>showP a<>" is "<>showSgns (map (source . snd3) sgnsa)<>"\n   The source of "<>showP b<>" is "<>showSgns (map (source . snd3) sgnsb))
                    (_:_,[]) -> mkVerboseTypeError env o opTree ("Cannot match the target concepts of the right side of the "<>kind<>".\n   The target of "<>showP a<>" is "<>showSgns (map (target . snd3) sgnsa)<>"\n   The target of "<>showP b<>" is "<>showSgns (map (target . snd3) sgnsb))
                    _        -> mkVerboseTypeError env o opTree ("Cannot match the signatures at either side of the "<>kind<>".\n   sgnsa: "<>tshow (map snd3 sgnsa)<>"\n   sgnsb: "<>tshow (map snd3 sgnsb))
                   where
                     showSgns :: Show a => [a] -> Text
                     showSgns sgns = case sgns of
                                      [] ->     "untyped"
                                      [sgn] ->  tshow sgn
                                      sgn:ss -> (T.intercalate ", " . map tshow) ss<>", or "<>tshow sgn
            [res] -> return (STbinary sgnaTree sgnbTree [res])
            results -> let baseMsg = "Ambiguous signatures at either side of the "<>kind<>".\n   You might mean one of: "<>T.concat [ "\n    -   "<>showP a<>" ; "<>showP b<>" with result " <> tshow sig | (_,sig,_)<-results]
                           opTree = STbinary sgnaTree sgnbTree results
                       in mkVerboseTypeError env o opTree baseMsg

      checkUnin :: Origin -> Text -> ((Expression, Expression) -> Expression) -> (Term TermPrim -> Term TermPrim -> Term TermPrim) -> Term TermPrim -> Term TermPrim -> Guarded (OpTree (Expression, Signature, Term TermPrim))
      checkUnin o kind combinator pCombinator a b =
        do
          sgnaTree <- signats mConstraintSig a
          sgnbTree <- signats mConstraintSig b
          let sgnsa = opSigns sgnaTree
              sgnsb = opSigns sgnbTree
          case [ (combinator (expr_a, expr_b), sgn, pCombinator trm_a trm_b)
               | (expr_a, sgn_a, trm_a)<-sgnsa, (expr_b, sgn_b, trm_b)<-sgnsb
               , Just sgn<-[joinSig sgn_a sgn_b]] of
            []  -> let errorExprs = [(combinator (expr_a, expr_b), Sign (source sgn_a) (target sgn_b), pCombinator trm_a trm_b) | (expr_a, sgn_a, trm_a)<-sgnsa, (expr_b, sgn_b, trm_b)<-sgnsb ]
                       opTree = STbinary sgnaTree sgnbTree errorExprs
                       conceptsTgt = L.nub [ tgt | (_,sgn_a,_)<-sgnsa, (_,sgn_b,_)<-sgnsb, Just tgt<-[join (target sgn_a) (target sgn_b)] ]
                       conceptsSrc = L.nub [ src | (_,sgn_a,_)<-sgnsa, (_,sgn_b,_)<-sgnsb, Just src<-[join (source sgn_a) (source sgn_b)] ]
                   in case (conceptsSrc, conceptsTgt) of
                    ([],[])  -> mkVerboseTypeError env o opTree ("Cannot match the concepts on both sides of the "<>kind<>")\n   sgnsa: "<>tshow (map snd3 sgnsa)<>"\n   sgnsb: "<>tshow (map snd3 sgnsb))
                    ([],_:_) -> mkVerboseTypeError env o opTree ("Cannot match the source concepts on the left side of the "<>kind<>".\n   The source of "<>showP a<>" is "<>showSgns (map (source . snd3) sgnsa)<>"\n   The source of "<>showP b<>" is "<>showSgns (map (source . snd3) sgnsb))
                    (_:_,[]) -> mkVerboseTypeError env o opTree ("Cannot match the target concepts of the right side of the "<>kind<>".\n   The target of "<>showP a<>" is "<>showSgns (map (target . snd3) sgnsa)<>"\n   The target of "<>showP b<>" is "<>showSgns (map (target . snd3) sgnsb))
                    _        -> mkVerboseTypeError env o opTree ("Cannot match the signatures at either side of the "<>kind<>".\n   sgnsa: "<>tshow (map snd3 sgnsa)<>"\n   sgnsb: "<>tshow (map snd3 sgnsb))
                   where
                     showSgns :: Show a => [a] -> Text
                     showSgns sgns = case sgns of
                                      [] ->     "untyped"
                                      [sgn] ->  tshow sgn
                                      sgn:ss -> (T.intercalate ", " . map tshow) ss<>", or "<>tshow sgn
            [res] -> return (STbinary sgnaTree sgnbTree [res])
            results -> let baseMsg = "Ambiguous signatures at either side of the "<>kind<>".\n   You might mean one of: "<>T.concat [ "\n    -   "<>showP a<>" ; "<>showP b<>" with result " <> tshow sig | (_,sig,_)<-results]
                           opTree = STbinary sgnaTree sgnbTree results
                       in mkVerboseTypeError env o opTree baseMsg

      checkIntra --, checkPeri
        :: {- o           -} Origin
        -> {- kind        -} Text
        -> {- combinator  -} ((Expression, Expression) -> Expression)
        -> {- pCombinator -} (Term TermPrim -> Term TermPrim -> Term TermPrim)
        -> {- a           -} Term TermPrim
        -> {- b           -} Term TermPrim
        -- extra parameters for tracing purpose:
        -> {- opStr       -} Text -- for tracing purpose
        -> Guarded (OpTree (Expression, Signature, Term TermPrim))
      checkIntra o kind combinator pCombinator a b _opStr = -- activate the last parameter opStr when using the trace statements: 
        do let showPa = showP a; showPb = showP b -- for tracing purposes only
           let lConstraint = case mConstraintSig of
                                Just (Sign src _) -> Just (Sign src botCpt)
                                Just (ISgn cpt)   -> Just (Sign cpt botCpt)
                                Nothing           -> Nothing
           let rConstraint = case mConstraintSig of
                                Just (Sign _ tgt) -> Just (Sign botCpt tgt)
                                Just (ISgn cpt)   -> Just (Sign botCpt cpt)
                                Nothing           -> Nothing
           sgnaTree <- signats lConstraint a; sgnbTree <- signats rConstraint b
           let triplesa = opSigns sgnaTree; triplesb = opSigns sgnbTree
               triples = makeTriples triplesa triplesb
               sgnsa = fmap (\(_,s,_) -> s) triplesa; sgnsb = fmap (\(_,s,_) -> s) triplesb
           -- trace ("10. makeTriples on "<>tshow o<>" yields "<>tshow (length triples)<>" triples: "<>T.intercalate ", " (map showTriple triples)) $
           case triples of
             []  -> let errorExprs = [(combinator (expr_a, expr_b), Sign (source sgn_a) (target sgn_b), pCombinator trm_a trm_b) | (expr_a, sgn_a, trm_a)<-triplesa, (expr_b, sgn_b, trm_b)<-triplesb ]
                        opTree = STbinary sgnaTree sgnbTree errorExprs
                    in mkVerboseTypeError env o opTree ("Cannot match the signatures on the left and right of the " <> kind <> "." <> diagnosis kind a b sgnsa sgnsb)
             [tr] -> -- trace ("\n11. checkIntra yields: \n"<>showOpTree (STbinary sgnaTree sgnbTree [tr])) $
                     return (STbinary sgnaTree sgnbTree [tr])
             results  -> let baseMsg = "Ambiguous signatures of the " <> kind <> " of " <> showPa <> " and " <> showPb
                             suggestions = Just $ ". You might mean one of: " <> T.concat [ "\n    -   " <> showPa <> " ; " <> showPb <> " with result " <> tshow sig | (_,sig,_)<-results]
                             opTree = STbinary sgnaTree sgnbTree results
                         in mkVerboseTypeMismatchError env o baseMsg suggestions opTree
        where
          -- | makeTriples constructs all possible (Expression, Signature, Term) triples for binary operations.
          makeTriples :: [(Expression, Signature, Term TermPrim)]
                      -> [(Expression, Signature, Term TermPrim)]
                      -> [(Expression, Signature, Term TermPrim)]
          makeTriples triplesa triplesb
           = -- trace ("\n12. makeTriples called with:\n  triplesa signatures: "<>T.intercalate ", " (map showTriple triplesa)<>"\n  triplesb signatures: "<>T.intercalate ", " (map showTriple triplesb)) $
             [ -- trace ("\n13. list comprehension with:\n  between: "<>tshow between) $
               let result_expr = combinator (refineANY (Sign srca between) expr_a, refineANY (Sign between tgtb) expr_b)
               in (result_expr, sign result_expr, pCombinator trm_a trm_b)
             | (expr_a, Sign srca tgta, trm_a)<-triplesa, (expr_b, Sign srcb tgtb, trm_b)<-triplesb
             , Just between<-[meet tgta srcb]
             ] <>
             [ let result_expr = combinator (refineANY (Sign srca between) expr_a, refineANY (ISgn between) expr_b)
               in (result_expr, sign result_expr, pCombinator trm_a trm_b)
             | (expr_a, Sign srca tgta, trm_a)<-triplesa, (expr_b, ISgn cptb, trm_b)<-triplesb
             , Just between<-[meet tgta cptb]
             ] <>
             [ let result_expr = combinator (refineANY (ISgn between) expr_a, refineANY (Sign between tgtb) expr_b)
               in (result_expr, sign result_expr, pCombinator trm_a trm_b)
             | (expr_a, ISgn cpta, trm_a)<-triplesa, (expr_b, Sign srcb tgtb, trm_b)<-triplesb
             , Just between<-[meet cpta srcb]
             ] <>
             [ let result_expr = combinator (refineANY (ISgn cpta) expr_a, refineANY (ISgn between) expr_b)
               in (result_expr, sign result_expr, pCombinator trm_a trm_b)
             | (expr_a, ISgn cpta, trm_a)<-triplesa, (expr_b, ISgn cptb, trm_b)<-triplesb
             , Just between<-[meet cpta cptb]
             ]

      -- diagnosis :: Text -> a -> a -> [a] -> [a] -> Text
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

      pCpt2aCpt = conceptMap ci
      signats = signatures env ci  -- Pass constraint to subexpressions for disambiguation

-- | term2Expr converts a Term to an Expression, ensuring that the Expression and each of its subexpressions has precisely one concrete signature.
--   (A signature is concrete if it does not contain TOP or BOT.)
--   If it cannot do that, it produces relevant error messages the user can understand.
--   Thus, term2Expr ensures that the expression has a well-defined population, as specified in the documentation.
--   As a consequence, the expression can be translated to SQL unambiguously and a database can realize those semantics.
--
--   The first step, signatures, produces sgnTree::OpTree, which is a tree structure that corresponds precisely to the tree structure of the term.
--   Every node of sgnTree contains all possible (Expression, Signature, Term) triples for the given subterm.
--   If the root of sgnTree has precisely one, concrete signature, Ampersand will assign that signature to the Expression.
--   But even then, the nodes in sgnTree may have multiple signatures.
--   This represents ambiguity in the typing of subexpressions.
--   The second step, refineSgn, recursively refines sgnTree by filtering out multiple signatures at each node,
--   based on the signature of the parent node.
--   If ambiguities still remain, these are reported as type errors to the user.
term2Expr :: (HasFSpecGenOpts env, HasRunner env) => env -> ContextInfo -> Maybe Signature -> Term TermPrim -> Guarded Expression
term2Expr env ci mConstraintSig term
  = do sgnTree <- signatures env ci mConstraintSig term
       case sgnTree of
         STnullary    triples@((_, _, trm):_:_) -> mkVerboseTypeError env (origin trm) sgnTree ("Ambiguous nullary term: " <> showP trm <> " might be one of " <> T.intercalate ", " (map (tshow . snd3) triples) <> ".\n  "<>"Please specify the signature explicitly.")
         STunary _    triples@((_, _, trm):_:_) -> mkVerboseTypeError env (origin trm) sgnTree ("Ambiguous unary term: "   <> showP trm <> " might be one of " <> T.intercalate ", " (map (tshow . snd3) triples) <> ".\n  "<>"Please specify the signature explicitly.")
         STbinary _ _ triples@((_, _, trm):_:_) -> mkVerboseTypeError env (origin trm) sgnTree ("Ambiguous binary term: "  <> showP trm <> " might be one of " <> T.intercalate ", " (map (tshow . snd3) triples) <> ".\n  "<>"Please specify the signature explicitly.")
         STnullary    [(expr, sig, _)]
           | isConcreteSignature sig -> pure expr  -- Single expression - already reduced, return it
           | otherwise -> trace ("\nterm2Expr (mConstraintSig="<>tshow mConstraintSig<>") ("<>showP term<>"):\n"<>showOpTree sgnTree) $
                           mkVerboseTypeError env (origin term) sgnTree ("Cannot determine a concrete type for nullary term " <> showP term <> ". The inferred signature contains " <> tshow sig)
         STunary _    [(_, sig, _)]
           | not (isConcreteSignature sig) -> mkVerboseTypeError env (origin term) sgnTree ("Cannot determine a concrete type for unary term " <> showP term <> ". The inferred signature contains " <> tshow sig)
         STbinary _ _ [(_, sig, _)]
           | not (isConcreteSignature sig) -> mkVerboseTypeError env (origin term) sgnTree ("Cannot determine a concrete type for binary term " <> showP term <> ". The inferred signature contains " <> tshow sig)
         STnullary    []     -> fatal "Empty triples list in STnullary"
         STunary _    []     -> fatal "Empty triples list in STunary"
         STbinary _ _ []     -> fatal "Empty triples list in STbinary"
         _ -> -- trace ("\nrefineSgn:\n"<>showOpTree sgnTree) $
              case refineSgn (rootSignature sgnTree) sgnTree of
                Errors errs -> Errors errs
                Checked refinedTree _ -> do
                  expr <- rootExpression refinedTree
                  let finalSig = rootSignature sgnTree
                  -- Apply refineANY one more time to eliminate any remaining TOP/BOT
                  let finalExpr = refineANY finalSig expr
                      finalExprSig = sign finalExpr
                  -- Check if the final expression is concrete
                  if isConcreteSignature finalExprSig
                    then pure finalExpr
                    else Errors . pure $ CTXE (origin term) $
                      "Cannot determine a concrete type for expression " <> showP term <> ". " <>
                      "The inferred signature " <> tshow finalExprSig <> " contains TOP or BOT. " <>
                      "Please add an explicit signature."
  where
    rootSignature sgnTree =
       case sgnTree of
         STbinary _ _ [(_, sgn, _)] -> sgn
         STunary _    [(_, sgn, _)] -> sgn
         STnullary    [(_, sgn, _)] -> sgn
         _                          -> fatal ("term2Expr: pattern match failure in rootSignature. Function signatures yields:\n"<>showOpTree sgnTree<>"\nThis is a bug in the compiler.")

    rootExpression sgnTree =
       case sgnTree of
         STnullary    [ (expr, _, _) ] -> pure expr
         STunary _    [ (expr, _, _) ] -> pure expr
         STbinary _ _ [ (expr, _, _) ] -> pure expr
         _                          -> fatal ("term2Expr: pattern match failure in rootExpression. Function signatures yields:\n"<>showOpTree sgnTree<>"\nThis is a bug in the compiler.")

    -- | refineSgn recursively refines an OpTree by filtering out triples that do not fit in the mold signature.
    --   It produces an OpTree with a single triple at each node, which can then be converted to Expression.
    --   Precondition: opSigns sgnTree is a singleton, whose signature is the mold for its subtrees.
    --   Precondition: mold == rootSignature sgnTree
    --   Precondition: if the triple in the root of sgnTree is (expr, sgn, t), then term2Expr t == expr
    refineSgn :: Signature -> OpTree (Expression, Signature, Term TermPrim) -> Guarded (OpTree (Expression, Signature, Term TermPrim))
    refineSgn mold sgnTree =
      -- trace ("\n18.>>> refineSgn (mold: "<>tshow mold<>") (["<>(case sgnTree of
      --              STnullary triples -> "STnullary "<>T.intercalate ", " (fmap showTriple triples)
      --              STunary _ triples -> "STunary "<>T.intercalate ", " (fmap showTriple triples)
      --              STbinary _ _ triples -> "STbinary "<>T.intercalate ", " (fmap showTriple triples))<>"]) on "<>showP trmP<>" yields "<>
      --              case result of
      --               Checked r _ -> tshow (rootSignature r)
      --               Errors _ -> "error(s)") $
      result
      where
        trmP :: Term TermPrim
        trmP = case opSigns sgnTree of
                 [(_, _, t)] -> t
                 _           -> fatal "refineSgn: pattern match failure in extracting term from opSigns. This is a bug in the compiler."
        result
           = case sgnTree of
               STnullary triples ->
                 case [ (expr, sn,  t) | (expr, sgn, t) <- triples, isIdentityOrVorB expr
                                       , Just sn <- [ -- trace ("       meetSig ("<>tshow mold<>") ("<>tshow sgn<>") = "<>tshow (meetSig mold sgn)) $
                                                      meetSig mold sgn ]]<>
                      [ (expr, sgn, t) | (expr, sgn, t) <- triples, not (isIdentityOrVorB expr)
                                       , Just _ <- [joinSig mold sgn]]
                  of
                   [triple] -> pure (STnullary [triple])
                   [] -> Errors . pure $ CTXE (origin (getTerm sgnTree)) ("No signature found that matches "<>tshow mold<>" in STnullary\n"<>showOpTree sgnTree)
                   filtered -> msg filtered sgnTree "Please specify the signature explicitly."
                 where
                   isIdentityOrVorB :: Expression -> Bool
                   isIdentityOrVorB (EDcI{}) = True
                   isIdentityOrVorB (EDcV{}) = True
                   isIdentityOrVorB (EBin{}) = True
                   isIdentityOrVorB _ = False

               STunary subTree triples ->
                 case [ (expr, sgn, trm) | (expr, sgn, trm) <- triples, Just _ <- [joinSig sgn mold] ] of
                   [triple@(_, sgn, trm)] -> do
                     -- For PFlp: the refined signature is flipped, but subTree contains unflipped relations
                     -- So we must flip the signature back before recursing
                     refinedSubTree <- refineSgn (if isFlip trm then flp sgn else sgn) subTree
                     pure (STunary refinedSubTree [triple])
                   [] -> Errors . pure $ CTXE (origin (getTerm sgnTree)) ("No signature found that matches "<>tshow mold<>" in STunary\n"<>showOpTree sgnTree)
                   filtered -> msg filtered sgnTree "Please specify the signature explicitly."
                  where
                     isFlip (PFlp _ _) = True
                     isFlip _          = False

               STbinary stLeft stRight triples | isInter sgnTree ->
                --  trace ("\n🔍 DEBUG isInter case for " <> showP trmP <> ":" <>
                --         "\n     mold = " <> tshow mold <>
                --         "\n     triples = " <> T.intercalate ", " [tshow sgn | (_, sgn, _) <- triples] <>
                --         T.intercalate "\n" [ "     geqSig " <> tshow mold <> " " <> tshow sgn <> " = " <> tshow (geqSig mold sgn) | (_, sgn, _) <- triples]) $
                 case [ (expr, sgn, trm) | (expr, sgn, trm) <- triples, Just True <- [geqSig mold sgn] ] of
                   [triple@(_, sgn, _)] -> do
                           refinedLeft  <- refineSgn sgn stLeft
                           refinedRight <- refineSgn sgn stRight
                           pure (STbinary refinedLeft refinedRight [triple])
                   [] -> Errors . pure $ CTXE (origin (getTerm sgnTree)) ("No signature of "<>showP trmP<>" found that matches "<>tshow mold<>" in STbinary\n"<>showOpTree sgnTree)
                   filtered -> msg filtered sgnTree "Please specify the signature explicitly."
               STbinary stLeft stRight triples | isPLrs sgnTree ->
                 case [ (expr, sgn, trm) | (expr, sgn, trm) <- triples, Just True <- [geqSig mold sgn] ] of
                   [triple@(expr, sgn, _)] -> do
                     let between = getBetweenConcept expr
                     refinedLeft  <- refineSgn (Sign (source sgn) between) stLeft
                     refinedRight <- refineSgn (Sign (target sgn) between) stRight
                     pure (STbinary refinedLeft refinedRight [triple])
                   [] -> Errors . pure $ CTXE (origin (getTerm sgnTree)) ("No signature of "<>showP trmP<>" found that matches "<>tshow mold<>" in STbinary\n"<>showOpTree sgnTree)
                   filtered -> msg filtered sgnTree "Please specify the signature explicitly."
               STbinary stLeft stRight triples | isPRrs sgnTree ->
                 case [ (expr, sgn, trm) | (expr, sgn, trm) <- triples, Just True <- [geqSig mold sgn] ] of
                   [triple@(expr, sgn, _)] -> do
                     let between = getBetweenConcept expr
                     refinedLeft  <- refineSgn (Sign between (source sgn)) stLeft
                     refinedRight <- refineSgn (Sign between (target sgn)) stRight
                     pure (STbinary refinedLeft refinedRight [triple])
                   [] -> Errors . pure $ CTXE (origin (getTerm sgnTree)) ("No signature of "<>showP trmP<>" found that matches "<>tshow mold<>" in STbinary\n"<>showOpTree sgnTree)
                   filtered -> msg filtered sgnTree "Please specify the signature explicitly."
               STbinary stLeft stRight triples | isIntra sgnTree ->
                 case [ (expr, sgn, trm) | (expr, sgn, trm) <- triples, Just True <- [geqSig mold sgn] ] of
                   [triple@(expr, sgn, _)] -> do
                     -- Extract the 'between' concept from the refined expression to properly constrain subtrees.
                     -- For compositions e1;e2, the between concept is target(e1) = source(e2).
                     -- Using botCpt would allow multiple signatures to pass through incorrectly.
                     let between = getBetweenConcept expr
                     refinedLeft  <- refineSgn (Sign (source sgn) between) stLeft
                     refinedRight <- refineSgn (Sign between (target sgn)) stRight
                     pure (STbinary refinedLeft refinedRight [triple])
                   [] -> Errors . pure $ CTXE (origin (getTerm sgnTree)) ("No signature of "<>showP trmP<>" found that matches "<>tshow mold<>" in STbinary\n"<>showOpTree sgnTree)
                   filtered -> msg filtered sgnTree "Please specify the signature explicitly."
               _ -> fatal ("refineSgn: pattern match failure.\n"<>showOpTree sgnTree<>"\nThis is a bug in the compiler.")

    getTerm :: OpTree (Expression, Signature, Term TermPrim) -> Term TermPrim
    getTerm sgnTree =
      case sgnTree of
        STnullary    ((_, _, t):_) -> t
        STunary _    ((_, _, t):_) -> t
        STbinary _ _ ((_, _, t):_) -> t
        _                          -> fatal ("getTerm: pattern match failure.\n"<>showOpTree sgnTree<>"\nThis is a bug in the compiler.")

    isInter, isPLrs, isPRrs, isIntra :: OpTree (Expression, Signature, Term TermPrim) -> Bool
    -- isInter handles operations where operands are refined to the common subtype (meet)
    isInter sgnTree = case sgnTree of
                        STbinary _ _ [(_, _, PEqu{})] -> True
                        STbinary _ _ [(_, _, PInc{})] -> True
                        STbinary _ _ [(_, _, PUni{})] -> True
                        STbinary _ _ [(_, _, PIsc{})] -> True
                        STbinary _ _ [(_, _, PDif{})] -> True
                        _                             -> False
    isPLrs sgnTree = case sgnTree of
                        STbinary _ _ [(_, _, PLrs{})] -> True
                        _                             -> False
    isPRrs sgnTree = case sgnTree of
                        STbinary _ _ [(_, _, PRrs{})] -> True
                        _                             -> False
    isIntra sgnTree = case sgnTree of
                        STbinary _ _ [(_, _, PCps{})] -> True
                        STbinary _ _ [(_, _, PDia{})] -> True
                        STbinary _ _ [(_, _, PRad{})] -> True
                        STbinary _ _ [(_, _, PPrd{})] -> True
                        _                             -> False

    msg :: [(Expression, Signature, Term TermPrim)] -> OpTree (Expression, Signature, Term TermPrim) -> Text -> Guarded (OpTree (Expression, Signature, Term TermPrim))
    msg triples@((_, _, trm):_:_) sgnTree str = mkVerboseTypeError env (origin trm) sgnTree ("Ambiguous term: " <> showP trm <> " might be one of " <> T.intercalate ", " (map (tshow . snd3) triples) <> ".\n  "<>str)
    msg [(_, sgn, trm)]           sgnTree str = mkVerboseTypeError env (origin trm) sgnTree ("Ambiguous term: " <> showP trm <> " has signature " <> tshow sgn <> ".\n  "<>str)
    msg  _                         _       _  = fatal "msg: pattern match failure. This is a bug in the compiler."

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
