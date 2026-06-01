{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure extraction of diagnostic data from an 'FSpec'.
--
-- The single entry point is 'extractDiagnostics'.  It produces a
-- 'DiagnosticData' value that contains everything the diagnosis
-- renderers (Pandoc summary, xlsx workbook) need to operate.
--
-- This module is deliberately free of Pandoc, xlsx and IO so that the
-- extraction logic can be exercised, inspected and tested in isolation
-- from any output formatting.
--
-- == Status of the per-row metrics
--
-- The fields filled in below cover the "essentials" of the diagnosis
-- spreadsheet (file/line, pattern membership, documentation coverage,
-- algebraic properties, simple counts and the per-rule violation
-- count).  The follow-up checklist (issue #1625) adds quite a few
-- richer metrics: term-complexity walking the AST, classification
-- depth, CRUD coverage, dependency graph between patterns, suspect
-- synonym detection, etc.  Those have placeholder values for now and
-- are clearly marked with @TODO #1625@; the column structure is in
-- place so subsequent iterations can land one metric at a time.
module Ampersand.Diagnosis.Extract
  ( extractDiagnostics,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import qualified Ampersand.Diagnosis.ClassifyGraph as CG
import qualified Ampersand.Diagnosis.PatternGraph as PG
import Ampersand.Diagnosis.Types
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Motivations (HasMeaning (..), Motivated (..))
import qualified Data.Text as T
import qualified RIO.List as L
import qualified RIO.Set as Set

-- | The single entry point.  Pure, total (modulo @fatal@ inside the
-- 'FSpec') and deterministic for a given @(Lang, FSpec)@ pair.
extractDiagnostics :: Lang -> FSpec -> DiagnosticData
extractDiagnostics lang fSpec =
  DiagnosticData
    { ddSummary = mkSummary,
      ddPatterns = patternRows,
      ddConcepts = conceptRows,
      ddRelations = relationRows,
      ddRules = ruleRows,
      ddInterfaces = interfaceRows
    }
  where
    ----------------------------------------------------------------
    -- Source material from the FSpec
    ----------------------------------------------------------------
    pats :: [Pattern]
    pats = vpatterns fSpec

    rels :: [Relation]
    rels = Set.toList (vrels fSpec)

    rules :: [Rule]
    rules = Set.toList (vrules fSpec)

    cdefs :: [AConceptDef]
    cdefs = conceptDefs fSpec

    ifcs :: [Interface]
    ifcs = interfaceS fSpec

    ----------------------------------------------------------------
    -- Generic helpers
    ----------------------------------------------------------------
    hasPurpose :: (Motivated a) => a -> Bool
    hasPurpose = not . null . purposesOf fSpec lang

    hasMeaning' :: (HasMeaning a) => a -> Bool
    hasMeaning' = isJust . meaning lang

    rolesFor :: Rule -> [Text]
    rolesFor r = L.nub [fullName rol | (rol, r') <- fRoleRuls fSpec, r == r']

    -- | Synthetic pattern name used to group every definition that
    -- lives in the CONTEXT outside any user-declared PATTERN.  The
    -- diagnostic spreadsheet treats this as a "real" pattern row so
    -- that all concepts, relations and rules belong to exactly one
    -- pattern, simplifying downstream aggregations (B1/B3 from the
    -- follow-up checklist).
    noPatternLabel :: Text
    noPatternLabel = "No Pattern"

    relationToPattern :: Relation -> Maybe Text
    relationToPattern rel =
      case [label p | p <- pats, rel `Set.member` ptdcs p] of
        (n : _) -> Just n
        [] -> Just noPatternLabel

    ruleToPattern :: Rule -> Maybe Text
    ruleToPattern r =
      case [label p | p <- pats, r `Set.member` ptrls p] of
        (n : _) -> Just n
        [] -> Just noPatternLabel

    conceptToPattern :: A_Concept -> Maybe Text
    conceptToPattern c =
      case [label p | p <- pats, any (\cd -> acdcpt cd == c) (ptcds p)] of
        (n : _) -> Just n
        [] -> Just noPatternLabel

    relationsReferencingMap :: [(Relation, Int)]
    relationsReferencingMap =
      [(rel, length [r | r <- rules, rel `Set.member` bindedRelationsIn r]) | rel <- rels]

    relReferencingCount :: Relation -> Int
    relReferencingCount rel = fromMaybe 0 (lookup rel relationsReferencingMap)

    ----------------------------------------------------------------
    -- Classification graph (D1..D4)
    --
    -- Computed once and queried per concept in 'mkConceptDiag'.  All
    -- four metrics live in 'Ampersand.Diagnosis.ClassifyGraph'.
    ----------------------------------------------------------------
    classifyGraph :: CG.ClassifyGraph
    classifyGraph = CG.buildClassifyGraph (vgens fSpec)

    ----------------------------------------------------------------
    -- Cross-pattern dependency graph (C3, C4, C7, C8, C9)
    ----------------------------------------------------------------
    patternGraph :: PG.PatternGraph
    patternGraph = PG.buildPatternGraph fSpec

    ----------------------------------------------------------------
    -- Term complexity / cohesion (F1, C2, C6)
    --
    -- 'ruleComplexity' is the term-complexity score of a single rule:
    -- the number of relation-like leaves (EDcD, EDcI, EDcV, EBin,
    -- EMp1) in its formal expression.  The same walker drives the
    -- per-pattern "largest term complexity" (C2) and the
    -- denominator of the per-pattern cohesion ratio (C6, which is
    -- @internal references / total references@ across all rules of
    -- the pattern).
    ----------------------------------------------------------------
    ruleComplexity :: Rule -> Int
    ruleComplexity = exprRelOccurrences . formalExpression

    -- | Internal references in a rule, given the relations the
    -- enclosing pattern declares.  Only @EDcD@ leaves can be
    -- attributed to a specific relation; the other leaf kinds
    -- (identity, V, binop, singleton) are counted as cross-cutting
    -- and contribute to the denominator but never to the numerator.
    ruleInternalRefs :: Set.Set Relation -> Rule -> Int
    ruleInternalRefs ownRels = exprInternalRelOccurrences ownRels . formalExpression

    ----------------------------------------------------------------
    -- Relation rows
    ----------------------------------------------------------------

    relationRows :: [RelationDiag]
    relationRows = map mkRelationDiag rels

    mkRelationDiag :: Relation -> RelationDiag
    mkRelationDiag rel =
      RelationDiag
        { rdName = fullName rel,
          rdSignature = tshow (sign rel),
          rdDefinedInPattern = relationToPattern rel,
          rdSourceLoc = mkSourceLoc rel,
          rdHasPurpose = hasPurpose rel,
          rdHasMeaning = hasMeaning' rel,
          rdProps = mkRelationProps rel,
          rdNrRulesReferencing = relReferencingCount rel,
          rdNrInterfacesReferencing =
            length [ifc | ifc <- ifcs, rel `Set.member` bindedRelationsIn ifc],
          rdNrViewsReferencing =
            length [v | v <- vviews fSpec, rel `Set.member` bindedRelationsIn v],
          rdNrIdentRulesReferencing =
            length [i | i <- vIndices fSpec, rel `Set.member` bindedRelationsIn i],
          rdNrPairsInitial = Set.size (pairsInExpr fSpec (EDcD rel)),
          -- TODO #1625 (E4, E6, E7): ENFORCE coupling, population
          -- source en similarity.
          rdComputedByEnforce = "",
          rdPopulationSource = "",
          rdSuspectDuplicates = []
        }

    ----------------------------------------------------------------
    -- Concept rows
    ----------------------------------------------------------------
    conceptRows :: [ConceptDiag]
    conceptRows = map mkConceptDiag allConcepts

    allConcepts :: [A_Concept]
    allConcepts =
      L.nub
        $ Set.toList (concs (vrels fSpec))
        <> map acdcpt cdefs

    conceptDefMap :: A_Concept -> [AConceptDef]
    conceptDefMap c = [cd | cd <- cdefs, acdcpt cd == c]

    mkConceptDiag :: A_Concept -> ConceptDiag
    mkConceptDiag c =
      ConceptDiag
        { cdName = fullName c,
          cdDefinedInPattern = conceptToPattern c,
          -- B2: fall back through CONCEPT-def, then first mentioning
          -- relation, so the spreadsheet never shows "None".
          cdSourceLoc = case conceptDefMap c of
            (cd : _) -> mkSourceLoc cd
            [] -> case [r | r <- rels, source r == c || target r == c] of
              (r : _) -> mkSourceLoc r
              [] -> SourceLoc {srcFile = "", srcLine = Nothing},
          cdHasConceptDef = not (null (conceptDefMap c)),
          cdHasPurpose = hasPurpose c,
          cdHasMeaning = any hasMeaning' (conceptDefMap c),
          cdRepresentation = tshow (cptTType fSpec c),
          cdNrRelationsAsSource = length [r | r <- rels, source r == c],
          cdNrRelationsAsTarget = length [r | r <- rels, target r == c],
          cdNrRulesReferencing = length [r | r <- rules, c `Set.member` concs r],
          cdGeneralisationDepth = CG.classifyDepth classifyGraph c,
          cdNrSpecialisations = Set.size (CG.childrenOf classifyGraph c),
          cdNrGeneralisations = Set.size (CG.parentsOf classifyGraph c),
          cdInClassifyCycle = CG.inCycle classifyGraph c,
          cdHasIdentRule = any ((== c) . idCpt) (vIndices fSpec),
          cdHasView = any ((== c) . vdcpt) (vviews fSpec),
          cdNrInterfacesReferencing =
            length [ifc | ifc <- ifcs, c `Set.member` concs ifc],
          cdPatternsUsing = [label p | p <- pats, c `Set.member` concs p],
          cdNrAtomsInitial = Set.size (atomsInCptIncludingSmaller fSpec c),
          -- TODO #1625 (D11, D12): population source en suspect synonyms.
          cdPopulationSource = "",
          cdSuspectSynonyms = []
        }

    ----------------------------------------------------------------
    -- Rule rows
    ----------------------------------------------------------------
    ruleRows :: [RuleDiag]
    ruleRows = map mkRuleDiag rules

    violationsFor :: Rule -> Int
    violationsFor r =
      case [Set.size ps | (r', ps) <- allViolations fSpec, r == r'] of
        (n : _) -> n
        [] -> 0

    -- | F4: pick up to three offending pairs and render them as
    -- @(left, right)@ for inclusion in the spreadsheet.  Showing
    -- "samples" rather than all of them keeps the cell readable in
    -- production-sized scripts.
    sampleViolationsFor :: Rule -> [Text]
    sampleViolationsFor r =
      case [ps | (r', ps) <- allViolations fSpec, r == r'] of
        (ps : _) ->
          map showPair . take 3 . Set.toList $ ps
        [] -> []
      where
        showPair p = "(" <> showValADL (apLeft p) <> ", " <> showValADL (apRight p) <> ")"

    mkRuleDiag :: Rule -> RuleDiag
    mkRuleDiag r =
      RuleDiag
        { rldName = fullName r,
          rldDefinedInPattern = ruleToPattern r,
          rldSourceLoc = mkSourceLoc r,
          rldKind = classifyRule fSpec r,
          rldMaintainedByRoles = rolesFor r,
          rldHasPurpose = hasPurpose r,
          rldHasMeaning = hasMeaning' r,
          rldHasMessage = not (null (rrmsg r)),
          rldHasViolation = isJust (rrviol r),
          rldSignature = tshow (sign (formalExpression r)),
          rldNrViolations = violationsFor r,
          rldNrRelationsReferenced = Set.size (bindedRelationsIn r),
          rldNrConceptsReferenced = Set.size (concs r),
          rldTermComplexity = ruleComplexity r,
          rldSampleViolations = sampleViolationsFor r,
          -- TODO #1625 (F5): unreachable flag.
          rldUnreachable = False
        }

    ----------------------------------------------------------------
    -- Pattern rows
    --
    -- In addition to one row per user-declared PATTERN, we always emit
    -- a synthetic row called "No Pattern".  It aggregates every
    -- definition (concept, relation, rule, IDENT-rule, view, ENFORCE)
    -- that lives directly in the CONTEXT, outside any PATTERN.  This
    -- guarantees that every definition in the script is counted in
    -- exactly one Patterns-sheet row, which keeps downstream
    -- aggregations (totals, percentages, cohesion checks) consistent.
    ----------------------------------------------------------------
    patternRows :: [PatternDiag]
    patternRows = map mkPatternDiag pats <> [mkNoPatternDiag]

    execEngineRulesIn :: Pattern -> Int
    execEngineRulesIn p =
      length
        [ ()
          | r <- Set.toList (ptrls p),
            (rol, r') <- fRoleRuls fSpec,
            r == r',
            fullName rol == fullName nameOfExecEngineRole
        ]

    violationsInPattern :: Pattern -> Int
    violationsInPattern p =
      sum [Set.size ps | (r, ps) <- allViolations fSpec, r `Set.member` ptrls p]

    mkPatternDiag :: Pattern -> PatternDiag
    mkPatternDiag p =
      PatternDiag
        { pdName = label p,
          pdSourceLoc = mkSourceLoc p,
          pdHasPurpose = hasPurpose p,
          pdNrConcepts = length (ptcds p),
          pdNrRelations = Set.size (ptdcs p),
          pdNrInvariantRules =
            length [r | r <- Set.toList (ptrls p), not (isSignal fSpec r)],
          pdNrProcessRules =
            length [r | r <- Set.toList (ptrls p), isSignal fSpec r],
          pdNrIdentRules = length (ptids p),
          pdNrEnforce = length (ptenfs p),
          pdNrViews = length (ptvds p),
          pdConceptsWithPurpose =
            length [cd | cd <- ptcds p, hasPurpose (acdcpt cd)],
          pdRelationsWithPurpose =
            length [r | r <- Set.toList (ptdcs p), hasPurpose r],
          pdRelationsWithMeaning =
            length [r | r <- Set.toList (ptdcs p), hasMeaning' r],
          pdRulesWithPurpose =
            length [r | r <- Set.toList (ptrls p), hasPurpose r],
          pdRulesWithMeaning =
            length [r | r <- Set.toList (ptrls p), hasMeaning' r],
          pdNrExecEngineRules = execEngineRulesIn p,
          pdNrViolationsInitial = violationsInPattern p,
          pdConceptsDeclared = [fullName (acdcpt cd) | cd <- ptcds p],
          pdLargestRuleComplexity =
            foldr max 0 (map ruleComplexity (Set.toList (ptrls p))),
          pdCohesionRatio =
            let ownRels = ptdcs p
                rulesInP = Set.toList (ptrls p)
                totalRefs = sum (map ruleComplexity rulesInP)
                internalRefs = sum (map (ruleInternalRefs ownRels) rulesInP)
             in if totalRefs == 0
                  then 0
                  else fromIntegral internalRefs / fromIntegral totalRefs,
          pdNrOrphanRelations = PG.nrOrphanRelations patternGraph p,
          pdNrUnreachableProcessRules =
            PG.unreachableProcessRules fSpec patternGraph p,
          pdSelfContained = PG.isSelfContained p,
          pdDependsOnPatterns = Set.toAscList (PG.dependsOn patternGraph p),
          pdDependedOnByPatterns = Set.toAscList (PG.dependedOnBy patternGraph p)
        }

    ----------------------------------------------------------------
    -- The synthetic "No Pattern" row
    --
    -- Aggregates everything that is declared directly in the CONTEXT,
    -- i.e. that does not belong to any user-defined PATTERN.  We
    -- subtract the union of every pattern's contents from the global
    -- FSpec collections so that the synthetic row plus the
    -- user-declared patterns together account for the whole script.
    ----------------------------------------------------------------
    relsInAnyPattern :: Set.Set Relation
    relsInAnyPattern = Set.unions (map ptdcs pats)

    rulesInAnyPattern :: Set.Set Rule
    rulesInAnyPattern = Set.unions (map ptrls pats)

    cdefsInAnyPattern :: Set.Set AConceptDef
    cdefsInAnyPattern = Set.fromList (concatMap ptcds pats)

    relsNoPattern :: [Relation]
    relsNoPattern = [r | r <- rels, r `Set.notMember` relsInAnyPattern]

    rulesNoPattern :: [Rule]
    rulesNoPattern = [r | r <- rules, r `Set.notMember` rulesInAnyPattern]

    cdefsNoPattern :: [AConceptDef]
    cdefsNoPattern = [cd | cd <- cdefs, cd `Set.notMember` cdefsInAnyPattern]

    identRulesInAnyPattern :: Set.Set IdentityRule
    identRulesInAnyPattern = Set.fromList (concatMap ptids pats)

    identRulesNoPattern :: [IdentityRule]
    identRulesNoPattern =
      [i | i <- vIndices fSpec, i `Set.notMember` identRulesInAnyPattern]

    viewsInAnyPattern :: Set.Set ViewDef
    viewsInAnyPattern = Set.fromList (concatMap ptvds pats)

    viewsNoPattern :: [ViewDef]
    viewsNoPattern =
      [v | v <- vviews fSpec, v `Set.notMember` viewsInAnyPattern]

    enforcesNoPattern :: [AEnforce]
    enforcesNoPattern =
      [e | e <- allEnforces fSpec, isNothing (enfPatName e)]

    mkNoPatternDiag :: PatternDiag
    mkNoPatternDiag =
      let ownRels = Set.fromList relsNoPattern
          totalRefs = sum (map ruleComplexity rulesNoPattern)
          internalRefs = sum (map (ruleInternalRefs ownRels) rulesNoPattern)
          -- "Orphan" relation: declared in this synthetic group and
          -- not referenced by any rule or interface anywhere.
          nrOrphans =
            length [r | r <- relsNoPattern, PG.isOrphanRelation patternGraph r]
          -- "Self-contained": every EDcD-leaf in any of the rules
          -- outside a pattern refers to a relation that is itself
          -- declared outside a pattern.
          selfContained =
            all
              ( \r ->
                  Set.fromList
                    [ d | d <- Set.toList (bindedRelationsIn r), d `Set.member` ownRels
                    ]
                    == bindedRelationsIn r
              )
              rulesNoPattern
       in PatternDiag
            { pdName = noPatternLabel,
              pdSourceLoc = SourceLoc {srcFile = "", srcLine = Nothing},
              pdHasPurpose = False,
              pdNrConcepts = length cdefsNoPattern,
              pdNrRelations = length relsNoPattern,
              pdNrInvariantRules =
                length [r | r <- rulesNoPattern, not (isSignal fSpec r)],
              pdNrProcessRules =
                length [r | r <- rulesNoPattern, isSignal fSpec r],
              pdNrIdentRules = length identRulesNoPattern,
              pdNrEnforce = length enforcesNoPattern,
              pdNrViews = length viewsNoPattern,
              pdConceptsWithPurpose =
                length [cd | cd <- cdefsNoPattern, hasPurpose (acdcpt cd)],
              pdRelationsWithPurpose =
                length [r | r <- relsNoPattern, hasPurpose r],
              pdRelationsWithMeaning =
                length [r | r <- relsNoPattern, hasMeaning' r],
              pdRulesWithPurpose =
                length [r | r <- rulesNoPattern, hasPurpose r],
              pdRulesWithMeaning =
                length [r | r <- rulesNoPattern, hasMeaning' r],
              pdNrExecEngineRules =
                length
                  [ ()
                    | r <- rulesNoPattern,
                      (rol, r') <- fRoleRuls fSpec,
                      r == r',
                      fullName rol == fullName nameOfExecEngineRole
                  ],
              pdNrViolationsInitial =
                sum
                  [ Set.size ps
                    | (r, ps) <- allViolations fSpec,
                      r `Set.notMember` rulesInAnyPattern
                  ],
              pdConceptsDeclared = [fullName (acdcpt cd) | cd <- cdefsNoPattern],
              pdLargestRuleComplexity =
                foldr max 0 (map ruleComplexity rulesNoPattern),
              pdCohesionRatio =
                if totalRefs == 0
                  then 0
                  else fromIntegral internalRefs / fromIntegral totalRefs,
              pdNrOrphanRelations = nrOrphans,
              -- Process rules outside a pattern are not subject to
              -- the per-pattern reachability analysis; we leave the
              -- column at 0 for the synthetic row.
              pdNrUnreachableProcessRules = 0,
              pdSelfContained = selfContained,
              -- "No Pattern" does not participate in the cross-pattern
              -- dependency graph; the two list columns are empty by
              -- construction.
              pdDependsOnPatterns = [],
              pdDependedOnByPatterns = []
            }

    ----------------------------------------------------------------
    -- Interface rows
    ----------------------------------------------------------------
    interfaceRows :: [InterfaceDiag]
    interfaceRows = map mkInterfaceDiag ifcs

    mkInterfaceDiag :: Interface -> InterfaceDiag
    mkInterfaceDiag ifc =
      let walked = walkInterface ifc
          fieldsList = iwFields walked
          crudsList = map objcrud fieldsList
          anyCrud f = any f crudsList
          crudCov =
            T.pack
              $ [c | (c, hit) <- [('C', anyCrud crudC), ('R', anyCrud crudR), ('U', anyCrud crudU), ('D', anyCrud crudD)], hit]
          relsByCrud :: (Cruds -> Bool) -> Set.Set Relation
          relsByCrud sel =
            Set.unions
              [bindedRelationsIn (objExpression o) | o <- fieldsList, sel (objcrud o)]
          -- "Read-only" is a useful diagnosis flag: no C/U/D anywhere
          -- below the interface root means the user can only browse.
          readOnly = not (anyCrud crudC || anyCrud crudU || anyCrud crudD)
       in InterfaceDiag
            { ifdName = fullName ifc,
              ifdForRoles = map fullName (ifcRoles ifc),
              ifdSourceLoc = mkSourceLoc ifc,
              ifdHasPurpose = hasPurpose ifc,
              ifdTopLevelConcept = fullName (source (objExpression (ifcObj ifc))),
              ifdTopLevelExpression = tshow (objExpression (ifcObj ifc)),
              ifdNrBoxes = iwNrBoxes walked,
              ifdMaxNestingDepth = iwMaxDepth walked,
              ifdNrFields = length fieldsList,
              ifdCrudCoverage = crudCov,
              ifdNrRelationsWithC = Set.size (relsByCrud crudC),
              ifdNrRelationsWithU = Set.size (relsByCrud crudU),
              ifdNrRelationsWithD = Set.size (relsByCrud crudD),
              ifdReadOnly = readOnly,
              ifdConceptsCovered = map fullName (Set.toList (concs ifc)),
              ifdRelationsCovered = map fullName (Set.toList (bindedRelationsIn ifc)),
              -- TODO #1625 (G remainder): unreachable role, orphan
              -- concept fields, VIEW-usage check.
              ifdUnreachableRole = False,
              ifdOrphanConceptFields = [],
              ifdUsesViewForTop = isJust (objmView (ifcObj ifc))
            }

    ----------------------------------------------------------------
    -- Aggregated summary
    ----------------------------------------------------------------
    mkSummary :: DiagSummary
    mkSummary =
      DiagSummary
        { dsScriptName = fullName fSpec,
          dsNrPatterns = length patternRows,
          dsNrConcepts = length conceptRows,
          dsNrConceptsWithPurpose = length (filter cdHasPurpose conceptRows),
          dsNrConceptsWithDef = length (filter cdHasConceptDef conceptRows),
          dsNrRelations = length relationRows,
          dsNrRelationsWithPurpose = length (filter rdHasPurpose relationRows),
          dsNrRelationsWithMeaning = length (filter rdHasMeaning relationRows),
          dsNrRelationsUnused =
            length (filter ((== 0) . rdNrRulesReferencing) relationRows),
          dsNrRules = length ruleRows,
          dsNrRulesWithPurpose = length (filter rldHasPurpose ruleRows),
          dsNrRulesWithMeaning = length (filter rldHasMeaning ruleRows),
          dsNrInterfaces = length interfaceRows,
          dsNrInvariantViolations =
            sum [n | r <- ruleRows, rldKind r /= RKProcess, let n = rldNrViolations r],
          dsNrProcessViolations =
            sum [n | r <- ruleRows, rldKind r == RKProcess, let n = rldNrViolations r]
        }

----------------------------------------------------------------
-- Standalone helpers
----------------------------------------------------------------

mkSourceLoc :: (Traced x) => x -> SourceLoc
mkSourceLoc x =
  case origin x of
    FileLoc (FilePos pth ln _) _ ->
      SourceLoc {srcFile = T.pack pth, srcLine = Just ln}
    other -> SourceLoc {srcFile = tshow other, srcLine = Nothing}

mkRelationProps :: Relation -> RelationProps
mkRelationProps rel =
  let ps = decprps rel
      sym = Sym `Set.member` ps
      asy = Asy `Set.member` ps
   in RelationProps
        { rpUni = Uni `Set.member` ps,
          rpInj = Inj `Set.member` ps,
          rpTot = Tot `Set.member` ps,
          rpSur = Sur `Set.member` ps,
          -- PROP is syntactic sugar in the source language for SYM + ASY.
          rpProp = sym && asy,
          rpIrf = Irf `Set.member` ps,
          rpRfx = Rfx `Set.member` ps,
          rpSym = sym,
          rpAsy = asy,
          rpTrn = Trn `Set.member` ps
        }

classifyRule :: FSpec -> Rule -> RuleKindDiag
classifyRule fSpec r
  | isSignal fSpec r = RKProcess
  | otherwise = RKInvariant

----------------------------------------------------------------
-- Interface walker (G2..G5 + G6..G9)
--
-- One single descent over the interface tree collects everything we
-- need for the spreadsheet:
--   * 'iwNrBoxes'  : number of @Box@-shaped 'SubInterface' nodes,
--                    including the root (a top-level interface always
--                    has at least one Box).
--   * 'iwMaxDepth' : maximum nesting depth of @Box@ nodes
--                    (root = 1, a Box inside a Box = 2, etc.)
--   * 'iwFields'   : every 'ObjectDef' reached by the walker, useful
--                    for field-count, CRUD coverage and relation
--                    statistics filtered by CRUD flags.
----------------------------------------------------------------

data InterfaceWalk = InterfaceWalk
  { iwNrBoxes :: !Int,
    iwMaxDepth :: !Int,
    iwFields :: ![ObjectDef]
  }

emptyWalk :: InterfaceWalk
emptyWalk = InterfaceWalk {iwNrBoxes = 0, iwMaxDepth = 0, iwFields = []}

combineSiblings :: InterfaceWalk -> InterfaceWalk -> InterfaceWalk
combineSiblings a b =
  InterfaceWalk
    { iwNrBoxes = iwNrBoxes a + iwNrBoxes b,
      iwMaxDepth = max (iwMaxDepth a) (iwMaxDepth b),
      iwFields = iwFields a <> iwFields b
    }

walkInterface :: Interface -> InterfaceWalk
walkInterface ifc = walkObject 1 (ifcObj ifc)

walkObject :: Int -> ObjectDef -> InterfaceWalk
walkObject depth obj =
  let here = emptyWalk {iwFields = [obj]}
      children = case objmsub obj of
        Nothing -> emptyWalk
        Just InterfaceRef {} -> emptyWalk
        Just Box {siObjs = items} ->
          let childDepth = depth + 1
              walks = map (walkBoxItem childDepth) items
              merged = foldr combineSiblings emptyWalk walks
           in merged
                { iwNrBoxes = iwNrBoxes merged + 1, -- count this Box
                  iwMaxDepth = max childDepth (iwMaxDepth merged)
                }
   in combineSiblings here children

walkBoxItem :: Int -> BoxItem -> InterfaceWalk
walkBoxItem depth item = case item of
  BxExpr {objE = o} -> walkObject depth o
  BxText {} -> emptyWalk

-- | Count the relation-like leaves in an 'Expression'.
--
-- "Relation-like leaves" are the AST nodes that ultimately refer to a
-- pair-set in the population: 'EDcD', 'EDcI', 'EDcV', 'EBin' and
-- 'EMp1'.  Composite and unary operator nodes recurse into their
-- children; brackets are transparent.  The result is the basic
-- "term complexity" metric used for rules (F1), the largest such
-- score in a pattern (C2) and the denominator of the per-pattern
-- cohesion ratio (C6).
exprRelOccurrences :: Expression -> Int
exprRelOccurrences expr = case expr of
  EEqu (a, b) -> exprRelOccurrences a + exprRelOccurrences b
  EInc (a, b) -> exprRelOccurrences a + exprRelOccurrences b
  EIsc (a, b) -> exprRelOccurrences a + exprRelOccurrences b
  EUni (a, b) -> exprRelOccurrences a + exprRelOccurrences b
  EDif (a, b) -> exprRelOccurrences a + exprRelOccurrences b
  ELrs (a, b) -> exprRelOccurrences a + exprRelOccurrences b
  ERrs (a, b) -> exprRelOccurrences a + exprRelOccurrences b
  EDia (a, b) -> exprRelOccurrences a + exprRelOccurrences b
  ECps (a, b) -> exprRelOccurrences a + exprRelOccurrences b
  ERad (a, b) -> exprRelOccurrences a + exprRelOccurrences b
  EPrd (a, b) -> exprRelOccurrences a + exprRelOccurrences b
  EKl0 e -> exprRelOccurrences e
  EKl1 e -> exprRelOccurrences e
  EFlp e -> exprRelOccurrences e
  ECpl e -> exprRelOccurrences e
  EBrk e -> exprRelOccurrences e
  EDcD _ -> 1
  EDcI _ -> 1
  EBin _ _ -> 1
  EDcV _ -> 1
  EMp1 _ _ -> 1

-- | Count the leaves that refer to a relation declared in the given
-- set ("internal" references from the perspective of the pattern that
-- declares those relations).  Identity, V, binop and singleton
-- leaves are deliberately not counted as internal: they are
-- cross-cutting and only contribute to the denominator of the
-- cohesion ratio, never to its numerator.
exprInternalRelOccurrences :: Set.Set Relation -> Expression -> Int
exprInternalRelOccurrences ownRels = go
  where
    go expr = case expr of
      EEqu (a, b) -> go a + go b
      EInc (a, b) -> go a + go b
      EIsc (a, b) -> go a + go b
      EUni (a, b) -> go a + go b
      EDif (a, b) -> go a + go b
      ELrs (a, b) -> go a + go b
      ERrs (a, b) -> go a + go b
      EDia (a, b) -> go a + go b
      ECps (a, b) -> go a + go b
      ERad (a, b) -> go a + go b
      EPrd (a, b) -> go a + go b
      EKl0 e -> go e
      EKl1 e -> go e
      EFlp e -> go e
      ECpl e -> go e
      EBrk e -> go e
      EDcD d -> if d `Set.member` ownRels then 1 else 0
      EDcI _ -> 0
      EBin _ _ -> 0
      EDcV _ -> 0
      EMp1 _ _ -> 0
