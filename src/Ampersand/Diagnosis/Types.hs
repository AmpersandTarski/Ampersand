-- | Pure datatypes that capture the diagnostic view of an Ampersand script.
--
-- This module is the contract between the data-extraction logic
-- (see "Ampersand.Diagnosis.Extract") and the various renderers
-- (Pandoc summary, xlsx workbook).  It deliberately holds no Pandoc, no
-- xlsx and no IO so it can be tested and inspected in isolation.
--
-- The record layout is intentionally flat: one record per diagnostic
-- category, with primitive field types ('Int', 'Text', 'Bool', '[Text]').
-- That keeps the renderers trivial and lets us evolve the extraction
-- and presentation halves independently.
module Ampersand.Diagnosis.Types
  ( DiagnosticData (..),
    DiagSummary (..),
    PatternDiag (..),
    ConceptDiag (..),
    RelationDiag (..),
    RuleDiag (..),
    InterfaceDiag (..),
    RuleKindDiag (..),
    RelationProps (..),
    SourceLoc (..),
    emptyRelationProps,
    percentage,
    percentRatio,
  )
where

import Ampersand.Basics

-- | A textual file:line marker.  Kept as a single 'Text' so renderers do
-- not need to know about the source-position type.
data SourceLoc = SourceLoc
  { srcFile :: !Text,
    srcLine :: !(Maybe Int)
  }
  deriving (Show, Eq)

-- | Boolean properties of a relation, mirroring the algebraic
-- properties that an Ampersand modeller cares about during diagnosis.
data RelationProps = RelationProps
  { rpUni :: !Bool,
    rpInj :: !Bool,
    rpTot :: !Bool,
    rpSur :: !Bool,
    rpProp :: !Bool,
    rpIrf :: !Bool,
    rpRfx :: !Bool,
    rpSym :: !Bool,
    rpAsy :: !Bool,
    rpTrn :: !Bool
  }
  deriving (Show, Eq)

emptyRelationProps :: RelationProps
emptyRelationProps =
  RelationProps
    { rpUni = False,
      rpInj = False,
      rpTot = False,
      rpSur = False,
      rpProp = False,
      rpIrf = False,
      rpRfx = False,
      rpSym = False,
      rpAsy = False,
      rpTrn = False
    }

-- | Categorisation of a rule that the modeller sees in the diagnosis.
data RuleKindDiag
  = RKInvariant
  | RKProcess
  | RKEnforce
  | RKIdent
  | RKPropertyRule
  deriving (Show, Eq)

----------------------------------------------------------------
-- Per-pattern diagnostic row.
--
-- The fields are ordered as they appear in the spreadsheet, with
-- documentation-coverage percentages computed by the renderer from
-- the count fields below.
----------------------------------------------------------------
data PatternDiag = PatternDiag
  { pdName :: !Text,
    pdSourceLoc :: !SourceLoc,
    pdHasPurpose :: !Bool,
    pdNrConcepts :: !Int,
    pdNrRelations :: !Int,
    pdNrInvariantRules :: !Int,
    pdNrProcessRules :: !Int,
    pdNrIdentRules :: !Int,
    pdNrEnforce :: !Int,
    pdNrViews :: !Int,
    pdConceptsWithPurpose :: !Int,
    pdRelationsWithPurpose :: !Int,
    pdRelationsWithMeaning :: !Int,
    pdRulesWithPurpose :: !Int,
    pdRulesWithMeaning :: !Int,
    -- C1..C10 (follow-up checklist):
    pdNrExecEngineRules :: !Int,
    pdLargestRuleComplexity :: !Int,
    pdNrOrphanRelations :: !Int,
    pdNrUnreachableProcessRules :: !Int,
    pdNrViolationsInitial :: !Int,
    pdCohesionRatio :: !Double,
    pdSelfContained :: !Bool,
    pdDependsOnPatterns :: ![Text],
    pdDependedOnByPatterns :: ![Text],
    pdConceptsDeclared :: ![Text]
  }
  deriving (Show, Eq)

----------------------------------------------------------------
-- Per-concept diagnostic row.
----------------------------------------------------------------
data ConceptDiag = ConceptDiag
  { cdName :: !Text,
    cdDefinedInPattern :: !(Maybe Text),
    cdSourceLoc :: !SourceLoc,
    cdHasConceptDef :: !Bool,
    cdHasPurpose :: !Bool,
    cdHasMeaning :: !Bool,
    cdRepresentation :: !Text,
    cdNrRelationsAsSource :: !Int,
    cdNrRelationsAsTarget :: !Int,
    -- D1..D12 (follow-up checklist):
    cdGeneralisationDepth :: !Int,
    cdNrSpecialisations :: !Int,
    cdNrGeneralisations :: !Int,
    cdInClassifyCycle :: !Bool,
    cdHasIdentRule :: !Bool,
    cdHasView :: !Bool,
    cdNrRulesReferencing :: !Int,
    cdNrInterfacesReferencing :: !Int,
    cdPatternsUsing :: ![Text],
    cdNrAtomsInitial :: !Int,
    cdPopulationSource :: !Text,
    cdSuspectSynonyms :: ![Text]
  }
  deriving (Show, Eq)

----------------------------------------------------------------
-- Per-relation diagnostic row.
----------------------------------------------------------------
data RelationDiag = RelationDiag
  { rdName :: !Text,
    rdSignature :: !Text,
    rdDefinedInPattern :: !(Maybe Text),
    rdSourceLoc :: !SourceLoc,
    rdHasPurpose :: !Bool,
    rdHasMeaning :: !Bool,
    rdProps :: !RelationProps,
    rdNrRulesReferencing :: !Int,
    -- E1..E7 (follow-up checklist; E8 removes the boolean "Used in any rule"):
    rdNrInterfacesReferencing :: !Int,
    rdNrViewsReferencing :: !Int,
    rdNrIdentRulesReferencing :: !Int,
    rdComputedByEnforce :: !Text,
    rdNrPairsInitial :: !Int,
    rdPopulationSource :: !Text,
    rdSuspectDuplicates :: ![Text]
  }
  deriving (Show, Eq)

----------------------------------------------------------------
-- Per-rule diagnostic row.
----------------------------------------------------------------
data RuleDiag = RuleDiag
  { rldName :: !Text,
    rldDefinedInPattern :: !(Maybe Text),
    rldSourceLoc :: !SourceLoc,
    rldKind :: !RuleKindDiag,
    rldMaintainedByRoles :: ![Text],
    rldHasPurpose :: !Bool,
    rldHasMeaning :: !Bool,
    rldHasMessage :: !Bool,
    rldHasViolation :: !Bool,
    rldSignature :: !Text,
    rldNrViolations :: !Int,
    -- F1..F5 (follow-up checklist):
    rldTermComplexity :: !Int,
    rldNrRelationsReferenced :: !Int,
    rldNrConceptsReferenced :: !Int,
    rldSampleViolations :: ![Text],
    rldUnreachable :: !Bool
  }
  deriving (Show, Eq)

----------------------------------------------------------------
-- Per-interface diagnostic row.
----------------------------------------------------------------
data InterfaceDiag = InterfaceDiag
  { ifdName :: !Text,
    ifdForRoles :: ![Text],
    ifdSourceLoc :: !SourceLoc,
    ifdHasPurpose :: !Bool,
    ifdTopLevelConcept :: !Text,
    -- G1..G14 (follow-up checklist):
    ifdTopLevelExpression :: !Text,
    ifdNrBoxes :: !Int,
    ifdMaxNestingDepth :: !Int,
    ifdNrFields :: !Int,
    ifdCrudCoverage :: !Text,
    ifdNrRelationsWithC :: !Int,
    ifdNrRelationsWithU :: !Int,
    ifdNrRelationsWithD :: !Int,
    ifdReadOnly :: !Bool,
    ifdConceptsCovered :: ![Text],
    ifdRelationsCovered :: ![Text],
    ifdUnreachableRole :: !Bool,
    ifdOrphanConceptFields :: ![Text],
    ifdUsesViewForTop :: !Bool
  }
  deriving (Show, Eq)

----------------------------------------------------------------
-- Aggregated statistics for the (concise) Pandoc chapter.
----------------------------------------------------------------
data DiagSummary = DiagSummary
  { dsScriptName :: !Text,
    dsNrPatterns :: !Int,
    dsNrConcepts :: !Int,
    dsNrConceptsWithPurpose :: !Int,
    dsNrConceptsWithDef :: !Int,
    dsNrRelations :: !Int,
    dsNrRelationsWithPurpose :: !Int,
    dsNrRelationsWithMeaning :: !Int,
    dsNrRelationsUnused :: !Int,
    dsNrRules :: !Int,
    dsNrRulesWithPurpose :: !Int,
    dsNrRulesWithMeaning :: !Int,
    dsNrInterfaces :: !Int,
    dsNrInvariantViolations :: !Int,
    dsNrProcessViolations :: !Int
  }
  deriving (Show, Eq)

-- | Container with everything the renderers need.
data DiagnosticData = DiagnosticData
  { ddSummary :: !DiagSummary,
    ddPatterns :: ![PatternDiag],
    ddConcepts :: ![ConceptDiag],
    ddRelations :: ![RelationDiag],
    ddRules :: ![RuleDiag],
    ddInterfaces :: ![InterfaceDiag]
  }
  deriving (Show, Eq)

-- | Render @"y/x (n%)"@ or @"-"@ when @x == 0@.  Centralised here so
-- renderers stay simple.
percentage :: Int -> Int -> Text
percentage total count'
  | total <= 0 = "-"
  | otherwise = tshow count' <> "/" <> tshow total <> " (" <> tshow (count' * 100 `div` total) <> "%)"

-- | Plain percentage as a 'Double' between 0 and 100, used by the
-- xlsx columns that want a sortable numeric value rather than a
-- formatted string.  Returns 0 when the denominator is zero.
percentRatio :: Int -> Int -> Double
percentRatio total count'
  | total <= 0 = 0
  | otherwise = fromIntegral count' * 100.0 / fromIntegral total
