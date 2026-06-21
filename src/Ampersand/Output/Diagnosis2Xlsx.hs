-- | Render 'DiagnosticData' as an @.xlsx@ workbook with one sheet per
-- diagnostic category.
--
-- This module is the *only* place where 'DiagnosticData' meets
-- @Codec.Xlsx@.  Keeping the conversion separate from the data
-- extraction (see "Ampersand.Diagnosis.Extract") allows both halves to
-- evolve independently, and gives users a clean point to plug in a
-- different output format should they ever want to.
--
-- Column conventions used throughout the sheets:
--   * Text and numeric columns come first.
--   * Boolean columns are rendered with a green tick mark (✓) for
--     True and an empty cell for False, and are grouped on the
--     right-hand side of every sheet so users can scan the data and
--     the flags separately (follow-up checklist item A5).
--   * Numeric columns never produce a 'None' / empty value: missing
--     source-line information is rendered as 0 (item B4).
--
-- See the documentation block on "Ampersand.Diagnosis.Extract" for the
-- current status of the per-row metrics: this sheet always renders a
-- column even when its metric is still stubbed in the extractor.
module Ampersand.Output.Diagnosis2Xlsx
  ( diagnostics2Xlsx,
  )
where

import Ampersand.Basics
import Ampersand.Diagnosis.Types
import Codec.Xlsx
import Data.Time.Clock.POSIX (POSIXTime)
import qualified RIO.ByteString.Lazy as BL

-- | Convert 'DiagnosticData' into the bytes of an @.xlsx@ workbook.
diagnostics2Xlsx :: POSIXTime -> DiagnosticData -> BL.ByteString
diagnostics2Xlsx ct dd =
  fromXlsx ct (def {_xlSheets = sheets})
  where
    sheets =
      [ ("Patterns", patternsSheet (ddPatterns dd)),
        ("Concepts", conceptsSheet (ddConcepts dd)),
        ("Relations", relationsSheet (ddRelations dd)),
        ("Rules", rulesSheet (ddRules dd)),
        ("Interfaces", interfacesSheet (ddInterfaces dd))
      ]

----------------------------------------------------------------
-- Sheet builders
----------------------------------------------------------------

patternsSheet :: [PatternDiag] -> Worksheet
patternsSheet rows = mkWorksheet headers (map toRow rows)
  where
    headers =
      [ "Pattern",
        "Source file",
        "Source line",
        "#concepts",
        "#relations",
        "#invariant rules",
        "#process rules",
        "#IDENT rules",
        "#ENFORCE rules",
        "#views",
        "Concepts with PURPOSE",
        "Relations with PURPOSE",
        "Relations with MEANING",
        "Rules with PURPOSE",
        "Rules with MEANING",
        "% concepts with PURPOSE",
        "% relations with PURPOSE",
        "% relations with MEANING",
        "% rules with PURPOSE",
        "% rules with MEANING",
        "#ExecEngine rules",
        "Largest rule term complexity",
        "#orphan relations",
        "#unreachable process rules",
        "#violations in initial population",
        "Cohesion ratio",
        "Concepts declared",
        "Depends on patterns",
        "Depended on by patterns",
        -- Boolean columns on the right
        "Has PURPOSE",
        "Self-contained"
      ]
    toRow p =
      [ textCell (pdName p),
        textCell (srcFile (pdSourceLoc p)),
        intCellFrom0 (srcLine (pdSourceLoc p)),
        intCell (pdNrConcepts p),
        intCell (pdNrRelations p),
        intCell (pdNrInvariantRules p),
        intCell (pdNrProcessRules p),
        intCell (pdNrIdentRules p),
        intCell (pdNrEnforce p),
        intCell (pdNrViews p),
        intCell (pdConceptsWithPurpose p),
        intCell (pdRelationsWithPurpose p),
        intCell (pdRelationsWithMeaning p),
        intCell (pdRulesWithPurpose p),
        intCell (pdRulesWithMeaning p),
        textCell (percentage (pdNrConcepts p) (pdConceptsWithPurpose p)),
        textCell (percentage (pdNrRelations p) (pdRelationsWithPurpose p)),
        textCell (percentage (pdNrRelations p) (pdRelationsWithMeaning p)),
        textCell
          ( percentage
              (pdNrInvariantRules p + pdNrProcessRules p)
              (pdRulesWithPurpose p)
          ),
        textCell
          ( percentage
              (pdNrInvariantRules p + pdNrProcessRules p)
              (pdRulesWithMeaning p)
          ),
        intCell (pdNrExecEngineRules p),
        intCell (pdLargestRuleComplexity p),
        intCell (pdNrOrphanRelations p),
        intCell (pdNrUnreachableProcessRules p),
        intCell (pdNrViolationsInitial p),
        doubleCell (pdCohesionRatio p),
        textCell (commaSep (pdConceptsDeclared p)),
        textCell (commaSep (pdDependsOnPatterns p)),
        textCell (commaSep (pdDependedOnByPatterns p)),
        tickCell (pdHasPurpose p),
        tickCell (pdSelfContained p)
      ]

conceptsSheet :: [ConceptDiag] -> Worksheet
conceptsSheet rows = mkWorksheet headers (map toRow rows)
  where
    headers =
      [ "Concept",
        "Defined in pattern",
        "Source file",
        "Source line",
        "Representation",
        "#relations as source",
        "#relations as target",
        "Generalisation depth",
        "#specialisations",
        "#generalisations",
        "#rules referencing",
        "#interfaces referencing",
        "Patterns using",
        "#atoms in initial population",
        "Population source",
        "Suspect synonyms",
        -- Boolean columns on the right
        "Has CONCEPT definition",
        "Has PURPOSE",
        "Has MEANING",
        "In CLASSIFY cycle",
        "Has IDENT rule",
        "Has VIEW"
      ]
    toRow c =
      [ textCell (cdName c),
        textCell (fromMaybe "" (cdDefinedInPattern c)),
        textCell (srcFile (cdSourceLoc c)),
        intCellFrom0 (srcLine (cdSourceLoc c)),
        textCell (cdRepresentation c),
        intCell (cdNrRelationsAsSource c),
        intCell (cdNrRelationsAsTarget c),
        intCell (cdGeneralisationDepth c),
        intCell (cdNrSpecialisations c),
        intCell (cdNrGeneralisations c),
        intCell (cdNrRulesReferencing c),
        intCell (cdNrInterfacesReferencing c),
        textCell (commaSep (cdPatternsUsing c)),
        intCell (cdNrAtomsInitial c),
        textCell (cdPopulationSource c),
        textCell (commaSep (cdSuspectSynonyms c)),
        tickCell (cdHasConceptDef c),
        tickCell (cdHasPurpose c),
        tickCell (cdHasMeaning c),
        tickCell (cdInClassifyCycle c),
        tickCell (cdHasIdentRule c),
        tickCell (cdHasView c)
      ]

relationsSheet :: [RelationDiag] -> Worksheet
relationsSheet rows = mkWorksheet headers (map toRow rows)
  where
    headers =
      [ "Relation",
        "Signature",
        "Defined in pattern",
        "Source file",
        "Source line",
        "#rules referencing",
        "#interfaces referencing",
        "#views referencing",
        "#IDENT rules referencing",
        "Computed by ENFORCE",
        "#pairs in initial population",
        "Population source",
        "Suspect duplicates",
        -- Boolean columns grouped on the right
        "Has PURPOSE",
        "Has MEANING",
        "UNI",
        "INJ",
        "TOT",
        "SUR",
        "PROP",
        "IRF",
        "RFX",
        "SYM",
        "ASY",
        "TRN"
      ]
    toRow r =
      [ textCell (rdName r),
        textCell (rdSignature r),
        textCell (fromMaybe "" (rdDefinedInPattern r)),
        textCell (srcFile (rdSourceLoc r)),
        intCellFrom0 (srcLine (rdSourceLoc r)),
        intCell (rdNrRulesReferencing r),
        intCell (rdNrInterfacesReferencing r),
        intCell (rdNrViewsReferencing r),
        intCell (rdNrIdentRulesReferencing r),
        textCell (rdComputedByEnforce r),
        intCell (rdNrPairsInitial r),
        textCell (rdPopulationSource r),
        textCell (commaSep (rdSuspectDuplicates r)),
        tickCell (rdHasPurpose r),
        tickCell (rdHasMeaning r),
        tickCell (rpUni (rdProps r)),
        tickCell (rpInj (rdProps r)),
        tickCell (rpTot (rdProps r)),
        tickCell (rpSur (rdProps r)),
        tickCell (rpProp (rdProps r)),
        tickCell (rpIrf (rdProps r)),
        tickCell (rpRfx (rdProps r)),
        tickCell (rpSym (rdProps r)),
        tickCell (rpAsy (rdProps r)),
        tickCell (rpTrn (rdProps r))
      ]

rulesSheet :: [RuleDiag] -> Worksheet
rulesSheet rows = mkWorksheet headers (map toRow rows)
  where
    headers =
      [ "Rule",
        "Defined in pattern",
        "Source file",
        "Source line",
        "Kind",
        "Maintained by roles",
        "Signature",
        "#violations",
        "Term complexity",
        "#relations referenced",
        "#concepts referenced",
        "Sample violations",
        -- Boolean columns on the right
        "Has PURPOSE",
        "Has MEANING",
        "Has MESSAGE",
        "Has VIOLATION text",
        "Unreachable"
      ]
    toRow r =
      [ textCell (rldName r),
        textCell (fromMaybe "" (rldDefinedInPattern r)),
        textCell (srcFile (rldSourceLoc r)),
        intCellFrom0 (srcLine (rldSourceLoc r)),
        textCell (showRuleKind (rldKind r)),
        textCell (commaSep (rldMaintainedByRoles r)),
        textCell (rldSignature r),
        intCell (rldNrViolations r),
        intCell (rldTermComplexity r),
        intCell (rldNrRelationsReferenced r),
        intCell (rldNrConceptsReferenced r),
        textCell (commaSep (rldSampleViolations r)),
        tickCell (rldHasPurpose r),
        tickCell (rldHasMeaning r),
        tickCell (rldHasMessage r),
        tickCell (rldHasViolation r),
        tickCell (rldUnreachable r)
      ]

interfacesSheet :: [InterfaceDiag] -> Worksheet
interfacesSheet rows = mkWorksheet headers (map toRow rows)
  where
    headers =
      [ "Interface",
        "For roles",
        "Source file",
        "Source line",
        "Top-level concept",
        "Top-level expression",
        "#boxes",
        "Max nesting depth",
        "#fields",
        "CRUD coverage",
        "#relations with C",
        "#relations with U",
        "#relations with D",
        "Concepts covered",
        "Relations covered",
        "Orphan concept fields",
        -- Boolean columns on the right
        "Has PURPOSE",
        "Read-only",
        "Unreachable role",
        "Uses VIEW for top concept"
      ]
    toRow i =
      [ textCell (ifdName i),
        textCell (commaSep (ifdForRoles i)),
        textCell (srcFile (ifdSourceLoc i)),
        intCellFrom0 (srcLine (ifdSourceLoc i)),
        textCell (ifdTopLevelConcept i),
        textCell (ifdTopLevelExpression i),
        intCell (ifdNrBoxes i),
        intCell (ifdMaxNestingDepth i),
        intCell (ifdNrFields i),
        textCell (ifdCrudCoverage i),
        intCell (ifdNrRelationsWithC i),
        intCell (ifdNrRelationsWithU i),
        intCell (ifdNrRelationsWithD i),
        textCell (commaSep (ifdConceptsCovered i)),
        textCell (commaSep (ifdRelationsCovered i)),
        textCell (commaSep (ifdOrphanConceptFields i)),
        tickCell (ifdHasPurpose i),
        tickCell (ifdReadOnly i),
        tickCell (ifdUnreachableRole i),
        tickCell (ifdUsesViewForTop i)
      ]

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Build a 'Worksheet' from a header row plus data rows of 'Cell's.
mkWorksheet :: [Text] -> [[Cell]] -> Worksheet
mkWorksheet headers dataRows =
  def {_wsCells = fromRows indexedRows}
  where
    headerRow = map textCell headers
    indexedRows =
      zip [RowIndex 1 ..] (map indexCols (headerRow : dataRows))
    indexCols :: [Cell] -> [(ColumnIndex, Cell)]
    indexCols = zip [ColumnIndex 1 ..]

textCell :: Text -> Cell
textCell t =
  Cell
    { _cellStyle = Nothing,
      _cellValue = Just (CellText t),
      _cellComment = Nothing,
      _cellFormula = Nothing
    }

intCell :: Int -> Cell
intCell n =
  Cell
    { _cellStyle = Nothing,
      _cellValue = Just (CellDouble (fromIntegral n)),
      _cellComment = Nothing,
      _cellFormula = Nothing
    }

doubleCell :: Double -> Cell
doubleCell d =
  Cell
    { _cellStyle = Nothing,
      _cellValue = Just (CellDouble d),
      _cellComment = Nothing,
      _cellFormula = Nothing
    }

-- | B4: a numeric column never shows 'None' or an empty cell; treat
-- a missing value as 0 so the sheet is sortable and pivottable.
intCellFrom0 :: Maybe Int -> Cell
intCellFrom0 = intCell . fromMaybe 0

-- | A5: render boolean values as a green tick mark for True and an
-- empty cell for False, so the columns can be made narrow.
tickCell :: Bool -> Cell
tickCell True = textCell "✓"
tickCell False =
  Cell
    { _cellStyle = Nothing,
      _cellValue = Nothing,
      _cellComment = Nothing,
      _cellFormula = Nothing
    }

showRuleKind :: RuleKindDiag -> Text
showRuleKind RKInvariant = "invariant"
showRuleKind RKProcess = "process"
showRuleKind RKEnforce = "ENFORCE"
showRuleKind RKIdent = "IDENT"
showRuleKind RKPropertyRule = "property"

commaSep :: [Text] -> Text
commaSep [] = ""
commaSep [x] = x
commaSep (x : xs) = x <> ", " <> commaSep xs
