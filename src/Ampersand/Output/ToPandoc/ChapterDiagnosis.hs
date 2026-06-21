{-# LANGUAGE ScopedTypeVariables #-}

-- | Concise Diagnosis chapter.
--
-- The detailed lists of concepts, relations, rules and interfaces that
-- previous versions inlined into the document have moved to a separate
-- spreadsheet ('Ampersand.Output.Diagnosis2Xlsx').  This module only
-- produces aggregated statistics plus references to that workbook, so
-- the chapter remains readable for real-life scripts.
--
-- All diagnostic data is supplied by the caller via 'DiagnosticData';
-- this module performs no extraction of its own.
module Ampersand.Output.ToPandoc.ChapterDiagnosis (chpDiagnosis) where

import Ampersand.Diagnosis.Types
import Ampersand.Output.ToPandoc.SharedAmongChapters
import qualified RIO.Text as T

chpDiagnosis ::
  (HasDirOutput env, HasDocumentOpts env, HasFSpecGenOpts env) =>
  env ->
  FSpec ->
  DiagnosticData ->
  (Blocks, [Picture])
chpDiagnosis env fSpec dd
  | Diagnosis `notElem` view chaptersL env = mempty
  | otherwise =
      ( xDefBlck env fSpec Diagnosis
          <> introPara
          <> overviewStats
          <> perPatternTable
          <> violationSummary
          <> xlsxReference,
        []
      )
  where
    -- localisation
    lang = outputLang env fSpec
    l :: LocalizedStr -> Text
    l = localize lang

    summary = ddSummary dd
    patternRows = ddPatterns dd

    ----------------------------------------------------------------
    -- Intro paragraph
    ----------------------------------------------------------------
    introPara :: Blocks
    introPara =
      para
        $ (str . l)
          ( NL "Dit hoofdstuk geeft een analyse van het Ampersand-script van ",
            EN "This chapter provides an analysis of the Ampersand script of "
          )
        <> (emph . singleQuoted . str . fullName) fSpec
        <> str ". "
        <> (str . l)
          ( NL "Detailgegevens (per patroon, concept, relatie, regel en interface) staan in het bijbehorende spreadsheet ",
            EN "Per-pattern, per-concept, per-relation, per-rule and per-interface details have been moved to the accompanying spreadsheet "
          )
        <> code xlsxFilename
        <> str ". "
        <> (str . l)
          ( NL "Hieronder vindt u alleen de samenvatting.",
            EN "This chapter shows only the summary."
          )

    ----------------------------------------------------------------
    -- Aggregate statistics
    ----------------------------------------------------------------
    overviewStats :: Blocks
    overviewStats =
      bulletList
        [ plain
            $ (strong . str . l) (NL "Patronen", EN "Patterns")
            <> str ": "
            <> (str . tshow . dsNrPatterns) summary
            <> dot,
          plain
            $ (strong . str . l) (NL "Concepten", EN "Concepts")
            <> str ": "
            <> (str . tshow . dsNrConcepts) summary
            <> str ", "
            <> (str . l) (NL "met definitie", EN "with CONCEPT definition")
            <> str ": "
            <> (str $ percentage (dsNrConcepts summary) (dsNrConceptsWithDef summary))
            <> str ", "
            <> (str . l) (NL "met PURPOSE", EN "with PURPOSE")
            <> str ": "
            <> (str $ percentage (dsNrConcepts summary) (dsNrConceptsWithPurpose summary))
            <> dot,
          plain
            $ (strong . str . l) (NL "Relaties", EN "Relations")
            <> str ": "
            <> (str . tshow . dsNrRelations) summary
            <> str ", "
            <> (str . l) (NL "met PURPOSE", EN "with PURPOSE")
            <> str ": "
            <> (str $ percentage (dsNrRelations summary) (dsNrRelationsWithPurpose summary))
            <> str ", "
            <> (str . l) (NL "met MEANING", EN "with MEANING")
            <> str ": "
            <> (str $ percentage (dsNrRelations summary) (dsNrRelationsWithMeaning summary))
            <> str ", "
            <> (str . l) (NL "ongebruikt in regels", EN "unused in any rule")
            <> str ": "
            <> (str . tshow . dsNrRelationsUnused) summary
            <> dot,
          plain
            $ (strong . str . l) (NL "Regels", EN "Rules")
            <> str ": "
            <> (str . tshow . dsNrRules) summary
            <> str ", "
            <> (str . l) (NL "met PURPOSE", EN "with PURPOSE")
            <> str ": "
            <> (str $ percentage (dsNrRules summary) (dsNrRulesWithPurpose summary))
            <> str ", "
            <> (str . l) (NL "met MEANING", EN "with MEANING")
            <> str ": "
            <> (str $ percentage (dsNrRules summary) (dsNrRulesWithMeaning summary))
            <> dot,
          plain
            $ (strong . str . l) (NL "Interfaces", EN "Interfaces")
            <> str ": "
            <> (str . tshow . dsNrInterfaces) summary
            <> dot
        ]
      where
        dot = str "."

    ----------------------------------------------------------------
    -- Per-pattern overview table (A1/A2 from the follow-up checklist)
    --
    -- Uses `simpleTable` (pandoc-builder) so that the markdown writer
    -- renders it as a pipe table instead of the dreaded [TABLE]
    -- placeholder.  Columns mandated by the user:
    --   Pattern, #concepts, #relations, #invariant rules, #process rules,
    --   % concepts w/ PURPOSE, % rels w/ PURPOSE, % rels w/ MEANING,
    --   % rules w/ PURPOSE, % rules w/ MEANING
    ----------------------------------------------------------------
    perPatternTable :: Blocks
    perPatternTable
      | null patternRows = mempty
      | otherwise =
          para
            ( (str . l)
                ( NL "Onderstaande tabel toont per patroon de aantallen en documentatiegraad. Voor details, zie sheet ",
                  EN "The table below shows, per pattern, the counts and documentation coverage. For details, see sheet "
                )
                <> code "Patterns"
                <> (str . l) (NL " van ", EN " of ")
                <> code xlsxFilename
                <> str "."
            )
            <> simpleTable
              ( map
                  (plain . str . l)
                  [ (NL "Patroon", EN "Pattern"),
                    (NL "#concepten", EN "#concepts"),
                    (NL "#relaties", EN "#relations"),
                    (NL "#invariant", EN "#invariant rules"),
                    (NL "#process", EN "#process rules"),
                    (NL "% concepten PURPOSE", EN "% concepts PURPOSE"),
                    (NL "% rel. PURPOSE", EN "% rels PURPOSE"),
                    (NL "% rel. MEANING", EN "% rels MEANING"),
                    (NL "% regels PURPOSE", EN "% rules PURPOSE"),
                    (NL "% regels MEANING", EN "% rules MEANING")
                  ]
              )
              [ [ (plain . str) (pdName p),
                  (plain . str . tshow) (pdNrConcepts p),
                  (plain . str . tshow) (pdNrRelations p),
                  (plain . str . tshow) (pdNrInvariantRules p),
                  (plain . str . tshow) (pdNrProcessRules p),
                  (plain . str) (percentage (pdNrConcepts p) (pdConceptsWithPurpose p)),
                  (plain . str) (percentage (pdNrRelations p) (pdRelationsWithPurpose p)),
                  (plain . str) (percentage (pdNrRelations p) (pdRelationsWithMeaning p)),
                  (plain . str) (percentage (totalRulesIn p) (pdRulesWithPurpose p)),
                  (plain . str) (percentage (totalRulesIn p) (pdRulesWithMeaning p))
                ]
                | p <- patternRows
              ]

    totalRulesIn :: PatternDiag -> Int
    totalRulesIn p = pdNrInvariantRules p + pdNrProcessRules p

    ----------------------------------------------------------------
    -- Violation summary (A4: split into two sentences and use the
    -- term "open signals" for process rules)
    ----------------------------------------------------------------
    violationSummary :: Blocks
    violationSummary =
      para
        ( (str . l)
            ( NL "De populatie in dit script overtreedt ",
              EN "The population in this script violates "
            )
            <> (str . tshow . dsNrInvariantViolations) summary
            <> (str . l)
              ( NL $ " invariant" <> plurNl (dsNrInvariantViolations summary) <> ". ",
                EN $ " invariant" <> plurEn (dsNrInvariantViolations summary) <> ". "
              )
            <> (str . l)
              ( NL "Daarnaast staan er ",
                EN "There are "
              )
            <> (str . tshow . dsNrProcessViolations) summary
            <> (str . l)
              ( NL $ " open signal" <> plurNl (dsNrProcessViolations summary) <> " voor procesregels open. ",
                EN $ " open signal" <> plurEn (dsNrProcessViolations summary) <> " for process rules. "
              )
            <> (str . l)
              ( NL "Per-regel aantallen staan in sheet ",
                EN "Per-rule counts can be found in sheet "
              )
            <> code "Rules"
            <> (str . l) (NL " van ", EN " of ")
            <> code xlsxFilename
            <> str "."
        )

    plurNl, plurEn :: Int -> Text
    plurNl 1 = ""
    plurNl _ = "en"
    plurEn 1 = ""
    plurEn _ = "s"

    ----------------------------------------------------------------
    -- Spreadsheet reference (A3: use real inline code instead of
    -- backticks inside plain text, so the markdown writer does not
    -- escape them).
    ----------------------------------------------------------------
    xlsxReference :: Blocks
    xlsxReference =
      para
        ( (str . l)
            ( NL "Het spreadsheet ",
              EN "The spreadsheet "
            )
            <> code xlsxFilename
            <> (str . l)
              ( NL " bevat vijf werkbladen: ",
                EN " contains five worksheets: "
              )
            <> code "Patterns"
            <> str ", "
            <> code "Concepts"
            <> str ", "
            <> code "Relations"
            <> str ", "
            <> code "Rules"
            <> (str . l) (NL " en ", EN " and ")
            <> code "Interfaces"
            <> str ". "
            <> (str . l)
              ( NL "Sorteer, filter of pivot daar de gegevens naar wens.",
                EN "Sort, filter or pivot the data there as you wish."
              )
        )

    xlsxFilename :: Text
    xlsxFilename = T.pack (baseName env) <> "-diagnosis.xlsx"
