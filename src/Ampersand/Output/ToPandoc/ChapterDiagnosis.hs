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
import Ampersand.FSpec.Oscillation (OscEdge (..), OscillationCycle (..))
import Ampersand.Output.ToPandoc.SharedAmongChapters
import RIO.FilePath (takeFileName)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
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
          <> oscillationBlocks
          <> xlsxReference,
        oscillationPics
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
    -- Oscillation analysis: one signed triggering-graph diagram per
    -- risky cycle of automated rules, plus reading instructions.
    -- The data comes from 'Ampersand.FSpec.Oscillation' (via
    -- 'ddOscillations'); the diagram design is documented in
    -- docs/ongoing-research/visualizing-oscillation-cycles.md.
    ----------------------------------------------------------------
    cycles' :: [OscillationCycle]
    cycles' = ddOscillations dd

    oscillationPics :: [Picture]
    oscillationPics =
      [ makePicture env fSpec (PTOscillationCycle i oc)
        | (i, oc) <- zip [1 ..] cycles'
      ]

    oscillationBlocks :: Blocks
    oscillationBlocks =
      header
        2
        ((str . l) (NL "Oscillatie-analyse", EN "Oscillation analysis"))
        <> ( if null cycles'
               then noRiskPara
               else riskIntro <> mconcat (zipWith perCycle cycles' oscillationPics)
           )
      where
        noRiskPara =
          para
            $ (str . l)
              ( NL "De statische analyse van de geautomatiseerde regels (onderhouden door de ExecEngine) vindt geen cyclus waarin herstelacties elkaar tegenwerken. Er is geen oscillatierisico gevonden.",
                EN "The static analysis of the automated rules (maintained by the ExecEngine) finds no cycle in which repair actions oppose each other. No oscillation risk was found."
              )
        riskIntro =
          para
            ( (str . l)
                ( NL "De geautomatiseerde regels bevatten ",
                  EN "The automated rules contain "
                )
                <> (str . tshow . length) cycles'
                <> (str . l)
                  ( NL " groep(en) regels die elkaar via hun herstelacties kunnen blijven hertriggeren. Elke figuur toont één zo'n groep. Een pijl van regel A naar regel B betekent dat een herstelactie van A nieuwe overtredingen van B kan veroorzaken; het label noemt de relatie die daarbij geschreven wordt, met ",
                    EN " group(s) of rules that can keep re-triggering each other through their repair actions. Each figure shows one such group. An arrow from rule A to rule B means that a repair action of A can create new violations of B; the label names the relation being written, with "
                  )
                <> code "+"
                <> (str . l)
                  ( NL " voor toevoegen en ",
                    EN " for inserting and "
                  )
                <> code "-"
                <> (str . l)
                  ( NL " voor verwijderen of samenvoegen. De zware gestreepte pijl markeert de verwijdering die de cyclus niet-monotoon maakt; daar kan een oscillatie ontstaan.",
                    EN " for deleting or merging. The heavy dashed arrow marks the deletion that makes the cycle non-monotone; that is where an oscillation can arise."
                  )
            )
        perCycle :: OscillationCycle -> Picture -> Blocks
        perCycle oc pict =
          xDefBlck env fSpec pict
            <> para ((str . l) (NL "De betrokken regels zijn:", EN "The rules involved are:"))
            <> bulletList (map ruleItem (NE.toList (ocRules oc)))
            <> para
              ( (str . l)
                  ( NL "Waarom kan dit oscilleren? In deze cyclus werken herstelacties op dezelfde relatie in tegengestelde richting:",
                    EN "Why can this oscillate? In this cycle, repair actions work on the same relation in opposite directions:"
                  )
              )
            <> bulletList (map collisionItem (ocCollisions oc))
            <> para
              ( (str . l)
                  ( NL "Elke toevoeging kan de regel achter de verwijdering opnieuw in overtreding brengen, en omgekeerd; de ExecEngine blijft dan herstellen tot hij afbreekt met \"Maximum reruns exceeded\". Of dat werkelijk gebeurt, hangt van de populatie af; deze analyse kan het niet uitsluiten. Doorbreek de cyclus door één van de herstelacties zó aan te passen dat toevoegingen en verwijderingen elkaar niet meer kunnen raken; zie de ",
                    EN "Every insert can put the rule behind the delete back in violation, and vice versa; the ExecEngine then keeps repairing until it aborts with \"Maximum reruns exceeded\". Whether that actually happens depends on the population; this analysis cannot rule it out. Break the cycle by changing one of the repair actions so that inserts and deletes can no longer meet; see the "
                  )
                  <> link
                    "https://ampersandtarski.github.io/ampersand/guides/oscillations"
                    ""
                    ((str . l) (NL "oscillatiegids", EN "oscillation guide"))
                  <> str "."
              )
          where
            -- One bullet per rule: its (generated) name, a file:line hyperlink to
            -- the Ampersand source, and the rule's term in Ampersand syntax.
            ruleItem :: Rule -> Blocks
            ruleItem r =
              plain
                $ ruleName r
                <> str " ("
                <> sourceLink r
                <> str "): "
                <> (code . showA . formalExpression) r
            ruleName :: Rule -> Inlines
            ruleName r = case rrkind r of
              Enforce -> case enforcedRel r of
                Just d -> code ("ENFORCE " <> label d)
                Nothing -> (str . l) (NL "ENFORCE-regel", EN "ENFORCE rule")
              _ -> code (label r)
            -- The relation an ENFORCE-generated rule maintains; its formal
            -- expression is 'subExpr |- rel' (InsPair) or 'rel |- subExpr'
            -- (DelPair) by construction.
            enforcedRel :: Rule -> Maybe Relation
            enforcedRel r = case formalExpression r of
              EInc (_, EDcD d) -> Just d
              EInc (EDcD d, _) -> Just d
              _ -> Nothing
            sourceLink :: Rule -> Inlines
            sourceLink r =
              link
                ("file://" <> T.pack (filenm r))
                ""
                (str (T.pack (takeFileName (filenm r)) <> ":" <> tshow (linenr r)))
            -- One bullet per colliding relation: who inserts, who deletes.
            collisionItem :: Text -> Blocks
            collisionItem rel =
              plain
                $ code rel
                <> str ": "
                <> names [oeFrom e | e <- ocEdges oc, oeRelation e == rel, not (oeNegative e)]
                <> (str . l) (NL " voegt paren toe, ", EN " inserts pairs, ")
                <> names [oeFrom e | e <- ocEdges oc, oeRelation e == rel, oeNegative e]
                <> (str . l) (NL " verwijdert ze weer.", EN " removes them again.")
            names :: [Rule] -> Inlines
            names [] = (str . l) (NL "geen enkele regel", EN "no rule")
            names rs =
              mconcat
                . L.intersperse (str ", ")
                . map ruleName
                . L.nubBy (\a b -> fullName a == fullName b)
                $ rs

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
