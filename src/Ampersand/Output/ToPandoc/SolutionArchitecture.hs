{-# LANGUAGE ScopedTypeVariables #-}

-- | Build the body of a solution architecture document from an FSpec.
--
-- A solution architecture describes how one concrete solution answers a
-- bounded (business) problem.  This module produces a /matrijs/ (template):
-- everything that can be derived from the model (purposes, meanings, business
-- agreements, the data model, the services) is filled in automatically, while
-- the sections that require human judgement (context, scope, stakeholders,
-- non-functional requirements, technology choices, architecture decisions,
-- risks) are emitted as clearly marked placeholders to be completed by the
-- architect.
module Ampersand.Output.ToPandoc.SolutionArchitecture (chpSolutionArchitecture) where

import Ampersand.Output.ToPandoc.SharedAmongChapters
import qualified RIO.List as L
import qualified RIO.Set as Set
import qualified RIO.Text as T

-- | The complete body of the solution architecture document.  The returned
-- pictures come from the reused data-analysis chapter; the large data-model
-- diagrams themselves are added by the caller (see "Ampersand.Output.FSpec2PandocSolArch").
chpSolutionArchitecture ::
  (HasDirOutput env, HasDocumentOpts env) =>
  env ->
  FSpec ->
  (Blocks, [Picture])
chpSolutionArchitecture env fSpec = (theBlocks, [])
  where
    -- shorthand for easy localizing
    l :: LocalizedStr -> Text
    l = localize lang
    lang :: Lang
    lang = outputLang env fSpec

    -- A placeholder block: an instruction for the architect to fill in.
    todo :: LocalizedStr -> Blocks
    todo instruction =
      blockQuote
        . para
        $ strong (text (l (NL "In te vullen — ", EN "To be completed — ")))
          <> emph (text (l instruction))

    -- A readable heading for a model element: the local (namespace-free) label.
    -- Ampersand sometimes generates synthetic rule names like "Rule2563498968447706209";
    -- in that case the rule's meaning makes a far better heading than the name.
    ruleHeading :: Rule -> Text
    ruleHeading r
      | uninformativeName nm = case definedMeaning r of
          Just m -> firstSentence (markup2PlainText (ameaMrk m))
          Nothing -> nm
      | otherwise = nm
      where
        nm = label r
    -- A rule name carries no information when Ampersand generated it: a numeric
    -- "Rule<digits>" or an origin-derived "the rule defined at <file>:<line>".
    uninformativeName :: Text -> Bool
    uninformativeName t =
      ("Rule" `T.isPrefixOf` t && T.all (`elem` ("0123456789" :: String)) (T.drop 4 t))
        || ".adl:" `T.isInfixOf` t
        || "defined at" `T.isInfixOf` t
    firstSentence :: Text -> Text
    firstSentence = T.strip . T.takeWhile (/= '.')

    -- A model element's meaning, unless it is an empty or placeholder text
    -- (FormalAmpersand and other models use "TODO: MEANING ONTBREEKT" as a stub).
    -- Such stubs are noise in an architecture document, so we drop them.
    definedMeaning :: (HasMeaning a) => a -> Maybe Meaning
    definedMeaning x = case meaning lang x of
      Just m | not (isPlaceholder (markup2PlainText (ameaMrk m))) -> Just m
      _ -> Nothing
    isPlaceholder :: Text -> Bool
    isPlaceholder t = T.null s || "TODO" `T.isPrefixOf` s
      where
        s = T.strip t

    -- An inline " — <first sentence of the meaning>", or nothing when the
    -- element has no real meaning. Used to annotate compact list entries.
    meaningSuffix :: (HasMeaning a) => a -> Inlines
    meaningSuffix x = case definedMeaning x of
      Just m -> text " — " <> text (firstSentence (markup2PlainText (ameaMrk m)))
      Nothing -> mempty

    -- Shared model selections used in several sections.
    userRelations :: [Relation]
    userRelations = L.sortOn label . filter decusr . Set.toList $ vrels fSpec
    nrOfInvariants :: Int
    nrOfInvariants = Set.size (vrules fSpec `Set.intersection` invariants fSpec)

    theBlocks =
      secIntroduction
        <> secContextAndScope
        <> secStakeholders
        <> secRequirements
        <> secArchitectureOverview
        <> secTechnologyChoices
        <> secArchitectureDecisions
        <> secRisksAndAssumptions

    --------------------------------------------------------------------------
    -- 1. Introduction
    --------------------------------------------------------------------------
    secIntroduction =
      header 1 (text (l (NL "Inleiding", EN "Introduction")))
        <> para
          ( text . l
              $ ( NL
                    $ "Dit document beschrijft de solution-architectuur van "
                      <> fullName fSpec
                      <> ". Het beschrijft hoe deze oplossing een afgebakend probleem invult: "
                      <> "de structurele keuzes en hun verantwoording, niet de implementatiedetails.",
                  EN
                    $ "This document describes the solution architecture of "
                      <> fullName fSpec
                      <> ". It explains how this solution answers a bounded problem: "
                      <> "the structural choices and their rationale, not the implementation details."
                )
          )
        <> para
          ( text . l
              $ ( NL
                    $ "De secties met een gegevensmodel, afspraken en services zijn "
                      <> "automatisch afgeleid uit het Ampersand-model. De overige secties "
                      <> "zijn een sjabloon: vul de aangegeven plaatsen aan.",
                  EN
                    $ "The sections with a data model, agreements and services are "
                      <> "derived automatically from the Ampersand model. The remaining "
                      <> "sections are a template: complete the marked places."
                )
          )
        <> para (strong (text (l (NL "In één oogopslag:", EN "At a glance:"))))
        <> bulletList
          [ glanceItem (Set.size (concs fSpec)) (NL "begrippen", EN "concepts"),
            glanceItem (length userRelations) (NL "relaties", EN "relations"),
            glanceItem nrOfInvariants (NL "invarianten", EN "invariants"),
            glanceItem (length (vpatterns fSpec)) (NL "componenten (patronen)", EN "components (patterns)"),
            glanceItem (length (interfaceS fSpec)) (NL "services", EN "services")
          ]
      where
        glanceItem n lbl = plain (strong (text (tshow n)) <> text " " <> text (l lbl))

    --------------------------------------------------------------------------
    -- 2. Context and scope
    --------------------------------------------------------------------------
    secContextAndScope =
      header 1 (text (l (NL "Context en scope", EN "Context and scope")))
        <> todo
          ( NL
              "Beschrijf het (bedrijfs)probleem, de doelen, en wat wel en niet binnen scope valt.",
            EN
              "Describe the (business) problem, the goals, and what is in and out of scope."
          )
        <> ( let purps = purposes2Blocks env (purposesOf fSpec lang fSpec)
              in if null purps
                   then mempty
                   else
                     header 2 (text (l (NL "Doelstellingen uit het model", EN "Goals from the model")))
                       <> purps
           )

    --------------------------------------------------------------------------
    -- 3. Stakeholders and concerns
    --------------------------------------------------------------------------
    secStakeholders =
      header 1 (text (l (NL "Belanghebbenden en hun zorgen", EN "Stakeholders and concerns")))
        <> todo
          ( NL
              $ "Benoem wie belang heeft bij deze oplossing en welke zorg (concern) "
                <> "het ontwerp voor elke belanghebbende moet adresseren.",
            EN
              $ "List who has a stake in this solution and which concern the design "
                <> "must address for each stakeholder."
          )
        <> rolesFromModel
      where
        -- The roles defined in the model are the actors the solution serves;
        -- they are a concrete starting point for the stakeholder analysis.
        roles = L.nubBy (\x y -> label x == label y) . L.sortOn label . map fst $ fRoles fSpec
        rolesFromModel
          | null roles = mempty
          | otherwise =
              header 2 (text (l (NL "Rollen uit het model", EN "Roles from the model")))
                <> para
                  ( text . l
                      $ ( NL "Het model onderkent de volgende rollen. Zij zijn een vertrekpunt voor de belanghebbendenanalyse:",
                          EN "The model recognises the following roles. They are a starting point for the stakeholder analysis:"
                        )
                  )
                <> bulletList
                  [ plain (strong (text (label r)) <> maintainsSuffix r)
                    | r <- roles
                  ]
        maintainsSuffix r =
          case Set.size (fMaintains fSpec r) of
            0 -> mempty
            n ->
              text . l
                $ ( NL $ " — bewaakt " <> tshow n <> (if n == 1 then " regel" else " regels"),
                    EN $ " — maintains " <> tshow n <> (if n == 1 then " rule" else " rules")
                  )

    --------------------------------------------------------------------------
    -- 4. Requirements
    --------------------------------------------------------------------------
    secRequirements =
      header 1 (text (l (NL "Requirements", EN "Requirements")))
        <> header 2 (text (l (NL "Afspraken (invarianten)", EN "Agreements (invariants)")))
        <> para
          ( text . l
              $ ( NL
                    $ "De volgende afspraken zijn de invarianten uit het model: regels die "
                      <> "te allen tijde gelden. Samen leggen zij de functionele kern van de "
                      <> "oplossing vast — de waarheden die de software moet handhaven.",
                  EN
                    $ "The following agreements are the invariants from the model: rules that "
                      <> "must always hold. Together they capture the functional core of the "
                      <> "solution — the truths the software must maintain."
                )
          )
        <> ( if null agreementRules
               then
                 para
                   ( text . l
                       $ ( NL "Het model bevat nog geen invarianten met een betekenis.",
                           EN "The model does not yet contain invariants with a meaning."
                         )
                   )
               else mconcat (map ruleBlocks agreementRules)
           )
        <> operationalRulesSection
        <> header 2 (text (l (NL "Niet-functionele eisen", EN "Non-functional requirements")))
        <> todo
          ( NL
              $ "Beschrijf de bepalende niet-functionele eisen: performance, security, "
                <> "schaalbaarheid, beschikbaarheid, onderhoudbaarheid.",
            EN
              $ "Describe the decisive non-functional requirements: performance, security, "
                <> "scalability, availability, maintainability."
          )
      where
        -- Agreements are the user-defined invariants: the business constraints
        -- that must always hold. Synthetic property/identity rules are excluded,
        -- and process rules (maintained by a role such as ExecEngine) are listed
        -- separately, because they are automation rather than architecture.
        agreementRules =
          L.sortOn label . Set.toList
            $ (vrules fSpec `Set.intersection` invariants fSpec)
        operationalRules =
          L.sortOn label . Set.toList
            $ (vrules fSpec `Set.intersection` signals fSpec)
        ruleBlocks r =
          header 3 (text (ruleHeading r))
            <> maybe mempty meaning2Blocks (definedMeaning r)
            <> purposes2Blocks env (purposesOf fSpec lang r)
        operationalRulesSection
          | null operationalRules = mempty
          | otherwise =
              header 2 (text (l (NL "Operationele regels", EN "Operational rules")))
                <> para
                  ( text . l
                      $ ( NL
                            $ "Naast de invarianten kent het model procesregels die automatisch "
                              <> "worden gehandhaafd (bijvoorbeeld door de ExecEngine). Zij beschrijven "
                              <> "geen vaste waarheid maar een actie die volgt op een situatie:",
                          EN
                            $ "Besides the invariants, the model has process rules that are maintained "
                              <> "automatically (for example by the ExecEngine). They do not state a fixed "
                              <> "truth but an action that follows from a situation:"
                        )
                  )
                <> bulletList
                  [ plain (strong (text (ruleHeading r)) <> meaningSuffix r)
                    | r <- operationalRules
                  ]

    --------------------------------------------------------------------------
    -- Functional components: the patterns of the model. In Ampersand a pattern
    -- groups the relations and rules of one coherent concern, so patterns are
    -- the natural functional building blocks of the solution.
    --------------------------------------------------------------------------
    secComponents
      | null pats = mempty
      | otherwise =
          header 2 (text (l (NL "Functionele componenten", EN "Functional components")))
            <> para
              ( text . l
                  $ ( NL
                        $ "De oplossing is opgebouwd uit de volgende componenten. Elk component "
                          <> "(een patroon in het model) bundelt de relaties en regels van één "
                          <> "samenhangend onderwerp:",
                      EN
                        $ "The solution is composed of the following components. Each component "
                          <> "(a pattern in the model) bundles the relations and rules of one "
                          <> "coherent concern:"
                    )
              )
            <> mconcat (map componentBlocks pats)
      where
        pats = L.sortOn label (vpatterns fSpec)
        componentBlocks p =
          header 3 (text (label p))
            <> purposes2Blocks env (purposesOf fSpec lang p)
            <> para
              ( emph . text . l
                  $ ( NL $ count (Set.size (ptdcs p)) "relatie" "relaties" <> ", " <> count (Set.size (ptrls p)) "regel" "regels" <> ".",
                      EN $ count (Set.size (ptdcs p)) "relation" "relations" <> ", " <> count (Set.size (ptrls p)) "rule" "rules" <> "."
                    )
              )
        count n one many = tshow n <> " " <> (if n == 1 then one else many)

    --------------------------------------------------------------------------
    -- Domain vocabulary: the concepts and what they mean. This is the
    -- "ubiquitous language" a software engineer needs before reading the data
    -- model. Driven by the concept definitions (CONCEPT ... statements).
    --------------------------------------------------------------------------
    secVocabulary
      | null conceptEntries = mempty
      | otherwise =
          header 2 (text (l (NL "Begrippenkader", EN "Domain vocabulary")))
            <> para
              ( text . l
                  $ ( NL
                        $ "De oplossing is gebouwd rond de volgende begrippen. Zij vormen de "
                          <> "gedeelde taal tussen domein en software; elk begrip wordt elders in "
                          <> "dit document als gegevensverzameling teruggevonden.",
                      EN
                        $ "The solution is built around the following concepts. They form the "
                          <> "shared language between domain and software; each concept reappears "
                          <> "as an entity type later in this document."
                    )
              )
            <> definitionList
              [ ( strong (text (label cd)),
                  [ meaning2Blocks (acddef2 cd)
                      <> purposes2Blocks env (purposesOf fSpec lang (acdcpt cd))
                  ]
                )
                | cd <- conceptEntries
              ]
      where
        -- One definition per concept (a concept may be defined more than once);
        -- keep those that actually carry a definition text.
        conceptEntries =
          L.nubBy (\x y -> label x == label y)
            . L.sortOn label
            . filter (not . T.null . T.strip . cdDefText)
            $ conceptDefs fSpec
        cdDefText = markup2PlainText . ameaMrk . acddef2

    --------------------------------------------------------------------------
    -- Data architecture, at solution-architecture altitude: the classification
    -- structure and logical data model (as diagrams, reusing Ampersand's data
    -- model generator) plus the relations as fact types with their meaning.
    -- The technical (SQL) data model is deliberately omitted — it is
    -- implementation detail, below the altitude of a solution architecture.
    --------------------------------------------------------------------------
    secDataArchitecture =
      header 2 (text (l (NL "Gegevensarchitectuur", EN "Data architecture")))
        <> para
          ( text . l
              $ ( NL
                    $ "De gegevensarchitectuur legt vast welke gegevens de oplossing kent en "
                      <> "hoe die samenhangen. We tonen de classificatiestructuur, het logische "
                      <> "gegevensmodel en de relaties (fact types) met hun betekenis. Het "
                      <> "technische (SQL-)model valt buiten de architectuur en is hier weggelaten.",
                  EN
                    $ "The data architecture records which data the solution knows and how it "
                      <> "relates. We show the classification structure, the logical data model "
                      <> "and the relations (fact types) with their meaning. The technical (SQL) "
                      <> "model is below architecture level and is omitted here."
                )
          )
        <> ( if null (vgens fSpec)
               then mempty
               else
                 header 3 (text (l (NL "Classificatiestructuur", EN "Classification structure")))
                   <> xDefBlck env fSpec (makePicture env fSpec PTClassificationDiagram)
           )
        <> header 3 (text (l (NL "Logisch gegevensmodel", EN "Logical data model")))
        <> xDefBlck env fSpec (makePicture env fSpec (PTLogicalDataModelOfContext False))
        <> header 3 (text (l (NL "Relaties (fact types)", EN "Relations (fact types)")))
        <> ( if null relEntries
               then mempty
               else
                 para
                   ( text . l
                       $ ( NL "Elke relatie legt een elementair feit vast tussen twee begrippen:",
                           EN "Each relation records an elementary fact between two concepts:"
                         )
                   )
                   <> bulletList
                     [ plain (code (relSignature r) <> meaningSuffix r)
                       | r <- relEntries
                     ]
           )
      where
        relEntries = userRelations
        relSignature r =
          label r
            <> " : "
            <> localNameOf (source r)
            <> " × "
            <> localNameOf (target r)
            <> propsText r
        propsText r =
          let ps = Set.toList (properties r)
           in if null ps
                then ""
                else " [" <> T.intercalate "," (map tshow ps) <> "]"

    --------------------------------------------------------------------------
    -- 5. Architecture overview
    --------------------------------------------------------------------------
    secArchitectureOverview =
      header 1 (text (l (NL "Architectuuroverzicht", EN "Architecture overview")))
        <> todo
          ( NL
              $ "Beschrijf de oplossing op hoofdlijnen: de componenten, hun "
                <> "verantwoordelijkheden en hun samenhang. Voeg waar nuttig een "
                <> "context- of containerdiagram toe.",
            EN
              $ "Describe the solution at a high level: the components, their "
                <> "responsibilities and how they fit together. Add a context or "
                <> "container diagram where useful."
          )
        <> secComponents
        <> secVocabulary
        <> secDataArchitecture
        <> header 2 (text (l (NL "Services", EN "Services")))
        <> ( if null (interfaceS fSpec)
               then
                 todo
                   ( NL "Het model definieert nog geen interfaces (services).",
                     EN "The model does not yet define any interfaces (services)."
                   )
               else
                 para
                   ( text . l
                       $ ( NL "De oplossing biedt de volgende services aan, afgeleid uit de interfaces in het model:",
                           EN "The solution offers the following services, derived from the interfaces in the model:"
                         )
                   )
                   <> bulletList
                     [ plain (strong (text (label ifc)) <> rolesSuffix ifc)
                         <> purposes2Blocks env (purposesOf fSpec lang ifc)
                       | ifc <- L.sortOn label (interfaceS fSpec)
                     ]
           )
      where
        rolesSuffix ifc = case ifcRoles ifc of
          [] -> text (l (NL " (voor alle rollen)", EN " (for all roles)"))
          rs -> text (l (NL " — voor rol(len): ", EN " — for role(s): ")) <> text (T.intercalate ", " (map label rs))

    --------------------------------------------------------------------------
    -- 6. Technology choices
    --------------------------------------------------------------------------
    secTechnologyChoices =
      header 1 (text (l (NL "Technologiekeuzes", EN "Technology choices")))
        <> todo
          ( NL
              $ "Benoem de stack en het platform, met per keuze de motivatie. "
                <> "Een Ampersand-oplossing draait standaard als Angular-frontend met een "
                <> "PHP-backend op een MariaDB-database; pas dit aan op je situatie.",
            EN
              $ "State the stack and platform, with the rationale for each choice. "
                <> "An Ampersand solution runs by default as an Angular front end with a "
                <> "PHP back end on a MariaDB database; adapt this to your situation."
          )

    --------------------------------------------------------------------------
    -- 7. Architecture decisions
    --------------------------------------------------------------------------
    secArchitectureDecisions =
      header 1 (text (l (NL "Architectuurbeslissingen", EN "Architecture decisions")))
        <> todo
          ( NL
              $ "Leg per beslissing vast: context, de gemaakte keuze, de overwogen "
                <> "alternatieven en de gevolgen (een ADR per beslissing). Kopieer het "
                <> "onderstaande sjabloon per beslissing.",
            EN
              $ "Record for each decision: context, the choice made, the alternatives "
                <> "considered and the consequences (one ADR per decision). Copy the "
                <> "template below for each decision."
          )
        <> header 3 (text (l (NL "ADR-001: <titel van de beslissing>", EN "ADR-001: <decision title>")))
        <> definitionList
          [ adrField (NL "Status", EN "Status") (NL "voorgesteld / aanvaard / vervangen", EN "proposed / accepted / superseded"),
            adrField (NL "Context", EN "Context") (NL "welke kracht of eis dwingt deze beslissing af?", EN "which force or requirement drives this decision?"),
            adrField (NL "Beslissing", EN "Decision") (NL "wat is gekozen?", EN "what was chosen?"),
            adrField (NL "Alternatieven", EN "Alternatives") (NL "wat is overwogen en waarom afgewezen?", EN "what was considered and why rejected?"),
            adrField (NL "Gevolgen", EN "Consequences") (NL "wat wordt makkelijker, wat moeilijker?", EN "what becomes easier, what harder?")
          ]
      where
        adrField termStr helpStr =
          ( strong (text (l termStr)),
            [plain (emph (text (l helpStr)))]
          )

    --------------------------------------------------------------------------
    -- 8. Risks and assumptions
    --------------------------------------------------------------------------
    secRisksAndAssumptions =
      header 1 (text (l (NL "Risico's en aannames", EN "Risks and assumptions")))
        <> todo
          ( NL
              "Benoem wat onzeker is en welke risico's je accepteert of mitigeert.",
            EN
              "State what is uncertain and which risks you accept or mitigate."
          )
