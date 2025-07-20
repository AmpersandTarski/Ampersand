{-# LANGUAGE ScopedTypeVariables #-}

module Ampersand.Output.ToPandoc.ChapterDiagnosis where

import Ampersand.Output.PandocAux
import Ampersand.Output.ToPandoc.SharedAmongChapters
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

chpDiagnosis ::
  (HasDirOutput env, HasDocumentOpts env) =>
  env ->
  FSpec ->
  (Blocks, [Picture])
chpDiagnosis env fSpec
  | Diagnosis `notElem` view chaptersL env = mempty
  | otherwise =
      ( xDefBlck env fSpec Diagnosis
          <> para
            ( (str . l)
                ( NL "Dit hoofdstuk geeft een analyse van het Ampersand-script van ",
                  EN "This chapter provides an analysis of the Ampersand script of "
                )
                <> (emph . singleQuoted . str . fullName) fSpec
                <> str ". "
                <> (str . l)
                  ( NL
                      $ "Deze analyse is bedoeld voor de auteur(s) van dit script. "
                      <> "Op basis hiervan kunnen zij het script completeren en mogelijke tekortkomingen verbeteren.",
                    EN
                      $ "This analysis is intended for the author(s) of this script. "
                      <> "It can be used to complete the script or to improve possible flaws."
                  )
            )
          <> roleomissions -- tells which role-rule, role-interface, and role-relation assignments are missing
          <> roleRuleTable -- gives an overview of rule-rule assignments
          <> missingConceptDefs -- tells which concept definitions have been declared without a purpose
          <> missingRels -- tells which relations have been declared without a purpose and/or without a meaning
          <> unusedConceptDefs -- tells which concept definitions are not used in any relation
          <> relsNotUsed -- tells which relations are not used in any rule
          <> missingRules -- tells which rule definitions are missing
          <> ruleRelationRefTable -- table that shows percentages of relations and rules that have references
          <> processrulesInPatterns --
          <> wipReport -- sums up the work items (i.e. the violations of process rules)
          <> violationReport, -- sums up the violations caused by the population of this script.
        pics
      )
  where
    -- shorthand for easy localizing
    l :: LocalizedStr -> Text
    l = localize outputLang'
    outputLang' = outputLang env fSpec
    roleomissions :: Blocks
    roleomissions
      | null (instanceList fSpec :: [Pattern]) = mempty
      | (null . fRoleRuls) fSpec && (not . null . vrules) fSpec =
          plain
            ( (emph . str . upCap . fullName) fSpec
                <> (str . l)
                  ( NL " kent geen regels aan rollen toe. ",
                    EN " does not assign rules to roles. "
                  )
                <> (str . l)
                  ( NL "Een generieke rol, User, zal worden gedefinieerd om al het werk te doen wat in het bedrijfsproces moet worden uitgevoerd.",
                    EN "A generic role, User, will be defined to do all the work that is necessary in the business process."
                  )
            )
      | otherwise = mempty

    roleRuleTable :: Blocks
    roleRuleTable
      | null ruls = mempty
      | null (fRoles fSpec) =
          para
            ( (emph . str . upCap . fullName) fSpec
                <> (str . l)
                  ( NL " specificeert geen rollen. ",
                    EN " does not define any roles. "
                  )
            )
      | otherwise =
          case filter (isSignal fSpec) . toList $ ruls of
            [] ->
              para
                ( (emph . str . upCap . fullName) fSpec
                    <> (str . l)
                      ( NL " kent geen procesregels. ",
                        EN " does not define any process rules. "
                      )
                )
            sigs ->
              para
                ( (emph . str . upCap . fullName) fSpec
                    <> (str . l)
                      ( NL " kent regels aan rollen toe. ",
                        EN " assigns rules to roles. "
                      )
                    <> (str . l)
                      ( NL "De volgende tabel toont welke regels door een bepaalde rol worden bewaakt.",
                        EN "The following table shows the rules that are being maintained by a given role."
                      )
                )
                <> legacyTable -- No caption:
                  mempty
                  -- Alignment:
                  ( (AlignLeft, 0.4)
                      : replicate (length . fRoles $ fSpec) (AlignLeft, 0.6 / (fromIntegral . length . fRoles $ fSpec))
                  )
                  -- Header row:
                  ( (plain . str . l) (NL "Regel", EN "Rule")
                      : map (plain . str . fullName . fst) (fRoles fSpec)
                  )
                  -- Content rows:
                  [ (plain . str . label) rul
                      : [f rol rul | (rol, _) <- fRoles fSpec]
                    | rul <- sigs
                  ]
      where
        ruls = Set.filter (isSignal fSpec) . vrules $ fSpec
        f :: Role -> Rule -> Blocks
        f rol rul =
          if (rol, rul) `elem` fRoleRuls fSpec
            then (plain . str) "✓"
            else mempty

    missingConceptDefs :: Blocks
    missingConceptDefs =
      case missing of
        [] ->
          if (null . concs) fSpec
            then mempty
            else
              (para . str . l)
                ( NL "Alle concepten in dit document zijn voorzien van een oogmerk (purpose).",
                  EN "All concepts in this document have been provided with a purpose."
                )
        [c] ->
          para
            ( (str . l)
                ( NL "Het oogmerk (purpose) van concept ",
                  EN "The concept "
                )
                <> (singleQuoted . str . label) c
                <> (str . l)
                  ( NL " is niet gedocumenteerd.",
                    EN " remains without a purpose."
                  )
            )
        xs ->
          para
            ( (str . l)
                ( NL "Het oogmerk (purpose) van de concepten: ",
                  EN "Concepts "
                )
                <> commaPandocAnd outputLang' (map (str . label) xs)
                <> (str . l)
                  ( NL " is niet gedocumenteerd.",
                    EN " remain without a purpose."
                  )
            )
      where
        missing =
          L.nub
            $ [c | c <- ccs, null (purposesOf fSpec outputLang' c)]
            <> [c | c <- ccs, null (concDefs fSpec c)]
        ccs = toList . concs . vrels $ fSpec

    unusedConceptDefs :: Blocks
    unusedConceptDefs =
      case NE.groupWith name undefinedConcepts of
        [] ->
          if (null . conceptDefs) fSpec
            then mempty
            else
              para
                . str
                . l
                $ ( NL "Alle concepten, die in dit document zijn voorzien van een definitie, worden gebruikt in relaties.",
                    EN "All concepts defined in this document are used in relations."
                  )
        [c :| _] ->
          para
            ( (str . l)
                ( NL "Het concept ",
                  EN "The concept "
                )
                <> (singleQuoted . str . fullName) c
                <> (str . l)
                  ( NL " is gedefinieerd, maar wordt niet gebruikt in een relatie.",
                    EN " is defined, but isn't used in a relation."
                  )
            )
        xs ->
          para
            ( (str . l) (NL "De concepten: ", EN "Concepts ")
                <> (commaPandocAnd outputLang' . fmap (str . fullName . NE.head) $ xs)
                <> (str . l)
                  ( NL " zijn gedefinieerd, maar worden niet gebruikt.",
                    EN " are defined, but not used."
                  )
            )
      where
        conceptsInUsedRelation :: [A_Concept]
        conceptsInUsedRelation = toList . concs . allUsedDecls $ fSpec
        undefinedConcepts = [cd | cd <- conceptDefs fSpec, name cd `notElem` map name conceptsInUsedRelation]

    missingRels :: Blocks
    missingRels =
      case bothMissing <> purposeOnlyMissing <> meaningOnlyMissing of
        [] ->
          (para . str . l)
            ( NL "Alle relaties in dit document zijn voorzien van zowel een reden van bestaan (purpose) als een betekenis (meaning).",
              EN "All relations in this document have been provided with a purpose as well as a meaning."
            )
        _ ->
          ( case bothMissing of
              [] -> mempty
              [d] ->
                para
                  ( (str . l) (NL "Van de relatie ", EN "The relation ")
                      <> showDclMath d
                      <> (str . l)
                        ( NL " ontbreekt zowel de betekenis (meaning) als de reden van bestaan (purpose).",
                          EN " lacks both a purpose as well as a meaning."
                        )
                  )
              ds ->
                if length ds < 10
                  then
                    para
                      ( (str . l) (NL "Van de relaties ", EN "The relations ")
                          <> commaPandocAnd outputLang' (map showDclMath ds)
                          <> (str . l)
                            ( NL " ontbreken zowel de betekenis (meaning) als de reden van bestaan (purpose).",
                              EN " all lack both a purpose and a meaning."
                            )
                      )
                  else
                    para
                      ( (str . l)
                          ( NL ("Van " <> tshow (length ds) <> " relaties is noch het oogmerk (purpose), noch de betekenis gespecificeerd. Drie voorbeelden daarvan zijn: "),
                            EN ("Neither purpose nor meaning is specified in " <> tshow (length ds) <> " relations. Three examples of such relations are: ")
                          )
                          <> (commaPandocAnd outputLang' . map showDclMath . take 3) ds
                      )
          )
            <> ( case purposeOnlyMissing of
                   [] -> mempty
                   [d] ->
                     para
                       ( (str . l) (NL "De reden waarom relatie ", EN "The purpose of relation ")
                           <> showDclMath d
                           <> (str . l)
                             ( NL " bestaat wordt niet uitgelegd.",
                               EN " remains unexplained."
                             )
                       )
                   ds ->
                     if length ds < 10
                       then
                         para
                           ( (str . l) (NL "Relaties ", EN "The purpose of relations ")
                               <> commaPandocAnd outputLang' (map showDclMath ds)
                               <> (str . l)
                                 ( NL " zijn niet voorzien van een reden van bestaan (purpose).",
                                   EN " is not documented."
                                 )
                           )
                       else
                         para
                           ( (str . l)
                               ( NL ("Het oogmerk (purpose) van " <> tshow (length ds) <> " relaties is niet gespecificeerd. Drie voorbeelden van relaties zonder oogmerk zijn: "),
                                 EN ("The purpose of " <> tshow (length ds) <> " relations is not specified. Three examples of relations without a purpose are: ")
                               )
                               <> (commaPandocAnd outputLang' . map showDclMath . take 3) ds
                           )
               )
            <> ( case meaningOnlyMissing of
                   [] -> mempty
                   [d] ->
                     para
                       ( (str . l) (NL "De betekenis van relatie ", EN "The meaning of relation ")
                           <> showDclMath d
                           <> (str . l)
                             ( NL " is niet gedocumenteerd.",
                               EN " is not documented."
                             )
                       )
                   ds ->
                     if length ds < 10
                       then
                         para
                           ( (str . l) (NL "De betekenis van relaties ", EN "The meaning of relations ")
                               <> commaPandocAnd outputLang' (map showDclMath ds)
                               <> (str . l)
                                 ( NL " is niet gedocumenteerd.",
                                   EN " is not documented."
                                 )
                           )
                       else
                         para
                           ( (str . l)
                               ( NL ("De betekenis van " <> tshow (length ds) <> " relaties is niet gedocumenteerd. Bijvoorbeeld: "),
                                 EN ("The meaning of " <> tshow (length ds) <> " relations is not documented. For instance: ")
                               )
                               <> (commaPandocAnd outputLang' . map showDclMath . take 3) ds
                           )
               )
      where
        bothMissing, purposeOnlyMissing, meaningOnlyMissing :: [Relation]
        bothMissing = filter (not . hasPurpose) . filter (not . hasMeaning) . toList $ decls
        purposeOnlyMissing = filter (not . hasPurpose) . filter hasMeaning . toList $ decls
        meaningOnlyMissing = filter hasPurpose . filter (not . hasMeaning) . toList $ decls
        decls = vrels fSpec
        showDclMath = math . tshow
    hasPurpose :: (Motivated a) => a -> Bool
    hasPurpose = not . null . purposesOf fSpec outputLang'
    hasMeaning :: (HasMeaning a) => a -> Bool
    hasMeaning = isJust . meaning outputLang'

    relsNotUsed :: Blocks
    pics :: [Picture]
    (relsNotUsed, pics) =
      ( case notUsed of
          [] ->
            if (null . bindedRelationsIn . vrules) fSpec
              then mempty
              else
                (para . str . l)
                  ( NL "Alle relaties in dit document worden in één of meer regels gebruikt.",
                    EN "All relations in this document are being used in one or more rules."
                  )
          [r] ->
            para
              ( (str . l) (NL "De relatie ", EN "Relation ")
                  <> r
                  <> (str . l)
                    ( NL " wordt in geen enkele regel gebruikt. ",
                      EN " is not being used in any rule. "
                    )
              )
          rs ->
            para
              ( (str . l) (NL "Relaties ", EN "Relations ")
                  <> commaPandocAnd outputLang' rs
                  <> (str . l)
                    ( NL " worden niet gebruikt in regels. ",
                      EN " are not used in any rule. "
                    )
              )
          <> ( case pictsWithUnusedRels of
                 [pict] ->
                   para
                     ( hyperLinkTo pict
                         <> (str . l)
                           ( NL " geeft een conceptueel diagram met alle relaties.",
                             EN " shows a conceptual diagram with all relations."
                           )
                     )
                     <> xDefBlck env fSpec pict
                 picts ->
                   mconcat
                     [ para
                         ( hyperLinkTo pict
                             <> (str . l)
                               ( NL " geeft een conceptueel diagram met alle relaties die gedeclareerd zijn in ",
                                 EN " shows a conceptual diagram with all relations declared in "
                               )
                             <> (singleQuoted . str . label) pat
                             <> "."
                         )
                         <> xDefBlck env fSpec pict
                       | (pict, pat) <- zip picts pats
                     ]
             ),
        pictsWithUnusedRels -- draw the conceptual diagram
      )
      where
        notUsed :: [Inlines]
        notUsed =
          [ showMath (EDcD d)
            | d <- toList (vrels fSpec), -- only relations that are used or defined in the selected themes
              decusr d,
              d `notElem` (bindedRelationsIn . vrules) fSpec
          ]
        pats =
          [ pat | pat <- instanceList fSpec, (not . null) (relsDefdIn pat Set.\\ bindedRelationsIn pat)
          ]
        pictsWithUnusedRels = makePicture env fSpec . PTConceptualModelOfRelationsInPattern <$> pats

    missingRules :: Blocks
    missingRules =
      case toList $ vrules fSpec of
        [] -> mempty
        ruls ->
          if all hasMeaning ruls && all hasPurpose ruls
            then
              (para . str . l)
                ( NL "Alle regels zijn voorzien van een uitleg.",
                  EN "All rules have been provided with a meaning and a purpose."
                )
            else
              ( case filter (not . hasPurpose) (filter (not . hasMeaning) ruls) of
                  [] -> mempty
                  [r] ->
                    (para . str . l)
                      ( NL "De volgende regel heeft noch oogmerk (purpose), noch betekenis:",
                        EN "The following rule has neither a purpose nor a meaning."
                      )
                      <> formalizations [r]
                  rls ->
                    (para . str . l)
                      ( NL "De volgende regels hebben noch oogmerk (purpose), noch betekenis:",
                        EN "The following rules have neither a purpose nor a meaning."
                      )
                      <> formalizations rls
              )
                <> ( case filter (not . hasPurpose) (filter hasMeaning ruls) of
                       [] -> mempty
                       [r] ->
                         (para . str . l)
                           ( NL "Van de volgende regel is het oogmerk (purpose) niet uitgelegd:",
                             EN "The following rule is defined without documenting its purpose:"
                           )
                           <> formalizations [r]
                       rls ->
                         (para . str . l)
                           ( NL "Van de volgende regels is het oogmerk (purpose) niet uitgelegd:",
                             EN "The following rules are defined without documenting their purpose:"
                           )
                           <> formalizations rls
                   )
                <> ( case filter hasPurpose (filter (not . hasMeaning) ruls) of
                       [] -> mempty
                       [r] ->
                         (para . str . l)
                           ( NL "De volgende regel gaat niet vergezeld van een betekenis in natuurlijke taal:",
                             EN "The following rule is not accompanied by a meaning written in natural language:"
                           )
                           <> formalizations [r]
                       rls ->
                         (para . str . l)
                           ( NL "De volgende regels gaan niet vergezeld van een betekenis in natuurlijke taal:",
                             EN "The following rules are not accompanied by a meaning written in natural language:"
                           )
                           <> formalizations rls
                   )
      where
        formalizations rls =
          bulletList
            [ para ((emph . str . label) r <> " (" <> (str . tShowOrigin) r <> ")")
                <> (para . showMath . formalExpression) r
                <> (para . showPredLogic outputLang' . formalExpression) r
              | r <- rls
            ]

    tShowOrigin :: (Traced x) => x -> Text
    tShowOrigin = tshow . stripDirectory . origin
      where
        stripDirectory :: Origin -> Origin
        stripDirectory (FileLoc pos' x) = FileLoc (f pos') x
        stripDirectory fileLoc = fileLoc
        -- data FilePos = FilePos FilePath Line Column deriving (Eq, Ord, Generic, Typeable, Data)
        f :: FilePos -> FilePos
        f (FilePos pth line col) = FilePos (takeFileName pth) line col

    ruleRelationRefTable :: Blocks
    ruleRelationRefTable =
      (para . str . l)
        ( NL
            $ "Onderstaande tabel bevat per thema (dwz. patroon) tellingen van het aantal relaties en regels, "
            <> "gevolgd door het aantal en het percentage daarvan dat een referentie bevat. Relaties die in meerdere thema's "
            <> "gedeclareerd worden, worden ook meerdere keren geteld.",
          EN
            $ "The table below shows for each theme (i.e. pattern) the number of relations and rules, followed "
            <> "by the number and percentage that have a reference. Relations declared in multiple themes are counted multiple "
            <> "times."
        )
        <> legacyTable -- No caption:
          mempty
          -- Alignment:
          ((AlignLeft, 0.4) : replicate 6 (AlignCenter, 0.1))
          -- Headers
          ( map
              (plain . str . l)
              [ (NL "Thema", EN "Theme"),
                (NL "Relaties", EN "Relations"),
                (NL "Met referentie", EN "With reference"),
                (NL "%", EN "%"),
                (NL "Regels", EN "Rules"),
                (NL "Gehele context", EN "Entire context"),
                (NL "%", EN "%")
              ]
          )
          -- Content rows
          ( map mkTableRowPat (instanceList fSpec :: [Pattern])
              <> [mkTableRow (l (NL "Gehele context", EN "Entire context")) (Set.filter decusr $ vrels fSpec) (vrules fSpec)]
          )
      where
        mkTableRow ::
          Text -> -- The name of the pattern / fSpec
          Relations -> -- The user-defined relations of the pattern / fSpec
          Rules -> -- The user-defined rules of the pattern / fSpec
          [Blocks]
        mkTableRowPat p = mkTableRow (label p) (relsDefdIn p) (udefrules p)
        mkTableRow nm rels ruls =
          map
            (plain . str)
            [ nm,
              (tshow . Set.size) rels,
              (tshow . Set.size) (Set.filter hasRef rels),
              showPercentage (Set.size rels) (Set.size . Set.filter hasRef $ rels),
              (tshow . Set.size) ruls,
              (tshow . Set.size) (Set.filter hasRef ruls),
              showPercentage (Set.size ruls) (Set.size . Set.filter hasRef $ ruls)
            ]

        hasRef x = any ((/= []) . explRefIds) (purposesOf fSpec outputLang' x)

        showPercentage x y = if x == 0 then "-" else tshow (y * 100 `div` x) <> "%"

    processrulesInPatterns :: Blocks
    processrulesInPatterns =
      if null (fRoleRuls fSpec)
        then mempty
        else
          (para . str . l)
            ( NL "Onderstaande tabel bevat een overzicht van de signaalregels per rol.",
              EN "The table below shows the signal rules per role."
            )
            <> legacyTable -- No caption:
              mempty
              -- Alignment:
              ( if multProcs
                  then replicate 3 (AlignLeft, 1 / 3)
                  else replicate 2 (AlignLeft, 1 / 2)
              )
              -- Headers:
              ( [(plain . str . l) (NL "rol", EN "role")]
                  <> [(plain . str . l) (NL "thema", EN "in pattern") | multProcs]
                  <> [ (plain . str . l) (NL "regel", EN "rule")
                     ]
              )
              -- Rows:
              [ [(plain . str . label) rol]
                  <> [(plain . str . fromMaybe "--" . rrpat) rul | multProcs]
                  <> [ (plain . str . label) rul,
                       (plain . str . fromMaybe "--" . rrpat) rul
                     ]
                | (rol, rul) <- fRoleRuls fSpec
              ]
      where
        multProcs = length (instanceList fSpec :: [Pattern]) > 1
    wipReport :: Blocks
    wipReport =
      case popwork of
        [] ->
          (para . str . l)
            ( NL "De populatie in dit script beschrijft geen onderhanden werk. ",
              EN "The population in this script does not specify any work in progress. "
            )
        [(r, ps)] ->
          para
            ( (str . l) (NL "Regel ", EN "Rule")
                <> quoterule r
                <> (str . l)
                  ( NL $ " laat " <> count Dutch (length ps) "taak" <> " zien.",
                    EN $ " shows " <> count English (length ps) "task" <> "."
                  )
            )
        _ ->
          (para . str . l)
            ( NL "Dit script bevat onderhanden werk. De volgende tabellen geven details met regelnummers in de oorspronkelijk script-bestanden.",
              EN "This script contains work in progress. The following tables provide details with line numbers from the original script files."
            )
        <> if null popwork
          then mempty
          else
            legacyTable -- No caption:
              mempty
              -- Alignment:
              ((AlignLeft, 1 / 3) : replicate 2 (AlignRight, 1 / 3))
              -- Header:
              ( map
                  (plain . str . l)
                  [ (NL "regel", EN "rule"),
                    (NL "locatie", EN "location"),
                    (NL "#taken", EN "#tasks")
                  ]
              )
              -- Rows:
              [ map
                  (plain . str)
                  [ label r,
                    tShowOrigin r,
                    (tshow . length) ps
                  ]
                | (r, ps) <- popwork
              ]
              <>
              -- the tables containing the actual work in progress population
              mconcat
                [ para
                    ( str (l (NL "Afspraak ", EN "Agreement "))
                        -- Pandoc does not yield hyperlinks in Word files, so we cannot do:
                        -- <> hyperLinkTo (XRefSharedLangRule r) <> " ( " <> quoterule r <> " )"
                        <> quoterule r
                        <> (str . l) (NL " luidt: ", EN " says: ")
                    )
                    <> printMeaning outputLang' r
                    <> para
                      ( (str . l)
                          ( NL "Deze regel bevat nog werk (voor ",
                            EN "This rule contains work (for "
                          )
                          <> (commaPandocOr outputLang' . map (str . fullName) . rolesOf $ r)
                          <> ")"
                          <> case Set.toList ps of
                            [v] ->
                              (str . l) (NL ", te weten ", EN " by ")
                                <> oneviol r v
                                <> "."
                            _ ->
                              (str . l)
                                ( NL $ ". De volgende tabel laat de " <> (if Set.size ps > 10 then "eerste tien " else "") <> "items zien die aandacht vragen.",
                                  EN $ "The following table shows the " <> (if Set.size ps > 10 then "first ten " else "") <> "items that require attention."
                                )
                      )
                    <> if Set.size ps <= 1
                      then mempty -- iff there is a single violation, it is already shown in the previous paragraph
                      else violtable r ps
                  | (r, ps) <- popwork
                ]
      where
        --      text r
        --       = if null expls
        --         then explains2Blocks (autoMeaning outputLang' r)
        --         else expls
        --         where expls = [Plain (block<>[Space]) | Means l econt<-rrxpl r, l==Just outputLang' || l==Nothing, Para block<-econt]
        quoterule :: Rule -> Inlines
        quoterule = singleQuoted . str . label
        oneviol :: Rule -> AAtomPair -> Inlines
        oneviol r p =
          if isEndo (formalExpression r) && apLeft p == apRight p
            then
              singleQuoted
                ( (str . label . source . formalExpression) r
                    <> (str . showValADL . apLeft) p
                )
            else
              "("
                <> (str . label . source . formalExpression) r
                <> (str . showValADL . apLeft) p
                <> ", "
                <> (str . label . target . formalExpression) r
                <> (str . showValADL . apRight) p
                <> ")"
        popwork :: [(Rule, AAtomPairs)]
        popwork = [(r, ps) | (r, ps) <- allViolations fSpec, isSignal fSpec r]

    violationReport :: Blocks
    violationReport =
      para
        ( case (invariantViolations, processViolations) of
            ([], []) ->
              (str . l)
                ( NL "De populatie in dit script overtreedt geen regels. ",
                  EN "The population in this script violates no rule. "
                )
            (iVs, pVs) ->
              (str . l)
                ( NL "De populatie in dit script overtreedt ",
                  EN "The population in this script violates "
                )
                <> (str . tshow . length) iVs
                <> (str . l)
                  ( NL $ " invariant" <> (if length iVs == 1 then "" else "en") <> " en ",
                    EN $ " invariant" <> (if length iVs == 1 then "" else "s") <> " and "
                  )
                <> (str . tshow . length) pVs
                <> (str . l)
                  ( NL $ " procesregel" <> if length pVs == 1 then "" else "s" <> ".",
                    EN $ " process rule" <> if length pVs == 1 then "" else "s" <> "."
                  )
        )
        <> bulletList (map showViolatedRule invariantViolations)
        <> bulletList (map showViolatedRule processViolations)
      where
        (processViolations, invariantViolations) = L.partition (isSignal fSpec . fst) (allViolations fSpec)
        showViolatedRule :: (Rule, AAtomPairs) -> Blocks
        showViolatedRule (r, ps) =
          (para . emph)
            ( (str . l) (NL "Regel ", EN "Rule ")
                <> (str . label) r
            )
            <> para
              ( ( if isSignal fSpec r
                    then (str . l) (NL "Totaal aantal taken: ", EN "Total number of work items: ")
                    else (str . l) (NL "Totaal aantal overtredingen: ", EN "Total number of violations: ")
                )
                  <> (str . tshow . length) ps
              )
            <> legacyTable -- Caption
              ( if isSignal fSpec r
                  then
                    (str . l) (NL "Openstaande taken voor ", EN "Tasks yet to be performed by ")
                      <> (commaPandocOr outputLang' . map (str . fullName) . rolesOf $ r)
                  else
                    (str . l) (NL "Inconsistenties in invariant ", EN "Inconsistencies in invariant ")
                      <> (str . label) r
              )
              -- Alignment:
              (replicate 1 (AlignLeft, 1))
              -- Headers:
              (map (amPandoc . ameaMrk) . meanings $ r)
              -- Rows:
              (mkInvariantViolationsError (applyViolText fSpec) (r, ps))

    rolesOf r = L.nub [rol | (rol, rul) <- fRoleRuls fSpec, r == rul]
    mkInvariantViolationsError :: (Rule -> AAtomPair -> Text) -> (Rule, AAtomPairs) -> [[Blocks]]
    mkInvariantViolationsError applyViolText' (r, ps) =
      [[(para . strong . text) violationMessage]]
      where
        violationMessage :: Text
        violationMessage =
          T.unlines
            $ [ if length ps == 1
                  then "There is a violation of RULE " <> fullName r <> ":"
                  else "There are " <> tshow (length ps) <> " violations of RULE " <> fullName r <> ":"
              ]
            <> (map ("  " <>) . listPairs 10 . Set.toList $ ps)
        listPairs :: Int -> [AAtomPair] -> [Text]
        listPairs i xs =
          case xs of
            [] -> []
            h : tl
              | i == 0 -> ["  ... (" <> tshow (length xs) <> " more)"]
              | otherwise -> applyViolText' r h : listPairs (i - 1) tl

    violtable :: Rule -> AAtomPairs -> Blocks
    violtable r ps =
      if hasantecedent r && isIdent (antecedent r) -- note: treat 'isIdent (consequent r) as binary table.
        then
          legacyTable -- No caption:
            mempty
            -- Alignment:
            [(AlignLeft, 1.0)]
            -- Header:
            [(plain . str . label . source . formalExpression) r]
            -- Data rows:
            [ [(plain . str . showValADL . apLeft) p]
              | p <- take 10 . toList $ ps -- max 10 rows
            ]
        else
          legacyTable -- No caption:
            mempty
            -- Alignment:
            (replicate 2 (AlignLeft, 1 / 2))
            -- Header:
            [(plain . str . label . source . formalExpression) r, (plain . str . label . target . formalExpression) r]
            -- Data rows:
            [ [(plain . str . showValADL . apLeft) p, (plain . str . showValADL . apRight) p]
              | p <- take 10 . toList $ ps -- max 10 rows
            ]
