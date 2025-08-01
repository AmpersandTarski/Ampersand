module Ampersand.Output.ToPandoc.ChapterDataAnalysis (chpDataAnalysis) where

import Ampersand.ADL1
import Ampersand.FSpec.Crud
import Ampersand.FSpec.ToFSpec.ADL2Plug
import Ampersand.Graphic.ClassDiagram -- (Class(..),CdAttribute(..))
import Ampersand.Graphic.Fspec2ClassDiagrams
import Ampersand.Output.ToPandoc.SharedAmongChapters
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

------------------------------------------------------------
-- DESCR -> the data analysis contains a section for each class diagram in the fSpec
--         the class diagram and property rules are printed
chpDataAnalysis ::
  (HasDirOutput env, HasDocumentOpts env) =>
  env ->
  FSpec ->
  (Blocks, [Picture])
chpDataAnalysis env fSpec = (theBlocks, [])
  where
    -- shorthand for easy localizing
    l :: LocalizedStr -> Text
    l = localize outputLang'
    outputLang' :: Lang
    outputLang' = outputLang env fSpec
    sectionLevel = 2

    theBlocks =
      xDefBlck env fSpec DataAnalysis -- The header
        <> ( case outputLang' of
               Dutch ->
                 para
                   ( "Dit hoofdstuk bevat het resultaat van de gegevensanalyse. "
                       <> "De opbouw is als volgt:"
                   )
                   <> para
                     ( "We beginnen met het classificatiemodel, gevolgd door "
                         <> "een overzicht van alle relaties, die samen de basis vormen van de rest van deze analyse. "
                         <> "Ten slotte volgen achtereenvolgend het logische- en technische gegevensmodel."
                     )
               English ->
                 para
                   ( "This chapter contains the result of the data analysis. "
                       <> "It is structured as follows:"
                   )
                   <> para
                     ( "We start with the classification model, followed by "
                         <> "a list of all relations, that are the foundation of the rest of the analysis. "
                         <> "Finally, the logical and technical data model are discussed."
                     )
           )
        <> ( if null (classes $ clAnalysis fSpec)
               then mempty
               else
                 header
                   sectionLevel
                   ( text . l $ (NL "Classificaties", EN "Classifications")
                   )
                   <> para
                     ( case outputLang' of
                         Dutch ->
                           "Een aantal concepten zit in een classificatiestructuur. "
                             <> if crossRefsAreFixed
                               then
                                 "Deze is weergegeven in "
                                   <> hyperLinkTo classificationPicture
                                   <> "."
                               else mempty
                         English ->
                           "A couple of concepts are in a classification structure. "
                             <> if crossRefsAreFixed
                               then
                                 "This is shown in "
                                   <> hyperLinkTo classificationPicture
                                   <> "."
                               else mempty
                     )
                   <> xDefBlck env fSpec classificationPicture
           )
        <> daRulesSection
        <> logicalDataModelBlocks
        <> technicalDataModelBlocks
        <> crudMatrixSection
      where
        classificationPicture = makePicture env fSpec PTClassificationDiagram

    logicalDataModelBlocks =
      header
        sectionLevel
        ( case outputLang' of
            Dutch -> text "Logisch gegevensmodel"
            English -> text "Logical data model"
        )
        <> para
          ( case outputLang' of
              Dutch ->
                text "De afspraken zijn vertaald naar een gegevensmodel. "
                  <> if crossRefsAreFixed
                    then
                      text "Dit gegevensmodel is in "
                        <> hyperLinkTo logicalDataModelPicture
                        <> text " weergegeven."
                    else mempty
              English ->
                text "The functional requirements have been translated into a data model. "
                  <> if crossRefsAreFixed
                    then
                      text "This model is shown by "
                        <> hyperLinkTo logicalDataModelPicture
                        <> text "."
                    else mempty
          )
        <> xDefBlck env fSpec logicalDataModelPicture
        <> let nrOfClasses = length (classes oocd)
            in case outputLang' of
                 Dutch ->
                   para
                     ( case nrOfClasses of
                         0 -> text "Er zijn geen gegevensverzamelingen."
                         1 -> text "Er is één gegevensverzameling, die in de volgende paragraaf in detail is beschreven:"
                         _ ->
                           text ("Er zijn " <> count Dutch nrOfClasses "gegevensverzameling" <> ". ")
                             <> text "De details van elk van deze gegevensverzameling worden, op alfabetische volgorde, in de twee nu volgende tabellen beschreven:"
                     )
                 English ->
                   para
                     ( case nrOfClasses of
                         0 -> text "There are no entity types."
                         1 -> text "There is only one entity type:"
                         _ ->
                           text ("There are " <> count English nrOfClasses "entity type" <> ".")
                             <> text "The details of each entity type are described (in alphabetical order) in the following two tables:"
                     )
                 <> conceptTables
                 <> mconcat (map detailsOfClass (L.sortBy (compare `on` name) (map fst $ classes oocd)))
      where
        logicalDataModelPicture = makePicture env fSpec (PTLogicalDataModelOfContext False)

    oocd :: ClassDiag
    oocd =
      cdAnalysis
        False
        env
        fSpec
        (fromMaybe (fatal "No context found in FSpec") (originalContext fSpec))

    conceptTables :: Blocks -- This produces two separate tables:
    -- The first table contains the concepts that have their own table in the logical data model.
    -- The second table contains all other concepts.
    conceptTables =
      legacyTable
        ( text
            . l
            $ ( NL "Logische gegevensverzamelingen",
                EN "Logical entity types"
              )
        )
        [(AlignLeft, 2 / 8), (AlignLeft, 4 / 8), (AlignLeft, 1 / 8), (AlignLeft, 1 / 8)]
        [ (plain . text . l) (NL "Concept", EN "Concept"),
          (plain . text . l) (NL "Betekenis", EN "Meaning"),
          (plain . text . l) (NL "Aantal", EN "Count"),
          (plain . text . l) (NL "Vullingsgraad", EN "Filling degree")
        ]
        [ [ (plain . text . label) c,
            meaningOf c
              <> (mconcat . map (amPandoc . explMarkup) . purposesOf fSpec outputLang') c,
            (plain . text . tshow . Set.size . atomsInCptIncludingSmaller fSpec) c,
            (plain . text . tshow)
              ( percent
                  ( sum
                      [ Set.size pairs
                        | attr <- attributesOfConcept fSpec c,
                          pairs <- [(pairsInExpr fSpec . attExpr) (attr :: SqlAttribute)]
                      ]
                  )
                  (Set.size (atomsInCptIncludingSmaller fSpec c) * length (attributesOfConcept fSpec c))
              )
          ]
          | c <-
              L.sortBy (compare `on` name)
                . filter isKey
                . L.delete ONE
                . toList
                $ concs fSpec
        ]
        <> legacyTable
          ( text
              . l
              $ ( NL "Overige attributen",
                  EN "Other attributes"
                )
          )
          [(AlignLeft, 1 / 6), (AlignLeft, 4 / 6), (AlignLeft, 1 / 6)]
          [ (plain . text . l) (NL "Concept", EN "Concept"),
            (plain . text . l) (NL "Voorbeelden", EN "Examples"),
            (plain . text . l) (NL "Aantal", EN "Count")
          ]
          [ [ (plain . text . label) c
            ] -- max 20 voorbeelden van atomen van concept c
              ++ (map (plain . text . showA) . take 20 . Set.toList . atomsInCptIncludingSmaller fSpec) c
              ++ [ (plain . text . tshow . Set.size . atomsInCptIncludingSmaller fSpec) c
                 ]
            | c <-
                L.sortBy (compare `on` name)
                  . toList
                  . Set.filter (not . isKey)
                  $ concs fSpec
          ]
      where
        isKey :: A_Concept -> Bool
        isKey cpt = cpt `elem` ooCpts oocd
        meaningOf :: A_Concept -> Blocks
        meaningOf = agregateMany . map (maybe mempty meaning2Blocks . meaning outputLang') . concDefs fSpec
    agregateMany :: [Many a] -> Many a
    agregateMany = Many . join . unMany . fromList . fmap unMany
    percent :: (Integral a, Show a) => a -> a -> Text
    percent num denom =
      if denom == 0
        then tshow num
        else tshow num <> "(" <> tshow (round ((fromIntegral num * 100.0 / fromIntegral denom) :: Float) :: Integer) <> "%)"

    detailsOfClass :: Class -> Blocks
    detailsOfClass cl =
      header
        (sectionLevel + 1)
        ((text . l) (NL "Gegevensverzameling: ", EN "Entity type: ") <> (emph . strong . text . fullName) cl)
        <> case clcpt cl of
          Nothing -> mempty
          Just (cpt, _) -> purposes2Blocks env (purposesOf fSpec outputLang' cpt)
        <> (para . text . l)
          ( NL ("Deze gegevensverzameling heeft " <> tshow n <> " elementen en bevat de volgende attributen: "),
            EN ("This entity type has " <> tshow n <> " elements and contains the following attributes: ")
          )
        <> simpleTable
          [ (plain . text . l) (NL "Attribuut", EN "Attribute"),
            (plain . text . l) (NL "Type", EN "Type"),
            (plain . text . l) (NL "gevuld", EN "filled"),
            (plain . text . l) (NL "#uniek", EN "#unique")
          ]
          ( [ [ (plain . text . text1ToText . sqlColumNameToText1 . attSQLColName) attr,
                (plain . text) ((label . target . attExpr) attr <> "(" <> tshow nTgtConcept <> ")"), -- use "tshow.attType" for the technical type.
                (plain . text) (percent (Set.size pairs) n),
                (plain . text . tshow . Set.size . Set.map apRight) pairs
              ]
              | Just cpt <- [clcpt cl],
                attr <- attributesOfConcept fSpec (fst cpt),
                nTgtConcept <- [(Set.size . atomsInCptIncludingSmaller fSpec . target . attExpr) (attr :: SqlAttribute)],
                pairs <- [(pairsInExpr fSpec . attExpr) (attr :: SqlAttribute)]
            ]
              <> [ [ (plain . text . text1ToText . sqlColumNameToText1 . attSQLColName) attr,
                     (plain . text) ((label . target . attExpr) attr <> "(" <> tshow nTgtConcept <> ")"), -- use "tshow.attType" for the technical type.
                     (plain . text) (percent (Set.size pairs) n),
                     (plain . text . tshow . Set.size . Set.map apRight) pairs
                     -- , (plain . text . tshow) nTgtConcept
                   ]
                   | Just (cpt, _) <- [clcpt cl],
                     cpt' <- generalizationsOf fSpec cpt,
                     cpt /= cpt',
                     attr <- attributesOfConcept fSpec cpt',
                     nTgtConcept <- [(Set.size . atomsInCptIncludingSmaller fSpec . target . attExpr) (attr :: SqlAttribute)],
                     pairs <- [pairsInExpr fSpec (EDcI cpt .:. EEps cpt' (Sign cpt cpt') .:. attExpr attr)]
                 ]
          )
        <> let asscs =
                 [ assoc | assoc <- assocs oocd, assSrc assoc == clName cl || assTgt assoc == clName cl
                 ]
            in case asscs of
                 [] -> para (text (fullName cl) <> text (l (NL " heeft geen associaties.", EN " has no associations.")))
                 _ ->
                   para (text (fullName cl) <> text (l (NL " heeft de volgende associaties: ", EN " has the following associations: ")))
                     <> simpleTable
                       [ (plain . text . l) (NL "Source", EN "Source"),
                         (plain . text . l) (NL "uniek", EN "unique"),
                         (plain . text . l) (NL "Associatie", EN "Association"),
                         (plain . text . l) (NL "Target", EN "Target"),
                         (plain . text . l) (NL "uniek", EN "unique")
                       ]
                       [ [ (plain . text) (label (source rel) <> "(" <> tshow nSrcConcept <> ")"), -- use "tshow.attType" for the technical type.
                           (plain . text) (percent (Set.size (Set.map apLeft pairs)) nSrcConcept),
                           (plain . text) (label rel <> "(" <> tshow (Set.size pairs) <> ")"),
                           (plain . text) (label (target rel) <> "(" <> tshow nTgtConcept <> ")"), -- use "tshow.attType" for the technical type.
                           (plain . text) (percent (Set.size (Set.map apRight pairs)) nTgtConcept)
                         ]
                         | Just rel <- map assmdcl asscs,
                           nSrcConcept <- [(Set.size . atomsInCptIncludingSmaller fSpec . source) rel],
                           nTgtConcept <- [(Set.size . atomsInCptIncludingSmaller fSpec . target) rel],
                           pairs <- [(pairsInExpr fSpec . EDcD) rel]
                       ]
      where
        n :: Int
        n = (Set.size . atomsInCptIncludingSmaller fSpec . fst . unJust . clcpt) cl
          where
            unJust (Just cpt) = cpt; unJust _ = fatal "unexpected Just"
    {- <>
       if (null.assrhr) assoc
       then fatal "Shouldn't happen: flip the relation for the right direction!"
       else para $ case outputLang' of
          Dutch   ->   case assrhm assoc of
                             Mult MinZero MaxOne  -> "Ieder(e) " <> (emph.text.assSrc) assoc <> " heeft hooguit één "   <> (emph.text.assTgt) assoc <> "."
                             Mult MinZero MaxMany -> mempty
                             Mult MinOne  MaxOne  -> "Ieder(e) " <> (emph.text.assSrc) assoc <> " heeft precies één "    <> (emph.text.assTgt) assoc <> "."
                             Mult MinOne  MaxMany -> "Ieder(e) " <> (emph.text.assSrc) assoc <> " heeft ten minste één " <> (emph.text.assTgt) assoc <> "."
                     <> case asslhm assoc of
                             Mult MinZero MaxOne  -> " Ieder(e) " <> (emph.text.assTgt) assoc <> " kan hooguit één "      <> (emph.text.assSrc) assoc <> " hebben."
                             Mult MinZero MaxMany -> mempty
                             Mult MinOne  MaxOne  -> " Ieder(e) " <> (emph.text.assTgt) assoc <> " kan precies één " <> (emph.text.assSrc) assoc <>       " hebben."
                             Mult MinOne  MaxMany -> " Ieder(e) " <> (emph.text.assTgt) assoc <> " kan ten minste één "    <> (emph.text.assSrc) assoc <> " hebben."

          English ->    case assrhm assoc of
                             Mult MinZero MaxOne  -> "Every " <> (emph.text.assSrc) assoc <>" has at most one "  <> (emph.text.assTgt) assoc <>  "."
                             Mult MinZero MaxMany -> mempty
                             Mult MinOne  MaxOne  -> "Every " <> (emph.text.assSrc) assoc <>" has exactly one "  <> (emph.text.assTgt) assoc <>  "."
                             Mult MinOne  MaxMany -> "Every " <> (emph.text.assSrc) assoc <>" has at least one " <> (emph.text.assTgt) assoc <>  "."
                     <> case asslhm assoc of
                             Mult MinZero MaxOne  -> " For this association each " <> (emph.text.assTgt) assoc <> " has at most one "  <> (emph.text.assSrc) assoc <> "."
                             Mult MinZero MaxMany -> mempty
                             Mult MinOne  MaxOne  -> " For this association each " <> (emph.text.assTgt) assoc <> " has exactly one "  <> (emph.text.assSrc) assoc <> "."
                             Mult MinOne  MaxMany -> " For this association each " <> (emph.text.assTgt) assoc <> " has at least one " <> (emph.text.assSrc) assoc <> "."
    -}

    crudMatrixSection :: Blocks
    crudMatrixSection =
      header sectionLevel (text . l $ (NL "Logisch gegevensmodel", EN "Logical data model"))
        <> mconcat
          [ simpleTable
              [plainText "Concept", plainText "C", plainText "R", plainText "U", plainText "D"]
              [ [ (plainText . label) cncpt,
                  mconcat . map (plainText . label) $ ifcsC,
                  mconcat . map (plainText . label) $ ifcsR,
                  mconcat . map (plainText . label) $ ifcsU,
                  mconcat . map (plainText . label) $ ifcsD
                ]
                | (cncpt, (ifcsC, ifcsR, ifcsU, ifcsD)) <- crudObjsPerConcept (crudInfo fSpec)
              ]
          ]

    technicalDataModelBlocks =
      header
        sectionLevel
        ( case outputLang' of
            Dutch -> "Technisch datamodel"
            English -> "Technical datamodel"
        )
        <> para
          ( case outputLang' of
              Dutch ->
                "De afspraken zijn vertaald naar een technisch datamodel. "
                  <> ( if crossRefsAreFixed
                         then "Dit model is in " <> hyperLinkTo technicalDataModelPicture <> " weergegeven."
                         else mempty
                     )
              English ->
                "The functional requirements have been translated into a technical data model. "
                  <> ( if crossRefsAreFixed
                         then "This model is shown by " <> hyperLinkTo technicalDataModelPicture <> "."
                         else mempty
                     )
          )
        <> xDefBlck env fSpec technicalDataModelPicture
        <> para
          ( let nrOfTables = length (filter isTable (plugInfos fSpec))
             in case outputLang' of
                  Dutch -> text ("Het technisch datamodel bestaat uit de volgende " <> tshow nrOfTables <> " tabellen:")
                  English -> text ("The technical datamodel consists of the following " <> tshow nrOfTables <> " tables:")
          )
        <> mconcat [detailsOfplug p | p <- L.sortBy (compare `on` (T.toLower . text1ToText . showUnique)) (plugInfos fSpec), isTable p]
      where
        isTable :: PlugInfo -> Bool
        isTable (InternalPlug TblSQL {}) = True
        isTable (InternalPlug BinSQL {}) = True
        detailsOfplug :: PlugInfo -> Blocks
        detailsOfplug p =
          header
            3
            ( case outputLang' of
                Dutch -> "Tabel: "
                English -> "Table: "
                <> (text . text1ToText . showUnique) p
            )
            <> case p of
              InternalPlug tbl@TblSQL {} ->
                ( case outputLang' of
                    Dutch ->
                      para (text $ "Deze tabel heeft de volgende " <> (tshow . length . attributes) tbl <> " attributen:")
                    English ->
                      para (text $ "This table has the following " <> (tshow . length . attributes) tbl <> " attributes:")
                )
                  <> showAttributes (plugAttributes tbl)
              InternalPlug bin@BinSQL {} ->
                para
                  ( (text . l)
                      ( NL "Dit is een koppeltabel, die ",
                        EN "This is a link-table, implementing "
                      )
                      <> primExpr2pandocMath
                        outputLang'
                        ( case dLkpTbl bin of
                            [store] -> EDcD (rsDcl store)
                            ss -> fatal ("Exactly one relation sould be stored in BinSQL. However, there are " <> tshow (length ss))
                        )
                      <> (text . l)
                        ( NL " implementeert. De tabel bestaat uit de volgende kolommen:",
                          EN ". It contains the following columns:"
                        )
                  )
                  <> showAttributes (plugAttributes bin)

        showAttributes :: NE.NonEmpty SqlAttribute -> Blocks
        showAttributes = bulletList . NE.toList . fmap showAttribute
          where
            showAttribute att =
              para
                ( (strong . text . text1ToText . sqlColumNameToText1 . attSQLColName) att
                    <> linebreak
                    <> case attUse att of
                      PrimaryKey _ -> case outputLang' of
                        Dutch -> "Dit attribuut is de primaire sleutel. "
                        English -> "This attribute is the primary key. "
                      ForeignKey c ->
                        ( case outputLang' of
                            Dutch -> "Dit attribuut verwijst naar een rij in de tabel "
                            English -> "This attribute is a foreign key to "
                        )
                          <> (text . label) c
                      PlainAttr ->
                        ( case outputLang' of
                            Dutch -> "Dit attribuut implementeert "
                            English -> "This attribute implements "
                        )
                          <> primExpr2pandocMath outputLang' (attExpr att)
                          <> "."
                    <> linebreak
                    <> (code . tshow . attType) att
                    <> ", "
                    <> ( case outputLang' of
                           Dutch ->
                             (if attNull att then "Optioneel" else "Verplicht")
                               <> (if attUniq att then ", Uniek" else "")
                               <> "."
                           English ->
                             (if attNull att then "Optional" else "Mandatory")
                               <> (if attUniq att then ", Unique" else "")
                               <> "."
                       )
                )
    technicalDataModelPicture = makePicture env fSpec PTTechnicalDataModel

    daRulesSection :: Blocks
    daRulesSection =
      mconcat
        [ header sectionLevel . text $ l (NL "Regels", EN "Rules"),
          para
            . text
            $ l
              ( NL
                  $ "Nu volgt een opsomming van alle regels door de term van elke regel af te drukken. "
                  <> "Eerst worden de procesregels gegeven, vervolgens de invarianten.",
                EN
                  $ "In this section an overview of all rules by printing the term of each rule. "
                  <> "The process rules are given first, followed by the invariants."
              ),
          docRules
            (NL "Procesregels", EN "Process rules")
            ( NL "Procesregels zijn regels waarvan de overtredingen worden gesignaleerd.",
              EN "Process rules are rules that are signalled. "
            )
            ( NL "Deze specificatie bevat geen procesregels.",
              EN "This specification does not contain any process rules."
            )
            (NL "Procesregel", EN "Process rule")
            (signals fSpec),
          docRules
            (NL "Invarianten", EN "Invariants")
            ( NL "Invarianten zijn regels die door de database worden afgedwongen. Er wordt gegarandeerd dat overtredingen niet kunnen voorkomen in de database.",
              EN "Invariants are rules that are enforced by the database. It is guaranteed that violations cannot occur in the database."
            )
            ( NL "Deze specificatie bevat geen invarianten.",
              EN "This specification does not contain any invariants."
            )
            (NL "Invariant", EN "Invariant")
            (invariants fSpec)
        ]
      where
        docRules :: LocalizedStr -> LocalizedStr -> LocalizedStr -> LocalizedStr -> Rules -> Blocks
        docRules title intro noRules heading rules =
          if null rules
            then (para . text . l) noRules
            else
              mconcat
                $ [ header (sectionLevel + 1) . text $ l title,
                    para . text $ l intro
                  ]
                <> map (docRule heading) (toList rules)

        docRule :: LocalizedStr -> Rule -> Blocks
        docRule heading rule =
          mconcat
            [ plain $ strong (text (l heading <> ": ") <> (emph . text . label) rule),
              mconcat . map (amPandoc . explMarkup) . purposesOf fSpec outputLang' $ rule,
              printMeaning outputLang' rule,
              para (showMath rule),
              if isSignal fSpec rule
                then mempty
                else case rrviol rule of
                  Nothing -> mempty
                  Just sgmts ->
                    para
                      ( if isSignal fSpec rule
                          then
                            (text . l)
                              ( NL "Een overtreding van deze regel wordt gesignaleerd door middel van de melding: ",
                                EN "Violations of this rule are reported with the following message: "
                              )
                          else
                            (text . l)
                              ( NL "Een overtreding van deze regel resulteert in de volgende foutmelding aan de gebruiker: ",
                                EN "Violations of this rule will result in an error message for the user: "
                              )
                      )
                      <> bulletList [para $ violation2Inlines env fSpec sgmts]
            ]

primExpr2pandocMath :: Lang -> Expression -> Inlines
primExpr2pandocMath lang e =
  case e of
    (EDcD d) ->
      case lang of
        Dutch -> text "de relatie "
        English -> text "the relation "
        <> math ((mathLabel . source) d <> " \\rightarrow {" <> mathLabel d <> "} " <> (mathLabel . target) d)
    (EFlp (EDcD d)) ->
      case lang of
        Dutch -> text "de relatie "
        English -> text "the relation "
        <> math ((mathLabel . source) d <> " \\leftarrow  {" <> mathLabel d <> "} " <> (mathLabel . target) d)
    (EIsc (r1, _)) ->
      let srcTable = case r1 of
            EDcI c -> c
            _ -> fatal ("Unexpected term: " <> tshow r1)
       in case lang of
            Dutch -> text "de identiteitsrelatie van "
            English -> text "the identityrelation of "
            <> math (mathLabel srcTable)
    (EDcI c) ->
      case lang of
        Dutch -> text "de identiteitsrelatie van "
        English -> text "the identityrelation of "
        <> math (mathLabel c)
    (EEps c _) ->
      case lang of
        Dutch -> text "de identiteitsrelatie van "
        English -> text "the identityrelation of "
        <> math (mathLabel c)
    _ -> fatal ("Have a look at the generated Haskell to see what is going on..\n" <> tshow e)

mathLabel :: (Labeled a) => a -> Text
mathLabel = T.concatMap escape . label
  where
    escape :: Char -> Text
    escape c
      | c == '\\' = "\\\\"
      | c `elem` ("#$%^&_{}~" :: String) = "\\" <> T.singleton c
      | otherwise = T.singleton c
