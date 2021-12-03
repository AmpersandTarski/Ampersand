{-# LANGUAGE ScopedTypeVariables #-}

module Ampersand.Output.ToPandoc.ChapterConceptualAnalysis where

import Ampersand.Graphic.ClassDiagram
import Ampersand.Graphic.Fspec2ClassDiagrams
import Ampersand.Output.ToPandoc.SharedAmongChapters
import qualified RIO.List as L
import qualified RIO.Set as Set
import qualified RIO.Text as T

chpConceptualAnalysis ::
  (HasDirOutput env, HasDocumentOpts env) =>
  env ->
  Int ->
  FSpec ->
  (Blocks, [Picture])
chpConceptualAnalysis env lev fSpec =
  ( --  *** Header ***
    xDefBlck env fSpec ConceptualAnalysis
      <> caIntro --  *** Intro  ***
      <> (mconcat . map caSection . orderingByTheme env) fSpec,
    pictures
  )
  where
    -- shorthand for easy localizing
    l :: LocalizedStr -> Text
    l = localize outputLang'
    outputLang' = outputLang env fSpec
    caIntro :: Blocks
    caIntro =
      if null purps
      then case outputLang' of
             Dutch ->
               para
                 ( "Dit hoofdstuk analyseert de \"taal van de business\", om functionele eisen ten behoeve van "
                     <> (singleQuoted . str . name) fSpec
                     <> " te kunnen bespreken. "
                     <> "Deze analyse beoogt om een bouwbare, maar oplossingsonafhankelijke specificatie op te leveren. "
                     <> "Het begrijpen van tekst vereist deskundigheid op het gebied van conceptueel modelleren."
                     <> "(Deze alinea is gegenereerd. Vervang deze tekst door een PURPOSE CONTEXT te beschrijven.)"
                 )
             English ->
               para
                 ( "This chapter analyses the \"language of the business\" for the purpose of discussing functional requirements of "
                     <> (singleQuoted . str . name) fSpec
                     <> "."
                     <> "The analysis is necessary is to obtain a buildable specification that is solution independent. "
                     <> "The text targets readers with sufficient skill in conceptual modeling."
                     <> "(This paragraph has been generated. Replace it by defining a PURPOSE CONTEXT in your script.)"
                 )
      else purps -- This explains the purpose of this context.
      where
        purps = purposes2Blocks env (purposesOf fSpec outputLang' fSpec)
    pictures =
      map pictOfPat (vpatterns fSpec)
        <> map pictOfConcept (Set.elems $ concs fSpec)
        <> map pictOfRule (Set.elems $ vrules fSpec)
    -----------------------------------------------------
    -- the Picture that represents this pattern's conceptual graph
    pictOfPat :: Pattern -> Picture
    pictOfPat = makePicture env fSpec . PTCDPattern
    pictOfRule :: Rule -> Picture
    pictOfRule = makePicture env fSpec . PTCDRule
    pictOfConcept :: A_Concept -> Picture
    pictOfConcept = makePicture env fSpec . PTCDConcept
    caSection :: ThemeContent -> Blocks
    caSection themeContent
      | isNothing (patOfTheme themeContent)
          && null (cptsOfTheme themeContent)
          && null (dclsOfTheme themeContent)
          && null (rulesOfTheme themeContent) =
        mempty
      | otherwise =
        --  *** Header of the theme: ***
        (xDefBlck env fSpec . XRefSharedLangTheme . patOfTheme) themeContent
          -- The section starts with the reason(s) why this pattern exist(s)
          <> case patOfTheme themeContent of
            Just pat -> purposes2Blocks env (purposesOf fSpec outputLang' pat)
            Nothing -> mempty
          -- followed by the purposes and definitions of every concept that is defined (by a CONCEPT statement) in this pattern
          <> (mconcat . map (printConcept env l) . cptsOfTheme) themeContent
          -- At this point the reader gets a diagram with the classes and relations between those classes.
          <> ( case (outputLang', patOfTheme themeContent) of
                 (Dutch, Just pat) ->
                   -- announce the conceptual diagram
                   para (hyperLinkTo (pictOfPat pat) <> "Conceptueel diagram van " <> (singleQuoted . str . name) pat <> ".")
                     -- draw the conceptual diagram
                     <> (xDefBlck env fSpec . pictOfPat) pat
                 (English, Just pat) ->
                   para (hyperLinkTo (pictOfPat pat) <> "Conceptual diagram of " <> (singleQuoted . str . name) pat <> ".")
                     <> (xDefBlck env fSpec . pictOfPat) pat
                 (_, Nothing) -> mempty
             )
          -- Now we discuss the attributes of each entity (with sufficiently documented attributes) in one subsection
          <> mconcat (map fst caSubsections)
          -- Finally we discuss the remaining attributes (of smaller entities) and remaining relations
          -- This list contains empty spots for relations without documentation.
          <> caRemainingRelations
          <> (
               -- print the rules that are defined in this pattern.
               case map caRule . Set.elems $ invariants fSpec `Set.intersection` (Set.fromList . map (cRul . theLoad) . rulesOfTheme) themeContent of
                 [] -> mempty
                 blocks ->
                   ( case outputLang' of
                       Dutch ->
                         header (lev + 3) "Regels"
                           <> plain "Deze paragraaf geeft een opsomming van de regels met een verwijzing naar de gemeenschappelijke taal van de belanghebbenden ten behoeve van de traceerbaarheid."
                       English ->
                         header (lev + 3) "Rules"
                           <> plain "This section itemizes the rules with a reference to the shared language of stakeholders for the sake of traceability."
                   )
                     <> definitionList blocks
             )
      where
        themeClasses :: [Class]
        themeClasses = case patOfTheme themeContent of
          Just pat -> classes (cdAnalysis False fSpec pat)
          Nothing -> []

        -- Every subsection documents one concept with its identities and attributes. If there are no attributes, there is no subsection.
        caSubsections :: [(Blocks, [Relation])]
        caSubsections =
          [ ( header 3 (str (name cl)) <> mconcat (identityBlocks cpt)<>entityBlocks,
              entityRels
            )
            | cl <- themeClasses, Just cpt<-[clcpt cl],
              (entityBlocks, entityRels) <- [caEntity cl],
              length entityRels > 1
          ] <>
          [ ( header 3 (str (l (NL "Overige identiteiten", EN "Other identities"))) <>
              mconcat [ blck | cpt<-orphanIdentities, blck<-identityBlocks (cCpt cpt)],
              mempty
            )
            | not (null orphanIdentities)
          ]
          where
            orphanIdentities :: [CptCont]
            orphanIdentities =
                [cc |
                ncc <- cptsOfTheme themeContent,
                let cc = theLoad ncc,
                cl <- themeClasses,
                Just cpt <- [clcpt cl],
                cpt /= cCpt cc,
                (_, entityRels) <- [caEntity cl],
                null entityRels]

{-
        identityBlocks :: A_Concept->[Blocks]
        identityBlocks cpt
         = [ purposes2Blocks env (purposesOf fSpec outputLang' r)
           | rc<-idRulesOfTheme themeContent, let r=cRul (theLoad rc), Identity c<-[rrkind r], cpt==c
           , trace (showA r<>"cpt = "<>showA cpt<>"\nc = "<>showA c) True
           ]
 data CptCont = CCpt
  { cCpt :: A_Concept,
    cCptDefs :: [AConceptDef],
    cCptPurps :: [Purpose]
  }
-}
        -- caEntity shows a table with the purposes and meanings of the attributes of one concept
        caEntity :: Class -> (Blocks, [Relation])
        caEntity cl =
          ( simpleTable
              [ (plain . text . l) (NL "Attribuut", EN "Attribute"),
                (plain . text . l) (NL "Betekenis", EN "Meaning")
              ]
              ( [ [ (plain . text . name) attr,
                    defineRel rel
                  ]
                  | attr <- clAtts cl,
                    rel <- lookupRel attr
                ]
              ),
            [rel | attr <- clAtts cl, rel <- lookupRel attr]
          )
          where
            lookupRel :: CdAttribute -> [Relation]
            lookupRel attr =
              L.nub
                [ r
                  | Nr _ decl <- dclsOfTheme themeContent,
                    let rel = cDcl decl,
                    (r, s, t) <- [(rel, source rel, target rel), (rel, target rel, source rel)],
                    name r == name attr,
                    name cl == name s,
                    attTyp attr == name t,
                    (not . null . decMean) rel
                ]

        defineRel :: Relation -> Blocks
        defineRel rel =
          case map (amPandoc . ameaMrk) (decMean rel) of
            [] -> mempty
            [blocks] -> blocks
            bss -> bulletList bss
            <> (printPurposes . purposesOf fSpec outputLang') rel

        -- identityBlocks documents the IDENT rules of the concept of an entity.
        identityBlocks :: A_Concept->[Blocks]
        identityBlocks cpt
         = [ purposes2Blocks env (purposesOf fSpec outputLang' r)
           | rc<-idRulesOfTheme themeContent, let r=cRul (theLoad rc), Identity c<-[rrkind r], cpt==c
           , trace (showA r<>"cpt = "<>showA cpt<>"\nc = "<>showA c) True
           ]

        caRemainingRelations :: Blocks
        caRemainingRelations =
          simpleTable
            [ (plain . text . l) (NL "Relatie", EN "Relation"),
              (plain . text . l) (NL "Betekenis", EN "Meaning")
            ]
            ( [ [ (plain . text) (name rel <> " " <> if null cls then tshow (sign rel) else l (NL " (Attribuut van ", EN " (Attribute of ") <> T.concat cls <> ")"),
                  defineRel rel -- use "tshow.attType" for the technical type.
                ]
                | rel <- rels,
                  let cls = [name cl | cl <- themeClasses, (_, entRels) <- [caEntity cl], rel `elem` entRels]
              ]
            )
          where
            rels :: [Relation]
            rels = map (cDcl . theLoad) (dclsOfTheme themeContent) L.\\ Set.toList entityRels

            entityRels :: Set Relation
            entityRels = Set.unions (map (Set.fromList . snd) caSubsections)

    {-
      printConcept :: Numbered CptCont -> Blocks
      printConcept nCpt
        = -- Purposes:
          case (nubByText . cCptDefs . theLoad) nCpt of
             []   -> mempty  -- There is no definition of the concept
             [cd] -> printCDef cd Nothing
             cds  -> mconcat
                    [printCDef cd (Just $ T.snoc "." suffx)
                    |(cd,suffx) <- zip cds ['a' ..]  -- There are multiple definitions. Which one is the correct one?
                    ]
          <> (printPurposes . cCptPurps . theLoad) nCpt
            where
             fspecFormat = view fspecFormatL env
             nubByText = L.nubBy (\x y -> acddef2 x ==acddef2 y && acdref x == acdref y) -- fixes https://github.com/AmpersandTarski/Ampersand/issues/617
             printCDef :: AConceptDef -- the definition to print
                    -> Maybe Text -- when multiple definitions exist of a single concept, this is to distinguish
                    -> Blocks
             printCDef cDef suffx
               = definitionList
                  [(   str (l (NL"Definitie " ,EN "Definition "))
                    <> ( if fspecFormat `elem` [Fpdf, Flatex]
                         then (str . tshow .theNr) nCpt
                         else (str . name) cDef
                       )
                    <> str (fromMaybe "" suffx) <> ":"
                   , [para (   newGlossaryEntry (name cDef<>fromMaybe "" suffx) (acddef cDef)
                            <> ( if fspecFormat `elem` [Fpdf, Flatex]
                                 then rawInline "latex"
                                        ("~"<>texOnlyMarginNote
                                                ("\\gls{"<>escapeLatex
                                                            (name cDef<>fromMaybe "" suffx)
                                                    <>"}"
                                                )
                                        )
                                 else mempty
                               )
                            <> str (acddef cDef)
                            <> if T.null (acdref cDef) then mempty
                               else str (" ["<>acdref cDef<>"]")
                           )
                     ]
                   )
                  ]

     unused code, possibly useful later...
      caRelation :: Relation -> (Inlines, [Blocks])
      caRelation d = (titel, [body])
         where
            titel = xDefInln env fSpec (XRefConceptualAnalysisRelation d) <> ": "<>showMath d
            purp =  purposes2Blocks env (purposesOf fSpec outputLang' d)
            body =  para linebreak
                    -- First the reason why the relation exists, if any, with its properties as fundamental parts of its being..
                    <> ( case ( null purp, outputLang') of
                      (True , Dutch)   -> plain ("De volgende " <> str nladjs <> " is gedefinieerd: ")
                      (True , English) -> plain ("The following " <> str ukadjs <> " has been defined: ")
                      (False, Dutch)   -> purp <> plain ("Voor dat doel is de volgende " <> str nladjs <> " gedefinieerd: ")
                      (False, English) -> purp <> plain ("For this purpose, the following " <> str ukadjs <> " has been defined: ")
                   )
                     -- Then the relation of the relation with its properties and its intended meaning
                  <> printMeaning outputLang' d
            ukadjs = if Uni `elem` properties d && Tot `elem` properties d
                        then commaEng "and" (map adj . Set.elems $ (properties d Set.\\ Set.fromList [Uni,Tot]))<>" function"
                        else commaEng "and" (map adj . Set.elems $ properties d)<>" relation"
            nladjs = if Uni `elem` properties d && Tot `elem` properties d
                      then commaNL "en" (map adj . Set.elems $ properties d Set.\\ Set.fromList [Uni,Tot])<>" functie"
                      else commaNL "en" (map adj . Set.elems $ properties d)<>" relatie"
            adj   = propFullName True outputLang'
    -}

    caRule :: Rule -> (Inlines, [Blocks])
    caRule r =
      let purp = purposes2Blocks env (purposesOf fSpec outputLang' r)
       in ( mempty,
            [ -- First the reason why the rule exists, if any..
              purp
                -- Then the rule as a requirement
                <> plain
                  ( if null purp
                      then
                        str (l (NL "De ongedocumenteerde afspraak ", EN "The undocumented agreement "))
                          <> (hyperLinkTo . XRefSharedLangRule) r
                          <> str (l (NL " bestaat: ", EN " has been made: "))
                      else
                        str (l (NL "Daarom bestaat afspraak ", EN "Therefore agreement "))
                          <> (hyperLinkTo . XRefSharedLangRule) r
                          <> str (l (NL " : ", EN " exists: "))
                  )
                <> ( case meaning outputLang' r of
                       Nothing -> plain . showPredLogic outputLang' . formalExpression $ r
                       Just ms -> printMarkup (ameaMrk ms)
                   )
                <> plain
                  ( str
                      ( l
                          ( NL "Dit is - gebruikmakend van relaties ",
                            EN "Using relations "
                          )
                      )
                      <> mconcat
                        ( L.intersperse
                            (str ", ")
                            [ hyperLinkTo (XRefConceptualAnalysisRelation d)
                                <> text (" (" <> name d <> ")")
                              | d <- Set.elems $ bindedRelationsIn r
                            ]
                        )
                      <> str
                        ( l
                            ( NL " - geformaliseerd als ",
                              EN ", this is formalized as "
                            )
                        )
                  )
                <> pandocEquationWithLabel env fSpec (XRefConceptualAnalysisRule r) (showMath r)
                -- followed by a conceptual model for this rule
                <> para
                  ( hyperLinkTo (pictOfRule r)
                      <> str
                        ( l
                            ( NL " geeft een conceptueel diagram van deze regel.",
                              EN " shows a conceptual diagram of this rule."
                            )
                        )
                  )
                <> xDefBlck env fSpec (pictOfRule r)
            ]
          )
