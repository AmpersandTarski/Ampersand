{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.ToPandoc.ChapterConceptualAnalysis
where
import           Ampersand.Graphic.ClassDiagram
import           Ampersand.Graphic.Fspec2ClassDiagrams
import           Ampersand.Output.ToPandoc.SharedAmongChapters
import qualified RIO.List as L
import qualified RIO.Set as Set
import qualified RIO.Text as T


chpConceptualAnalysis :: (HasDirOutput env, HasDocumentOpts env) 
   => env -> Int -> FSpec -> (Blocks,[Picture])
chpConceptualAnalysis env lev fSpec
 = (    --  *** Header ***
     xDefBlck env fSpec ConceptualAnalysis
     <> --  *** Intro  ***
     caIntro
     <> --  *** For all patterns, a section containing the conceptual analysis for that pattern  ***
     caBlocks
   , pictures)
  where
  -- shorthand for easy localizing
  l :: LocalizedStr -> Text
  l = localize outputLang'
  outputLang' = outputLang env fSpec
  caIntro :: Blocks
  caIntro
   = (case outputLang' of
        Dutch   -> para
                    (  "Dit hoofdstuk beschrijft een formele taal, waarin functionele eisen ten behoeve van "
                    <> (singleQuoted.str.name) fSpec
                    <> " kunnen worden besproken en uitgedrukt. "
                    <> "De formalisering dient om een bouwbare specificatie te verkrijgen. "
                    <> "Een derde met voldoende deskundigheid kan op basis van dit hoofdstuk toetsen of de gemaakte afspraken "
                    <> "overeenkomen met de formele regels en definities. "
                    )
        English -> para
                    (  "This chapter defines the formal language, in which functional requirements of "
                    <> (singleQuoted.str.name) fSpec
                    <> " can be analysed and expressed."
                    <> "The purpose of this formalisation is to obtain a buildable specification. "
                    <> "This chapter allows an independent professional with sufficient background to check whether the agreements made "
                    <> "correspond to the formal rules and definitions. "
                    )
     )<> purposes2Blocks env (purposesOf fSpec outputLang' fSpec) -- This explains the purpose of this context.

  caBlocks = 
         mconcat (map caSection (vpatterns fSpec))
      <>(case outputLang' of
           Dutch   -> para "De definities van concepten zijn te vinden in de index."
                   <> header (lev+3) "Gedeclareerde relaties"
                   <> para "Deze paragraaf geeft een opsomming van de gedeclareerde relaties met eigenschappen en betekenis."
           English -> para "The definitions of concepts can be found in the glossary."
                   <> header (lev+3) "Declared relations"
                   <> para "This section itemizes the declared relations with properties and purpose."
        )
      <> definitionList (map caRelation (Set.elems $ vrels fSpec))
     
  pictures = map pictOfPat (vpatterns fSpec)
          <> map pictOfConcept (Set.elems $ concs fSpec)
          <> map pictOfRule (Set.elems $ vrules fSpec)
  -----------------------------------------------------
  -- the Picture that represents this pattern's conceptual graph
  pictOfPat ::  Pattern ->  Picture
  pictOfPat  = makePicture env fSpec . PTCDPattern
  pictOfRule :: Rule -> Picture
  pictOfRule = makePicture env fSpec . PTCDRule
  pictOfConcept :: A_Concept -> Picture
  pictOfConcept = makePicture env fSpec . PTCDConcept
  caSection :: Pattern -> Blocks
  caSection pat
   =    -- new section to explain this pattern
        xDefBlck env fSpec (XRefConceptualAnalysisPattern pat)
        -- The section starts with the reason why this pattern exists
     <> purposes2Blocks env (purposesOf fSpec outputLang' pat)
        -- followed by a conceptual model for this pattern
     <> ( case outputLang' of
               Dutch   -> -- announce the conceptual diagram
                          para (hyperLinkTo (pictOfPat pat) <> "Conceptueel diagram van " <> (singleQuoted . str . name) pat<> ".")
                          -- draw the conceptual diagram
                          <>(xDefBlck env fSpec . pictOfPat) pat
               English -> para (hyperLinkTo (pictOfPat pat) <> "Conceptual diagram of " <> (singleQuoted . str . name) pat<> ".")
                          <>(xDefBlck env fSpec . pictOfPat) pat
        )
     <> mconcat (map fst caSubsections)
     <> caRemainingRelations
     <>
    (
        -- now provide the text of this pattern.
       case map caRule . Set.elems $ invariants fSpec `Set.intersection` udefrules pat of
         []     -> mempty
         blocks -> (case outputLang' of
                      Dutch   -> header (lev+3) "Regels"
                              <> plain "Deze paragraaf geeft een opsomming van de regels met een verwijzing naar de gemeenschappelijke taal van de belanghebbenden ten behoeve van de traceerbaarheid."
                      English -> header (lev+3) "Rules"
                              <> plain "This section itemizes the rules with a reference to the shared language of stakeholders for the sake of traceability."
                   )
                   <> definitionList blocks
    )
    where
      oocd :: ClassDiag
      oocd = cdAnalysis fSpec pat
      
      caEntity :: Class -> (Blocks, [Relation])
      caEntity cl
        = ( simpleTable [ (plain.text.l) (NL "Attribuut", EN "Attribute")
                        ,(plain.text.l) (NL "Betekenis", EN "Meaning")
                        ]
                        ( [[ plain (text (name attr) <> " (" <> text (attTyp attr) <> ")")
                           , (plain . text . T.intercalate " " . map tshow . decMean) rel  -- use "tshow.attType" for the technical type.
                           ]
                          | attr<-clAtts cl, rel<-lookupRel attr
                          ]
                        )
          , [ rel | attr<-clAtts cl, rel<-lookupRel attr ]
          )
          where
             lookupRel :: CdAttribute -> [Relation]
             lookupRel attr
              = [ rel
                | rel <- Set.toList (ptdcs pat)
                , name rel==name attr, name cl==name (source rel), attTyp attr==name (target rel) ] <>
                [ rel
                | rel <- Set.toList (ptdcs pat)
                , name rel==name attr, name cl==name (target rel), attTyp attr==name (source rel) ]

      caSubsections :: [(Blocks, [Relation])]
      caSubsections =
        [ ( header 2 (str (name cl)) <> entityBlocks
          , entityRels)
        | cl <- classes oocd, (entityBlocks, entityRels) <- [caEntity cl], length entityRels>1
        ]
      
      caRemainingRelations :: Blocks
      caRemainingRelations
        = simpleTable [ (plain.text.l) (NL "Relatie", EN "Relation")
                      , (plain.text.l) (NL "Betekenis", EN "Meaning")
                      ]
                      ( [[ (plain.text.showA) rel
                         , (plain . text . T.intercalate " " . map tshow . decMean) rel  -- use "tshow.attType" for the technical type.
                         ]
                        | rel<-rels
                        ]
                      )
       where
          rels :: [Relation]
          rels = Set.toList (ptdcs pat `Set.difference` entityRels)
          entityRels :: Set Relation
          entityRels = Set.unions (map (Set.fromList . snd) caSubsections)

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

  caRule :: Rule -> (Inlines, [Blocks])
  caRule r
        = let purp = purposes2Blocks env (purposesOf fSpec outputLang' r)
          in ( mempty
             , [  -- First the reason why the rule exists, if any..
                  purp
                  -- Then the rule as a requirement
               <> plain
                   ( if null purp
                     then str (l (NL "De ongedocumenteerde afspraak ", EN "The undocumented agreement "))
                       <> (hyperLinkTo . XRefSharedLangRule) r
                       <> str (l (NL " bestaat: " ,EN " has been made: "))
                     else str (l (NL "Daarom bestaat afspraak ", EN "Therefore agreement "))
                       <> (hyperLinkTo . XRefSharedLangRule) r
                       <> str (l (NL " : ", EN " exists: "))
                   )
               <> ( case meaning outputLang' r of
                     Nothing -> plain . showPredLogic outputLang' . formalExpression $ r
                     Just ms -> printMarkup (ameaMrk ms)
                  )
               <> plain
                   (  str (l (NL "Dit is - gebruikmakend van relaties "
                             ,EN "Using relations "  ))
                    <> mconcat (L.intersperse  (str ", ")
                                [   hyperLinkTo (XRefConceptualAnalysisRelation d)
                                 <> text (" ("<>name d<>")")
                                | d<-Set.elems $ bindedRelationsIn r])
                    <> str (l (NL " - geformaliseerd als "
                              ,EN ", this is formalized as "))
                   )
               <> pandocEquationWithLabel env fSpec (XRefConceptualAnalysisRule r) (showMath r) 
               -- followed by a conceptual model for this rule
               <> para (   hyperLinkTo (pictOfRule r)
                        <> str (l (NL " geeft een conceptueel diagram van deze regel."
                                  ,EN " shows a conceptual diagram of this rule."))
                       )
               <> xDefBlck env fSpec (pictOfRule r)
               ]
             )