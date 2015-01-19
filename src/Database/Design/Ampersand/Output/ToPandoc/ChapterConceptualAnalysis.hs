{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterConceptualAnalysis
where
import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Output.PandocAux
import Data.List (intersperse )

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterConceptualAnalysis"

chpConceptualAnalysis :: Int -> FSpec -> (Blocks,[Picture])
chpConceptualAnalysis lev fSpec = (
      --  *** Header ***
   chptHeader (fsLang fSpec) ConceptualAnalysis
   <> --  *** Intro  ***
   caIntro
   <> --  *** For all themes, a section containing the conceptual analysis for that theme  ***
   caBlocks, pictures)
  where
  caIntro :: Blocks
  caIntro
   = (case fsLang fSpec of
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
     )<> purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) fSpec) -- This explains the purpose of this context.

  caBlocks = mconcat (map caSection (patterns fSpec))
  pictures = concatMap patPicts (patterns fSpec)
  -----------------------------------------------------
  -- the Picture that represents this pattern's conceptual graph
  patPicts :: Pattern -> [Picture]
  patPicts pat = pictOfPat pat :
                (map pictOfRule (invariants pat `isc` udefrules pat))
  pictOfPat  :: Pattern ->  Picture
  pictOfPat  = makePicture fSpec . PTRelsUsedInPat
  pictOfRule :: Rule -> Picture
  pictOfRule = makePicture fSpec . PTSingleRule
  caSection :: Pattern -> Blocks
  caSection pat
   =    -- new section to explain this pattern
        headerWithLabel (XRefConceptualAnalysisPattern pat) (lev+2) ((text.name) pat)
        -- The section starts with the reason why this pattern exists
     <> (purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) pat))
        -- followed by a conceptual model for this pattern
     <> ( case (genGraphics (getOpts fSpec), fsLang fSpec) of
               (True,Dutch  ) -> -- announce the conceptual diagram
                                 para ("Figuur " <> xRefReference (getOpts fSpec) (pictOfPat pat) <> " geeft een conceptueel diagram van dit pattern.")
                                 -- draw the conceptual diagram
                               <>((plain . showImage (getOpts fSpec) . pictOfPat) pat)
               (True,English) -> para ("Figure " <> xRefReference (getOpts fSpec) (pictOfPat pat) <> " shows a conceptual diagram of this pattern.")
                               <>((plain . showImage (getOpts fSpec) . pictOfPat) pat)
               _              -> mempty
        ) <>
    (
        -- now provide the text of this pattern.
        (case fsLang fSpec of
           Dutch   -> para "De definities van concepten zijn te vinden in de index."
                   <> header (lev+3) "Gedeclareerde relaties"
                   <> para "Deze paragraaf geeft een opsomming van de gedeclareerde relaties met eigenschappen en betekenis."
           English -> para "The definitions of concepts can be found in the glossary."
                   <> header (lev+3) "Declared relations"
                   <> para "This section itemizes the declared relations with properties and purpose."
        )
     <> definitionList (map caRelation [d | d@Sgn{}<-relsDefdIn pat `uni` relsMentionedIn pat])
     <> (case fsLang fSpec of
           Dutch   -> header (lev+3) "Formele regels"
                   <> plain "Deze paragraaf geeft een opsomming van de formele regels met een verwijzing naar de gemeenschappelijke taal van de belanghebbenden ten behoeve van de traceerbaarheid."
           English -> header (lev+3) "Formal rules"
                   <> plain "This section itemizes the formal rules with a reference to the shared language of stakeholders for the sake of traceability."
        )
     <> fromList [DefinitionList blocks | let blocks = map caRule (invariants pat `isc` udefrules pat), not(null blocks)]
    )
  caRelation :: Declaration -> (Inlines, [Blocks])
  caRelation d
        = let purp = toList (purposes2Blocks (getOpts fSpec) [p | p<-purposesDefinedIn fSpec (fsLang fSpec) d])
          in (mempty
             ,[   -- First the reason why the relation exists, if any, with its properties as fundamental parts of its being..
                ( case ( null purp, fsLang fSpec) of
                   (True , Dutch)   -> plain ("De volgende " <> str(nladjs d) <> " is gedefinieerd ")
                   (True , English) -> plain ("The following " <> str(ukadjs d) <> " has been defined ")
                   (False, Dutch)   -> plain ("Voor dat doel is de volgende " <> str(nladjs d) <> " is gedefinieerd ")
                   (False, English) -> plain ("For this purpose, the following " <> str(ukadjs d) <> " has been defined ")
                )
                  -- Then the declaration of the relation with its properties and its intended meaning
               <> fromList ( pandocEqnArrayWithLabel (XRefConceptualAnalysisDeclaration d)
                     [ ( texOnly_Id(name d)
                       , ":"
                       , texOnly_Id(name (source d))++(if isFunction d then texOnly_fun else texOnly_rel)++texOnly_Id(name(target d)) 
                       )  ])
               <> case meaning2Blocks (fsLang fSpec) d of
                    [] -> case fsLang fSpec of
                           Dutch   -> case commaNL  "en"  [ show (amLang markup) | markup<-ameaMrk (decMean d), amLang markup/=fsLang fSpec] of
                                       []    -> plain "(Geen betekenis gespecificeerd)"
                                       langs -> plain (str ("(Geen betekenis gespecificeerd, maar wel in het "++langs++")"))
                           English -> case commaEng "and" [ show (amLang markup) | markup<-ameaMrk (decMean d), amLang markup/=fsLang fSpec] of
                                       []    -> plain "(No meaning has been specified)"
                                       langs -> plain (str ("(No meaning has been specified, except in "++langs++")"))
                    ms -> fromList ms
              ])
  ukadjs d  = case [Uni,Tot]>-multiplicities d of
               [] -> commaEng "and" (map ukadj (multiplicities d>-[Uni,Tot]))++" function"
               _  -> commaEng "and" (map ukadj (multiplicities d))++" relation"
   where
    ukadj Uni = "univalent"
    ukadj Inj = "injective"
    ukadj Sur = "surjective"
    ukadj Tot = "total"
    ukadj Sym = "symmetric"
    ukadj Asy = "antisymmetric"
    ukadj Trn = "transitive"
    ukadj Rfx = "reflexive"
    ukadj Irf = "irreflexive"
    ukadj Aut = "automatically computed"    
  nladjs d = case [Uni,Tot]>-multiplicities d of
               [] -> commaNL "en" (map nladj (multiplicities d>-[Uni,Tot]))++" functie"
               _  -> commaNL "en" (map nladj (multiplicities d))++" relatie"
   where
    nladj Uni = "univalente"
    nladj Inj = "injectieve"
    nladj Sur = "surjectieve"
    nladj Tot = "totale"
    nladj Sym = "symmetrische"
    nladj Asy = "antisymmetrische"
    nladj Trn = "transitieve"
    nladj Rfx = "reflexieve"
    nladj Irf = "irreflexieve"
    nladj Aut = "automatisch berekende"  
  caRule :: Rule -> ([Inline], [[Block]])
  caRule r
        = let purp = toList (purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) r))

          in ( []
             , [  -- First the reason why the rule exists, if any..
                  purp
                  -- Then the rule as a requirement
               ++ [Plain$[if null purp then Str "De volgende afspraak is gesteld in paragraaf "
                                       else Str "Daarom is als afspraak gesteld in paragraaf " | fsLang fSpec==Dutch]
                      ++ [if null purp then Str "The following requirement has been defined in section "
                                       else Str "Therefore the following requirement has been defined in section " | fsLang fSpec==English]
                      ++ (toList . xRefTo . XRefNaturalLanguageRule) r
--                      ++ [RawInline (Format "latex") "~"]
--                      ++ toList ( symReqRef (getOpts fSpec) r)
--                      ++ [Str " p."
--                         ,RawInline (Format "latex") "~"]
--                      ++ toList (symReqPageRef (getOpts fSpec) r)
                      ++ [Str ": "]]
               ++ meaning2Blocks (fsLang fSpec) r
                  -- then the formal rule
               ++ [Plain$[Str "Dit is geformaliseerd - gebruikmakend van relaties " | fsLang fSpec==Dutch]
                      ++ [Str "This is formalized - using relations "     | fsLang fSpec==English]
                      ++ toList (mconcat (intersperse  (str ", ") [ xRefTo (XREFXXX d) | d@Sgn{}<-relsMentionedIn r]))
                      ++ [Str " - als " | fsLang fSpec==Dutch]
                      ++ [Str " - as "     | fsLang fSpec==English]]
               ++ (if showPredExpr (getOpts fSpec)
                   then pandocEqnArrayWithLabel (XRefConceptualAnalysisRule r) ((showLatex.toPredLogic) r)
                   else pandocEquationWithLabel (XRefConceptualAnalysisRule r) (showMath r)
                  )
               -- followed by a conceptual model for this rule
               ++ (toList
               ( case (genGraphics (getOpts fSpec), fsLang fSpec) of
                  (True,Dutch  ) ->
                        para ("Figuur " <> xRefReference (getOpts fSpec) (pictOfRule r) <> " geeft een conceptueel diagram van deze regel.")
                     <> plain (showImage (getOpts fSpec) (pictOfRule r))
                  (True,English) ->
                        para ("Figure " <> xRefReference (getOpts fSpec) (pictOfRule r) <> " shows a conceptual diagram of this rule.")
                     <> plain (showImage (getOpts fSpec) (pictOfRule r))
                  _              -> mempty))

               ])
