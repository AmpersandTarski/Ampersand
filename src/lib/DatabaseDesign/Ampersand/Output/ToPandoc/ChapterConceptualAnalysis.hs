{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterConceptualAnalysis
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters
import DatabaseDesign.Ampersand.ADL1 (Prop(..)) 
import DatabaseDesign.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Output.PandocAux
import Data.List (intercalate)

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterConceptualAnalysis"

chpConceptualAnalysis :: Int -> Fspc -> Options -> (Blocks,[Picture])
chpConceptualAnalysis lev fSpec flags = (chptHeader (fsLang fSpec) ConceptualAnalysis <> caIntro <> caBlocks, pictures)
  where
  caIntro :: Blocks
  caIntro
   = fromList $ (case fsLang fSpec of
        Dutch   -> [Para
                    [ Str "Dit hoofdstuk beschrijft een formele taal, waarin functionele eisen ten behoeve van "
                    , Quoted  SingleQuote [Str (name fSpec)]
                    , Str " kunnen worden besproken en uitgedrukt. "
                    , Str "De formalisering dient om een bouwbare specificatie te verkrijgen. "
                    , Str "Een derde met voldoende deskundigheid kan op basis van dit hoofdstuk toetsen of de gemaakte afspraken "
                    , Str "overeenkomen met de formele regels en definities. "
                   ]]
        English -> [Para
--                    [ Str "This chapter provides an analysis of the principles described in chapter "
--                    
--                    , Str ". Each section in that chapter is analysed in terms of relations "
--                    , Str "and each principle is then translated in a rule. "
                    [ Str "This chapter defines the formal language, in which functional requirements of "
                    , Quoted  SingleQuote [Str (name fSpec)]
                    , Str " can be analysed and expressed."
                    , Str "The purpose of this formalisation is to obtain a buildable specification. "
                    , Str "This chapter allows an independent professional with sufficient background to check whether the agreements made "
                    , Str "correspond to the formal rules and definitions. "
                    ]])
     ++ purposes2Blocks flags (purposesDefinedIn fSpec (fsLang fSpec) fSpec) -- This explains the purpose of this context.
     
  caBlocks = fromList $ concat(map caSection (patterns fSpec))
  pictures =        map pict      (patterns fSpec)
  -----------------------------------------------------
  -- the Picture that represents this pattern's conceptual graph
  pict :: Pattern -> Picture
  pict pat = (makePicture flags fSpec (name pat++"ConceptDiagram") Plain_CG pat)
                {caption = case fsLang fSpec of
                            Dutch   ->"Conceptdiagram van "++name pat
                            English ->"Concept diagram of "++name pat}
  caSection :: Pattern -> [Block]
  caSection pat
   =    -- new section to explain this pattern  
        toList ( labeledThing flags (lev+1) (xLabel ConceptualAnalysis++"_"++name pat) (name pat))
        -- The section starts with the reason why this pattern exists 
     ++ purposes2Blocks flags (purposesDefinedIn fSpec (fsLang fSpec) pat)
        -- followed by a conceptual model for this pattern
     ++ ( case (genGraphics flags, fsLang fSpec) of
               (True,Dutch  ) -> -- announce the conceptual diagram
                                 [Para [Str "Figuur ", xrefReference (pict pat), Str " geeft een conceptueel diagram van dit pattern."]
                                 -- draw the conceptual diagram
                                 ,Plain (xrefFigure1 (pict pat))]          
               (True,English) -> [Para [Str "Figure ", xrefReference (pict pat), Str " shows a conceptual diagram of this pattern."]
                                 ,Plain (xrefFigure1 (pict pat))]
               _              -> [])
        -- now provide the text of this pattern.
     ++ (case fsLang fSpec of
           Dutch   -> [Para [Str "De definities van concepten zijn te vinden in de index."]
                      ]++
                      toList (labeledThing flags (lev+2) (xLabel ConceptualAnalysis++"_relationsOf_"++name pat) "Gedeclareerde relaties")
                      ++
                      [Para [Str "Deze paragraaf geeft een opsomming van de gedeclareerde relaties met eigenschappen en een betekenis."]]
           English -> [Para [Str "The definitions of concepts can be found in the glossary."]
                      ]++
                      toList (labeledThing flags (lev+2) (xLabel ConceptualAnalysis++"_relationsOf_"++name pat) "Declared relations")
                      ++
                      [Para [Str "This section itemizes the declared relations with properties and a meaning."]])
     ++ [DefinitionList blocks | let blocks = map caRelation [d | d@Sgn{}<-relsDefdIn pat `uni` relsMentionedIn pat], not(null blocks)]
     ++ (case fsLang fSpec of
           Dutch   -> toList (labeledThing flags (lev+2) (xLabel ConceptualAnalysis++"_rulesOf_"++name pat) "Formele regels")
                      ++
                      [Plain [Str "Deze paragraaf geeft een opsomming van de formele regels met een verwijzing naar de gemeenschappelijke taal van de belanghebbenden ten behoeve van de traceerbaarheid."]]
           English -> toList (labeledThing flags (lev+2) (xLabel ConceptualAnalysis++"_rulesOf_"++name pat) "Formal rules")
                      ++
                      [Plain [Str "This section itemizes the formal rules with a reference to the shared language of stakeholders for the sake of traceability."]])
     ++ [DefinitionList blocks | let blocks = map caRule (invariants pat `isc` udefrules pat), not(null blocks)]

  caRelation :: Declaration -> ([Inline], [[Block]])
  caRelation d 
        = let purp = purposes2Blocks flags [p | p<-purposesDefinedIn fSpec (fsLang fSpec) d]
          in ([]
             ,[   -- First the reason why the relation exists, if any, with its properties as fundamental parts of its being..
                ( if null purp
                  then [ Plain$[ Str ("De volgende "++nladjs d++" is gedefinieerd ")      | fsLang fSpec==Dutch]
                            ++ [ Str ("The following "++ukadjs d++" has been defined ") | fsLang fSpec==English] ]
                  else purp++
                       [ Plain$[ Str ("Voor dat doel is de volgende "++nladjs d++" gedefinieerd ")      | fsLang fSpec==Dutch]
                            ++ [ Str ("For this purpose, the following "++ukadjs d++" has been defined ") | fsLang fSpec==English] ] )
                  -- Then the declaration of the relation with its properties and its intended meaning 
               ++ pandocEqnArray 
                     [ ( texOnly_Id(name d)
                       , ":"
                       , texOnly_Id(name (source d))++(if isFunction d then texOnly_fun else texOnly_rel)++texOnly_Id(name(target d))++symDefLabel d
                       )  ]
               ++ [Plain$[Str $ let langs=commaNL "en" [ show (amLang markup) | markup<-ameaMrk (decMean d), amLang markup/=Dutch] in
                                if null langs then "(Geen betekenis gespecificeerd)" else "(Geen betekenis gespecificeerd, maar wel in het "++langs++")"| fsLang fSpec==Dutch]++
                         [Str $ let langs=commaEng "and" [ show (amLang markup) | markup<-ameaMrk (decMean d), amLang markup/=Dutch] in
                                if null langs then "(No meaning has been specified)" else "(No meaning has been specified, except in "++langs++")"| fsLang fSpec==English]
                  | null (meaning2Blocks (fsLang fSpec) d)]
               ++ meaning2Blocks (fsLang fSpec) d
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
  caRule :: Rule -> ([Inline], [[Block]])
  caRule r 
        = let purp = purposes2Blocks flags (purposesDefinedIn fSpec (fsLang fSpec) r)
          in ( []
             , [  -- First the reason why the rule exists, if any..
                  purp  
                  -- Then the rule as a requirement
               ++ [Plain$[if null purp then Str "De volgende afspraak is gesteld in paragraaf " 
                                       else Str "Daarom is als afspraak gesteld in paragraaf " | fsLang fSpec==Dutch]
                      ++ [if null purp then Str "The following requirement has been defined in section " 
                                       else Str "Therefore the following requirement has been defined in section " | fsLang fSpec==English]
                      ++ [RawInline (DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters.Format "latex") "~"
                         ,RawInline (DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters.Format "latex") $ symReqRef r
                         ,Str " p."
                         ,RawInline (DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters.Format "latex") "~"
                         ,RawInline (DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters.Format "latex") $ symReqPageRef r
                         ,Str ": "]]
               ++ meaning2Blocks (fsLang fSpec) r
                  -- then the formal rule
               ++ [Plain$[Str "Dit is geformaliseerd - gebruikmakend van relaties " | fsLang fSpec==Dutch]
                      ++ [Str "This is formalized - using relations "     | fsLang fSpec==English]
                      ++ intercalate [Str ", "] [[RawInline (DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters.Format "latex") $ symDefRef d] | d@Sgn{}<-relsMentionedIn r]
                      ++ [Str " - als " | fsLang fSpec==Dutch]
                      ++ [Str " - as "     | fsLang fSpec==English]]
               ++ (if showPredExpr flags
                   then pandocEqnArrayOnelabel (symDefLabel r) ((showLatex.toPredLogic) r)
                   else pandocEquation (showMath r++symDefLabel r)
                  )
               ])



