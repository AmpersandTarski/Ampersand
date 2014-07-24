{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterConceptualAnalysis
where
import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.ADL1 (Prop(..)) 
import Database.Design.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Output.PandocAux
import Data.List (intersperse )

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterConceptualAnalysis"

chpConceptualAnalysis :: Int -> Fspc -> Options -> (Blocks,[Picture])
chpConceptualAnalysis lev fSpec flags = (
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
     )<> purposes2Blocks flags (purposesDefinedIn fSpec (fsLang fSpec) fSpec) -- This explains the purpose of this context.
     
  caBlocks = mconcat (map caSection (patterns fSpec))
  pictures = concatMap patPicts (patterns fSpec)
  -----------------------------------------------------
  -- the Picture that represents this pattern's conceptual graph
  patPicts :: Pattern -> [Picture] 
  patPicts pat = pictOfPat pat :
                (map pictOfRule (invariants pat `isc` udefrules pat))
  pictOfPat  :: Pattern ->  Picture
  pictOfPat  = makePicture flags fSpec . PTRelsUsedInPat 
  pictOfRule :: Rule -> Picture
  pictOfRule = makePicture flags fSpec . PTSingleRule
  caSection :: Pattern -> Blocks
  caSection pat
   =    -- new section to explain this pattern  
        ( labeledThing flags (lev+1) (xLabel ConceptualAnalysis++"_"++name pat) (name pat))
        -- The section starts with the reason why this pattern exists 
     <> (purposes2Blocks flags (purposesDefinedIn fSpec (fsLang fSpec) pat))
        -- followed by a conceptual model for this pattern
     <> ( case (genGraphics flags, fsLang fSpec) of
               (True,Dutch  ) -> -- announce the conceptual diagram
                                 para ("Figuur " <> xRefReference flags (pictOfPat pat) <> " geeft een conceptueel diagram van dit pattern.")
                                 -- draw the conceptual diagram
                               <>((plain . showImage flags . pictOfPat) pat)          
               (True,English) -> para ("Figure " <> xRefReference flags (pictOfPat pat) <> " shows a conceptual diagram of this pattern.")
                               <>((plain . showImage flags . pictOfPat) pat)
               _              -> mempty
        ) <> 
    (    
        -- now provide the text of this pattern.
        (case fsLang fSpec of
           Dutch   -> para "De definities van concepten zijn te vinden in de index."
                   <> (labeledThing flags (lev+2) (xLabel ConceptualAnalysis++"_relationsOf_"++name pat) "Gedeclareerde relaties")
                   <> para "Deze paragraaf geeft een opsomming van de gedeclareerde relaties met eigenschappen en betekenis."
           English -> para "The definitions of concepts can be found in the glossary."
                   <> (labeledThing flags (lev+2) (xLabel ConceptualAnalysis++"_relationsOf_"++name pat) "Declared relations")
                   <> para "This section itemizes the declared relations with properties and purpose."
        )
     <> definitionList (map caRelation [d | d@Sgn{}<-relsDefdIn pat `uni` relsMentionedIn pat])
     <> (case fsLang fSpec of
           Dutch   -> (labeledThing flags (lev+2) (xLabel ConceptualAnalysis++"_rulesOf_"++name pat) "Formele regels")
                   <> plain "Deze paragraaf geeft een opsomming van de formele regels met een verwijzing naar de gemeenschappelijke taal van de belanghebbenden ten behoeve van de traceerbaarheid."
           English -> (labeledThing flags (lev+2) (xLabel ConceptualAnalysis++"_rulesOf_"++name pat) "Formal rules")
                   <> plain "This section itemizes the formal rules with a reference to the shared language of stakeholders for the sake of traceability."
        )
     <> fromList [DefinitionList blocks | let blocks = map caRule (invariants pat `isc` udefrules pat), not(null blocks)]
    )
  caRelation :: Declaration -> (Inlines, [Blocks])
  caRelation d 
        = let purp = toList (purposes2Blocks flags [p | p<-purposesDefinedIn fSpec (fsLang fSpec) d])
          in (mempty
             ,[   -- First the reason why the relation exists, if any, with its properties as fundamental parts of its being..
                ( case ( null purp, fsLang fSpec) of
                   (True , Dutch)   -> plain ("De volgende " <> str(nladjs d) <> " is gedefinieerd ")
                   (True , English) -> plain ("The following " <> str(ukadjs d) <> " has been defined ")
                   (False, Dutch)   -> plain ("Voor dat doel is de volgende " <> str(nladjs d) <> " is gedefinieerd ")
                   (False, English) -> plain ("For this purpose, the following " <> str(ukadjs d) <> " has been defined ")
                )
                  -- Then the declaration of the relation with its properties and its intended meaning 
               <> fromList ( pandocEqnArray 
                     [ ( texOnly_Id(name d)
                       , ":"
                       , texOnly_Id(name (source d))++(if isFunction d then texOnly_fun else texOnly_rel)++texOnly_Id(name(target d)) ++symDefLabel d -- TODO: HJO,20140724 Dit is niet de plaats van een label, want de relatie kan in meerdere patterns voorkomen. Maar wat is dan wél de juiste plaats? 
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
        = let purp = toList (purposes2Blocks flags (purposesDefinedIn fSpec (fsLang fSpec) r))
              
          in ( []
             , [  -- First the reason why the rule exists, if any..
                  purp  
                  -- Then the rule as a requirement
               ++ [Plain$[if null purp then Str "De volgende afspraak is gesteld in paragraaf " 
                                       else Str "Daarom is als afspraak gesteld in paragraaf " | fsLang fSpec==Dutch]
                      ++ [if null purp then Str "The following requirement has been defined in section " 
                                       else Str "Therefore the following requirement has been defined in section " | fsLang fSpec==English]
                      ++ [RawInline (Format "latex") "~"]
                      ++ toList ( symReqRef flags r)
                      ++ [Str " p."
                         ,RawInline (Format "latex") "~"]
                      ++ toList (symReqPageRef flags r)
                      ++ [Str ": "]]
               ++ meaning2Blocks (fsLang fSpec) r
                  -- then the formal rule
               ++ [Plain$[Str "Dit is geformaliseerd - gebruikmakend van relaties " | fsLang fSpec==Dutch]
                      ++ [Str "This is formalized - using relations "     | fsLang fSpec==English]
                      ++ toList (mconcat (intersperse  (str ", ") [ symDefRef flags d | d@Sgn{}<-relsMentionedIn r]))
                      ++ [Str " - als " | fsLang fSpec==Dutch]
                      ++ [Str " - as "     | fsLang fSpec==English]]
               ++ (if showPredExpr flags
                   then pandocEqnArrayOnelabel (symDefLabel r) ((showLatex.toPredLogic) r)
                   else pandocEquation (showMath r++symDefLabel r)
                  )
               -- followed by a conceptual model for this rule
               ++ toList
               ( case (genGraphics flags, fsLang fSpec) of
                  (True,Dutch  ) -> 
                        para ("Figuur " <> xRefReference flags (pictOfRule r) <> " geeft een conceptueel diagram van deze regel.")
                     <> plain (showImage flags (pictOfRule r))          
                  (True,English) -> 
                        para ("Figure " <> xRefReference flags (pictOfRule r) <> " shows a conceptual diagram of this rule.")
                     <> plain (showImage flags (pictOfRule r))
                  _              -> mempty)
               
               ])
