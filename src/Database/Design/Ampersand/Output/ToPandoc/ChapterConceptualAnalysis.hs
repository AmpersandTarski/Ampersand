{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterConceptualAnalysis
where
import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import Data.List (intersperse )

chpConceptualAnalysis :: Int -> FSpec -> (Blocks,[Picture])
chpConceptualAnalysis lev fSpec = (
      --  *** Header ***
   chptHeader (fsLang fSpec) ConceptualAnalysis
   <> --  *** Intro  ***
   caIntro
   <> --  *** For all themes, a section containing the conceptual analysis for that theme  ***
   caBlocks, pictures)
  where
  -- shorthand for easy localizing
  l :: LocalizedStr -> String
  l lstr = localize (fsLang fSpec) lstr
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

  caBlocks = mconcat (map caSection (vpatterns fSpec))
  pictures = concatMap patPicts (vpatterns fSpec)
  -----------------------------------------------------
  -- the Picture that represents this pattern's conceptual graph
  patPicts :: Pattern -> [Picture]
  patPicts pat = pictOfPat pat :
                (map pictOfRule (invariants fSpec `isc` udefrules pat))
  pictOfPat ::  Pattern ->  Picture
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
     <> ( case (fsLang fSpec) of
               (Dutch  ) -> -- announce the conceptual diagram
                            para ("Figuur " <> xRefReference (getOpts fSpec) (pictOfPat pat) <> " geeft een conceptueel diagram van dit pattern.")
                            -- draw the conceptual diagram
                          <>((plain . showImage (getOpts fSpec) . pictOfPat) pat)
               (English) -> para ("Figure " <> xRefReference (getOpts fSpec) (pictOfPat pat) <> " shows a conceptual diagram of this pattern.")
                          <>((plain . showImage (getOpts fSpec) . pictOfPat) pat)
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
     <> case map caRule (invariants fSpec `isc` udefrules pat) of
         []     -> mempty
         blocks -> (case fsLang fSpec of
                      Dutch   -> header (lev+3) "Regels"
                              <> plain "Deze paragraaf geeft een opsomming van de regels met een verwijzing naar de gemeenschappelijke taal van de belanghebbenden ten behoeve van de traceerbaarheid."
                      English -> header (lev+3) "Rules"
                              <> plain "This section itemizes the rules with a reference to the shared language of stakeholders for the sake of traceability."
                   )
                   <> definitionList blocks
    )
  caRelation :: Declaration -> (Inlines, [Blocks])
  caRelation d
        = let purp =  (purposes2Blocks (getOpts fSpec) [p | p<-purposesDefinedIn fSpec (fsLang fSpec) d])
          in (mempty
             ,[   -- First the reason why the relation exists, if any, with its properties as fundamental parts of its being..
                ( case ( isNull purp, fsLang fSpec) of
                   (True , Dutch)   -> plain ("De volgende " <> str(nladjs d) <> " is gedefinieerd ")
                   (True , English) -> plain ("The following " <> str(ukadjs d) <> " has been defined ")
                   (False, Dutch)   -> purp <> plain ("Voor dat doel is de volgende " <> str(nladjs d) <> " gedefinieerd ")
                   (False, English) -> purp <> plain ("For this purpose, the following " <> str(ukadjs d) <> " has been defined ")
                )
                  -- Then the declaration of the relation with its properties and its intended meaning
               <> pandocEqnArrayWithLabel (XRefConceptualAnalysisDeclaration d)
                     [ [ texOnly_Id(name d)
                       , ":"
                       , texOnly_Id(name (source d))++(if isFunction d then texOnly_fun else texOnly_rel)++texOnly_Id(name(target d))
                       ]
                     ]
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
  ukadjs d  = case [Uni,Tot]>-properties d of
               [] -> commaEng "and" (map ukadj (properties d>-[Uni,Tot]))++" function"
               _  -> commaEng "and" (map ukadj (properties d))++" relation"
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
    ukadj Prop = "symmetric and antisymmetric"
  nladjs d = case [Uni,Tot]>-properties d of
               [] -> commaNL "en" (map nladj (properties d>-[Uni,Tot]))++" functie"
               _  -> commaNL "en" (map nladj (properties d))++" relatie"
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
    nladj Prop  = "symmetrische en antisymmetrische"
  caRule :: Rule -> (Inlines, [Blocks])
  caRule r
        = let purp = (purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) r))
          in ( mempty
             , [  -- First the reason why the rule exists, if any..
                  purp
                  -- Then the rule as a requirement
               <> plain
                   ( if isNull purp
                     then (xRefTo . XRefNaturalLanguageRule) r
                       <> str (l (NL " is gemaakt :" ,EN " has been made:"))
                     else str (l (NL "Daarom bestaat ", EN "Therefore "))
                       <> (xRefTo . XRefNaturalLanguageRule) r
                       <> str (l (NL ":", EN " exists:"))
                   )
               <> fromList (meaning2Blocks  (fsLang fSpec) r)
                  -- then the formal rule
               <> plain
                   (  str (l (NL "Dit is - gebruikmakend van relaties "
                             ,EN "Using relations "  ))
                    <>(mconcat (intersperse  (str ", ")
                                [   xRefTo (XRefConceptualAnalysisDeclaration d)
                                 <> text (" ("++name d++")")
                                | d@Sgn{}<-relsMentionedIn r]))
                    <> str (l (NL " - geformaliseerd als "
                              ,EN ", this is formalized as "))
                   )
               <> (if showPredExpr (getOpts fSpec)
                   then pandocEqnArrayWithLabel (XRefConceptualAnalysisRule r) ((showLatex.toPredLogic) r)
                   else pandocEquationWithLabel (XRefConceptualAnalysisRule r) (showMath r)
                  )
               -- followed by a conceptual model for this rule
               <> para
                     (   str (l (NL "Figuur ", EN "Figure "))
                      <> xRefReference (getOpts fSpec) (pictOfRule r)
                      <> str (l (NL " geeft een conceptueel diagram van deze regel."
                                ,EN " shows a conceptual diagram of this rule."))
                     )
               <>plain (showImage (getOpts fSpec) (pictOfRule r))
               ]
             )
