{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.ToPandoc.ChapterConceptualAnalysis
where
import Ampersand.Output.ToPandoc.SharedAmongChapters
import Data.List (intersperse )

chpConceptualAnalysis :: Int -> FSpec -> (Blocks,[Picture])
chpConceptualAnalysis lev fSpec = (
      --  *** Header ***
   xDefBlck fSpec ConceptualAnalysis
   <> --  *** Intro  ***
   caIntro
   <> --  *** For all themes, a section containing the conceptual analysis for that theme  ***
   caBlocks, pictures)
  where
  -- shorthand for easy localizing
  l :: LocalizedStr -> String
  l = localize (fsLang fSpec)
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

  caBlocks = 
         mconcat (map caSection (vpatterns fSpec))
      <>(case fsLang fSpec of
           Dutch   -> para "De definities van concepten zijn te vinden in de index."
                   <> header (lev+3) "Gedeclareerde relaties"
                   <> para "Deze paragraaf geeft een opsomming van de gedeclareerde relaties met eigenschappen en betekenis."
           English -> para "The definitions of concepts can be found in the glossary."
                   <> header (lev+3) "Declared relations"
                   <> para "This section itemizes the declared relations with properties and purpose."
        )
      <> definitionList (map caRelation [d | d@Sgn{}<-vrels fSpec])
     
  pictures = map pictOfPat (vpatterns fSpec)
          ++ map pictOfConcept (concs fSpec)
          ++ map pictOfRule (vrules fSpec)
  -----------------------------------------------------
  -- the Picture that represents this pattern's conceptual graph
  pictOfPat ::  Pattern ->  Picture
  pictOfPat  = makePicture fSpec . PTCDPattern
  pictOfRule :: Rule -> Picture
  pictOfRule = makePicture fSpec . PTCDRule
  pictOfConcept :: A_Concept -> Picture
  pictOfConcept = makePicture fSpec . PTCDConcept
  caSection :: Pattern -> Blocks
  caSection pat
   =    -- new section to explain this pattern
        xDefBlck fSpec (XRefConceptualAnalysisPattern pat)
        -- The section starts with the reason why this pattern exists
     <> purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) pat)
        -- followed by a conceptual model for this pattern
     <> ( case fsLang fSpec of
               Dutch   -> -- announce the conceptual diagram
                          para (xRef (pictOfPat pat) <> " geeft een conceptueel diagram van dit pattern.")
                          -- draw the conceptual diagram
                          <>(xDefBlck fSpec . pictOfPat) pat
               English -> para (xRef (pictOfPat pat) <> " shows a conceptual diagram of this pattern.")
                          <>(xDefBlck fSpec . pictOfPat) pat
        ) <>
    (
        -- now provide the text of this pattern.
       case map caRule (invariants fSpec `isc` udefrules pat) of
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
        = let purp =  purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) d)
          in (mempty
             ,[   -- First the reason why the relation exists, if any, with its properties as fundamental parts of its being..
                ( case ( isNull purp, fsLang fSpec) of
                   (True , Dutch)   -> plain ("De volgende " <> str(nladjs d) <> " is gedefinieerd ")
                   (True , English) -> plain ("The following " <> str(ukadjs d) <> " has been defined ")
                   (False, Dutch)   -> purp <> plain ("Voor dat doel is de volgende " <> str(nladjs d) <> " gedefinieerd ")
                   (False, English) -> purp <> plain ("For this purpose, the following " <> str(ukadjs d) <> " has been defined ")
                )
                  -- Then the declaration of the relation with its properties and its intended meaning
               <> pandocEquationWithLabel fSpec (XRefConceptualAnalysisDeclaration d) (showMathWithSign d)
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
    nladj Prop  = "symmetrische en antisymmetrische"
  caRule :: Rule -> (Inlines, [Blocks])
  caRule r
        = let purp = purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) r)
          in ( mempty
             , [  -- First the reason why the rule exists, if any..
                  purp
                  -- Then the rule as a requirement
               <> plain
                   ( if isNull purp
                     then (xRef . XRefSharedLangRule) r
                       <> str (l (NL " is gemaakt :" ,EN " has been made:"))
                     else str (l (NL "Daarom bestaat ", EN "Therefore "))
                       <> (xRef . XRefSharedLangRule) r
                       <> str (l (NL ":", EN " exists:"))
                   )
               <> fromList (meaning2Blocks  (fsLang fSpec) r)
                  -- then the formal rule
               <> plain
                   (  str (l (NL "Dit is - gebruikmakend van relaties "
                             ,EN "Using relations "  ))
                    <> mconcat (intersperse  (str ", ")
                                [   xRef (XRefConceptualAnalysisDeclaration d)
                                 <> text (" ("++name d++")")
                                | d@Sgn{}<-relsMentionedIn r])
                    <> str (l (NL " - geformaliseerd als "
                              ,EN ", this is formalized as "))
                   )
               <> pandocEquationWithLabel fSpec (XRefConceptualAnalysisRule r) (showMath r) 
               -- followed by a conceptual model for this rule
               <> para (   xRef (pictOfRule r)
                        <> str (l (NL " geeft een conceptueel diagram van deze regel."
                                  ,EN " shows a conceptual diagram of this rule."))
                       )
               <> xDefBlck fSpec (pictOfRule r)
               ]
             )
