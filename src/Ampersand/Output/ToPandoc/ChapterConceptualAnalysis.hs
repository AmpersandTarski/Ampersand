{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.ToPandoc.ChapterConceptualAnalysis
where
import           Ampersand.Output.ToPandoc.SharedAmongChapters
import           Data.List (intersperse )
import qualified Data.Set as Set

chpConceptualAnalysis :: Int -> FSpec -> (Blocks,[Picture])
chpConceptualAnalysis lev fSpec = (
      --  *** Header ***
   xDefBlck fSpec ConceptualAnalysis
   <> --  *** Intro  ***
   caIntro
   <> --  *** For all patterns, a section containing the conceptual analysis for that pattern  ***
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
     )<> purposes2Blocks (getOpts fSpec) (purposesInLang fSpec (fsLang fSpec) fSpec) -- This explains the purpose of this context.

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
      <> definitionList (map caRelation (Set.elems $ vrels fSpec))
     
  pictures = map pictOfPat (vpatterns fSpec)
          ++ map pictOfConcept (Set.elems $ concs fSpec)
          ++ map pictOfRule (Set.elems $ vrules fSpec)
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
     <> purposes2Blocks (getOpts fSpec) (purposesInLang fSpec (fsLang fSpec) pat)
        -- followed by a conceptual model for this pattern
     <> ( case fsLang fSpec of
               Dutch   -> -- announce the conceptual diagram
                          para (hyperLinkTo (pictOfPat pat) <> " geeft een conceptueel diagram van dit pattern.")
                          -- draw the conceptual diagram
                          <>(xDefBlck fSpec . pictOfPat) pat
               English -> para (hyperLinkTo (pictOfPat pat) <> " shows a conceptual diagram of this pattern.")
                          <>(xDefBlck fSpec . pictOfPat) pat
        ) <>
    (
        -- now provide the text of this pattern.
       case map caRule . Set.elems $ invariants fSpec `Set.intersection` udefrules pat of
         []     -> mempty
         blocks -> (case fsLang fSpec of
                      Dutch   -> header (lev+3) "Regels"
                              <> plain "Deze paragraaf geeft een opsomming van de regels met een verwijzing naar de gemeenschappelijke taal van de belanghebbenden ten behoeve van de traceerbaarheid."
                      English -> header (lev+3) "Rules"
                              <> plain "This section itemizes the rules with a reference to the shared language of stakeholders for the sake of traceability."
                   )
                   <> definitionList blocks
    )
  caRelation :: Relation -> (Inlines, [Blocks])
  caRelation d
        = let purp =  purposes2Blocks (getOpts fSpec) (purposesInLang fSpec (fsLang fSpec) d)
          in ((xDefInln fSpec (XRefConceptualAnalysisRelation d) <> ": "<>(showMathWithSign d))
             ,[   -- First the reason why the relation exists, if any, with its properties as fundamental parts of its being..
                ( case ( isNull purp, fsLang fSpec) of
                   (True , Dutch)   -> plain ("De volgende " <> str(nladjs d) <> " is gedefinieerd ")
                   (True , English) -> plain ("The following " <> str(ukadjs d) <> " has been defined ")
                   (False, Dutch)   -> purp <> plain ("Voor dat doel is de volgende " <> str(nladjs d) <> " gedefinieerd ")
                   (False, English) -> purp <> plain ("For this purpose, the following " <> str(ukadjs d) <> " has been defined ")
                )
                  -- Then the relation of the relation with its properties and its intended meaning
               <> printMeaning fSpec (fsLang fSpec) d
              ])
  ukadjs d  = if Uni `elem` (properties d) && Tot `elem` (properties d)
              then commaEng "and" (map adj . Set.elems $ (properties d Set.\\ Set.fromList [Uni,Tot]))++" function"
              else commaEng "and" (map adj . Set.elems $ (properties d))++" relation"
  nladjs d = if Uni `elem` (properties d) && Tot `elem` (properties d)
             then commaNL "en" (map adj . Set.elems $ properties d Set.\\ Set.fromList [Uni,Tot])++" functie"
             else commaNL "en" (map adj . Set.elems $ properties d)++" relatie"
  adj = propFullName (fsLang fSpec) 
  caRule :: Rule -> (Inlines, [Blocks])
  caRule r
        = let purp = purposes2Blocks (getOpts fSpec) (purposesInLang fSpec (fsLang fSpec) r)
          in ( mempty
             , [  -- First the reason why the rule exists, if any..
                  purp
                  -- Then the rule as a requirement
               <> plain
                   ( if isNull purp
                     then str (l (NL "De ongedocumenteerde afspraak ", EN "The undocumented agreement "))
                       <> (hyperLinkTo . XRefSharedLangRule) r
                       <> str (l (NL " bestaat: " ,EN " has been made: "))
                     else str (l (NL "Daarom bestaat afspraak ", EN "Therefore agreement "))
                       <> (hyperLinkTo . XRefSharedLangRule) r
                       <> str (l (NL " : ", EN " exists: "))
                   )
               <> printMeaning fSpec (fsLang fSpec) r
                  -- then the formal rule
               <> plain
                   (  str (l (NL "Dit is - gebruikmakend van relaties "
                             ,EN "Using relations "  ))
                    <> mconcat (intersperse  (str ", ")
                                [   hyperLinkTo (XRefConceptualAnalysisRelation d)
                                 <> text (" ("++name d++")")
                                | d<-Set.elems $ bindedRelationsIn r])
                    <> str (l (NL " - geformaliseerd als "
                              ,EN ", this is formalized as "))
                   )
               <> pandocEquationWithLabel fSpec (XRefConceptualAnalysisRule r) (showMath r) 
               -- followed by a conceptual model for this rule
               <> para (   hyperLinkTo (pictOfRule r)
                        <> str (l (NL " geeft een conceptueel diagram van deze regel."
                                  ,EN " shows a conceptual diagram of this rule."))
                       )
               <> xDefBlck fSpec (pictOfRule r)
               ]
             )
