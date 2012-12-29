{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterConceptualAnalysis
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters
import DatabaseDesign.Ampersand.ADL1 (Prop(..)) 
import DatabaseDesign.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.PandocAux
import Data.List (intercalate)

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterConceptualAnalysis.hs"

chpConceptualAnalysis :: Int -> Fspc -> Options -> ([Block],[Picture])
chpConceptualAnalysis lev fSpec flags = (header ++ caIntro ++ caBlocks, pictures)
  where
  header :: [Block]
  header = toList $ chptHeader flags ConceptualAnalysis
  caIntro :: [Block]
  caIntro
   = (case language flags of
        Dutch   -> [Para
                    [ Str "Dit hoofdstuk beschrijft een formele taal, waarin functionele eisen ten behoeve van "
                    , Quoted  SingleQuote [Str (name fSpec)]
                    , Str " kunnen worden besproken en uitgedrukt. "
                    , Str "Het doel van dit hoofdstuk is het vastleggen van de totstandkoming van de formele regels vanuit het gedeelde begrip van de belanghebbende."
                    , Str "De formele taal van ", Quoted  SingleQuote [Str (name fSpec)], Str " bestaat uit binaire relaties en concepten."
                    , Str "Iedere regel wordt uitgedrukt in termen van deze binaire relaties als een assertie in relatie algebra."
                   ]]
        English -> [Para
--                    [ Str "This chapter provides an analysis of the principles described in chapter "
--                    , xrefReference FunctReqts
--                    , Str ". Each section in that chapter is analysed in terms of relations "
--                    , Str "and each principle is then translated in a rule. "
                    [ Str "This chapter defines the formal language, in which functional requirements of "
                    , Quoted  SingleQuote [Str (name fSpec)]
                    , Str " can be analysed and expressed."
                    , Str "The purpose of this chapter is to document the translation of the shared understanding among stakeholders into formal rules. "
                    , Str "The formal language of ", Quoted  SingleQuote [Str (name fSpec)], Str " consists of binary relations on concepts."
                    , Str "All rules are expressed on these binary relations as relation algebraic assertions. "
                    ]])
     ++ purposes2Blocks flags (purposesDefinedIn fSpec (language flags) fSpec) -- This explains the purpose of this context.
     
  caBlocks = concat(map caSection (patterns fSpec))
  pictures =        map pict      (patterns fSpec)
  -----------------------------------------------------
  -- the Picture that represents this pattern's conceptual graph
  pict :: Pattern -> Picture
  pict pat = (makePicture flags fSpec Plain_CG pat)
                {caption = case language flags of
                            Dutch   ->"Conceptdiagram van "++name pat
                            English ->"Concept diagram of "++name pat}
  caSection :: Pattern -> [Block]
  caSection pat
   =    -- new section to explain this pattern  
        toList ( labeledThing flags (lev+1) (xLabel ConceptualAnalysis++"_"++name pat) (name pat))
        -- The section starts with the reason why this pattern exists 
     ++ purposes2Blocks flags (purposesDefinedIn fSpec (language flags) pat)
        -- followed by a conceptual model for this pattern
     ++ ( case (genGraphics flags, language flags) of
               (True,Dutch  ) -> -- announce the conceptual diagram
                                 [Para [Str "Figuur ", xrefReference (pict pat), Str " geeft een conceptueel diagram van dit pattern."]
                                 -- draw the conceptual diagram
                                 ,Plain (xrefFigure1 (pict pat))]          
               (True,English) -> [Para [Str "Figure ", xrefReference (pict pat), Str " shows a conceptual diagram of this pattern."]
                                 ,Plain (xrefFigure1 (pict pat))]
               _              -> [])
        -- now provide the text of this pattern.
     ++ (case language flags of
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
     ++ [DefinitionList blocks | let blocks = map caRelation (declarations pat), not(null blocks)]
     ++ (case language flags of
           Dutch   -> toList (labeledThing flags (lev+2) (xLabel ConceptualAnalysis++"_rulesOf_"++name pat) "Formele regels")
                      ++
                      [Plain [Str "Deze paragraaf geeft een opsomming van de formele regels met een verwijzing naar de gemeenschappelijke taal van de belanghebbenden ten behoeve van de traceerbaarheid."]]
           English -> toList (labeledThing flags (lev+2) (xLabel ConceptualAnalysis++"_rulesOf_"++name pat) "Formal rules")
                      ++
                      [Plain [Str "This section itemizes the formal rules with a reference to the shared language of stakeholders for the sake of traceability."]])
     ++ [DefinitionList blocks | let blocks = map caRule (invariants pat `isc` udefrules pat), not(null blocks)]

  caRelation :: Declaration -> ([Inline], [[Block]])
  caRelation d 
        = let purp = purposes2Blocks flags [p | p<-purposesDefinedIn fSpec (language flags) d]
          in ([]
             ,[   -- First the reason why the relation exists, if any, with its properties as fundamental parts of its being..
                  purp
                  -- Then the declaration of the relation with its properties and its intended meaning 
               ++ [ Plain$[if null purp then Str ("De volgende "++nladjs d++" relatie is gedeclareerd ")
                                        else Str ("Daarom is de volgende "++nladjs d++" relatie gedeclareerd ") | language flags==Dutch]
                       ++ [if null purp then Str ("The following "++ukadjs d++" relation has been declared ")
                                        else Str ("Therefore the following "++ukadjs d++" relation has been declared ") | language flags==English] ]
               ++ pandocEqnArray 
                     [ ( texOnly_Id(name d)
                       , ":"
                       , texOnly_Id(name (source d))++(if isFunction d then texOnly_fun else texOnly_rel)++texOnly_Id(name(target d))++symDefLabel d
                       )  ]
               ++ [Plain$[Str ", hetgeen betekent: "| language flags==Dutch]
                      ++ [Str ", which means: "| language flags==English]]
               ++ meaning2Blocks (language flags) d
              ])
  ukadjs d = intercalate ", " (map ukadj (multiplicities d))
  ukadj Uni = "univalent"
  ukadj Inj = "injective"
  ukadj Sur = "surjective"
  ukadj Tot = "total"
  ukadj Sym = "symmetric"
  ukadj Asy = "antisymmetric"
  ukadj Trn = "transitive"
  ukadj Rfx = "reflexive"
  ukadj Irf = "irreflexive"
  nladjs d = intercalate ", " (map nladj (multiplicities d))
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
        = let purp = purposes2Blocks flags (purposesDefinedIn fSpec (language flags) r)
          in ( []
             , [  -- First the reason why the rule exists, if any..
                  purp  
                  -- Then the rule as a requirement
               ++ [Plain$[if null purp then Str "De volgende eis is gesteld in paragraaf " 
                                       else Str "Daarom is als eis gesteld in paragraaf " | language flags==Dutch]
                      ++ [if null purp then Str "The following requirement has been defined in section " 
                                       else Str "Therefore the following requirement has been defined in section " | language flags==English]
                      ++ [RawInline "latex" "~"
                         ,RawInline "latex" $ symReqRef r
                         ,Str " p."
                         ,RawInline "latex" "~"
                         ,RawInline "latex" $ symReqPageRef r
                         ,Str ": "]]
               ++ meaning2Blocks (language flags) r
                  -- then the formal rule
               ++ [Plain$[Str "Dit is geformalizeerd - gebruikmakend van relaties " | language flags==Dutch]
                      ++ [Str "This is formalized - using relations "     | language flags==English]
                      ++ intercalate [Str ", "] [[RawInline "latex" $ symDefRef d] | d@(Sgn{})<-map makeDeclaration (mors r)]
                      ++ [Str " - als " | language flags==Dutch]
                      ++ [Str " - as "     | language flags==English]]
               ++ (if showPredExpr flags
                   then pandocEquation ((showLatex.toPredLogic) r++symDefLabel r)
                   else pandocEquation (showMath r++symDefLabel r)
                  )
               ])



