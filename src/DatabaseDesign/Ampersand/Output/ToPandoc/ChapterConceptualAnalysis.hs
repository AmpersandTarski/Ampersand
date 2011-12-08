{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterConceptualAnalysis
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.PandocAux

fatal :: Int -> String -> a
fatal = fatalMsg "ChapterConceptualAnalysis.hs"

chpConceptualAnalysis :: Int -> Fspc -> Options -> ([Block],[Picture])
chpConceptualAnalysis lev fSpec flags = (header ++ caIntro ++ caBlocks , pictures)
  where
  header :: [Block]
  header = labeledHeader lev (xLabel ConceptualAnalysis)
                                        (case language flags of
                                            Dutch   ->  "Conceptuele Analyse"   
                                            English ->  "Conceptual Analysis"
                                        )
  caIntro :: [Block]
  caIntro =
   purposes2Blocks flags purps ++ -- This explains the purpose of this context.
   (case language flags of
      Dutch   -> [Para
                  [ Str "Dit hoofdstuk geeft een analyse van de regels uit hoofdstuk "
                  , xrefReference FunctionalRequirements
                  , Str ". Ieder thema in dat hoofdstuk wordt geanalyseerd in termen van relaties "
                  , Str "en elke afspraak krijgt een formele representatie. "
                  ]]
      English -> [Para
                  [ Str "This chapter provides an analysis of the principles described in chapter "
                  , xrefReference FunctionalRequirements
                  , Str ". Each section in that chapter is analysed in terms of relations "
                  , Str "and each principle is then translated in a rule. "
                  ]]
   ) where
      purps = purposes fSpec (language flags) fSpec

  (caBlocks,pictures) = ( [b | (blocks,_)<-ca, b<-blocks], [picture | (_,picture)<-ca] )
                        where ca=if null (themes fSpec)
                                 then caSections (patterns fSpec)
                                 else caSections [ pat | pat<-patterns fSpec, name pat `elem` themes fSpec ]

  caSections :: [Pattern] -> [([Block],Picture)]
  caSections pats = iterat pats 1 [] []
   where
    iterat :: [Pattern] -> Int -> [A_Concept] -> [Declaration] -> [([Block],Picture)]
    iterat [] _ _ _ = []
    iterat (pat:ps) i seenConcepts seenDeclarations
     = ( [Header (lev+1) [Str (name pat)]]    -- new section to explain this theme
       ++ sctMotivation                       -- The section startss with the reason why this theme exists,
                                              -- followed by a conceptual model for this theme
       ++ ( case (genGraphics flags, language flags) of             -- announce the conceptual diagram
                 (True,Dutch  ) -> [Para [Str "Figuur ", xrefReference pict, Str " geeft een conceptueel diagram van dit thema."]
                                   ,Plain (xrefFigure1 pict)]          -- draw the conceptual diagram
                 (True,English) -> [Para [Str "Figure ", xrefReference pict, Str " shows a conceptual diagram of this theme."]
                                   ,Plain (xrefFigure1 pict)]          -- draw the conceptual diagram
                 _              -> []
          )                                   -- now provide the text of this theme.
       ++ (if null blocks then [] else [DefinitionList blocks])
       , pict):  iterat ps i'' seenCss seenDss
       where
         pict = (makePicture flags fSpec Rel_CG pat)   -- the Picture that represents this pattern's knowledge graph
                {caption = case language flags of
                            Dutch   ->"Conceptdiagram van "++name pat
                            English ->"Concept diagram of "++name pat}
         blocks  :: [([Inline], [[Block]])]
         blocks = sctRules ++ sctSignals
         sctMotivation
          = purposes2Blocks flags purps
            where purps = purposes fSpec (language flags) pat
         (sctRules,   i',  seenCrs, seenDrs) = dpRule fSpec flags (invariants pat) i seenConcepts seenDeclarations
         (sctSignals, i'', seenCss, seenDss) = dpRule fSpec flags (processRules pat) i' seenCrs seenDrs

