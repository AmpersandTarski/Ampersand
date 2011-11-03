{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterSoftwareMetrics
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import Data.List
import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms
import DatabaseDesign.Ampersand.Fspec.FPA (fpa) 
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Fspec.Fspec
import Text.Pandoc
import Text.Pandoc.Builder  (toList, codeBlock)
import DatabaseDesign.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.Switchboard      (SwitchBdDiagram(..), switchboardAct,sbDiagram)
import DatabaseDesign.Ampersand.Output.AdlExplanation (purpose,meaning,Explainable(..))
import DatabaseDesign.Ampersand.Output.Statistics (Statistics(..))
import DatabaseDesign.Ampersand.Output.PandocAux


------------------ Function Point Analysis --------------------
-- TODO: Engels en Nederlands netjes scheiden.
-- TODO: Andere formaten dan LaTeX ondersteunen.

fpAnalysis :: Int -> Fspc -> Options ->  [Block]
fpAnalysis lev fSpec flags = header ++ caIntro ++ fpa2Blocks
 where 
  header :: [Block]
  header = labeledHeader lev (xLabel SoftwareMetrics)
                         (case language flags of
                               Dutch   ->  "Functiepunt Analyse"   
                               English ->  "Function Point Analysis"
                         )
  caIntro :: [Block]
  caIntro = 
   case language flags of
      Dutch   -> [Para
                  [ Str "De specificatie van "
                  , Quoted  SingleQuote [Str (name fSpec)]
                  , Str " is geanalyseerd door middel van een functiepuntentelling"
                  , xrefCitation "IFPUG"
                  , Str ". "
                  , Str $ "Dit heeft geresulteerd in een geschat totaal van "++(show.nFpoints) fSpec++" functiepunten."
                  ]]
      English -> [Para
                  [ Str "The specification of "
                  , Quoted  SingleQuote [Str (name fSpec)]
                  , Str " has been analysed by counting function points"
                  , xrefCitation "IFPUG"
                  , Str ". "
                  , Str $ "This has resulted in an estimated total of "++(show.nFpoints) fSpec++" function points."
                  ]]
   

  fpa2Blocks :: [Block]
  fpa2Blocks
   = [ Table [] [AlignLeft,AlignLeft,AlignRight] [0.0,0.0,0.0]
                  ( case language flags of
                      Dutch   -> [ [Plain [Str "gegevensverzameling"]]
                                 , [Plain [Str "analyse"]]
                                 , [Plain [Str "FP"]]]
                      English -> [ [Plain [Str "data set"]]
                                 , [Plain [Str "analysis"]]
                                 , [Plain [Str "FP"]]]
                  )
                  [[[Plain [(Str . name)                 plug]]
                   ,[Plain [(Str . show . fpa)           plug]]
                   ,[Plain [(Str . show . fPoints . fpa) plug]]]
                  | plug<-plugInfos fSpec, fPoints (fpa plug)>0
                  ]
{- was:         [ Para $ 
                  [ Math InlineMath $ "\\begin{tabular}{|l|l|r|}\\hline \n" ++
                          intercalate "&" ["data set", "analysis", "points"] ++"\\\\\\hline\n"++
                          intercalate "\\\\\n" [ intercalate "&" [name plug, show (fpa plug), (show.fPoints.fpa) plug]
                                         | plug<-plugInfos fSpec
                                         , fPoints (fpa plug)>0] ++
                          "\\\\\\hline\\end{tabular}" ]
-}
                , Table [] [AlignLeft,AlignLeft,AlignRight] [0.0,0.0,0.0]
                  ( case language flags of
                     Dutch   ->
                         [ [Plain [Str "interface"]]
                         , [Plain [Str "analyse"]]
                         , [Plain [Str "FP"]]]
                     English ->
                         [ [Plain [Str "interface"]]
                         , [Plain [Str "analysis"]]
                         , [Plain [Str "FP"]]]
                  )
                  [ [ [Plain [(Str . name)                    act]]
                    , [Plain [(Str . show . actFPA)           act]]
                    , [Plain [(Str . show . fPoints . actFPA) act]]]
                  | act<-fActivities fSpec
                  ]
                ]            

