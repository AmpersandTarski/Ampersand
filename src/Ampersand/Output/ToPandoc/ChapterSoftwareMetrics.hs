{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Output.ToPandoc.ChapterSoftwareMetrics
where
import Ampersand.Output.ToPandoc.SharedAmongChapters
--import Ampersand.Output.Statistics (Statistics(..))
--import Ampersand.Output.PandocAux

------------------ Function Point Analysis --------------------
-- TODO: Engels en Nederlands netjes scheiden.
-- TODO: Andere formaten dan LaTeX ondersteunen.

fpAnalysis :: FSpec -> Blocks
fpAnalysis _ = mempty -- if null (themes fSpec) then header ++ caIntro ++ fpa2Blocks else []
-- where
--  header :: Blocks
--  header = xDefBlck fSpec SoftwareMetrics
--  caIntro :: [Block]
--  caIntro =
--   case fsLang fSpec of
--      Dutch   -> [Para
--                  [ Str "De specificatie van "
--                  , Quoted  SingleQuote [Str (name fSpec)]
--                  , Str " is geanalyseerd door middel van een functiepuntentelling"
--                  , xrefCitation "IFPUG"
--                  , Str ". "
--                  , Str $ "Dit heeft geresulteerd in een geschat totaal van "++(show.nFpoints) fSpec++" functiepunten."
--                  ]]
--      English -> [Para
--                  [ Str "The specification of "
--                  , Quoted  SingleQuote [Str (name fSpec)]
--                  , Str " has been analysed by counting function points"
--                  , xrefCitation "IFPUG"
--                  , Str ". "
--                  , Str $ "This has resulted in an estimated total of "++(show.nFpoints) fSpec++" function points."
--                  ]]
--
--
--  fpa2Blocks' :: [Block]
--  fpa2Blocks' = []
--   = [ Table [] [AlignLeft,AlignLeft,AlignRight] [0.0,0.0,0.0]
--             ( case fsLang fSpec of
--                 Dutch   -> [ [Plain [Str "gegevensverzameling"]]
--                            , [Plain [Str "analyse"]]
--                            , [Plain [Str "FP"]]]
--                 English -> [ [Plain [Str "data set"]]
--                            , [Plain [Str "analysis"]]
--                            , [Plain [Str "FP"]]]
--             )
--             [ [ [Plain [(Str . name)                 plug]]
--               , [Plain [(Str . show . fpa)           plug]]
--               , [Plain [(Str . show . fPoints . fpa) plug]]
--               ]
--             | plug<-plugInfos fSpec, fPoints (fpa plug)>0
--             ]
--     , Table [] [AlignLeft,AlignLeft,AlignRight] [0.0,0.0,0.0]
--             ( case fsLang fSpec of
--                Dutch   ->
--                    [ [Plain [Str "interface"]]
--                    , [Plain [Str "analyse"]]
--                    , [Plain [Str "FP"]]]
--                English ->
--                    [ [Plain [Str "interface"]]
--                    , [Plain [Str "analysis"]]
--                    , [Plain [Str "FP"]]]
--             )
--             [ [ [Plain [(Str . name)                    act]]
--               , [Plain [(Str . show . actFPA)           act]]
--               , [Plain [(Str . show . fPoints . actFPA) act]]]
--             | act<-fActivities fSpec
--             ]
--     ]

