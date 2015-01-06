{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterFunctionPointAnalysis where

import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.FSpec.FPA

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterFunctionPointAnalysis"

-- TODO: add introductory and explanatory text to chapter
-- TODO: what about KGVs?

chpFunctionPointAnalysis :: FSpec -> (Blocks,[Picture])
chpFunctionPointAnalysis fSpec
 = ( (chptHeader (fsLang fSpec) FunctionPointAnalysis) <>
     (fromList . concat)
       [ introBlocks
       , dataModelFPABlocks
       , userTransactionFPABlocks
       ]
   , []) -- no pictures at the moment
  where
  lang = Dutch -- TODO: add English labels and use (fsLang fSpec) here
  fpa = fpAnalyze fSpec 
  introBlocks :: [Block]
  introBlocks =
    case lang of
      Dutch   -> [Para
                  [ Str "Dit hoofdstuk ..."
                  ]]
      English -> [Para
                  [ Str "This chapter ..."
                  ]]

  dataModelFPABlocks :: [Block]
  dataModelFPABlocks =
    [ Table [Str "Datamodel"]
                [AlignLeft,        AlignLeft,         AlignLeft,                             AlignLeft                     ]
                [0.0,              0.0,               0.0,                                   0.0                           ]
              [ [plainStr "Type"], [plainStr "Naam"], [plainStr "Complexiteit"],             [plainStr "FP"]               ] $
            [ [ [plainStr "ILGV"], [plainStr nm],     [plainStr $ showLang lang cmplxty], [plainStr $ show $ fpVal fp ] ]
            | fp@FP{fpType=ILGV, fpName=nm, fpComplexity=cmplxty} <- fps
            ] ++
            [ []
            , [ [plainStr "Totaal"], [],              [],                                    [plainStr $ show totalFP]     ]
            ]
    ]
   where (fps, totalFP) = dataModelFPA fpa

  userTransactionFPABlocks :: [Block]
  userTransactionFPABlocks =
    [ Table [Str "Gebruikerstransacties"]
                [AlignLeft,        AlignLeft,         AlignLeft,                             AlignLeft                     ]
                [0.0,              0.0,               0.0,                                   0.0                           ]
              [ [plainStr "Type"], [plainStr "Naam"], [plainStr "Complexiteit"],             [plainStr "FP"]               ] $
            [ [ [plainStr tpStr],  [plainStr nm],     [plainStr $ showLang lang cmplxty], [plainStr $ show $ fpVal fp ] ]
            | fp@FP{fpType=tp, fpName=nm, fpComplexity=cmplxty} <- fps
            , let tpStr = showLang lang tp
            ] ++
            [ []
            , [ [plainStr "Totaal"], [],              [],                                    [plainStr $ show totalFP]     ] 
            ]
    ]
   where (fps, totalFP) = userTransactionFPA fpa
    
plainStr :: String -> Block
plainStr str'  = Plain [Str str']
