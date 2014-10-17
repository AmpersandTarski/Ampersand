module Database.Design.Ampersand.Output.ToPandoc.ChapterFunctionPointAnalysis where

import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Classes
import Data.List
import Database.Design.Ampersand.Output.PandocAux

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterFunctionPointAnalysis"

-- TODO: add introductory and explanatory text to chapter
-- TODO: what about KGVs?

chpFunctionPointAnalysis :: Fspc -> (Blocks,[Picture])
chpFunctionPointAnalysis fSpec
 = ( (chptHeader (fsLang fSpec) FunctionPointAnalysis) <>
     (fromList . concat)
       [ introBlocks
       , dataModelFPABlocks
       , userTransactionFPABlocks
       ]
   , []) -- no pictures at the moment
  where
  fpa = fpAnalyze fSpec 
  introBlocks :: [Block]
  introBlocks =
    case fsLang fSpec of
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
            [ [ [plainStr "ILGV"], [plainStr nm],     [plainStr $ showNLComplexity cmplxty], [plainStr $ show $ fpVal fp ] ]
            | fp@FP{fpType=ILGV, fpName=nm, fpComplexity=cmplxty} <- fps
            ] ++
            [ []
            , [ [plainStr "Totaal"], [],              [],                                    [plainStr $ show totalFP]     ]
            ]
    ]
   where (fps, totalFP) = dataModelFPA fpa

  userTransactionFPABlocks :: [Block]
  userTransactionFPABlocks =
    [ Table [Str "Gerbruikerstransacties"]
                [AlignLeft,        AlignLeft,         AlignLeft,                             AlignLeft                     ]
                [0.0,              0.0,               0.0,                                   0.0                           ]
              [ [plainStr "Type"], [plainStr "Naam"], [plainStr "Complexiteit"],             [plainStr "FP"]               ] $
            [ [ [plainStr tpStr],  [plainStr nm],     [plainStr $ showNLComplexity cmplxty], [plainStr $ show $ fpVal fp ] ]
            | fp@FP{fpType=tp, fpName=nm, fpComplexity=cmplxty} <- fps
            , let tpStr = showNLFPType tp
            ] ++
            [ []
            , [ [plainStr "Totaal"], [],              [],                                    [plainStr $ show totalFP]     ] 
            ]
    ]
   where (fps, totalFP) = userTransactionFPA fpa
    
plainStr :: String -> Block
plainStr str = Plain [Str str]

showNLFPType :: FPType -> String
showNLFPType ILGV = "ILGV" 
showNLFPType KGV  = "KGV" 
showNLFPType IF   = "IF"
showNLFPType UF   = "UF"
showNLFPType OF   = "OF"

showNLComplexity :: Complexity -> String
showNLComplexity Eenvoudig = "Eenvoudig" 
showNLComplexity Gemiddeld = "Gemiddeld"
showNLComplexity Moeilijk  = "Moeilijk"

{-
showENFPType :: FPType -> String
showENFPType ILGV = "ILF" -- Internal Logical File 
showENFPType KGV  = "ELF" -- External Logical File 
showENFPType IF   = "EI"  -- External Input
showENFPType UF   = "EO"  -- External Output
showENFPType OF   = "EQ"  -- External inQuiry

showENComplexity :: Complexity -> String
showENComplexity Eenvoudig = "Low" 
showENComplexity Gemiddeld = "Average"
showENComplexity Moeilijk  = "High"
-}


-- To be moved to FPA.hs (so we can call it from AdlToFSpec and put FPA in the FSpec)

data FPA = FPA { dataModelFPA :: ([FP], Int), userTransactionFPA :: ([FP],Int) } deriving Show

-- NOTE: FPA constructors are in Dutch (so 'output function' is not OF, but UF)   

data Complexity = Eenvoudig | Gemiddeld | Moeilijk deriving (Show, Eq, Ord)

data FPType = ILGV | KGV | IF | UF | OF deriving Show

data FP = FP { fpType :: FPType, fpName :: String, fpComplexity :: Complexity } deriving Show


-- | Valuing of function points according to par. 3.9 (UK) or par. 2.9 (NL), see http://www.nesma.nl/sectie/fpa/hoefpa.asp
fpVal :: FP -> Int
fpVal FP{fpType=ILGV, fpComplexity=Eenvoudig} = 7
fpVal FP{fpType=ILGV, fpComplexity=Gemiddeld} = 10
fpVal FP{fpType=ILGV, fpComplexity=Moeilijk}  = 15
fpVal FP{fpType=KGV,  fpComplexity=Eenvoudig} = 5
fpVal FP{fpType=KGV,  fpComplexity=Gemiddeld} = 7
fpVal FP{fpType=KGV,  fpComplexity=Moeilijk}  = 10
fpVal FP{fpType=IF,   fpComplexity=Eenvoudig} = 3
fpVal FP{fpType=IF,   fpComplexity=Gemiddeld} = 4
fpVal FP{fpType=IF,   fpComplexity=Moeilijk}  = 6
fpVal FP{fpType=UF,   fpComplexity=Eenvoudig} = 4
fpVal FP{fpType=UF,   fpComplexity=Gemiddeld} = 5
fpVal FP{fpType=UF,   fpComplexity=Moeilijk}  = 7
fpVal FP{fpType=OF,   fpComplexity=Eenvoudig} = 3
fpVal FP{fpType=OF,   fpComplexity=Gemiddeld} = 4
fpVal FP{fpType=OF,   fpComplexity=Moeilijk}  = 6

fpAnalyze :: Fspc -> FPA
fpAnalyze fSpec = FPA (countFPs $ fpaDataModel fSpec) (countFPs $ fpaUserTransactions fSpec)
  where countFPs :: [FP] -> ([FP], Int)
        countFPs fps = (fps, sum $ map fpVal fps)

fpaDataModel :: Fspc -> [FP]
fpaDataModel fSpec = [ FP ILGV (name plugInfo) (ilgvComplexity nrOfFields)
                     | plugInfo <- plugInfos fSpec
                     , let nrOfFields = case plugInfo of
                                          InternalPlug plug -> length $ plugFields plug
                                          ExternalPlug _    -> fatal 42 "unhandled ExternalPlug."
                     ] 
 where ilgvComplexity n | n <= 1    = Eenvoudig
                        | n <= 5    = Gemiddeld
                        | otherwise = Moeilijk

fpaUserTransactions :: Fspc -> [FP]
fpaUserTransactions fSpec = map fpaInterface $ interfaceS fSpec
 where fpaInterface ifc = 
         let nm = name ifc
             cmplxty = depth2Cmplxty $ getDepth $ ifcObj ifc
             fpType = case ifc of
                        Ifc{ifcClass=Just "IF"} -> IF
                        Ifc{ifcClass=Just "UF"} -> UF
                        Ifc{ifcClass=Just "OF"} -> OF
                        
                        -- code for interfaces without CLASS comes from old FPA.hs
                        ifc | (not.null.ifcParams)  ifc -> IF  -- In case there are editable relations, this must be an import function.
                            | (isUni.objctx.ifcObj) ifc -> OF  -- If there is max one atom, this is a simple function.
                            | otherwise                 -> UF  -- Otherwise, it is a UF
          in FP fpType nm cmplxty
        where depth2Cmplxty d | d <= 1    = Eenvoudig
                              | d == 2    = Gemiddeld
                              | otherwise = Moeilijk 

       getDepth :: ObjectDef -> Int
       getDepth Obj{objmsub=Nothing}             = 0
       getDepth Obj{objmsub=Just InterfaceRef{}} = 1 -- TODO: shouldn't we follow the ref? (this def. is from old FPA.hs)
       getDepth Obj{objmsub=Just (Box _ objs)}   = 1 + maximum (map getDepth objs)
