{-# LANGUAGE RecordWildCards #-}
-- | This module contains the building blocks that are available in the Ampersand Library. These building blocks will be described further at [ampersand.sourceforge.net |the wiki pages of our project].
--
module Ampersand.Components
  ( -- * Type checking and calculus
     makeFSpec
    -- * Generators of output
   , generateAmpersandOutput
  )
where
import           Ampersand.Basics
import           Ampersand.Core.ShowAStruct
import           Ampersand.Core.AbstractSyntaxTree
import           Ampersand.FSpec
import           Ampersand.FSpec.GenerateUML
import           Ampersand.Graphic.Graphics (writePicture)
import           Ampersand.Misc
import           Ampersand.Output
import           Ampersand.Prototype.GenFrontend (doGenFrontend)
import           Ampersand.Prototype.ValidateSQL (validateRulesSQL)
import           Control.Monad
import qualified Data.ByteString.Lazy as L
import           Data.Function (on)
import           Data.List
import qualified Data.Set as Set
import qualified Data.Text.IO as Text (writeFile)-- This should become the standard way to write all files as Text, not String.
import           Data.Maybe (isJust, fromJust)
import           System.Directory
import           System.FilePath
import           Text.Pandoc
import           Text.Pandoc.Builder

--  | The FSpec is the datastructure that contains everything to generate the output. This monadic function
--    takes the FSpec as its input, and spits out everything the user requested.
generateAmpersandOutput :: Options -> MultiFSpecs -> IO ()
generateAmpersandOutput opts@Options{..} multi = 
  do { verboseLn "Checking for rule violations..."
     ; reportInvViolations violationsOfInvariants
     ; reportSignals (initialConjunctSignals fSpec)
     ; createDirectoryIfMissing True dirOutput
     ; sequence_ . map snd . filter fst $ conditionalActions
     }
  where 
   conditionalActions :: [(Bool, IO())]
   conditionalActions = 
      [ ( genUML               , doGenUML              )
      , ( haskell               , doGenHaskell          )
      , ( sqlDump               , doGenSQLdump          )
      , ( export2adl            , doGenADL              )
      , ( genFSpec              , doGenDocument         )
      , ( genFPAExcel           , doGenFPAExcel         )
      , ( genPOPExcel           , doGenPopsXLSX         )
      , ( proofs                , doGenProofs           )
      , ( validateSQL           , doValidateSQLTest     )
      , ( genPrototype          , doGenProto            )
      , ( genRapPopulationOnly  , doGenRapPopulation    )
      , ( isJust testRule       , ruleTest . fromJust $ testRule)
      ]
   fSpec = userFSpec multi
   doGenADL :: IO()
   doGenADL =
    do { putStrLn $ "Generating Ampersand script (ADL) for "  ++ name fSpec ++ "..."
       ; writeFile outputFile . showA . originalContext $ fSpec
       ; verboseLn $ ".adl-file written to " ++ outputFile ++ "."
       }
    where outputFile = dirOutput </> outputfile

   doGenProofs :: IO()
   doGenProofs =
    do { putStrLn $ "Generating Proof for " ++ name fSpec ++ " into " ++ outputFile ++ "..."
   --  ; verboseLn $ writeTextile def thePandoc
       ; content <- runIO (writeHtml5String def thePandoc) >>= handleError
       ; Text.writeFile outputFile content
       ; verboseLn "Proof written."
       }
    where outputFile = dirOutput </> "proofs_of_"++baseName -<.> ".html"
          thePandoc = setTitle title (doc theDoc)
          title  = text $ "Proofs for "++name fSpec
          theDoc = fDeriveProofs fSpec
          --theDoc = plain (text "Aap")  -- use for testing...

   doGenHaskell :: IO()
   doGenHaskell =
    do { putStrLn $ "Generating Haskell source code for " ++ name fSpec ++ "..."
   --  ; verboseLn $ fSpec2Haskell fSpec -- switch this on to display the contents of Installer.php on the command line. May be useful for debugging.
       ; writeFile outputFile (fSpec2Haskell opts fSpec)
       ; verboseLn ("Haskell written into " ++ outputFile ++ ".")
       }
    where outputFile = dirOutput </> baseName -<.> ".hs"
   doGenSQLdump :: IO()
   doGenSQLdump =
    do { putStrLn $ "Generating SQL queries dumpfile for " ++ name fSpec ++ "..."
       ; Text.writeFile outputFile (dumpSQLqueries opts multi)
       ; verboseLn ("SQL queries dumpfile written into " ++ outputFile ++ ".")
       }
    where outputFile = dirOutput </> baseName ++ "_dump" -<.> ".sql"
   
   doGenUML :: IO()
   doGenUML =
    do { putStrLn "Generating UML..."
       ; writeFile outputFile $ generateUML fSpec
       ; verboseLn ("Generated file: " ++ outputFile ++ ".")
       }
      where outputFile = dirOutput </> baseName -<.> ".xmi"

   -- This function will generate all Pictures for a given FSpec.
   -- the returned FSpec contains the details about the Pictures, so they
   -- can be referenced while rendering the FSpec.
   -- This function generates a pandoc document, possibly with pictures from an fSpec.
   doGenDocument :: IO()
   doGenDocument =
    do { putStrLn $ "Generating functional design document for " ++ name fSpec ++ "..."
       ; -- First we need to output the pictures, because they should be present 
         -- before the actual document is written
         when (not(noGraphics) && fspecFormat /=FPandoc) $
           mapM_ (writePicture opts) (reverse thePictures) -- NOTE: reverse is used to have the datamodels generated first. This is not required, but it is handy.
       ; writepandoc opts fSpec thePandoc
       }
     where (thePandoc,thePictures) = fSpec2Pandoc opts fSpec
        

   -- | This function will generate an Excel workbook file, containing an extract from the FSpec
   doGenFPAExcel :: IO()
   doGenFPAExcel =
     putStrLn "Sorry, FPA analisys is discontinued. It needs maintenance." -- See https://github.com/AmpersandTarski/Ampersand/issues/621
     --  ; writeFile outputFile $ fspec2FPA_Excel fSpec
    
--      where outputFile = dirOutput </> "FPA_"++baseName -<.> ".xml"  -- Do not use .xls here, because that generated document contains xml.

   doGenPopsXLSX :: IO()
   doGenPopsXLSX =
    do { putStrLn "Generating .xlsx file containing the population..."
       ; ct <- runIO getPOSIXTime >>= handleError
       ; L.writeFile outputFile $ fSpec2PopulationXlsx ct fSpec
       ; verboseLn ("Generated file: " ++ outputFile)
       }
      where outputFile = dirOutput </> baseName ++ "_generated_pop" -<.> ".xlsx"

   doValidateSQLTest :: IO ()
   doValidateSQLTest =
    do { putStrLn "Validating SQL expressions..."
       ; errMsg <- validateRulesSQL opts fSpec
       ; unless (null errMsg) (exitWith $ InvalidSQLExpression errMsg)
       }

   doGenProto :: IO ()
   doGenProto =
     if null violationsOfInvariants || allowInvariantViolations
     then sequence_ $
          [ putStrLn "Generating prototype..."
          , createDirectoryIfMissing True dirPrototype
          , doGenFrontend opts fSpec
          , generateDatabaseFile opts multi
          , generateJSONfiles opts multi
          , verboseLn $ "Prototype files have been written to " ++ dirPrototype
          ]
     else do exitWith NoPrototypeBecauseOfRuleViolations

   doGenRapPopulation :: IO ()
   doGenRapPopulation =
     if null violationsOfInvariants || allowInvariantViolations
     then sequence_ $
          [ putStrLn "Generating RAP population..."
          , createDirectoryIfMissing True dirPrototype
          , generateJSONfiles opts multi
          , verboseLn $ "RAP population file has been written to " ++ dirPrototype
          ]
     else do exitWith NoPrototypeBecauseOfRuleViolations

   violationsOfInvariants :: [(Rule,AAtomPairs)]
   violationsOfInvariants
     = [(r,vs) |(r,vs) <- allViolations fSpec
               , not (isSignal r)
               , not (elemOfTemporarilyBlocked r)
       ]
     where
       elemOfTemporarilyBlocked rul =
         if atlasWithoutExpressions 
         then name rul `elem` 
                 [ "TOT formalExpression[Rule*Expression]"
                 , "TOT objExpression[BoxItem*Expression]"
                 ]
         else False
   reportInvViolations :: [(Rule,AAtomPairs)] -> IO()
   reportInvViolations []    = verboseLn $ "No invariant violations found for the initial population"
   reportInvViolations viols =
     if allowInvariantViolations && not verboseP
     then
       -- TODO: this is a nice use case for outputting warnings
       putStrLn "There are invariant violations that are ignored. Use --verbose to output the violations"
     else
       let ruleNamesAndViolStrings = [ (name r, showprs p) | (r,p) <- viols ]
       in  putStrLn $ 
                  intercalate "\n"
                      [ "Violations of rule "++show r++":\n"++ concatMap (\(_,p) -> "- "++ p ++"\n") rps
                      | rps@((r,_):_) <- groupBy (on (==) fst) $ sort ruleNamesAndViolStrings
                      ]
   
   showprs :: AAtomPairs -> String
   showprs aprs = "["++intercalate ", " (Set.elems $ Set.map showA aprs)++"]"
   -- showpr :: AAtomPair -> String
   -- showpr apr = "( "++(showVal.apLeft) apr++", "++(showVal.apRight) apr++" )"
   reportSignals []        = verboseLn "No signals for the initial population" 
   reportSignals conjViols = 
     if verboseP
     then
       verboseLn $ "Signals for initial population:\n" ++ intercalate "\n"
         [   "Rule(s): "++(show . map name . Set.elems . rc_orgRules) conj
         ++"\n  Conjunct   : " ++ showA (rc_conjunct conj)
         ++"\n  Violations : " ++ showprs viols
         | (conj, viols) <- conjViols
         ]
     else
       putStrLn "There are signals for the initial population. Use --verbose to output the violations"
   ruleTest :: String -> IO ()
   ruleTest ruleName =
    case [ rule | rule <- Set.elems $ grules fSpec `Set.union` vrules fSpec, name rule == ruleName ] of
      [] -> putStrLn $ "\nRule test error: rule "++show ruleName++" not found."
      (rule:_) -> do { putStrLn $ "\nContents of rule "++show ruleName++ ": "++showA (formalExpression rule)
                     ; putStrLn $ showContents rule
                     ; let rExpr = formalExpression rule
                     ; let ruleComplement = rule { formalExpression = notCpl (EBrk rExpr) }
                     ; putStrLn $ "\nViolations of "++show ruleName++" (contents of "++showA (formalExpression ruleComplement)++"):"
                     ; putStrLn $ showContents ruleComplement
                     }
    where showContents rule = "[" ++ intercalate ", " pairs ++ "]"
            where pairs = [ "("++(show.showValADL.apLeft) v++"," ++(show.showValADL.apRight) v++")" 
                          | (r,vs) <- allViolations fSpec, r == rule, v <- Set.elems vs]
   