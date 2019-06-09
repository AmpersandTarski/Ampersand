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
import qualified RIO.ByteString.Lazy as BL
import           Data.Function (on)
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NEL
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           Data.Maybe (isJust, fromJust)
import           System.Directory
import           System.FilePath
import           Text.Pandoc
import           Text.Pandoc.Builder

--  | The FSpec is the datastructure that contains everything to generate the output. This monadic function
--    takes the FSpec as its input, and spits out everything the user requested.
generateAmpersandOutput :: Options -> MultiFSpecs -> RIO App ()
generateAmpersandOutput opts@Options{..} multi = do
    verboseLn "Checking for rule violations..."
    if dataAnalysis then verboseLn "Not checking for rule violations because of data analysis." else reportInvViolations violationsOfInvariants
    reportSignals (initialConjunctSignals fSpec)
    liftIO $ createDirectoryIfMissing True dirOutput
    sequence_ . map snd . filter fst $ conditionalActions
  where 
   conditionalActions :: [(Bool, RIO App ())]
   conditionalActions = 
      [ ( genUML                , doGenUML              )
      , ( haskell               , doGenHaskell          )
      , ( sqlDump               , doGenSQLdump          )
      , ( export2adl            , doGenADL              )
      , ( dataAnalysis          , doGenADL              )
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

   -- | For importing and analysing data, Ampersand allows you to annotate an Excel spreadsheet (.xlsx) and turn it into an Ampersand model.
   -- By default 'doGenADL' exports the model to Export.adl, ready to be picked up by the user and refined by adding rules.
   -- 1. To analyze data in a spreadsheet, prepare your spreadsheet, foo.xlsx,  and run "Ampersand --dataAnalysis foo.xlsx".
   --    Expect to find a file "MetaModel.adl" in your working directory upon successful termination.
   -- 2. To perform a round-trip test, use an Ampersand-script foo.adl and run and run "Ampersand --export foo.adl".
   --    Expect to find a file "Export.adl" in your working directory which should be semantically equivalent to foo.adl.
   doGenADL :: RIO App ()
   doGenADL = do
       verboseLn $ "Generating Ampersand script (ADL) for "  ++ name fSpec ++ "..."
       liftIO $ writeFile outputFile (showA ctx) 
       verboseLn $ ".adl-file written to " ++ outputFile ++ "."
    where outputFile = dirOutput </> outputfile
          ctx = originalContext fSpec
 
   doGenProofs :: RIO App ()
   doGenProofs = do 
       putStrLn $ "Generating Proof for " ++ name fSpec ++ " into " ++ outputFile ++ "..."
       content <- liftIO $ (runIO (writeHtml5String def thePandoc)) >>= handleError
       writeFileUtf8 outputFile content
       verboseLn "Proof written."
    where outputFile = dirOutput </> "proofs_of_"++baseName -<.> ".html"
          thePandoc = setTitle title (doc theDoc)
          title  = text $ "Proofs for "++name fSpec
          theDoc = fDeriveProofs fSpec
          --theDoc = plain (text "Aap")  -- use for testing...

   doGenHaskell :: RIO App ()
   doGenHaskell = do
       putStrLn $ "Generating Haskell source code for " ++ name fSpec ++ "..."
       writeFileUtf8 outputFile (T.pack $ fSpec2Haskell opts fSpec)
       verboseLn ("Haskell written into " ++ outputFile ++ ".")
    where outputFile = dirOutput </> baseName -<.> ".hs"

   doGenSQLdump :: RIO App ()
   doGenSQLdump = do
       putStrLn $ "Generating SQL queries dumpfile for " ++ name fSpec ++ "..."
       writeFileUtf8 outputFile (dumpSQLqueries opts multi)
       verboseLn ("SQL queries dumpfile written into " ++ outputFile ++ ".")
    where outputFile = dirOutput </> baseName ++ "_dump" -<.> ".sql"
   
   doGenUML :: RIO App ()
   doGenUML = do
       putStrLn "Generating UML..."
       liftIO . writeFile outputFile $ generateUML fSpec
       verboseLn ("Generated file: " ++ outputFile ++ ".")
      where outputFile = dirOutput </> baseName -<.> ".xmi"

   -- This function will generate all Pictures for a given FSpec.
   -- the returned FSpec contains the details about the Pictures, so they
   -- can be referenced while rendering the FSpec.
   -- This function generates a pandoc document, possibly with pictures from an fSpec.
   doGenDocument :: RIO App ()
   doGenDocument = do
       putStrLn $ "Generating functional design document for " ++ name fSpec ++ "..."
       -- First we need to output the pictures, because they should be present 
       -- before the actual document is written
       when (not(noGraphics) && fspecFormat /=FPandoc) $
         mapM_ writePicture (reverse thePictures) -- NOTE: reverse is used to have the datamodels generated first. This is not required, but it is handy.
       writepandoc fSpec thePandoc
     where (thePandoc,thePictures) = fSpec2Pandoc opts fSpec
        

   -- | This function will generate an Excel workbook file, containing an extract from the FSpec
   doGenFPAExcel :: RIO App ()
   doGenFPAExcel =
     putStrLn "Sorry, FPA analisys is discontinued. It needs maintenance." -- See https://github.com/AmpersandTarski/Ampersand/issues/621
     --  ; writeFile outputFile $ fspec2FPA_Excel fSpec
    
--      where outputFile = dirOutput </> "FPA_"++baseName -<.> ".xml"  -- Do not use .xls here, because that generated document contains xml.

   doGenPopsXLSX :: RIO App ()
   doGenPopsXLSX = do
       putStrLn "Generating .xlsx file containing the population..."
       ct <- liftIO $ runIO getPOSIXTime >>= handleError
       BL.writeFile outputFile $ fSpec2PopulationXlsx ct fSpec
       verboseLn ("Generated file: " ++ outputFile)
     where outputFile = dirOutput </> baseName ++ "_generated_pop" -<.> ".xlsx"

   doValidateSQLTest :: RIO App ()
   doValidateSQLTest = do
       putStrLn "Validating SQL expressions..."
       errMsg <- validateRulesSQL fSpec
       unless (null errMsg) (exitWith $ InvalidSQLExpression errMsg)

   doGenProto :: RIO App ()
   doGenProto =
     if null violationsOfInvariants || allowInvariantViolations
     then sequence_ $
          [ putStrLn "Generating prototype..."
          , liftIO $ createDirectoryIfMissing True dirPrototype
          , doGenFrontend fSpec
          , generateDatabaseFile multi
          , generateJSONfiles multi
          , verboseLn $ "Prototype files have been written to " ++ dirPrototype
          ]
     else do exitWith NoPrototypeBecauseOfRuleViolations

   doGenRapPopulation :: RIO App ()
   doGenRapPopulation =
     if null violationsOfInvariants || allowInvariantViolations
     then sequence_ $
          [ putStrLn "Generating RAP population..."
          , liftIO $ createDirectoryIfMissing True dirPrototype
          , generateJSONfiles multi
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

   reportInvViolations :: [(Rule,AAtomPairs)] -> RIO App ()
   reportInvViolations []    = verboseLn $ "No invariant violations found for the initial population"
   reportInvViolations viols =
     if allowInvariantViolations && verbosity == Silent
     then
       -- TODO: this is a nice use case for outputting warnings
       putStrLn "There are invariant violations that are ignored. Use --verbose to output the violations"
     else
       let ruleNamesAndViolStrings = [ (name r, showprs p) | (r,p) <- viols ]
       in  putStrLn $ 
                  L.intercalate "\n"
                      [ "Violations of rule "++show r++":\n"++ concatMap (\(_,p) -> "- "++ p ++"\n") rps
                      | rps@((r,_):_) <- L.groupBy (on (==) fst) $ L.sort ruleNamesAndViolStrings
                      ]
   
   showprs :: AAtomPairs -> String
   showprs aprs = "["++L.intercalate ", " (Set.elems $ Set.map showA aprs)++"]"
   -- showpr :: AAtomPair -> String
   -- showpr apr = "( "++(showVal.apLeft) apr++", "++(showVal.apRight) apr++" )"
   reportSignals []        = verboseLn "No signals for the initial population" 
   reportSignals conjViols = 
     if verbosity == Loud
     then
       verboseLn $ "Signals for initial population:\n" ++ L.intercalate "\n"
         [   "Rule(s): "++(show . map name . NEL.toList . rc_orgRules) conj
         ++"\n  Conjunct   : " ++ showA (rc_conjunct conj)
         ++"\n  Violations : " ++ showprs viols
         | (conj, viols) <- conjViols
         ]
     else
       putStrLn "There are signals for the initial population. Use --verbose to output the violations"
   ruleTest :: String -> RIO App ()
   ruleTest ruleName =
    case [ rule | rule <- Set.elems $ grules fSpec `Set.union` vrules fSpec, name rule == ruleName ] of
      [] -> putStrLn $ "\nRule test error: rule "++show ruleName++" not found."
      (rule:_) -> do 
            putStrLn $ "\nContents of rule "++show ruleName++ ": "++showA (formalExpression rule)
            putStrLn $ showContents rule
            let rExpr = formalExpression rule
                ruleComplement = rule { formalExpression = notCpl (EBrk rExpr) }
            putStrLn $ "\nViolations of "++show ruleName++" (contents of "++showA (formalExpression ruleComplement)++"):"
            putStrLn $ showContents ruleComplement
    where showContents rule = "[" ++ L.intercalate ", " pairs ++ "]"
            where pairs = [ "("++(show.showValADL.apLeft) v++"," ++(show.showValADL.apRight) v++")" 
                          | (r,vs) <- allViolations fSpec, r == rule, v <- Set.elems vs]
   