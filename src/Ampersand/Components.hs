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
generateAmpersandOutput :: Options -> FSpecKinds -> RIO App ()
generateAmpersandOutput opts@Options{..} fsKinds = do
    verboseLn "Checking for rule violations..."
    if dataAnalysis 
    then verboseLn "Not checking for rule violations because of data analysis." 
    else do
      reportInvViolations $ violationsOfInvariants (plainFSpec fsKinds)
      reportSignals (initialConjunctSignals $ plainProto fsKinds)
    liftIO $ createDirectoryIfMissing True dirOutput
    sequence_ . map snd . filter fst $ conditionalActions
  where 
   conditionalActions :: [(Bool, RIO App ())]
   conditionalActions = 
      [ ( genUML                , doGenUML      (plainFSpec fsKinds))
      , ( haskell               , doGenHaskell  (plainFSpec fsKinds))
      , ( sqlDump               , doGenSQLdump  (plainProto fsKinds))
      , ( export2adl            , doGenADL      (plainFSpec fsKinds))
      , ( dataAnalysis          , doGenADL      (plainFSpec fsKinds))
      , ( genFSpec              , doGenDocument (plainFSpec fsKinds))
      , ( genFPAExcel           , doGenFPAExcel (plainFSpec fsKinds))
      , ( genPOPExcel           , doGenPopsXLSX (plainFSpec fsKinds))
      , ( proofs                , doGenProofs   (plainFSpec fsKinds))
      , ( validateSQL           , doValidateSQL (plainProto fsKinds))
      , ( genPrototype          , doGenProto    (plainProto fsKinds))
      , ( genRapPopulationOnly  , doGenRapPopulation (rapPopulation fsKinds))
      , ( isJust testRule       , ruleTest (plainProto fsKinds) . fromJust $ testRule )
      ]
     
   -- | For importing and analysing data, Ampersand allows you to annotate an Excel spreadsheet (.xlsx) and turn it into an Ampersand model.
   -- By default 'doGenADL' exports the model to Export.adl, ready to be picked up by the user and refined by adding rules.
   -- 1. To analyze data in a spreadsheet, prepare your spreadsheet, foo.xlsx,  and run "Ampersand --dataAnalysis foo.xlsx".
   --    Expect to find a file "MetaModel.adl" in your working directory upon successful termination.
   -- 2. To perform a round-trip test, use an Ampersand-script foo.adl and run and run "Ampersand --export foo.adl".
   --    Expect to find a file "Export.adl" in your working directory which should be semantically equivalent to foo.adl.
   doGenADL :: FSpec -> RIO App ()
   doGenADL fSpec = do
       verboseLn $ "Generating Ampersand script (ADL) for "  ++ name fSpec ++ "..."
       liftIO $ writeFile outputFile (showA ctx) 
       verboseLn $ ".adl-file written to " ++ outputFile ++ "."
    where outputFile = dirOutput </> outputfile
          ctx = originalContext fSpec
 
   doGenProofs :: FSpec -> RIO App ()
   doGenProofs fSpec = do 
       putStrLn $ "Generating Proof for " ++ name fSpec ++ " into " ++ outputFile ++ "..."
       content <- liftIO $ (runIO (writeHtml5String def thePandoc)) >>= handleError
       writeFileUtf8 outputFile content
       verboseLn "Proof written."
    where outputFile = dirOutput </> "proofs_of_"++baseName -<.> ".html"
          thePandoc = setTitle title (doc theDoc)
          title  = text $ "Proofs for "++name fSpec
          theDoc = fDeriveProofs fSpec
          --theDoc = plain (text "Aap")  -- use for testing...

   doGenHaskell :: FSpec -> RIO App ()
   doGenHaskell fSpec = do
       putStrLn $ "Generating Haskell source code for " ++ name fSpec ++ "..."
       writeFileUtf8 outputFile (T.pack $ fSpec2Haskell opts fSpec)
       verboseLn ("Haskell written into " ++ outputFile ++ ".")
    where outputFile = dirOutput </> baseName -<.> ".hs"

   doGenSQLdump :: FSpec -> RIO App ()
   doGenSQLdump fSpec = do
       putStrLn $ "Generating SQL queries dumpfile for " ++ name fSpec ++ "..."
       writeFileUtf8 outputFile (dumpSQLqueries opts fSpec)
       verboseLn ("SQL queries dumpfile written into " ++ outputFile ++ ".")
    where outputFile = dirOutput </> baseName ++ "_dump" -<.> ".sql"
   
   doGenUML :: FSpec -> RIO App ()
   doGenUML fSpec = do
       putStrLn "Generating UML..."
       liftIO . writeFile outputFile $ generateUML fSpec
       verboseLn ("Generated file: " ++ outputFile ++ ".")
      where outputFile = dirOutput </> baseName -<.> ".xmi"

   -- This function will generate all Pictures for a given FSpec.
   -- the returned FSpec contains the details about the Pictures, so they
   -- can be referenced while rendering the FSpec.
   -- This function generates a pandoc document, possibly with pictures from an fSpec.
   doGenDocument :: FSpec -> RIO App ()
   doGenDocument fSpec = do
       putStrLn $ "Generating functional design document for " ++ name fSpec ++ "..."
       -- First we need to output the pictures, because they should be present 
       -- before the actual document is written
       when (not(noGraphics) && fspecFormat /=FPandoc) $
         mapM_ writePicture (reverse thePictures) -- NOTE: reverse is used to have the datamodels generated first. This is not required, but it is handy.
       writepandoc fSpec thePandoc
     where (thePandoc,thePictures) = fSpec2Pandoc opts fSpec
        

   -- | This function will generate an Excel workbook file, containing an extract from the FSpec
   doGenFPAExcel :: FSpec -> RIO App ()
   doGenFPAExcel _ =
     putStrLn "Sorry, FPA analisys is discontinued. It needs maintenance." -- See https://github.com/AmpersandTarski/Ampersand/issues/621
     --  ; writeFile outputFile $ fspec2FPA_Excel fSpec
    
--      where outputFile = dirOutput </> "FPA_"++baseName -<.> ".xml"  -- Do not use .xls here, because that generated document contains xml.

   doGenPopsXLSX :: FSpec -> RIO App ()
   doGenPopsXLSX fSpec = do
       putStrLn "Generating .xlsx file containing the population..."
       ct <- liftIO $ runIO getPOSIXTime >>= handleError
       BL.writeFile outputFile $ fSpec2PopulationXlsx ct fSpec
       verboseLn ("Generated file: " ++ outputFile)
     where outputFile = dirOutput </> baseName ++ "_generated_pop" -<.> ".xlsx"

   doValidateSQL :: FSpec -> RIO App ()
   doValidateSQL fSpec = do
       putStrLn "Validating SQL expressions..."
       errMsg <- validateRulesSQL fSpec
       unless (null errMsg) (exitWith $ InvalidSQLExpression errMsg)

   doGenProto :: FSpec -> RIO App ()
   doGenProto fSpec =
     if null (violationsOfInvariants $ plainFSpec fsKinds)|| allowInvariantViolations
     then if genRapPopulationOnly then fatal $ "A prototype cannot be generated while only asking for RAP population."
          else sequence_ $
          [ putStrLn "Generating prototype..."
          , liftIO $ createDirectoryIfMissing True dirPrototype
          , doGenFrontend fSpec
          , generateDatabaseFile fSpec
          , generateJSONfiles fSpec
          , verboseLn $ "Prototype files have been written to " ++ dirPrototype
          ]
     else do exitWith NoPrototypeBecauseOfRuleViolations

   doGenRapPopulation :: FSpec -> RIO App ()
   doGenRapPopulation fSpec =
     if null (violationsOfInvariants fSpec) || allowInvariantViolations
     then sequence_ $
          [ putStrLn "Generating RAP population..."
          , liftIO $ createDirectoryIfMissing True dirPrototype
          , generateJSONfilesRap fSpec 
          , verboseLn $ "RAP population file has been written to " ++ dirPrototype
          ]
     else do exitWith NoPrototypeBecauseOfRuleViolations

   violationsOfInvariants :: FSpec -> [(Rule,AAtomPairs)]
   violationsOfInvariants fSpec = 
     [(r,vs) |(r,vs) <- allViolations fSpec
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
     if allowInvariantViolations && not verboseP
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
     if verboseP
     then
       verboseLn $ "Signals for initial population:\n" ++ L.intercalate "\n"
         [   "Rule(s): "++(show . map name . NEL.toList . rc_orgRules) conj
         ++"\n  Conjunct   : " ++ showA (rc_conjunct conj)
         ++"\n  Violations : " ++ showprs viols
         | (conj, viols) <- conjViols
         ]
     else
       putStrLn "There are signals for the initial population. Use --verbose to output the violations"
   ruleTest :: FSpec -> String -> RIO App ()
   ruleTest fSpec ruleName =
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
   