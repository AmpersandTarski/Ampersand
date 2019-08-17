{-# LANGUAGE RecordWildCards #-}
-- | This module contains the building blocks that are available in the Ampersand Library. These building blocks will be described further at [ampersand.sourceforge.net |the wiki pages of our project].
--
module Ampersand.Components
  ( -- * Type checking and calculus
--     makeFSpec
    -- * Generators of output
--   , generateAmpersandOutput
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
import           Ampersand.Prototype.ValidateSQL (validateRulesSQL)
import qualified RIO.ByteString.Lazy as BL
import           Data.Function (on)
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NEL
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           RIO.Time (getCurrentTime)
import           System.Directory
import           System.FilePath ((</>), (-<.>))
import           Text.Pandoc hiding (getCurrentTime)
import           Text.Pandoc.Builder
--import           Ampersand.Commands.Proto
import           Ampersand.Input.ADL1.CtxError(Guarded(..))

--  | The FSpec is the datastructure that contains everything to generate the output. This monadic function
--    takes the FSpec as its input, and spits out everything the user requested.
-- generateAmpersandOutput :: (HasBlackWhite env, HasEnvironment env, HasRunComposer env, HasDirCustomizations env, HasZwolleVersion env, HasProtoOpts env, HasAllowInvariantViolations env, HasDirPrototype env,HasOutputFile env, HasDirOutput env, HasDocumentOpts env, HasRootFile env, HasLogFunc env) 
--        => FSpec -> RIO env ()
-- generateAmpersandOutput fSpec = do
--     env <- ask 
--     dataAnalysis <- view dataAnalysisL
--     dirOutput <- view dirOutputL
--     sayWhenLoudLn "Checking for rule violations..."
--     if dataAnalysis then sayWhenLoudLn "Not checking for rule violations because of data analysis." else reportInvViolations (violationsOfInvariants fSpec)
--     reportSignals (initialConjunctSignals fSpec)
--     liftIO $ createDirectoryIfMissing True dirOutput
--     sequence_ . map snd . filter fst $ conditionalActions env
--   where 
--    conditionalActions :: (HasBlackWhite env, HasEnvironment env, HasRunComposer env, HasDirCustomizations env, HasZwolleVersion env, HasProtoOpts env, HasAllowInvariantViolations env, HasDirPrototype env,HasOutputFile env, HasDirOutput env, HasDocumentOpts env, HasRootFile env, HasLogFunc env) 
--            => env -> [(Bool, RIO env ())]
--    conditionalActions env = []
--       -- [ ( view genUMLL env            , doGenUML              )
--       -- , ( view genHaskellL env        , doGenHaskell          )
--       -- , ( view sqlDumpL env           , doGenSQLdump          )
--       -- , ( view export2adlL env        , doGenADL              )
--       -- , ( view dataAnalysisL env      , doGenADL              )
--       -- , ( view documentOptsL env          , doGenDocument         )
--       -- , ( view genFPAExcelL env       , doGenFPAExcel         )
--       -- , ( view genPOPExcelL env       , doGenPopsXLSX         )
--       -- , ( view proofsL env            , doGenProofs           )
--       -- , ( view validateSQLL env       , doValidateSQLTest     )
--       -- , ( view genPrototypeL env      , doGenProto            )
--       -- , ( view genRapPopulationL env  , doGenRapPopulation    )
--       -- ]

--    -- | For importing and analysing data, Ampersand allows you to annotate an Excel spreadsheet (.xlsx) and turn it into an Ampersand model.
--    -- By default 'doGenADL' exports the model to Export.adl, ready to be picked up by the user and refined by adding rules.
--    -- 1. To analyze data in a spreadsheet, prepare your spreadsheet, foo.xlsx,  and run "Ampersand --dataAnalysis foo.xlsx".
--    --    Expect to find a file "MetaModel.adl" in your working directory upon successful termination.
--    -- 2. To perform a round-trip test, use an Ampersand-script foo.adl and run and run "Ampersand --export foo.adl".
--    --    Expect to find a file "Export.adl" in your working directory which should be semantically equivalent to foo.adl.
--    doGenADL :: (HasOutputFile env, HasDirOutput env, HasLogFunc env) => RIO env ()
--    doGenADL = do
--        env <- ask
--        sayWhenLoudLn $ "Generating Ampersand script (ADL) for "  ++ name fSpec ++ "..."
--        liftIO $ writeFile (outputFile' env) (showA ctx) 
--        sayWhenLoudLn $ ".adl-file written to " ++ outputFile' env++ "."
--     where outputFile' env = view dirOutputL env </> view outputfileL env
--              where outputfileL 
--                      | view export2adlL env = outputfileAdlL
--                      | view dataAnalysisL env = outputfileDataAnalysisL
--                      | otherwise = fatal "outputfile not defined for this command."
--           ctx = originalContext fSpec
 
--    doGenProofs :: (HasDirOutput env, HasRootFile env, HasLogFunc env) => RIO env ()
--    doGenProofs = do 
--        env <- ask
--        sayLn $ "Generating Proof for " ++ name fSpec ++ " into " ++ outputFile env ++ "..."
--        content <- liftIO $ (runIO (writeHtml5String def thePandoc)) >>= handleError
--        writeFileUtf8 (outputFile env) content
--        sayWhenLoudLn "Proof written."
--     where outputFile env = view dirOutputL env </> "proofs_of_"++baseName env -<.> ".html"
--           thePandoc = setTitle title (doc theDoc)
--           title  = text $ "Proofs for "++name fSpec
--           theDoc = fDeriveProofs fSpec
--           --theDoc = plain (text "Aap")  -- use for testing...

--    doGenHaskell :: (HasDirOutput env, HasRootFile env, HasLogFunc env) => RIO env ()
--    doGenHaskell = do
--        env <- ask
--        now <- getCurrentTime
--        outputFile <- outputFile' <$> ask
--        sayLn $ "Generating Haskell source code for " ++ name fSpec ++ "..."
--        writeFileUtf8 outputFile (T.pack $ fSpec2Haskell env now fSpec)
--        sayWhenLoudLn ("Haskell written into " ++ outputFile ++ ".")
--     where outputFile' env = view dirOutputL env </> baseName env -<.> ".hs"

--    doGenSQLdump :: (HasDirOutput env, HasRootFile env, HasLogFunc env) => RIO env ()
--    doGenSQLdump = do
--        env <- ask
--        outputFile <- outputFile' <$> ask
--        sayLn $ "Generating SQL queries dumpfile for " ++ name fSpec ++ "..."
--        writeFileUtf8 outputFile (dumpSQLqueries env fSpec)
--        sayWhenLoudLn ("SQL queries dumpfile written into " ++ outputFile ++ ".")
--     where outputFile' env = view dirOutputL env </> baseName env ++ "_dump" -<.> ".sql"
   
--    doGenUML :: (HasDirOutput env, HasRootFile env, HasLogFunc env) => RIO env ()
--    doGenUML = do
--        outputFile <- outputFile' <$> ask
--        sayLn "Generating UML..."
--        liftIO . writeFile outputFile $ generateUML fSpec
--        sayWhenLoudLn ("Generated file: " ++ outputFile ++ ".")
--       where outputFile' env = view dirOutputL env </> baseName env -<.> ".xmi"


--    -- | This function will generate an Excel workbook file, containing an extract from the FSpec
--    doGenFPAExcel :: (HasLogFunc env) => RIO env ()
--    doGenFPAExcel =
--      sayLn "Sorry, FPA analysis is discontinued. It needs maintenance." -- See https://github.com/AmpersandTarski/Ampersand/issues/621
--      --  ; writeFile outputFile $ fspec2FPA_Excel fSpec
    
-- --      where outputFile = dirOutput </> "FPA_"++baseName -<.> ".xml"  -- Do not use .xls here, because that generated document contains xml.

--    doGenPopsXLSX :: (HasDirOutput env, HasRootFile env, HasLogFunc env) => RIO env ()
--    doGenPopsXLSX = do
--        outputFile <- outputFile' <$> ask
--        sayLn "Generating .xlsx file containing the population..."
--        ct <- liftIO $ runIO getPOSIXTime >>= handleError
--        BL.writeFile outputFile $ fSpec2PopulationXlsx ct fSpec
--        sayWhenLoudLn ("Generated file: " ++ outputFile)
--      where outputFile' env = view dirOutputL env </> baseName env ++ "_generated_pop" -<.> ".xlsx"

--    doValidateSQLTest :: (HasProtoOpts env, HasLogFunc env) => RIO env ()
--    doValidateSQLTest = do
--        sayLn "Validating SQL expressions..."
--        errMsg <- validateRulesSQL fSpec
--        unless (null errMsg) (exitWith $ InvalidSQLExpression errMsg)

--   --  doGenRapPopulation :: (HasEnvironment env, HasProtoOpts env, HasCommands env, HasLogFunc env, HasAllowInvariantViolations env, HasDirPrototype env) 
--   --       => Guarded (RIO env ())
--   --  doGenRapPopulation = do
--   --    dirPrototype <- view dirPrototypeL
--   --    allowInvariantViolations <- view allowInvariantViolationsL
--   --    let recipe :: [BuildStep]
--   --        recipe = []
--   --        aap :: RIO env (Guarded FSpec)
--   --        aap = createFspec recipe
--   --        wim :: RIO env (Guarded FSpec) -> (Guarded (Guarded FSpec))
--   --        wim x = undefined
--   --    gFSpec <- wim $ createFspec recipe
--   --    let noot :: Guarded FSpec
--   --        noot = gFSpec
--   --        mies :: Guarded (RIO env0 ())
--   --        mies = action <$> gFSpec
--   --    (action <$> gFSpec )
--   --    where
--   --       action :: FSpec -> RIO env ()
--   --       action fSpec = do 
--   --         if null (violationsOfInvariants fSpec) || allowInvariantViolations
--   --         then do
--   --             sayLn "Generating RAP population..."
--   --             liftIO $ createDirectoryIfMissing True dirPrototype
--   --             generateJSONfiles fSpec
--   --             sayWhenLoudLn $ "RAP population file has been written to " ++ dirPrototype
--   --         else do exitWith NoPrototypeBecauseOfRuleViolations

--    violationsOfInvariants :: FSpec -> [(Rule,AAtomPairs)]
--    violationsOfInvariants fSpec = 
--      [(r,vs) |(r,vs) <- allViolations fSpec
--              , not (isSignal r)
--        ]

--    reportInvViolations :: (HasAllowInvariantViolations env, HasLogFunc env) => [(Rule,AAtomPairs)] -> RIO env ()
--    reportInvViolations []    = sayWhenLoudLn $ "No invariant violations found for the initial population"
--    reportInvViolations viols = do
--      allowInvariantViolations <- view allowInvariantViolationsL
--      verbosity <- view verbosityL
--      if allowInvariantViolations && verbosity == Silent
--      then
--        -- TODO: this is a nice use case for outputting warnings
--        sayLn "There are invariant violations that are ignored. Use --verbose to output the violations"
--      else
--        let ruleNamesAndViolStrings = [ (name r, showprs p) | (r,p) <- viols ]
--        in  sayLn $ 
--                   L.intercalate "\n"
--                       [ "Violations of rule "++show r++":\n"++ concatMap (\(_,p) -> "- "++ p ++"\n") rps
--                       | rps@((r,_):_) <- L.groupBy (on (==) fst) $ L.sort ruleNamesAndViolStrings
--                       ]
   
--    showprs :: AAtomPairs -> String
--    showprs aprs = "["++L.intercalate ", " (Set.elems $ Set.map showA aprs)++"]"
--    -- showpr :: AAtomPair -> String
--    -- showpr apr = "( "++(showVal.apLeft) apr++", "++(showVal.apRight) apr++" )"
--    reportSignals []        = sayWhenLoudLn "No signals for the initial population" 
--    reportSignals conjViols = do
--      verbosity <- view verbosityL
--      if verbosity == Loud
--      then
--        sayWhenLoudLn $ "Signals for initial population:\n" ++ L.intercalate "\n"
--          [   "Rule(s): "++(show . map name . NEL.toList . rc_orgRules) conj
--          ++"\n  Conjunct   : " ++ showA (rc_conjunct conj)
--          ++"\n  Violations : " ++ showprs viols
--          | (conj, viols) <- conjViols
--          ]
--      else
--        sayLn "There are signals for the initial population. Use --verbose to output the violations"
   