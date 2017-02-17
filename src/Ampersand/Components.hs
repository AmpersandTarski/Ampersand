-- | This module contains the building blocks that are available in the Ampersand Library. These building blocks will be described further at [ampersand.sourceforge.net |the wiki pages of our project].
--
module Ampersand.Components
  ( -- * Type checking and calculus
     makeFSpec
    -- * Generators of output
   , generateAmpersandOutput
  )
where
import Prelude hiding (putStr,putStrLn,readFile,writeFile)
import Ampersand.Misc
import Text.Pandoc
import Text.Pandoc.Builder
import Ampersand.Basics
import Ampersand.FSpec
import Ampersand.FSpec.GenerateUML
import Ampersand.Graphic.Graphics (writePicture)
import Ampersand.Output
import Control.Monad
import System.FilePath
import System.Directory
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy as L
import Data.List
import qualified Data.Text.IO as Text
import Data.Function (on)
import Data.Maybe (maybeToList)
import Ampersand.Prototype.WriteStaticFiles   (writeStaticFiles)
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ShowAStruct
import Ampersand.Prototype.GenBericht  (doGenBericht)
import Ampersand.Prototype.ValidateSQL (validateRulesSQL)
import Ampersand.Prototype.GenFrontend (doGenFrontend, clearTemplateDirs)
import Ampersand.Prototype.ProtoUtil   (installComposerLibs)

--  | The FSpec is the datastructure that contains everything to generate the output. This monadic function
--    takes the FSpec as its input, and spits out everything the user requested.
generateAmpersandOutput :: MultiFSpecs -> IO ()
generateAmpersandOutput multi = do
   createDirectoryIfMissing True (dirOutput opts)
   when (genPrototype opts)
        (createDirectoryIfMissing True (dirPrototype opts))
   mapM_ doWhen conditionalActions
  where 
   doWhen :: (Options -> Bool, IO ()) -> IO()
   doWhen (b,x) = when (b opts) x
   conditionalActions :: [(Options -> Bool, IO())]
   conditionalActions = 
      [ ( genSampleConfigFile , doGenSampleConfigFile) 
      , ( genUML      , doGenUML           )
      , ( haskell     , doGenHaskell       )
      , ( sqlDump     , doGenSQLdump       )
      , ( export2adl  , doGenADL           )
      , ( genFSpec    , doGenDocument      )
      , ( genFPAExcel , doGenFPAExcel      )
      , ( genPOPExcel , doGenPopsXLSX      )
      , ( proofs      , doGenProofs        )
      , ( validateSQL , doValidateSQLTest  )
      , ( genPrototype, doGenProto         )
      , ( genBericht  , doGenBericht fSpec )
      , ( const True  , putStrLn "Finished processing your model.")]
   opts = getOpts fSpec
   fSpec = userFSpec multi
   doGenADL :: IO()
   doGenADL =
    do { writeFile outputFile . showA . originalContext $ fSpec
       ; verboseLn opts $ ".adl-file written to " ++ outputFile ++ "."
       }
    where outputFile = dirOutput opts </> outputfile opts
   doGenSampleConfigFile :: IO()
   doGenSampleConfigFile = writeConfigFile
   doGenProofs :: IO()
   doGenProofs =
    do { verboseLn opts $ "Generating Proof for " ++ name fSpec ++ " into " ++ outputFile ++ "."
   --  ; verboseLn opts $ writeTextile def thePandoc
       ; writeFile outputFile $ writeHtmlString def thePandoc
       ; verboseLn opts "Proof written."
       }
    where outputFile = dirOutput opts </> "proofs_of_"++baseName opts -<.> ".html"
          thePandoc = setTitle title (doc theDoc)
          title  = text $ "Proofs for "++name fSpec
          theDoc = fDeriveProofs fSpec
          --theDoc = plain (text "Aap")  -- use for testing...

   doGenHaskell :: IO()
   doGenHaskell =
    do { verboseLn opts $ "Generating Haskell source code for "++name fSpec
   --  ; verboseLn opts $ fSpec2Haskell fSpec -- switch this on to display the contents of Installer.php on the command line. May be useful for debugging.
       ; writeFile outputFile (fSpec2Haskell fSpec)
       ; verboseLn opts $ "Haskell written into " ++ outputFile ++ "."
       }
    where outputFile = dirOutput opts </> baseName opts -<.> ".hs"
   doGenSQLdump :: IO()
   doGenSQLdump =
    do { verboseLn opts $ "Generating SQL queries dumpfile for "++name fSpec
       ; Text.writeFile outputFile (dumpSQLqueries multi)
       ; verboseLn opts $ "SQL queries dumpfile written into " ++ outputFile ++ "."
       }
    where outputFile = dirOutput opts </> baseName opts -<.> ".sqlDump"

   doGenUML :: IO()
   doGenUML =
    do { verboseLn opts "Generating UML..."
       ; writeFile outputFile $ generateUML fSpec
       ; verboseLn opts $ "Generated file: " ++ outputFile ++ "."
       }
      where outputFile = dirOutput opts </> baseName opts -<.> ".xmi"

   -- This function will generate all Pictures for a given FSpec.
   -- the returned FSpec contains the details about the Pictures, so they
   -- can be referenced while rendering the FSpec.
   -- This function generates a pandoc document, possibly with pictures from an fSpec.
   doGenDocument :: IO()
   doGenDocument =
    do { verboseLn opts ("Processing "++name fSpec)
       ; -- First we need to output the pictures, because they should be present before the actual document is written
         when (not(null thePictures) && fspecFormat opts/=FPandoc) $
           mapM_ (writePicture opts) (reverse thePictures) -- NOTE: reverse is used to have the datamodels generated first. This is not required, but it is handy.
       ; writepandoc fSpec thePandoc
       }
     where (thePandoc,thePictures) = fSpec2Pandoc fSpec
        

   -- | This function will generate an Excel workbook file, containing an extract from the FSpec
   doGenFPAExcel :: IO()
   doGenFPAExcel =
     verboseLn opts "FPA analisys is discontinued. (It needs maintenance). Sorry. " -- See https://github.com/AmpersandTarski/Ampersand/issues/621
     --  ; writeFile outputFile $ fspec2FPA_Excel fSpec
    
--      where outputFile = dirOutput opts </> "FPA_"++baseName opts -<.> ".xml"  -- Do not use .xls here, because that generated document contains xml.

   doGenPopsXLSX :: IO()
   doGenPopsXLSX =
    do { verboseLn opts "Generating .xlsx file containing the population "
       ; ct <- getPOSIXTime 
       ; L.writeFile outputFile $ fSpec2PopulationXlsx ct fSpec
       ; verboseLn opts $ "Generated file: " ++ outputFile
       }
      where outputFile = dirOutput opts </> baseName opts ++ "_generated_pop" -<.> ".xlsx"

   doValidateSQLTest :: IO ()
   doValidateSQLTest =
    do { verboseLn opts "Validating SQL expressions..."
       ; errMsg <- validateRulesSQL fSpec
       ; unless (null errMsg) (exitWith $ InvalidSQLExpression errMsg)
       }

   doGenProto :: IO ()
   doGenProto =
    sequence_ $
       [ verboseLn opts "Checking on rule violations..."
       , reportViolations violationsOfInvariants
       , reportSignals (initialConjunctSignals fSpec)
       ]++
       (if null violationsOfInvariants || development opts
        then if genRap
             then [ generateJSONfiles multi]
             else [ verboseLn opts "Generating prototype..."
                  , clearTemplateDirs fSpec
                  , writeStaticFiles opts
                  , generateJSONfiles multi
                  , doGenFrontend fSpec
                  , verboseLn opts "\n"
                  , verboseLn opts $ "Prototype files have been written to " ++ dirPrototype opts
                  , installComposerLibs opts
                  ]
        else [exitWith NoPrototypeBecauseOfRuleViolations]
       )++
       maybeToList (fmap ruleTest (testRule opts))

    where genRap = genRapPopulationOnly (getOpts fSpec)
          violationsOfInvariants :: [(Rule,[AAtomPair])]
          violationsOfInvariants
            = [(r,vs) |(r,vs) <- allViolations fSpec
                      , not (isSignal r)
              ]
          reportViolations :: [(Rule,[AAtomPair])] -> IO()
          reportViolations []    = verboseLn opts "No violations found."
          reportViolations viols =
            let ruleNamesAndViolStrings = [ (name r, showprs p) | (r,p) <- viols ]
            in  putStrLn $ 
                         intercalate "\n"
                             [ "Violations of rule "++show r++":\n"++ concatMap (\(_,p) -> "- "++ p ++"\n") rps
                             | rps@((r,_):_) <- groupBy (on (==) fst) $ sort ruleNamesAndViolStrings
                             ]

          showprs :: [AAtomPair] -> String
          showprs aprs = "["++intercalate ", " (map showA aprs)++"]"
   --       showpr :: AAtomPair -> String
   --       showpr apr = "( "++(showVal.apLeft) apr++", "++(showVal.apRight) apr++" )"
          reportSignals []        = verboseLn opts "No signals for the initial population."
          reportSignals conjViols = verboseLn opts $ "Signals for initial population:\n" ++ intercalate "\n"
            [   "Rule(s): "++(show . map name . rc_orgRules) conj
            ++"\n  Conjunct   : " ++ showA (rc_conjunct conj)
            ++"\n  Violations : " ++ showprs viols
            | (conj, viols) <- conjViols
            ]
          ruleTest :: String -> IO ()
          ruleTest ruleName =
           case [ rule | rule <- grules fSpec ++ vrules fSpec, name rule == ruleName ] of
             [] -> putStrLn $ "\nRule test error: rule "++show ruleName++" not found."
             (rule:_) -> do { putStrLn $ "\nContents of rule "++show ruleName++ ": "++showA (rrexp rule)
                            ; putStrLn $ showContents rule
                            ; let rExpr = rrexp rule
                            ; let ruleComplement = rule { rrexp = notCpl (EBrk rExpr) }
                            ; putStrLn $ "\nViolations of "++show ruleName++" (contents of "++showA (rrexp ruleComplement)++"):"
                            ; putStrLn $ showContents ruleComplement
                            }
           where showContents rule = "[" ++ intercalate ", " pairs ++ "]"
                   where pairs = [ "("++(show.showValADL.apLeft) v++"," ++(show.showValADL.apRight) v++")" 
                                 | (r,vs) <- allViolations fSpec, r == rule, v <- vs]
                               
   
   