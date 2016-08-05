-- | This module contains the building blocks that are available in the Ampersand Library. These building blocks will be described further at [ampersand.sourceforge.net |the wiki pages of our project].
--
module Database.Design.Ampersand.Components
  ( -- * Type checking and calculus
     makeFSpec
    -- * Generators of output
   , generateAmpersandOutput
  )
where
import Prelude hiding (putStr,readFile,writeFile)
import Database.Design.Ampersand.Misc
import Text.Pandoc
import Text.Pandoc.Builder
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.FSpec.GenerateUML
import Database.Design.Ampersand.Graphic.Graphics (writePicture)
import Database.Design.Ampersand.Output
import Control.Monad
import System.FilePath
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy as L
import Data.List
import qualified Data.Text.IO as Text
import Data.Function (on)

import System.Exit
import Database.Design.Ampersand.Output.ToJSON.ToJson  (generateJSONfiles)
import Database.Design.Ampersand.Prototype.WriteStaticFiles   (writeStaticFiles)
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Prototype.GenBericht  (doGenBericht)
import Database.Design.Ampersand.Prototype.ValidateSQL (validateRulesSQL)
import Database.Design.Ampersand.Prototype.GenFrontend (doGenFrontend, clearTemplateDirs)
import Database.Design.Ampersand.Prototype.ProtoUtil   (installComposerLibs)

--  | The FSpec is the datastructure that contains everything to generate the output. This monadic function
--    takes the FSpec as its input, and spits out everything the user requested.
generateAmpersandOutput :: FSpec -> IO ()
generateAmpersandOutput fSpec =
 do { when (genUML (getOpts fSpec))      $ doGenUML      fSpec
    ; when (haskell (getOpts fSpec))     $ doGenHaskell  fSpec
    ; when (sqlDump (getOpts fSpec))     $ doGenSQLdump  fSpec
    ; when (export2adl (getOpts fSpec))  $ doGenADL      fSpec
    ; when (genFSpec (getOpts fSpec))    $ doGenDocument fSpec
    ; when (genFPAExcel (getOpts fSpec)) $ doGenFPAExcel fSpec
    ; when (genPOPExcel (getOpts fSpec)) $ doGenPopsXLSX fSpec
    ; when (proofs (getOpts fSpec))      $ doGenProofs   fSpec
    ; when (validateSQL (getOpts fSpec)) $ doValidateSQLTest fSpec
    ; when (genPrototype (getOpts fSpec)) $ doGenProto   fSpec
    ; when (genBericht (getOpts fSpec))  $ doGenBericht fSpec
    }

doGenADL :: FSpec -> IO()
doGenADL fSpec =
 do { writeFile outputFile . showADL . originalContext $ fSpec
    ; verboseLn (getOpts fSpec) $ ".adl-file written to " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput (getOpts fSpec)) (outputfile (getOpts fSpec))

doGenProofs :: FSpec -> IO()
doGenProofs fSpec =
 do { verboseLn (getOpts fSpec) $ "Generating Proof for " ++ name fSpec ++ " into " ++ outputFile ++ "."
--  ; verboseLn (getOpts fSpec) $ writeTextile def thePandoc
    ; writeFile outputFile $ writeHtmlString def thePandoc
    ; verboseLn (getOpts fSpec) "Proof written."
    }
 where outputFile = combine (dirOutput (getOpts fSpec)) $ replaceExtension ("proofs_of_"++baseName (getOpts fSpec)) ".html"
       thePandoc = setTitle title (doc theDoc)
       title  = text $ "Proofs for "++name fSpec
       theDoc = fDeriveProofs fSpec
       --theDoc = plain (text "Aap")  -- use for testing...

doGenHaskell :: FSpec -> IO()
doGenHaskell fSpec =
 do { verboseLn (getOpts fSpec) $ "Generating Haskell source code for "++name fSpec
--  ; verboseLn (getOpts fSpec) $ fSpec2Haskell fSpec -- switch this on to display the contents of Installer.php on the command line. May be useful for debugging.
    ; writeFile outputFile (fSpec2Haskell fSpec)
    ; verboseLn (getOpts fSpec) $ "Haskell written into " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput (getOpts fSpec)) $ replaceExtension (baseName (getOpts fSpec)) ".hs"
doGenSQLdump :: FSpec -> IO()
doGenSQLdump fSpec =
 do { verboseLn (getOpts fSpec) $ "Generating SQL queries dumpfile for "++name fSpec
    ; Text.writeFile outputFile (dumpSQLqueries fSpec)
    ; verboseLn (getOpts fSpec) $ "SQL queries dumpfile written into " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput (getOpts fSpec)) $ replaceExtension (baseName (getOpts fSpec)) ".sqlDump"

doGenUML :: FSpec -> IO()
doGenUML fSpec =
 do { verboseLn (getOpts fSpec) "Generating UML..."
    ; writeFile outputFile $ generateUML fSpec
    ; Prelude.putStrLn $ "Generated file: " ++ outputFile ++ "."
    }
   where outputFile = combine (dirOutput (getOpts fSpec)) $ replaceExtension (baseName (getOpts fSpec)) ".xmi"

-- This function will generate all Pictures for a given FSpec.
-- the returned FSpec contains the details about the Pictures, so they
-- can be referenced while rendering the FSpec.
-- This function generates a pandoc document, possibly with pictures from an fSpec.
doGenDocument :: FSpec -> IO()
doGenDocument fSpec =
 do { verboseLn (getOpts fSpec) ("Processing "++name fSpec)
    ; -- First we need to output the pictures, because they should be present before the actual document is written
      when (not(null thePictures) && fspecFormat (getOpts fSpec)/=FPandoc) $
        mapM_ (writePicture (getOpts fSpec)) thePictures
    ; writepandoc fSpec thePandoc
    }
  where (thePandoc,thePictures) = fSpec2Pandoc fSpec
        

-- | This function will generate an Excel workbook file, containing an extract from the FSpec
doGenFPAExcel :: FSpec -> IO()
doGenFPAExcel fSpec =
 do { verboseLn (getOpts fSpec) "Generating Excel containing FPA..."
    ; writeFile outputFile $ fspec2FPA_Excel fSpec
    }
   where outputFile = combine (dirOutput (getOpts fSpec)) $ replaceExtension ("FPA_"++baseName (getOpts fSpec)) ".xml"  -- Do not use .xls here, because that generated document contains xml.

doGenPopsXLSX :: FSpec -> IO()
doGenPopsXLSX fSpec =
 do { verboseLn (getOpts fSpec) "Generating .xlsx file containing the population "
    ; ct <- getPOSIXTime 
    ; L.writeFile outputFile $ fSpec2PopulationXlsx ct fSpec
    ; Prelude.putStrLn $ "Generated file: " ++ outputFile
    }
   where outputFile = combine (dirOutput (getOpts fSpec)) $ replaceExtension (baseName (getOpts fSpec)++ "_generated_pop") ".xlsx"

doValidateSQLTest :: FSpec -> IO ()
doValidateSQLTest fSpec =
 do { verboseLn (getOpts fSpec) "Validating SQL expressions..."
    ; isValidRule <- validateRulesSQL fSpec
    ; unless isValidRule (exitWith (ExitFailure 30))
    }

doGenProto :: FSpec -> IO ()
doGenProto fSpec =
 do { verboseLn (getOpts fSpec) "Checking on rule violations..."
    ; reportViolations violationsOfInvariants
    ; reportSignals (initialConjunctSignals fSpec)
    ; if null violationsOfInvariants || development (getOpts fSpec)
      then do { verboseLn (getOpts fSpec) "Generating prototype..."
              ; clearTemplateDirs fSpec
              ; writeStaticFiles (getOpts fSpec)
              ; generateJSONfiles fSpec
              ; doGenFrontend fSpec
              ; verboseLn (getOpts fSpec) "\n"
              ; verboseLn (getOpts fSpec) $ "Prototype files have been written to " ++ dirPrototype (getOpts fSpec)
              ; installComposerLibs fSpec
              }
      else do { Prelude.putStrLn $ "\nERROR: No prototype generated because of rule violations."
                                 ++"\n(Compile with --dev to generate a prototype regardless of violations)"
              ; exitWith $ ExitFailure 40
              }
    ; case testRule (getOpts fSpec) of
             Just ruleName -> ruleTest ruleName
             Nothing       -> return ()
    }
 where violationsOfInvariants :: [(Rule,[AAtomPair])]
       violationsOfInvariants
         = [(r,vs) |(r,vs) <- allViolations fSpec
                   , not (isSignal r)
           ]
       reportViolations :: [(Rule,[AAtomPair])] -> IO()
       reportViolations []    = verboseLn (getOpts fSpec) "No violations found."
       reportViolations viols =
         let ruleNamesAndViolStrings = [ (name r, showprs p) | (r,p) <- viols ]
         in  Prelude.putStrLn $ 
                      intercalate "\n"
                          [ "Violations of rule "++show r++":\n"++ concatMap (\(_,p) -> "- "++ p ++"\n") rps
                          | rps@((r,_):_) <- groupBy (on (==) fst) $ sort ruleNamesAndViolStrings
                          ]

       showprs :: [AAtomPair] -> String
       showprs aprs = "["++intercalate ", " (map showADL aprs)++"]"
--       showpr :: AAtomPair -> String
--       showpr apr = "( "++(showVal.apLeft) apr++", "++(showVal.apRight) apr++" )"
       reportSignals []        = verboseLn (getOpts fSpec) "No signals for the initial population."
       reportSignals conjViols = verboseLn (getOpts fSpec) $ "Signals for initial population:\n" ++ intercalate "\n"
         [   "Rule(s): "++(show . map name . rc_orgRules) conj
         ++"\n  Conjunct   : " ++ showADL (rc_conjunct conj)
         ++"\n  Violations : " ++ showprs viols
         | (conj, viols) <- conjViols
         ]
       ruleTest :: String -> IO ()
       ruleTest ruleName =
        case [ rule | rule <- grules fSpec ++ vrules fSpec, name rule == ruleName ] of
          [] -> Prelude.putStrLn $ "\nRule test error: rule "++show ruleName++" not found."
          (rule:_) -> do { Prelude.putStrLn $ "\nContents of rule "++show ruleName++ ": "++showADL (rrexp rule)
                         ; Prelude.putStrLn $ showContents rule
                         ; let rExpr = rrexp rule
                         ; let ruleComplement = rule { rrexp = notCpl (EBrk rExpr) }
                         ; Prelude.putStrLn $ "\nViolations of "++show ruleName++" (contents of "++showADL (rrexp ruleComplement)++"):"
                         ; Prelude.putStrLn $ showContents ruleComplement
                         }
        where showContents rule = "[" ++ intercalate ", " pairs ++ "]"
                where pairs = [ "("++(show.showValADL.apLeft) v++"," ++(show.showValADL.apRight) v++")" 
                              | (r,vs) <- allViolations fSpec, r == rule, v <- vs]
                               
   
   