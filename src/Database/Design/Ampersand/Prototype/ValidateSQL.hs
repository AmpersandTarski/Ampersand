module Database.Design.Ampersand.Prototype.ValidateSQL (validateRulesSQL)

where

import Data.List
import Data.Maybe
import Control.Monad
import System.Process
import System.Exit
import System.IO hiding (hPutStr,hGetContents)
import System.Directory
import Database.Design.Ampersand hiding (putStr, origin)
import Database.Design.Ampersand.Prototype.RelBinGenBasics
import Database.Design.Ampersand.Prototype.RelBinGenSQL
import Database.Design.Ampersand.Prototype.Installer
import Control.Exception
import Prelude hiding (exp)

{-
Validate the generated SQL for all rules in the fSpec, by comparing the evaluation results
with the results from Haskell-based Ampersand rule evaluator. The latter is much simpler and
therefore most likely to be correct in case of discrepancies.
-}

fatal :: Int -> String -> a
fatal = fatalMsg "ValidateSQL"

tempDbName :: String
tempDbName = "ampersand_TemporaryValidationDatabase"

validateRulesSQL :: FSpec -> IO Bool
validateRulesSQL fSpec =
 do { when (any (not.isSignal.fst) (allViolations fSpec))
        (do { putStrLn "The population would violate invariants. Could not generate your database."
            ; exitWith $ ExitFailure 10
                 })
    ; hSetBuffering stdout NoBuffering

    ; putStrLn "Initializing temporary database"
    ; createTempDatabase fSpec

    ; let allExps = getAllInterfaceExps fSpec ++
                    getAllRuleExps fSpec ++
                    getAllPairViewExps fSpec ++
                    getAllIdExps fSpec ++
                    getAllViewExps fSpec

    ; putStrLn $ "Number of expressions to be validated: "++show (length allExps)
    ; results <- mapM (validateExp fSpec) allExps

--    ; putStrLn "\nRemoving temporary database"
--    ; removeTempDatabase (flags fSpec)

    ; case [ ve | (ve, False) <- results] of
        [] -> do { putStrLn "\nValidation successful.\nWith the provided populations, all generated SQL code has passed validation."
                 ; return True
                 }
        ves -> do { putStrLn ( "\n\nValidation error. The following expressions failed validation:\n" ++
                               unlines (map showVExp ves)
                             )
                  ; return False
                  }
    }

-- functions for extracting all expressions from the context

getAllInterfaceExps :: FSpec -> [ValidationExp]
getAllInterfaceExps fSpec = concat [ getObjExps (name ifc) $ ifcObj ifc
                                   | ifc <- interfaceS fSpec ++ interfaceG fSpec ]
 where getObjExps iName objDef = (objctx objDef, "interface " ++ show iName) :
                                 concatMap (getObjExps iName) (attributes objDef)

-- we check the complement of the rule, since that is the expression evaluated in the prototype
getAllRuleExps :: FSpec -> [ValidationExp]
getAllRuleExps fSpec = map getRuleExp $ vrules fSpec ++ grules fSpec
 where getRuleExp rule = (notCpl (rrexp rule), "rule "++show (name rule))

getAllPairViewExps :: FSpec -> [ValidationExp]
getAllPairViewExps fSpec = concatMap getPairViewExps $ vrules fSpec ++ grules fSpec
 where getPairViewExps r@Ru{rrviol = Just (PairView pvsegs)} =
         [ (exp, "violation view for rule "++show (name r)) | PairViewExp _ exp <- pvsegs ]
       getPairViewExps _    = []

getAllIdExps :: FSpec -> [ValidationExp]
getAllIdExps fSpec = concatMap getIdExps $ vIndices fSpec
 where getIdExps identity = [ (objctx objDef, "identity "++show (name identity))
                            | IdentityExp objDef <- identityAts identity ]

getAllViewExps :: FSpec -> [ValidationExp]
getAllViewExps fSpec = concatMap getViewExps $ vviews fSpec
 where getViewExps view = [ (objctx objDef, "view "++show (name view))
                          | ViewExp objDef <- vdats view ]

type ValidationExp = (Expression, String)
-- a ValidationExp is an expression together with the place in the context where we
-- obtained it from (e.g. rule/interface/..)
showVExp :: ShowADL a => (a, String) -> String
showVExp (exp, origin) = "Origin: "++origin++", expression: "++showADL exp

-- validate a single expression and report the results
validateExp :: FSpec -> ValidationExp -> IO (ValidationExp, Bool)
validateExp _  vExp@(EDcD{}, _)   = -- skip all simple relations
 do { putStr "."
    ; return (vExp, True)
    }
validateExp fSpec vExp@(exp, origin) =
 do { --putStr $ "Checking "++origin ++": expression = "++showADL exp
    ; violationsSQL <- fmap sort . evaluateExpSQL fSpec$ exp
    ; let violationsAmp = sort [(srcPaire p, trgPaire p) | p <- fullContents (gens fSpec) (initialPops fSpec) exp]

    ; if violationsSQL == violationsAmp
      then
       do { putStr "." -- ++show violationsSQL
          ; return (vExp, True)
          }
      else
       do { putStr $ "Checking "++origin ++": expression = "++showADL exp
          ; putStrLn "\nMismatch between SQL and Ampersand"
          ; putStrLn $ showVExp vExp
          ; putStrLn $ "SQL violations:\n"++show violationsSQL
          ; putStrLn $ "Ampersand violations:\n" ++ show violationsAmp
          ; return (vExp, False)
          }
    }

-- evaluate normalized exp in SQL
evaluateExpSQL :: FSpec -> Expression -> IO [(String,String)]
evaluateExpSQL fSpec exp =
  fmap sort (performQuery (flags fSpec) violationsQuery)
 where violationsExpr = conjNF (flags fSpec) exp
       violationsQuery = selectExpr fSpec 26 "src" "tgt" violationsExpr

performQuery :: Options -> String -> IO [(String,String)]
performQuery opts queryStr =
 do { queryResult <- (executePHP . showPHP) php
    ; if "Error" `isPrefixOf` queryResult -- not the most elegant way, but safe since a correct result will always be a list
      then do verboseLn opts{verboseP=True} ("\n******Problematic query:\n"++queryStr++"\n******")
              fatal 141 $ "PHP/SQL problem: "++queryResult
      else case reads queryResult of
             [(pairs,"")] -> return pairs
             _            -> fatal 143 $ "Parse error on php result: "++show queryResult
    }
   where
    php =
      [ "// Try to connect to the database"
      , "$DB_name='"++addSlashes tempDbName++"';"
      , "global $DB_host,$DB_user,$DB_pass;"
      , "$DB_host='"++addSlashes (sqlHost opts)++"';"
      , "$DB_user='"++addSlashes (sqlLogin opts)++"';"
      , "$DB_pass='"++addSlashes (sqlPwd opts)++"';"
      , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass,$DB_name);"
      , "// Check connection"
      , "if (mysqli_connect_errno()) {"
      , "  die(\"Error: Failed to connect to $DB_name: \" . mysqli_connect_error());"
      , "  }"
      , ""
      , "$sql="++showPhpStr queryStr++";"
      , "$result=mysqli_query($DB_link,$sql);"
      , "if(!$result)"
      , "  die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
      , "$rows=Array();"
      , "  while ($row = mysqli_fetch_array($result)) {"
      , "    $rows[]=$row;"
      , "    unset($row);"
      , "  }"
      , "echo '[';"
      , "for ($i = 0; $i < count($rows); $i++) {"
      , "  if ($i==0) echo ''; else echo ',';"
      , "  echo '(\"'.addslashes($rows[$i]['src']).'\", \"'.addslashes($rows[$i]['tgt']).'\")';"
      , "}"
      , "echo ']';"
      ]

createTempDatabase :: FSpec -> IO ()
createTempDatabase fSpec =
 do { _ <- executePHP php
    ; return ()
    }
 where
  php = showPHP
     ([ "// Try to connect to the database"
      , "$DB_name='"++addSlashes tempDbName++"';"
      , "global $DB_host,$DB_user,$DB_pass;"
      , "$DB_host='"++addSlashes (sqlHost (flags fSpec))++"';"
      , "$DB_user='"++addSlashes (sqlLogin (flags fSpec))++"';"
      , "$DB_pass='"++addSlashes (sqlPwd (flags fSpec))++"';"

      , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass);"
      , "// Check connection"
      , "if (mysqli_connect_errno()) {"
      , "  die(\"Failed to connect to MySQL: \" . mysqli_connect_error());"
      , "}"
      , ""
      , "// Drop the database if it exists"
      , "$sql=\"DROP DATABASE $DB_name\";"
      , "mysqli_query($DB_link,$sql);"
      , "// Don't bother about the error if the database didn't exist..."
      , ""
      , "// Create the database"
      , "$sql=\"CREATE DATABASE $DB_name DEFAULT CHARACTER SET UTF8\";"
      , "if (!mysqli_query($DB_link,$sql)) {"
      , "  die(\"Error creating the database: \" . mysqli_error($DB_link));"
      , "  }"
      , ""
      , "// Connect to the freshly created database"
      , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass,$DB_name);"
      , "// Check connection"
      , "if (mysqli_connect_errno()) {"
      , "  die(\"Failed to connect to the database: \" . mysqli_connect_error());"
      , "  }"
      , ""
      ]++
      createTablesPHP fSpec ++
      populateTablesPHP fSpec
     )

-- call the command-line php with phpStr as input
executePHP :: String -> IO String
executePHP phpStr =
 do { tempdir <- catch getTemporaryDirectory
                       (\e -> do let err = show (e :: IOException)
                                 hPutStr stderr ("Warning: Couldn't find temp directory. Using current directory : " ++ err)
                                 return ".")

    ; (tempfile, temph) <- openTempFile tempdir "phpInput"
    ; hPutStr temph phpStr
    ; hClose temph

    ; let cp = CreateProcess
                { cmdspec       = RawCommand "php" [tempfile]
                , cwd           = Nothing -- path
                , env           = Just [("TERM","dumb")] -- environment
                , std_in        = Inherit
                , std_out       = CreatePipe
                , std_err       = CreatePipe
                , close_fds     = False -- no need to close all other file descriptors
                , create_group  = False
                , delegate_ctlc = False -- don't let php handle ctrl-c
                }

    ; (_, mStdOut, mStdErr, _) <- createProcess cp
    ; outputStr <-
        case (mStdOut, mStdErr) of
          (Nothing, _) -> fatal 105 "no output handle"
          (_, Nothing) -> fatal 106 "no error handle"
          (Just stdOutH, Just stdErrH) ->
           do { --putStrLn "done"
              ; errStr <- hGetContents stdErrH
              ; seq (length errStr) $ return ()
              ; hClose stdErrH
              ; unless (null errStr) $
                  putStrLn $ "Error during PHP execution:\n" ++ errStr
              ; outputStr' <- hGetContents stdOutH --and fetch the results from the output pipe
              ; seq (length outputStr') $ return ()
              ; hClose stdOutH
              ; return outputStr'
              }
    ; removeFile tempfile
--    ; putStrLn $ "Results:\n" ++ outputStr
    ; return outputStr
    }
-- | The syntax of the generated SQL code might not be valid for the database in use. This could be
-- because the prototype contains errors. For this reason, a validation can be done. The idea is
-- that every single SQL expression is thrown against the generated database. If the SQL syntax
-- is incorrect, an error is generated.
-- result of the query, a check is d
validateSQLsyntax :: FSpec -> Bool
validateSQLsyntax _ = False

showPHP :: [String] -> String
showPHP phpLines = unlines $ ["<?php"]++phpLines++["?>"]
