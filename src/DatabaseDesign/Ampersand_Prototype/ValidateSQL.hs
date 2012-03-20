module DatabaseDesign.Ampersand_Prototype.ValidateSQL where

import Data.List
import Data.Maybe
import Control.Monad
import System.Process
import System.IO
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.ADL1.Rule
import DatabaseDesign.Ampersand_Prototype.CoreImporter hiding (putStr)
import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics
import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL
import DatabaseDesign.Ampersand_Prototype.Installer
import DatabaseDesign.Ampersand_Prototype.Version 

-- TODO: fail with error code if validation fails or something goes wrong along the way
-- TODO: get rid of read
-- TODO: escaping?

{-
Validate the generated SQL for all rules in the fSpec, by comparing the evaluation results
with the results from Haskell-based Ampersand rule evaluator. The latter is much simpler and
therefore most likely to be correct in case of discrepancies.
-}

fatal :: Int -> String -> a
fatal = fatalMsg "ValidateSQL"

tempDbName :: String
tempDbName = "TemporaryValidationDatabase"

validateRuleSQL :: Fspc -> Options -> IO ()
validateRuleSQL fSpec opts =
 do { removeTempDatabase opts -- in case it exists when we start, just drop it
    ; putStrLn "Initializing temporary database"
    ; createTempDatabase fSpec opts
    ; 
    ; results <- mapM (validateExp fSpec opts) $ 
                   getAllInterfaceExps fSpec ++ 
                   getAllRuleExps fSpec ++
                   getAllPairViewExps fSpec ++
                   getAllKeyExps fSpec
    ; case [ ve | (ve, False) <- results] of
        [] -> return ()
        ves -> error $ "\n\nERROR: The following expressions failed validation:\n" ++
                (unlines [ origin++", expression: "++showADL exp | (exp,origin) <- ves ]) 
    }

-- functions for extracting all expressions from the context

getAllInterfaceExps :: Fspc -> [ValidationExp]
getAllInterfaceExps fSpec = concat [ getObjExps (name ifc) $ ifcObj ifc 
                                   | ifc <- interfaceS fSpec ++ interfaceG fSpec ]
 where getObjExps iName objDef = (objctx objDef, "interface " ++ show iName) :
                                 concatMap (getObjExps iName) (objAts objDef)

-- we check the complement of the rule, since that is the expression evaluated in the prototype
getAllRuleExps :: Fspc -> [ValidationExp]
getAllRuleExps fSpec = map getRuleExp $ vrules fSpec ++ grules fSpec
 where getRuleExp rule = (ECpl . rrexp $ rule, "rule "++show (name rule))
 
getAllPairViewExps :: Fspc -> [ValidationExp]
getAllPairViewExps fSpec = concatMap getPairViewExps $ vrules fSpec ++ grules fSpec
 where getPairViewExps r@Ru{rrviol = Just (PairView pvsegs)} =
         [ (exp, "violation view for rule "++show (name r)) | PairViewExp _ exp <- pvsegs ]
       getPairViewExps _    = []              

getAllKeyExps :: Fspc -> [ValidationExp]
getAllKeyExps fSpec = concatMap getKeyExps $ vkeys fSpec
 where getKeyExps key = [ (objctx objDef, "key "++show (name key)) 
                        | KeyExp objDef <- kdats key ]


type ValidationExp = (Expression, String) 
-- a ValidationExp is an expression together with the place in the context where we 
-- obtained it from (e.g. rule/interface/..)
 
-- validate a single expression and report the results
validateExp :: Fspc -> Options -> ValidationExp -> IO (ValidationExp, Bool)
validateExp _     _    vExp@(ERel _, _)   = return (vExp, True) -- skip all simple relations
validateExp fSpec opts vExp@(exp, origin) =
 do { putStr $ "Checking "++origin ++": expression = "++showADL exp
    ; violationsSQL <- fmap sort . evaluateExpSQL fSpec opts $ exp
    ; let violationsAmp = sort $ contents exp
    
    ; if violationsSQL == violationsAmp 
      then 
       do { putStrLn $ " (pass)\n" -- ++show violationsSQL
          ; return (vExp, True)
          }    
      else
       do { putStrLn "(fail)\nMismatch between SQL and Ampersand"
          ; putStrLn $ "SQL violations:\n"++show violationsSQL
          ; putStrLn $ "Ampersand violations:\n" ++ show violationsAmp
          ; return (vExp, False)
          }
    }

-- evaluate normalized exp in SQL
evaluateExpSQL :: Fspc -> Options -> Expression -> IO [(String,String)]
evaluateExpSQL fSpec opts exp =
 do { violations <- fmap sort $ performQuery opts violationsQuery
    ; return violations
    }
 where violationsExpr = conjNF exp
       violationsQuery = fromMaybe (fatal 100 $ "No sql generated for "++showHS opts "" violationsExpr)
                                   (selectExpr fSpec 26 "src" "tgt" $ violationsExpr) 
  
performQuery :: Options -> String -> IO [(String,String)]
performQuery opts queryStr =
 do { queryResult <- executePHP . showPHP $ 
        connectToServer opts ++
        ["mysql_query('"++queryStr++"');"
        , "mysql_select_db('"++tempDbName++"');"
        , "$result=mysql_query('"++queryStr++"');"
        , "$rows=Array();"
        , "  while (($row = @mysql_fetch_array($result))!==false) {"
        , "    $rows[]=$row;"
        , "    unset($row);"
        , "  }"
        , "echo '[';"
        , "for ($i = 0; $i < count($rows); $i++) {"
        , "  if ($i==0) echo ''; else echo ',';"
        , "  echo '(\"'.$rows[$i]['src'].'\", \"'.$rows[$i]['tgt'].'\")';"
        , "}"
        , "echo ']';"
        ]
    ; return $ read queryResult -- ugh.. read
    }

 
removeTempDatabase :: Options -> IO ()
removeTempDatabase opts =
 do { executePHP . showPHP $ 
        connectToServer opts ++
        ["mysql_query('DROP DATABASE "++tempDbName++"');"]
    ; return ()
    }

createTempDatabase :: Fspc -> Options -> IO ()
createTempDatabase fSpec opts =
 do { executePHP php
    ; return ()
    }
 where php = showPHP $
               connectToServer opts ++
               createDatabasePHP tempDbName ++
               ["mysql_select_db('"++tempDbName++"');"] ++
               createTablesPHP fSpec ++
               ["echo 'done creating tables';"]

connectToServer :: Options -> [String]
connectToServer opts =
  ["mysql_connect('"++addSlashes (fromMaybe "localhost" $ sqlHost opts)++"'"
              ++",'"++addSlashes (fromMaybe "root" $ sqlLogin opts)++"'"
              ++",'"++addSlashes (fromMaybe "" $ sqlPwd opts)++"');"] 
               

executePHP :: String -> IO String
executePHP phpStr =
 do { --putStrLn $ "Executing PHP:\n" ++ phpStr
    ; (mStdIn, mStdOut, mStdErr, procHandle) <- createProcess cp 
    ; case (mStdIn, mStdOut, mStdErr) of
        (Nothing, _, _) -> fatal 104 "no input handle"
        (_, Nothing, _) -> fatal 105 "no output handle"
        (_, _, Nothing) -> fatal 106 "no error handle"
        (Just stdInH, Just stdOutH, Just stdErrH) ->
         do { --putStrLn "done"
            ; hPutStr stdInH phpStr -- feed php script into the input pipe
            ; hClose stdInH
            ; errStr <- hGetContents stdErrH
            ; seq (length errStr) $ return ()
            ; hClose stdErrH
            ; when (not $ null errStr) $
                putStrLn $ "Error during PHP execution:\n" ++ errStr 
            ; outputStr <- hGetContents stdOutH --and fetch the results from the output pipe
            ; seq (length outputStr) $ return ()
            ; hClose stdOutH
            --; putStrLn $ "Results:\n" ++ outputStr
            ; return outputStr
            }
    }
 where cp = CreateProcess
              { cmdspec      = RawCommand "php" []
              , cwd          = Nothing -- path
              , env          = Nothing -- environment
              , std_in       = CreatePipe 
              , std_out      = CreatePipe
              , std_err      = CreatePipe
              , close_fds    = False -- no need to close all other file descriptors
              }


showPHP phpLines = unlines $ ["<?php"]++phpLines++["?>"]



testQuery = "/* case: ETyp x _" ++
            "                 ETyp ( \"V\" ) _ */" ++
            "              /* case: (ERel (V (Sign s t)))" ++
            "                 ERel [ \"V[Klant]\" ] */" ++
            "              SELECT DISTINCT cfst0.`Klant` AS src, cfst1.`Klant` AS tgt" ++
            "              FROM `Klant` AS cfst0, `Klant` AS cfst1" 
            