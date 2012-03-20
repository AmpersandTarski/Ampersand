module DatabaseDesign.Ampersand_Prototype.ValidateSQL where

import Data.Maybe
import System.Process
import System.IO
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics
import DatabaseDesign.Ampersand_Prototype.Installer

-- TODO: fail with error code if validation fails or something goes wrong along the way
{-
Validate the generated SQL for all rules in the fSpec, by comparing the evaluation results
with the results from Haskell-based Ampersand rule evaluator. The latter is much simpler and
therefore most likely to be correct in case of discrepancies.
-}

validateRuleSQL :: Fspc -> Options -> IO ()
validateRuleSQL fSpec opts =
 do { res <- executePHP "<?php echo 'PHP says hi'; ?>"
    ; removeTempDatabase opts -- in case it exists when we start, just drop it
    ; createTempDatabase fSpec opts
    ; removeTempDatabase opts
    ; return ()
    }

tempDbName = "TemporaryValidationDatabase"
       
removeTempDatabase :: Options -> IO ()
removeTempDatabase opts =
 do { executePHP . showPHP $ 
        ["@mysql_connect('"++addSlashes (fromMaybe "localhost" $ sqlHost opts)++"'"
                           ++",'"++addSlashes (fromMaybe "root" $ sqlLogin opts)++"'"
                           ++",'"++addSlashes (fromMaybe "" $ sqlPwd opts)++"');"] ++
        ["mysql_query('DROP DATABASE "++tempDbName++"');"]
    ; return ()
    }

createTempDatabase :: Fspc -> Options -> IO ()
createTempDatabase fSpec opts =
 do { executePHP php
    ; return ()
    }
 where php = showPHP $
               ["@mysql_connect('"++addSlashes (fromMaybe "localhost" $ sqlHost opts)++"'"
                                     ++",'"++addSlashes (fromMaybe "root" $ sqlLogin opts)++"'"
                                     ++",'"++addSlashes (fromMaybe "" $ sqlPwd opts)++"');"] ++
               createDatabasePHP tempDbName ++
               ["@mysql_select_db('"++tempDbName++"');"] ++
               createTablesPHP fSpec ++
               ["echo 'done creating tables';"]
 
executePHP :: String -> IO String
executePHP phpStr =
 do { putStrLn $ "Executing PHP:\n" ++ phpStr
    ; (mStdIn, mStdOut, mStdErr, procHandle) <- createProcess cp 
    ; case (mStdIn, mStdOut, mStdErr) of
        (Nothing, _, _) -> error "no input handle"
        (_, Nothing, _) -> error "no output handle"
        (_, _, Nothing) -> error "no error handle"
        (Just stdInH, Just stdOutH, Just stdErrH) ->
         do { putStrLn "done"
            ; hPutStr stdInH phpStr -- feed php script into the input pipe
            ; hClose stdInH
            ; outputStr <- hGetContents stdOutH --and fetch the results from the output pipe
            ; errStr <- hGetContents stdErrH
            ; seq (length outputStr) $ return ()
            ; seq (length errStr) $ return ()
            ; hClose stdOutH
            ; hClose stdErrH -- TODO: read stdErr
            ; putStrLn $ "Results:\n" ++ outputStr
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
              , close_fds    = False -- don't close all other file descr.
              }
              
showPHP phpLines = unlines $ ["<?php"]++phpLines++["?>"]