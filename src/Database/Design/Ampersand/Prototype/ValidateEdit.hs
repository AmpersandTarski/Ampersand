module Database.Design.Ampersand.Prototype.ValidateEdit where

import Prelude hiding (putStr, putStrLn)
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.Prototype.PHP

tempDbName :: String
tempDbName = "ampersand_temporaryeditvalidationdb"

validateEditScript :: FSpec -> [Population] -> [Population] -> [Char] -> IO Bool
validateEditScript fSpec beforePops afterPops editScriptPath =
 do { mFileContents <- readUTF8File editScriptPath
    ; case mFileContents of
        Left err -> error $ "ERROR reading file " ++ editScriptPath ++ ":\n" ++ err
        Right editScript ->
         do { putStrLn $ "Population before edit operations:\n" ++ show beforePops
            ; putStrLn $ "Expected population after edit operations:\n" ++ show afterPops
            ; putStrLn $ "Edit script:\n" ++ editScript
            
            ; createTempDatabase fSpec beforePops
            ; return True
            }
    }
    
createTempDatabase :: FSpec -> [Population] -> IO ()
createTempDatabase fSpec pops =
 do { _ <- executePHPStr . showPHP $ sqlServerConnectPHP fSpec ++
                                     createTempDbPHP tempDbName ++
                                     createTablesPHP fSpec ++
                                     populateTablesWithPopsPHP fSpec pops
    ; return ()
    }
 