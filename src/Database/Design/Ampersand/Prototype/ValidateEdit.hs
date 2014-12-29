module Database.Design.Ampersand.Prototype.ValidateEdit where

import Prelude hiding (putStr, putStrLn)
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.AbstractSyntaxTree

validateEditScript :: [Population] -> [Population] -> [Char] -> IO Bool
validateEditScript beforePops afterPops editScriptPath =
 do { mFileContents <- readUTF8File editScriptPath
    ; case mFileContents of
        Left err -> error $ "ERROR reading file " ++ editScriptPath ++ ":\n" ++ err
        Right editScript ->
         do { putStrLn $ "Population before edit operations:\n" ++ show beforePops
            ; putStrLn $ "Expected population after edit operations:\n" ++ show afterPops
            ; putStrLn $ "Edit script:\n" ++ editScript
            ; return True
            }
    }