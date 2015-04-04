{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Database.Design.Ampersand.Test.Parser.ParserTest (parseFile, parse, parseReparse) where

import Database.Design.Ampersand.Input.ADL1.CtxError (Guarded(..))
import Database.Design.Ampersand.ADL1.PrettyPrinters(pretty_print)
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.Input.Parsing
import Debug.Trace

unguard :: FilePath -> String -> Guarded (P_Context, [String]) -> (P_Context, Bool)
unguard file txt result =
    case result of
        Errors  e     -> trace (show e ++ "\n" ++ txt) (dummy, False)
        Checked (p,_) -> trace ("Parsed: " ++ file)    (p, True)
        --- Checked (p,_) -> (p, True)
    where dummy = PCtx "DUMMY"  [] English Nothing [] [] [] [] [] [] [] [] [] [] [] [] [] [] []

parseFile :: FilePath -> IO Bool
parseFile name =
     do contents <- readFile name
        if null contents then return True
        else return $ snd $ parseReparse name contents

parse :: FilePath -> String -> (P_Context, Bool)
parse file txt = unguard file txt $ runParser pContext file txt

parseReparse :: FilePath -> String -> (P_Context, Bool)
parseReparse file txt = if isParsed then
                            if isReparsed then (reparsed, True)
                            else trace ("Error pretty printing parse tree:\n" ++ show parsed) (parsed, False) 
                        else (parsed,isParsed)
                  where (parsed,isParsed) = run file txt
                        (reparsed,isReparsed) = run (file ++ "**pretty") $ pretty_print parsed
                        run nm text = unguard nm text $ runParser pContext nm text
