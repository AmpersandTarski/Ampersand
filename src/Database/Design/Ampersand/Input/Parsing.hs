{-# OPTIONS_GHC  -XScopedTypeVariables #-}
module Database.Design.Ampersand.Input.Parsing ( parseADL1pExpr
                                        , runParser)
where

-- TODO: This module is obsolete and should be consolidated with InputProcessing.hs (i.e. extend InputProcessing.hs and rename it to Parsing.hs)

import Database.Design.Ampersand.Input.ADL1.Parser (pTerm,keywordstxt, keywordsops, specialchars, opchars)
import Database.Design.Ampersand.Input.ADL1.ParsingLib -- New Stub to convert UULib to Parsec
--import Database.Design.Ampersand.Input.ADL1.UU_Scanner  -- Old UU Lib
import UU.Parsing -- (getMsgs,parse,evalSteps,parseIO)
import Database.Design.Ampersand.ADL1

type ParseError = Message Token (Maybe Token)

 
-- | Parse isolated ADL1 expression strings
parseADL1pExpr :: String            -- ^ The string to be parsed
               -> String            -- ^ The name of the file (used for error messages)
               -> Either String (Term TermPrim)  -- ^ The result: Either an error message,  or a good result
parseADL1pExpr str fn =
    case runParser pTerm fn str of
      Right result -> Right result
      Left  msg    -> Left $ "Parse errors:\n"++show msg

runParser :: forall res . Parser Token res -> String -> String -> Either ParseError res
runParser parser filename input =
  let scanner = scan keywordstxt keywordsops specialchars opchars filename initPos
      steps = parse parser (scanner input)
  in  case  getMsgs steps of
         []    -> let Pair res _ = evalSteps steps
                  in  Right res
         msg:_ -> Left msg
