{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Parsing ( parseADL
                                        , parsePops
                                        , parseExpr
                                        , ParserError)
where


import DatabaseDesign.Ampersand.Input.ADL1.CCv221 (pContext,pPopulations,pExpr,keywordstxt, keywordsops, specialchars, opchars)
import qualified DatabaseDesign.Ampersand.Input.ADL1.CC664 as CC664
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner -- (scan,initPos)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing --  (getMsgs,parse,evalSteps,parseIO)
import DatabaseDesign.Ampersand.ADL1
 
type ParserError = Message Token

parseADL :: String        -- ^ The string to be parsed
         -> String        -- ^ The name of the .adl file (used for error messages)
         -> ParserVersion -- ^ The specific version of the parser to be used
         -> Options       -- ^ Options to use
         -> Either P_Context [ParserError] -- ^ The result: Either the parsetree, or some errors. 

parseADL str fn pv _ =
    case  getMsgs steps of
      []  -> Left presult
      msg -> Right msg
  where
    Pair presult _ = evalSteps steps 
    steps :: Steps (Pair P_Context (Pair [Token] a)) Token  
    steps = case pv of
      PV664  -> parse CC664.pContext (scan CC664.keywordstxt CC664.keywordsops CC664.specialchars CC664.opchars fn initPos str)
      PV211  -> parse       pContext (scan       keywordstxt       keywordsops       specialchars       opchars fn initPos str)

-- | Same as passeCtx_ , however this one si for a list of populations
parsePops :: String            -- ^ The string to be parsed
          -> String            -- ^ The name of the .pop file (used for error messages)
          -> ParserVersion     -- ^ The specific version of the parser to be used
          -> Either [P_Population] String -- ^ The result: Either a list of populations, or some errors. 
parsePops str fn pv =
    case  getMsgs steps of
      []  -> Left presult
      msg -> Right $ "Parse errors for "++show pv++":\n"++show (head msg)
  where
    Pair presult _ = evalSteps steps 
    steps :: Steps (Pair [P_Population] (Pair [Token] a)) Token  
    steps = parse pPopulations (scan keywordstxt keywordsops specialchars opchars fn initPos str)

-- | Parse isolated ADL1 expression strings
parseExpr :: String            -- ^ The string to be parsed
          -> String            -- ^ The name of the .pop file (used for error messages)
          -> ParserVersion     -- ^ The specific version of the parser to be used
          -> Either P_Expression String -- ^ The result: Either a list of populations, or some errors. 
parseExpr str fn pv =
    case  getMsgs steps of
      []  -> Left presult
      msg -> Right $ "Parse errors for "++show pv++":\n"++show (head msg)
  where
    Pair presult _ = evalSteps steps 
    steps :: Steps (Pair P_Expression (Pair [Token] a)) Token  
    steps = parse pExpr (scan keywordstxt keywordsops specialchars opchars fn initPos str)



