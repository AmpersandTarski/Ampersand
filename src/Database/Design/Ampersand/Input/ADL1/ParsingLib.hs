{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash, FlexibleInstances #-}
module Database.Design.Ampersand.Input.ADL1.ParsingLib(
    pSucceed, AmpParser,
    -- Operators
    (DF.<$>), (P.<|>), (<$), (CA.<*>), (CA.<*), (CA.*>), (<??>),
    -- Combinators
    pList, pList1, opt, pListSep, pList1Sep, try,
    -- Basic parsers
    pAtom, pConid, pString, pExpl, pVarid,
    -- Special symbols
    pComma, pParens, pBraces, pBrackets, pChevrons,
    -- Positions
    SourcePos, sourceName, sourceLine, sourceColumn, posOrigin,
    posOf, valPosOf,
    -- Keywords
    pKeyINCLUDE, pKeyCONTEXT, pKeyENDCONTEXT, pKeyEXTENDS, pKeyTHEMES, pKeyMETA,
    pKeyPATTERN, pKeyENDPATTERN, pKeyPROCESS, pKeyENDPROCESS, pKeyINTERFACE, pKeyCLASS,
    pKeyFOR, pKeyBOX, pKeyROWS, pKeyTABS, pKeyCOLS, pKeyINITIAL,
    pKeySQLPLUG, pKeyPHPPLUG, pKeyTYPE, pKeyPOPULATION, pKeyCONTAINS, pKeyUNI,
    pKeyINJ, pKeySUR, pKeyTOT, pKeySYM, pKeyASY, pKeyTRN,
    pKeyRFX, pKeyIRF, pKeyAUT, pKeyPROP, pKeyALWAYS, pKeyRULE,
    pKeyMESSAGE, pKeyVIOLATION, pKeySRC, pKeyTGT, pKeyTEST, pKeyRELATION,
    pKeyMEANING, pKeyCONCEPT, pKeyIDENT, pKeyVIEW, pKeyTXT, pKeyPRIMHTML,
    pKeyKEY, pKeyIMPORT, pKeySPEC, pKeyISA, pKeyIS, pKeyI,
    pKeyV, pKeyCLASSIFY, pKeyPRAGMA, pKeyPURPOSE, pKeyIN, pKeyREF,
    pKeyENGLISH, pKeyDUTCH, pKeyREST, pKeyHTML, pKeyLATEX, pKeyMARKDOWN,
    pKeyONE, pKeyBYPLUG, pKeyROLE, pKeyEDITS, pKeyMAINTAINS,
    -- Operators
    pOpImplication, pDash, pOpRightArrow, pOpLeftArrow, pEqual, pOpConversion,
    pPlus, pAsterisk, pSemi, pOpRelAdd, pOpProduct, pOpRelation,
    pColon, pOpUnion, pOpIntersection, pOpRightResidual, pOpLeftResidual, pOpDiamond,
    pOpMultiplicity, pOpDot, pOpZero, pOpOne
) where

import Control.Monad.Identity (Identity)
import Database.Design.Ampersand.Input.ADL1.LexerToken
import qualified Control.Applicative as CA
import qualified Data.Functor as DF
import qualified Text.Parsec.Prim as P
import Text.Parsec as P hiding(satisfy)

type AmpParser a = P.ParsecT [Token] SourcePos Identity a

-----------------------------------------------------------
-- Operators
-----------------------------------------------------------

-- TODO: Use Parsec operators
infixl 4 <$
(<$) :: a -> AmpParser b -> AmpParser a
a <$ p = do { _ <- p; return a }

(<**>) :: AmpParser a -> AmpParser (a -> b) -> AmpParser b
p <**> q = (\ x f -> f x) CA.<$> p CA.<*> q

(<??>) :: AmpParser a -> AmpParser (a -> a) -> AmpParser a
p <??> q = p <**> (q `opt` id)

----------------------------------------------------------------------------------
-- Functions copied from Lexer after decision to split lexer and parser
----------------------------------------------------------------------------------

check :: (Lexeme -> Maybe a) -> AmpParser a
check predicate = tokenPrim showTok nextPos matchTok
  where  -- Token pretty-printing function
         showTok :: Token -> String
         showTok (Tok lx _)   = show lx
         -- Next position calculating function
         nextPos :: SourcePos -> Token -> [Token] -> SourcePos
         nextPos pos _ [] = pos
         nextPos _ _ ((Tok _ pos):_) = pos
         -- ^ Matching function for the token to parse.
         matchTok (Tok l _) = predicate l

match :: Lexeme -> AmpParser String
match lx = check (\lx' -> if (lx == lx') then Just (get_lex_val lx) else Nothing) <?> show lx

pSucceed :: a -> AmpParser a
pSucceed = P.parserReturn

pList :: AmpParser a -> AmpParser [a]
pList = P.many

pList1 ::  AmpParser a -> AmpParser [a]
pList1 = P.many1

--TODO: Replace the pListSep functions with specialized functions with separators & outer parenthesis
pListSep :: AmpParser sep -> AmpParser a -> AmpParser [a]
pListSep sep a = P.sepBy a sep

pList1Sep ::  AmpParser sep -> AmpParser a -> AmpParser [a]
pList1Sep sep a = P.sepBy1 a sep

opt ::  AmpParser a -> a -> AmpParser a
a `opt` b = P.option b a

-----------------------------------------------------------
-- Keywords
-----------------------------------------------------------

pKey :: String -> AmpParser String
pKey key = match (LexKeyword key)

pKeyINCLUDE :: AmpParser String
pKeyINCLUDE = pKey "INCLUDE"

pKeyCONTEXT :: AmpParser String
pKeyCONTEXT = pKey "CONTEXT"

pKeyENDCONTEXT :: AmpParser String
pKeyENDCONTEXT = pKey "ENDCONTEXT"

pKeyEXTENDS :: AmpParser String
pKeyEXTENDS = pKey "EXTENDS"

pKeyTHEMES :: AmpParser String
pKeyTHEMES = pKey "THEMES"

pKeyMETA :: AmpParser String
pKeyMETA = pKey "META"

pKeyPATTERN :: AmpParser String
pKeyPATTERN = pKey "PATTERN"

pKeyENDPATTERN :: AmpParser String
pKeyENDPATTERN = pKey "ENDPATTERN"

pKeyPROCESS :: AmpParser String
pKeyPROCESS = pKey "PROCESS"

pKeyENDPROCESS :: AmpParser String
pKeyENDPROCESS = pKey "ENDPROCESS"

pKeyINTERFACE :: AmpParser String
pKeyINTERFACE = pKey "INTERFACE"

pKeyCLASS :: AmpParser String
pKeyCLASS = pKey "CLASS"

pKeyFOR :: AmpParser String
pKeyFOR = pKey "FOR"

pKeyBOX :: AmpParser String
pKeyBOX = pKey "BOX"

pKeyROWS :: AmpParser String
pKeyROWS = pKey "ROWS"

pKeyTABS :: AmpParser String
pKeyTABS = pKey "TABS"

pKeyCOLS :: AmpParser String
pKeyCOLS = pKey "COLS"

pKeyINITIAL :: AmpParser String
pKeyINITIAL = pKey "INITIAL"

pKeySQLPLUG :: AmpParser String
pKeySQLPLUG = pKey "SQLPLUG"

pKeyPHPPLUG :: AmpParser String
pKeyPHPPLUG = pKey "PHPPLUG"

pKeyTYPE :: AmpParser String
pKeyTYPE = pKey "TYPE"

pKeyPOPULATION :: AmpParser String
pKeyPOPULATION = pKey "POPULATION"

pKeyCONTAINS :: AmpParser String
pKeyCONTAINS = pKey "CONTAINS"

pKeyUNI :: AmpParser String
pKeyUNI = pKey "UNI"

pKeyINJ :: AmpParser String
pKeyINJ = pKey "INJ"

pKeySUR :: AmpParser String
pKeySUR = pKey "SUR"

pKeyTOT :: AmpParser String
pKeyTOT = pKey "TOT"

pKeySYM :: AmpParser String
pKeySYM = pKey "SYM"

pKeyASY :: AmpParser String
pKeyASY = pKey "ASY"

pKeyTRN :: AmpParser String
pKeyTRN = pKey "TRN"

pKeyRFX :: AmpParser String
pKeyRFX = pKey "RFX"

pKeyIRF :: AmpParser String
pKeyIRF = pKey "IRF"

pKeyAUT :: AmpParser String
pKeyAUT = pKey "AUT"

pKeyPROP :: AmpParser String
pKeyPROP = pKey "PROP"

pKeyALWAYS :: AmpParser String
pKeyALWAYS = pKey "ALWAYS"

pKeyRULE :: AmpParser String
pKeyRULE = pKey "RULE"

pKeyMESSAGE :: AmpParser String
pKeyMESSAGE = pKey "MESSAGE"

pKeyVIOLATION :: AmpParser String
pKeyVIOLATION = pKey "VIOLATION"

pKeySRC :: AmpParser String
pKeySRC = pKey "SRC"

pKeyTGT :: AmpParser String
pKeyTGT = pKey "TGT"

pKeyTEST :: AmpParser String
pKeyTEST = pKey "TEST"

pKeyRELATION :: AmpParser String
pKeyRELATION = pKey "RELATION"

pKeyMEANING :: AmpParser String
pKeyMEANING = pKey "MEANING"

pKeyCONCEPT :: AmpParser String
pKeyCONCEPT = pKey "CONCEPT"

pKeyIDENT :: AmpParser String
pKeyIDENT = pKey "IDENT"

pKeyVIEW :: AmpParser String
pKeyVIEW = pKey "VIEW"

pKeyTXT :: AmpParser String
pKeyTXT = pKey "TXT"

pKeyPRIMHTML :: AmpParser String
pKeyPRIMHTML = pKey "PRIMHTML"

pKeyKEY :: AmpParser String
pKeyKEY = pKey "KEY"

pKeyIMPORT :: AmpParser String
pKeyIMPORT = pKey "IMPORT"

pKeySPEC :: AmpParser String
pKeySPEC = pKey "SPEC"

pKeyISA :: AmpParser String
pKeyISA = pKey "ISA"

pKeyIS :: AmpParser String
pKeyIS = pKey "IS"

pKeyI :: AmpParser String
pKeyI = pKey "I"

pKeyV :: AmpParser String
pKeyV = pKey "V"

pKeyCLASSIFY :: AmpParser String
pKeyCLASSIFY = pKey "CLASSIFY"

pKeyPRAGMA :: AmpParser String
pKeyPRAGMA = pKey "PRAGMA"

pKeyPURPOSE :: AmpParser String
pKeyPURPOSE = pKey "PURPOSE"

pKeyIN :: AmpParser String
pKeyIN = pKey "IN"

pKeyREF :: AmpParser String
pKeyREF = pKey "REF"

pKeyENGLISH :: AmpParser String
pKeyENGLISH = pKey "ENGLISH"

pKeyDUTCH :: AmpParser String
pKeyDUTCH = pKey "DUTCH"

pKeyREST :: AmpParser String
pKeyREST = pKey "REST"

pKeyHTML :: AmpParser String
pKeyHTML = pKey "HTML"

pKeyLATEX :: AmpParser String
pKeyLATEX = pKey "LATEX"

pKeyMARKDOWN :: AmpParser String
pKeyMARKDOWN = pKey "MARKDOWN"

pKeyONE :: AmpParser String
pKeyONE = pKey "ONE"

pKeyBYPLUG :: AmpParser String
pKeyBYPLUG = pKey "BYPLUG"

pKeyROLE :: AmpParser String
pKeyROLE = pKey "ROLE"

pKeyEDITS :: AmpParser String
pKeyEDITS = pKey "EDITS"

pKeyMAINTAINS :: AmpParser String
pKeyMAINTAINS = pKey "MAINTAINS"

-----------------------------------------------------------
-- Operators
-----------------------------------------------------------

pOperator :: String -> AmpParser String
pOperator op = match (LexOperator op)

pOpImplication :: AmpParser String
pOpImplication = pOperator "|-"

pDash :: AmpParser String
pDash = pOperator "-"

pOpRightArrow :: AmpParser String
pOpRightArrow = pOperator "->"

pOpLeftArrow :: AmpParser String
pOpLeftArrow = pOperator "<-"

pEqual :: AmpParser String
pEqual = pOperator "="

pOpConversion :: AmpParser String
pOpConversion = pOperator "~"

pPlus :: AmpParser String
pPlus = pOperator "+"

pAsterisk :: AmpParser String
pAsterisk = pOperator "*"

pSemi :: AmpParser String
pSemi = pOperator ";"

pOpRelAdd :: AmpParser String
pOpRelAdd = pOperator "!"

pOpProduct :: AmpParser String
pOpProduct = pOperator "#"

pOpRelation :: AmpParser String
pOpRelation = pOperator "::"

pColon :: AmpParser String
pColon = pOperator ":"

pOpUnion :: AmpParser String
pOpUnion = pOperator "\\/"

pOpIntersection :: AmpParser String
pOpIntersection = pOperator "/\\"

pOpRightResidual :: AmpParser String
pOpRightResidual = pOperator "\\"

pOpLeftResidual :: AmpParser String
pOpLeftResidual = pOperator "/"

pOpDiamond :: AmpParser String
pOpDiamond = pOperator "<>"

pOpMultiplicity :: AmpParser String
pOpMultiplicity = pOperator ".."

pOpDot :: AmpParser String
pOpDot = pOperator "."

pOpZero :: AmpParser String
pOpZero = pOperator "0"

pOpOne :: AmpParser String
pOpOne = pOperator "1"

-----------------------------------------------------------
-- Other token parsers
-----------------------------------------------------------

--- Conid ::= UpperChar (Char | '_')*
pConid :: AmpParser String
pConid = check (\lx -> case lx of { LexConId s -> Just s; _ -> Nothing })

--- String ::= '"' Any* '"'
--- StringListSemi ::= String (';' String)*
pString :: AmpParser String
pString = check (\lx -> case lx of { LexString s -> Just s; _ -> Nothing })

--- Expl ::= '{+' Any* '-}'
pExpl :: AmpParser String
pExpl = check (\lx -> case lx of { LexExpl s -> Just s; _ -> Nothing })

--- Varid ::= (LowerChar | '_') (Char | '_')*
pVarid :: AmpParser String
pVarid = check (\lx -> case lx of { LexVarId s -> Just s; _ -> Nothing })

-- TODO: does not escape, i.e. 'Mario\'s Pizzas' will fail to parse (this remark is old. Is it valid to the new lexer?)
pAtom :: AmpParser String
pAtom = check (\lx -> case lx of { LexAtom s -> Just s; _ -> Nothing })

-----------------------------------------------------------
-- Special characters
-----------------------------------------------------------

-- matches special characters
pSpec :: Char -> AmpParser String
pSpec sym = match (LexSymbol sym)

pComma :: AmpParser String
pComma  = pSpec ','

pParens :: AmpParser a -> AmpParser a
pParens parser = pSpec '(' CA.*> parser CA.<* pSpec ')'

pBraces :: AmpParser a -> AmpParser a
pBraces parser = pSpec '{' CA.*> parser CA.<* pSpec '}'

pBrackets :: AmpParser a -> AmpParser a
pBrackets parser = pSpec '[' CA.*> parser CA.<* pSpec ']'

pChevrons :: AmpParser a -> AmpParser a
pChevrons parser = pSpec '<' CA.*> parser CA.<* pSpec '>'

-----------------------------------------------------------
-- Token positioning
-----------------------------------------------------------

posOrigin :: Show a => a -> SourcePos -> Origin
posOrigin sym p = FileLoc (FilePos (sourceName p, p, show sym))

posOf :: Show a => AmpParser a -> AmpParser Origin
posOf parser = do { pos <- getPosition; a <- parser; return (posOrigin a pos) }

valPosOf :: Show a => AmpParser a -> AmpParser (a, Origin)
valPosOf parser = do { pos <- getPosition; a <- parser; return (a, posOrigin a pos) }
