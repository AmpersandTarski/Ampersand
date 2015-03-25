{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash, FlexibleInstances #-}
module Database.Design.Ampersand.Input.ADL1.ParsingLib(
    pSucceed, AmpParser,
    -- Operators
    (DF.<$>), (P.<|>), (<$), (CA.<*>), (CA.<*), (CA.*>), (<??>),
    -- Combinators
    pList, pList1, opt, pListSep, pList1Sep, try,
    -- Positions
    SourcePos, sourceName, sourceLine, sourceColumn, posOrigin,
    currPos, posOf, valPosOf,
    -- Basic parsers
    pAtom, pConid, pString, pExpl, pVarid,
    -- Special symbols
    pComma, pParens, pBraces, pBrackets, pChevrons,
    -- Keywords
    pKey,
    -- Operators
    pOpImplication, pDash, pOpRightArrow, pOpLeftArrow, pEqual, pOpConversion,
    pPlus, pAsterisk, pSemi, pOpRelAdd, pOpProduct, pOpRelation,
    pColon, pOpUnion, pOpIntersection, pOpRightResidual, pOpLeftResidual, pOpDiamond,
    pOpMultiplicity, pOpDot,
    -- Integers
    pZero, pOne, pInteger
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
p <**> q = (\x f -> f x) CA.<$> p CA.<*> q

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
-- Integers
-----------------------------------------------------------

pNumber :: Int -> AmpParser String
pNumber nr = match (LexDecimal nr) <|> match (LexHex nr) <|> match (LexOctal nr)

pInteger :: AmpParser Int
pInteger = check isNr
    where isNr (LexDecimal i) = Just i
          isNr (LexHex i)     = Just i
          isNr (LexOctal i)   = Just i
          isNr _              = Nothing

pZero :: AmpParser String
pZero = pNumber 0

pOne :: AmpParser String
pOne = pNumber 1

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

currPos :: AmpParser Origin
currPos = posOf $ return ()

posOf :: Show a => AmpParser a -> AmpParser Origin
posOf parser = do { pos <- getPosition; a <- parser; return (posOrigin a pos) }

valPosOf :: Show a => AmpParser a -> AmpParser (a, Origin)
valPosOf parser = do { pos <- getPosition; a <- parser; return (a, posOrigin a pos) }
