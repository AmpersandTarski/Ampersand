{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash, FlexibleInstances #-}
module Database.Design.Ampersand.Input.ADL1.ParsingLib(
    AmpParser, pIsThere, optList,
    -- Operators
    --TODO: Maybe we shouldn't export these here, but import in the parser directly
    (DF.<$>), (P.<|>), (<$), (CA.<*>), (CA.<*), (CA.*>), (<??>),
    -- Combinators
    sepBy, sepBy1, many, many1, opt, try, choice, pMaybe,
    -- Positions
    currPos, posOf, valPosOf,
    -- Basic parsers
    pAtom, pConid, pString, pExpl, pVarid,
    -- Special symbols
    pComma, pParens, pBraces, pBrackets, pChevrons,
    -- Keywords
    pKey,
    -- Operators
    pOperator, pDash, pSemi, pColon,
    -- Integers
    pZero, pOne, pInteger
) where

--TODO! Haddock comments to the parsing lib

import Control.Monad.Identity (Identity)
import Database.Design.Ampersand.Input.ADL1.FilePos (Origin(..))
import Database.Design.Ampersand.Input.ADL1.LexerToken
import qualified Control.Applicative as CA
import qualified Data.Functor as DF
import qualified Text.Parsec.Prim as P
import Text.Parsec as P hiding(satisfy)
import Text.Parsec.Pos (newPos)

type AmpParser a = P.ParsecT [Token] FilePos Identity a

-----------------------------------------------------------
-- Useful functions
-----------------------------------------------------------

-- TODO! Use Parsec operators
infixl 4 <$
(<$) :: a -> AmpParser b -> AmpParser a
a <$ p = do { _ <- p; return a }

(<??>) :: AmpParser a -> AmpParser (a -> a) -> AmpParser a
p <??> q = (\x f -> f x) CA.<$> p CA.<*> (q `opt` id)

pIsThere :: AmpParser a -> AmpParser Bool
pIsThere p = (True <$ p) `opt` False

optList :: AmpParser [a] -> AmpParser [a]
optList p = p `opt` []

pMaybe :: AmpParser a -> AmpParser (Maybe a)
pMaybe p = Just CA.<$> p <|> P.parserReturn Nothing

--TODO! Remove `opt` and use Parsec's option instead.
opt ::  AmpParser a -> a -> AmpParser a
a `opt` b = P.option b a

-----------------------------------------------------------
-- Keywords & operators
-----------------------------------------------------------
pKey :: String -> AmpParser String
pKey key = match (LexKeyword key)

pOperator :: String -> AmpParser String
pOperator op = match (LexOperator op)

pDash :: AmpParser String
pDash = pOperator "-"

pSemi :: AmpParser String
pSemi = pOperator ";"

pColon :: AmpParser String
pColon = pOperator ":"

-----------------------------------------------------------
-- Token parsers
-----------------------------------------------------------

check :: (Lexeme -> Maybe a) -> AmpParser a
check predicate = tokenPrim showTok nextPos matchTok
  where  -- Token pretty-printing function
         showTok :: Token -> String
         showTok (Tok lx _)   = show lx
         -- Next position calculating function
         nextPos :: SourcePos -> Token -> [Token] -> SourcePos
         nextPos pos _ [] = pos
         nextPos _ _ (Tok _ (FilePos file line col):_) = newPos file line col
         -- ^ Matching function for the token to parse.
         matchTok (Tok l _) = predicate l

match :: Lexeme -> AmpParser String
match lx = check (\lx' -> if lx == lx' then Just (lexemeText lx) else Nothing) <?> show lx

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
posOrigin sym p = FileLoc (FilePos (sourceName p) (sourceLine p) (sourceColumn p)) (show sym)

currPos :: AmpParser Origin
currPos = posOf $ return ()

posOf :: Show a => AmpParser a -> AmpParser Origin
posOf parser = do { pos <- getPosition; a <- parser; return (posOrigin a pos) }

valPosOf :: Show a => AmpParser a -> AmpParser (a, Origin)
valPosOf parser = do { pos <- getPosition; a <- parser; return (a, posOrigin a pos) }
