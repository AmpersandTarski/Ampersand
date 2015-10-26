{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash, FlexibleInstances #-}
module Database.Design.Ampersand.Input.ADL1.ParsingLib(
    AmpParser, pIsThere, optList,
    -- Operators
    --TODO: Maybe we shouldn't export these here, but import in the parser directly
    (DF.<$>), (P.<|>), (P.<?>), (<$), (CA.<*>), (CA.<*), (CA.*>), (<??>),
    -- Combinators
    sepBy, sepBy1, many, many1, opt, try, choice, pMaybe,
    -- Positions
    currPos, posOf, valPosOf,
    -- Basic parsers
    pConid, pString, pExpl, pVarid,
    -- special parsers
    pAtomInExpression, pAtomValInPopulation, Value(..),
    -- Special symbols
    pComma, pParens, pBraces, pBrackets, pChevrons,
    -- Keywords
    pKey,
    -- Operators
    pOperator, pDash, pSemi, pColon,
    -- Integers
    pZero, pOne
) where

import Control.Monad.Identity (Identity)
import Database.Design.Ampersand.Input.ADL1.FilePos (Origin(..))
import Database.Design.Ampersand.Input.ADL1.LexerToken
import Database.Design.Ampersand.Input.ADL1.Lexer (lexer)
import qualified Control.Applicative as CA
import qualified Data.Functor as DF
import qualified Text.Parsec.Prim as P
import Text.Parsec as P hiding(satisfy)
import Text.Parsec.Pos (newPos)
import Data.Time.Calendar
import Data.Time.Clock
import Database.Design.Ampersand.Basics (fatal)
import Data.Maybe
import Prelude hiding ((<$))

-- | The Ampersand parser type
type AmpParser a = P.ParsecT [Token] FilePos Identity a -- ^ The Parsec parser for a list of tokens with a file position.

-----------------------------------------------------------
-- Useful functions
-----------------------------------------------------------

infixl 4 <$

-- | Applies the given parser and returns the given constructor
(<$) :: a           -- ^ The value to return
     -> AmpParser b -- ^ The parser to apply
     -> AmpParser a -- ^ The result
a <$ p = do { _ <- p; return a }

(<??>) :: AmpParser a -> AmpParser (a -> a) -> AmpParser a
p <??> q = (\x f -> f x) CA.<$> p CA.<*> (q `opt` id)

-- | Tries to apply the given parser and returns a parser with a boolean indicating whether it succeeded
pIsThere :: AmpParser a     -- ^ The parser to run
         -> AmpParser Bool  -- ^ The parser with the result
pIsThere p = (True <$ p) `opt` False

-- | Optionally applies a list parser, returning an empty list if it doesn't succeed
optList :: AmpParser [a]
        -> AmpParser [a]
optList p = p `opt` []

-- | Tries to apply the given parser and encapsulates the result in Maybe
pMaybe :: AmpParser a           -- ^ The parser to apply
       -> AmpParser (Maybe a)   -- ^ The result
pMaybe p = Just CA.<$> p <|> P.parserReturn Nothing

-- | Tries to apply the given parser and returns the second argument if it doesn't succeed
opt ::  AmpParser a  -- ^ The parser to try
    -> a             -- ^ The item to return if the parser doesn't succeed
    -> AmpParser a   -- ^ The resulting parser
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
pConid = check (\lx -> case lx of { LexConId s -> Just s; _ -> Nothing }) <?> "upper case identifier"

--- String ::= '"' Any* '"'
--- StringListSemi ::= String (';' String)*
pString :: AmpParser String
pString = check (\lx -> case lx of { LexString s -> Just s; _ -> Nothing }) <?> "string"

--- Expl ::= '{+' Any* '-}'
pExpl :: AmpParser String
pExpl = check (\lx -> case lx of { LexExpl s -> Just s; _ -> Nothing }) <?> "explanation"

--- Varid ::= (LowerChar | '_') (Char | '_')*
pVarid :: AmpParser String
pVarid = check (\lx -> case lx of { LexVarId s -> Just s; _ -> Nothing }) <?> "lower case identifier"

--- Atom ::= "'" Any* "'"
pAtomInExpression :: AmpParser Value
pAtomInExpression = check (\lx -> case lx of
                                   LexSingleton s -> Just (VSingleton s (mval s))
                                   _              -> Nothing
                          ) <?> "Singleton value"
   where
    mval s =
      case lexer [] (fatal 141 $ "Reparse without fileName of `"++s ++"`") s of
        Left _  -> Nothing
        Right (toks,_)
           -> case runParser pAtomValInPopulation
                               (FilePos ("Reparse `"++s++"` ") 0 0) -- Todo: Fix buggy position
                                "" toks of
                Left _ -> Nothing
                Right a -> Just a

data Value = VRealString String
           | VSingleton String (Maybe Value)
           | VInt Int
           | VFloat Double
           | VBoolean Bool
           | VDateTime UTCTime
           | VDate Day
pAtomValInPopulation :: AmpParser Value
pAtomValInPopulation =
              VBoolean True  <$ pKey "TRUE"
          <|> VBoolean False <$ pKey "FALSE"
          <|> VRealString DF.<$> pString
          <|> VDateTime DF.<$> pUTCTime
          <|> VDate DF.<$> pDay
          <|> fromNumeric DF.<$> pNumeric
   where fromNumeric :: Either Int Double -> Value
         fromNumeric num = case num of
             Left i -> VInt i
             Right d -> VFloat d
-----------------------------------------------------------
-- Date / DateTime (ISO 8601 format)
-----------------------------------------------------------

pDay :: AmpParser Day
pDay = check (\lx -> case lx of { LexDate s -> Just s; _ -> Nothing }) <?> "iso 8601 Date"

pUTCTime :: AmpParser UTCTime
pUTCTime  = check (\lx -> case lx of { LexDateTime s -> Just s; _ -> Nothing }) <?> "iso 8601 DateTime"

-----------------------------------------------------------
-- Integers /float(Double)
-----------------------------------------------------------

pNumber :: Int -> AmpParser String
pNumber nr = match (LexDecimal nr) <|> match (LexHex nr) <|> match (LexOctal nr)

pNumeric :: AmpParser (Either Int Double)
pNumeric = (f DF.<$> pIsNeg CA.<*> pUnsignedNumeric) <?> "numerical value"
  where
     f :: Bool -> Either Int Double -> Either Int Double
     f isNeg b =
        case b of
          Left i  -> Left . (if isNeg then (0-) else id) $ i
          Right d -> Right. (if isNeg then (0-) else id) $ d

pIsNeg :: AmpParser Bool
pIsNeg = fromMaybe False
            DF.<$> pMaybe ( True  <$ pOperator "-" <|>
                            False <$ pOperator "+"
                          )
pUnsignedNumeric :: AmpParser (Either Int Double)
pUnsignedNumeric = check isNr
    where isNr (LexDecimal i) = Just (Left i)
          isNr (LexHex i)     = Just (Left i)
          isNr (LexOctal i)   = Just (Left i)
          isNr (LexFloat d)   = Just (Right d)
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
