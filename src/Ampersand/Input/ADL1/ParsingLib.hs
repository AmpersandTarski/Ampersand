{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Input.ADL1.ParsingLib
  ( AmpParser,
    pIsThere,
    optList,
    optSet,

    -- * Combinators
    (<?>),
    (<??>),
    sepBy,
    sepBy1,
    many,
    many1,
    opt,
    try,
    choice,
    pMaybe,

    -- * Positions
    currPos,
    posOf,
    valPosOf,

    -- * Basic parsers
    pConid,
    pDoubleQuotedString,
    pAmpersandMarkup,
    pVarid,
    pCrudString,

    -- * special parsers
    pAtomValInPopulation,
    Value (..),

    -- * Parsers for special symbols
    pComma,
    pParens,
    pBraces,
    pBrackets,
    pChevrons,

    -- * Keyword parsers
    pKey,

    -- * Operator parsers
    pOperator,
    pDash,
    pSemi,
    pColon,

    -- * Integer parsers
    pZero,
    pOne,
  )
where

import Ampersand.Basics hiding (many, try)
import Ampersand.Input.ADL1.FilePos (FilePos (..), Origin (..))
import Ampersand.Input.ADL1.LexerToken
  ( Lexeme (..),
    Token (..),
    lexemeText,
  )
import RIO.Char (isLower, isUpper, toUpper)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import RIO.Time
import Text.Parsec as P hiding
  ( satisfy,
    sepBy1,
    (<|>),
  )
import Text.Parsec.Pos (newPos)

-- | The Ampersand parser type
type AmpParser a =
  -- | The Parsec parser for a list of tokens with a file position.
  P.ParsecT [Token] FilePos Identity a

-----------------------------------------------------------
-- Useful functions
-----------------------------------------------------------

(<??>) :: AmpParser a -> AmpParser (a -> a) -> AmpParser a
p <??> q = (\x f -> f x) <$> p <*> (q `opt` id)

-- | Tries to apply the given parser and returns a parser with a boolean indicating whether it succeeded
pIsThere ::
  -- | The parser to run
  AmpParser a ->
  -- | The parser with the result
  AmpParser Bool
pIsThere p = (True <$ p) `opt` False

-- | Optionally applies a list parser, returning an empty list if it doesn't succeed
optList ::
  AmpParser [a] ->
  AmpParser [a]
optList p = p `opt` []

-- | Optionally applies a Set parser, returning an empty Set if it doesn't succeed
optSet ::
  AmpParser (Set.Set a) ->
  AmpParser (Set.Set a)
optSet p = p `opt` Set.empty

-- | Tries to apply the given parser and encapsulates the result in Maybe
pMaybe ::
  -- | The parser to apply
  AmpParser a ->
  -- | The result
  AmpParser (Maybe a)
pMaybe p = Just <$> p <|> P.parserReturn Nothing

-- | Tries to apply the given parser and returns the second argument if it doesn't succeed
opt ::
  -- | The parser to try
  AmpParser a ->
  -- | The item to return if the parser doesn't succeed
  a ->
  -- | The resulting parser
  AmpParser a
a `opt` b = P.option b a

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a non-empty list of values returned by @p@.
sepBy1 :: AmpParser a -> AmpParser b -> AmpParser (NE.NonEmpty a)
sepBy1 p sep = liftM2 (NE.:|) p (many (sep >> p))

-----------------------------------------------------------
-- Keywords & operators
-----------------------------------------------------------

-- | Take a keyword and return a parser for that keyword
pKey :: String -> AmpParser String
pKey key = match (LexKeyword key)

-- | Take an operator and return a parser for that operator
pOperator :: String -> AmpParser String
pOperator op = match (LexOperator op)

-- | a parser for a dash (-)
pDash :: AmpParser String
pDash = pOperator "-"

-- | a parser for a semicolon (;)
pSemi :: AmpParser String
pSemi = pOperator ";"

-- | a parser for a colon (:)
pColon :: AmpParser String
pColon = pOperator ":"

-----------------------------------------------------------
-- Token parsers
-----------------------------------------------------------

-- | given a predicate for a token, return a parser for tokens that comply to that predicate
check :: (Lexeme -> Maybe a) -> AmpParser a
check predicate = tokenPrim showTok nextPos matchTok
  where
    -- Token pretty-printing function
    showTok :: Token -> String
    showTok (Tok lx _) = show lx
    -- Next position calculating function
    nextPos :: SourcePos -> Token -> [Token] -> SourcePos
    nextPos pos _ [] = pos
    nextPos _ _ (Tok _ (FilePos file line col) : _) = newPos file line col
    matchTok (Tok l _) = predicate l

-- | a parser for a given @Lexeme@
match :: Lexeme -> AmpParser String
match lx = check (\lx' -> if lx == lx' then Just (lexemeText lx) else Nothing) <?> show lx

--- Conid ::= UpperChar AlphaNumericChar*
pConid :: AmpParser String
pConid =
  check
    ( \case
        LexSafeID s@(h : _) -> if isUpper h then Just s else Nothing
        _ -> Nothing
    )
    <?> "upper case identifier"

--- String ::= '"' Any* '"'
--- StringListSemi ::= String (';' String)*
pDoubleQuotedString :: AmpParser String
pDoubleQuotedString =
  check
    ( \case
        LexDubbleQuotedString s -> Just s
        _ -> Nothing
    )
    <?> "double quoted string"

--- Markup ::= '{+' Any* '+}'
pAmpersandMarkup :: AmpParser String
pAmpersandMarkup =
  check
    ( \case
        LexMarkup s -> Just s
        _ -> Nothing
    )
    <?> "markup"

--- Varid ::= LowerChar AlphaNumericChar*
pVarid :: AmpParser String
pVarid =
  check
    ( \case
        LexSafeID s@(h : _) -> if isLower h then Just s else Nothing
        _ -> Nothing
    )
    <?> "lower case identifier"

-- A non-empty string that contains only the the characters "crud" in any case (upper/lower), but each of them
-- at most once. The order of the characters is free.
pCrudString :: AmpParser String
pCrudString =
  check
    ( \case
        LexSafeID s -> testCrud s
        _ -> Nothing
    )
    <?> "crud definition"
  where
    testCrud s =
      if and $
        [ not (null s),
          L.nub caps == caps
        ]
          ++ map (`elem` ['C', 'R', 'U', 'D']) caps
        then Just s
        else Nothing
      where
        caps = map toUpper s

data Value
  = VRealString Text
  | VSingleton Text (Maybe Value)
  | VInt Int
  | VFloat Double
  | VBoolean Bool
  | VDateTime UTCTime
  | VDate Day

pAtomValInPopulation :: Bool -> AmpParser Value
-- An atomvalue can be lots of things. However, since it can be used in
-- a term (singleton term), an ambiguity might occur if we allow
-- negative numbers. The minus sign could be confused with a complement operator.
-- For this reason, we introduced a possibility to constrain the value.
-- constrained values have the constraint that a negative number is not allowed.
-- the user can lift the constraints by embeding the value in curly brackets. In
-- such a case, the user could use a negative number as a singleton term.
pAtomValInPopulation constrainsApply =
  VBoolean True <$ pKey "TRUE"
    <|> VBoolean False <$ pKey "FALSE"
    <|> VRealString <$> (T.pack <$> pDoubleQuotedString)
    <|> VDateTime <$> pUTCTime
    <|> VDate <$> pDay
    <|> fromNumeric <$> (if constrainsApply then pUnsignedNumeric else pNumeric) -- Motivated in issue #713
  where
    fromNumeric :: Either Int Double -> Value
    fromNumeric num = case num of
      Left i -> VInt i
      Right d -> VFloat d

-----------------------------------------------------------
-- Date / DateTime (ISO 8601 format)
-----------------------------------------------------------

pDay :: AmpParser Day
pDay =
  check
    ( \case
        LexDate s -> Just s
        _ -> Nothing
    )
    <?> "iso 8601 Date"

pUTCTime :: AmpParser UTCTime
pUTCTime =
  check
    ( \case
        LexDateTime s -> Just s
        _ -> Nothing
    )
    <?> "iso 8601 DateTime"

-----------------------------------------------------------
-- Integers /float(Double)
-----------------------------------------------------------

pNumber :: Int -> AmpParser String
pNumber nr = match (LexDecimal nr) <|> match (LexHex nr) <|> match (LexOctal nr)

pNumeric :: AmpParser (Either Int Double)
pNumeric = (f <$> pIsNeg <*> pUnsignedNumeric) <?> "numerical value"
  where
    f :: Bool -> Either Int Double -> Either Int Double
    f isNeg b =
      case b of
        Left i -> Left . (if isNeg then (0 -) else id) $ i
        Right d -> Right . (if isNeg then (0 -) else id) $ d

pIsNeg :: AmpParser Bool
pIsNeg =
  fromMaybe False
    <$> pMaybe
      ( True <$ pOperator "-"
          <|> False <$ pOperator "+"
      )

pUnsignedNumeric :: AmpParser (Either Int Double)
pUnsignedNumeric = check isNr
  where
    isNr (LexDecimal i) = Just (Left i)
    isNr (LexHex i) = Just (Left i)
    isNr (LexOctal i) = Just (Left i)
    isNr (LexFloat d) = Just (Right d)
    isNr _ = Nothing

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
pComma = pSpec ','

pParens :: AmpParser a -> AmpParser a
pParens parser = pSpec '(' *> parser <* pSpec ')'

pBraces :: AmpParser a -> AmpParser a
pBraces parser = pSpec '{' *> parser <* pSpec '}'

pBrackets :: AmpParser a -> AmpParser a
pBrackets parser = pSpec '[' *> parser <* pSpec ']'

pChevrons :: AmpParser a -> AmpParser a
pChevrons parser = pSpec '<' *> parser <* pSpec '>'

-----------------------------------------------------------
-- Token positioning
-----------------------------------------------------------

posOrigin :: Show a => a -> SourcePos -> Origin
posOrigin sym p = FileLoc (FilePos (sourceName p) (sourceLine p) (sourceColumn p)) (tshow sym)

currPos :: AmpParser Origin
currPos = posOf $ return ()

posOf :: Show a => AmpParser a -> AmpParser Origin
posOf parser = do pos <- getPosition; a <- parser; return (posOrigin a pos)

valPosOf :: Show a => AmpParser a -> AmpParser (a, Origin)
valPosOf parser = do pos <- getPosition; a <- parser; return (a, posOrigin a pos)
