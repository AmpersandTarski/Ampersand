{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Ampersand.Input.ADL1.ParsingLib
  ( AmpParser,
    ParserState,
    pIsThere,
    optList,
    optSet,

    -- * ParserState manipulators
    initialParserState,
    addParserWarning,
    parseMessages,

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
    unexpected,

    -- * Positions
    currPos,
    posOf,
    valPosOf,

    -- * Basic parsers
    pAmpersandMarkup,
    pCrudString,
    pDoubleQuotedString,
    pDoubleQuotedString1,
    pName,
    pSingleWord,

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
    pAnyKeyWord,
    pKeyWordWithFilter,

    -- * Operator parsers
    pOperator,
    pDash,
    pSemi,
    pColon,

    -- * Integer parsers
    pZero,
    pOne,

    -- * Runners
    runParser,
  )
where

import Ampersand.Basics hiding (many, try)
import Ampersand.Input.ADL1.CtxError
import Ampersand.Input.ADL1.FilePos (FilePos (..), Origin (..))
import Ampersand.Input.ADL1.Lexer
  ( Token,
    keywords,
    lexer,
  )
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
  ( runParser,
    satisfy,
    sepBy1,
    (<|>),
  )
import Text.Parsec.Pos (newPos)

-- | The Ampersand parser type
type AmpParser a =
  -- | The Parsec parser for a list of tokens with a file position.
  P.ParsecT [Token] ParserState Identity a

-- | the state of the parser. Note: the position in the text is managed by the lexer: Every Token has a position in it
data ParserState = ParserState
  { parseMessages :: ![(Origin, Text)]
  }

initialParserState :: ParserState
initialParserState = ParserState []

addParserWarning :: Origin -> Text -> AmpParser ()
addParserWarning orig msg = modifyState update
  where
    update :: ParserState -> ParserState
    update (ParserState xs) = ParserState (xs <> [(orig, msg)])

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
pKey :: Text1 -> AmpParser Text1
pKey key = toText1Unsafe <$> match (LexKeyword key)

-- | Take an operator and return a parser for that operator
pOperator :: Text1 -> AmpParser Text1
pOperator op = toText1Unsafe <$> match (LexOperator op)

-- | a parser for a dash (-)
pDash :: AmpParser Text1
pDash = pOperator (Text1 '-' mempty)

-- | a parser for a plus (+)
pPlus :: AmpParser Text1
pPlus = pOperator (Text1 '+' mempty)

-- | a parser for a semicolon (;)
pSemi :: AmpParser Text1
pSemi = pOperator (Text1 ';' mempty)

-- | a parser for a colon (:)
pColon :: AmpParser Text1
pColon = pOperator (Text1 ':' mempty)

-- | a parser for a dot (.)
pDot :: AmpParser Text1
pDot = pOperator (Text1 '.' mempty)

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
match :: Lexeme -> AmpParser Text
match lx = check (\lx' -> if lx == lx' then Just (lexemeText lx) else Nothing) <?> show lx

--- String ::= '"' Any* '"'
--- StringListSemi ::= String (';' String)*
pDoubleQuotedString :: AmpParser Text
pDoubleQuotedString =
  check
    ( \case
        LexDubbleQuotedString s -> Just s
        _ -> Nothing
    )
    <?> "double quoted string"

pDoubleQuotedString1 :: AmpParser Text1
pDoubleQuotedString1 =
  check
    ( \case
        LexDubbleQuotedString t -> case T.uncons t of
          Nothing -> Nothing
          Just _ -> Just (toText1Unsafe t)
        _ -> Nothing
    )
    <?> "double quoted non-empty string"

--- Markup ::= '{+' Any* '+}'
pAmpersandMarkup :: AmpParser Text
pAmpersandMarkup =
  check
    ( \case
        LexMarkup s -> Just s
        _ -> Nothing
    )
    <?> "markup"

--- Conid ::= UpperChar AlphaNumericChar*
pUpperCaseID :: AmpParser Text1
pUpperCaseID =
  pKeyWordWithFilter upper'
    <|> ( check
            ( \case
                LexSafeID t1@(Text1 h _) ->
                  if isUpper h then Just t1 else Nothing
                _ -> Nothing
            )
            <?> "upper case identifier"
        )
  where
    upper' :: Text1 -> Bool
    upper' (Text1 h _) = isUpper h

--- Varid ::= LowerChar AlphaNumericChar*
pLowerCaseID :: AmpParser Text1
pLowerCaseID =
  check
    ( \case
        LexSafeID t1@(Text1 h _) ->
          if isLower h then Just t1 else Nothing
        _ -> Nothing
    )
    <?> "lower case identifier"

--- ADLid ::= Varid | Conid
--- ADLidList ::= ADLid (',' ADLid)*
--- ADLidListList ::= ADLid+ (',' ADLid+)*
pUnrestrictedID :: AmpParser Text1
pUnrestrictedID = pLowerCaseID <|> pUpperCaseID <|> pAnyKeyWord

pName :: NameType -> AmpParser Name
pName typ =
  build
    <$> many namespacePart
    <*> localNamePart
  where
    build :: [NamePart] -> NamePart -> Name
    build ns nm =
      mkName typ . NE.reverse $ nm NE.:| reverse ns
    localNamePart :: AmpParser NamePart
    localNamePart =
      buildNamePart
        <$> currPos
        <*> case typ of
          ConceptName -> pUpperCaseID
          ContextName -> pUpperCaseID
          IdentName -> pUnrestrictedID
          InterfaceName -> pUnrestrictedID
          PatternName -> pUpperCaseID
          PropertyName -> pUpperCaseID
          RelationName -> pLowerCaseID
          RoleName -> pUnrestrictedID
          RuleName -> pUnrestrictedID
          SqlAttributeName -> pUnrestrictedID
          SqlTableName -> pUnrestrictedID
          ViewName -> pUnrestrictedID
    namespacePart :: AmpParser NamePart
    namespacePart =
      try
        $ buildNamePart
        <$> currPos
        <*> pUnrestrictedID
        <* pDot
    buildNamePart :: Origin -> Text1 -> NamePart
    buildNamePart orig txt1 = case toNamePart1 txt1 of
      Nothing -> fatal $ "An unrestrictedID should be a valid namepart, but it isn't: " <> tshow txt1 <> "\n   " <> tshow orig
      Just np -> np

pAnyKeyWord :: AmpParser Text1
pAnyKeyWord = case map pKey keywords of
  [] -> fatal "There seem to be no keywords at all!"
  h : tl -> foldr (<|>) h tl

pKeyWordWithFilter :: (Text1 -> Bool) -> AmpParser Text1
pKeyWordWithFilter p = case map pKey . filter p $ keywords of
  [] -> fatal "We should have keywords that match the filter."
  h : tl -> foldr (<|>) h tl

pSingleWord :: AmpParser Text1
pSingleWord =
  check
    ( \case
        LexSafeID s -> Just s
        _ -> Nothing
    )
    <?> "single word identifier"

-- A non-empty string that contains only the the characters "crud" in any case (upper/lower), but each of them
-- at most once. The order of the characters is free.
pCrudString :: AmpParser Text1
pCrudString =
  check
    ( \case
        LexSafeID s -> testCrud s
        _ -> Nothing
    )
    <?> "crud definition"
  where
    testCrud :: Text1 -> Maybe Text1
    testCrud (Text1 h tl) =
      if and
        $ [ not (null s),
            L.nub caps == caps
          ]
        ++ map (`elem` ['C', 'R', 'U', 'D']) caps
        then Just (Text1 h tl)
        else Nothing
      where
        s = h : T.unpack tl
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
  VBoolean True
    <$ pKey (toText1Unsafe "TRUE")
    <|> VBoolean False
    <$ pKey (toText1Unsafe "FALSE")
    <|> VRealString
    <$> pDoubleQuotedString
    <|> VDateTime
    <$> pUTCTime
    <|> VDate
    <$> pDay
    <|> fromNumeric
    <$> (if constrainsApply then pUnsignedNumeric else pNumeric) -- Motivated in issue #713
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

pNumber :: Int -> AmpParser Text
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
      ( True
          <$ pDash
          <|> False
          <$ pPlus
      )

pUnsignedNumeric :: AmpParser (Either Int Double)
pUnsignedNumeric = check isNr
  where
    isNr (LexDecimal i) = Just (Left i)
    isNr (LexHex i) = Just (Left i)
    isNr (LexOctal i) = Just (Left i)
    isNr (LexFloat d) = Just (Right d)
    isNr _ = Nothing

pZero :: AmpParser Text
pZero = pNumber 0

pOne :: AmpParser Text
pOne = pNumber 1

-----------------------------------------------------------
-- Special characters
-----------------------------------------------------------

-- matches special characters
pSpec :: Char -> AmpParser Text1
pSpec sym = toText1Unsafe <$> match (LexSymbol sym)

pComma :: AmpParser Text1
pComma = pSpec ','

pParens :: AmpParser a -> AmpParser a
pParens parser = pSpec '(' *> parser <* pSpec ')'

pBraces :: AmpParser a -> AmpParser a
pBraces parser = pSpec '{' *> parser <* pSpec '}'

pBrackets :: AmpParser a -> AmpParser a
pBrackets parser = pSpec '[' *> parser <* pSpec ']'

pChevrons :: AmpParser a -> AmpParser a
pChevrons parser =
  pOperator (toText1Unsafe "<")
    *> parser
    <* pOperator (toText1Unsafe ">")

-----------------------------------------------------------
-- Token positioning
-----------------------------------------------------------

posOrigin :: (Show a) => a -> SourcePos -> Origin
posOrigin sym p = FileLoc (FilePos (sourceName p) (sourceLine p) (sourceColumn p)) (tshow sym)

currPos :: AmpParser Origin
currPos = posOf $ return ()

posOf :: (Show a) => AmpParser a -> AmpParser Origin
posOf parser = do pos <- getPosition; a <- parser; return (posOrigin a pos)

valPosOf :: (Show a) => AmpParser a -> AmpParser (a, Origin)
valPosOf parser = do pos <- getPosition; a <- parser; return (a, posOrigin a pos)

ampParse :: AmpParser a -> FilePath -> [Token] -> Guarded a
ampParse p fn ts =
  -- runP :: Parsec s u a -> u -> FilePath -> s -> Either ParseError a
  case runP p initialParserState fn ts of
    -- TODO: Add language support to the parser errors
    Left err -> Errors $ pure $ PE err
    Right a -> pure a

-- | Runs the given parser
runParser ::
  -- | The parser to run
  AmpParser a ->
  -- | Name of the file (for error messages)
  FilePath ->
  -- | Text to parse
  Text ->
  -- | The result
  Guarded a
runParser parser filename input =
  case lexer filename (T.unpack input) of
    Left err -> Errors . pure $ lexerError2CtxError err
    Right (tokens', lexerWarnings) ->
      addWarnings
        (map lexerWarning2Warning lexerWarnings)
        (ampParse parser filename tokens')
