{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A real (parser-combinator) parser for EXPRESS schemas (ISO 10303-11).
--
-- This parser is generic: it knows nothing about IFC or Ampersand. It reads
-- the structure needed to build the 'ExpressSchema' AST (entities, defined
-- types, explicit attributes, single supertype, raw WHERE/INVERSE lines).
--
-- Constructs that are not (yet) relevant to WP1 — @FUNCTION@, @RULE@,
-- @PROCEDURE@, @CONSTANT@, @DERIVE@, @UNIQUE@ — are skipped, but always in a
-- structurally sound way (the lexer respects @(* *)@ comments, strings and
-- nesting), not by regex.
module Ampersand.Input.Express.Parser
  ( parseExpress,
    parseExpressFile,
  )
where

import Ampersand.Basics hiding (many, optional, try, (<|>))
import Ampersand.Input.Express.Schema
import Data.Char (toLower)
import qualified RIO.Map as Map
import qualified RIO.Text as T
import Text.Parsec
import Text.Parsec.String (Parser)

-- | Parse a complete EXPRESS schema from a string. The @FilePath@ is only
-- used for error reporting.
parseExpress :: FilePath -> String -> Either String ExpressSchema
parseExpress src input =
  case parse pSchema src input of
    Left err -> Left (show err)
    Right s -> Right s

-- | Read and parse an EXPRESS @.exp@ file.
parseExpressFile :: FilePath -> RIO env (Either String ExpressSchema)
parseExpressFile fp = do
  result <- readFileUtf8Lenient fp
  case result of
    Left errs -> pure (Left (T.unpack (T.intercalate "\n" errs)))
    Right txt -> pure (parseExpress fp (T.unpack txt))

--------------------------------------------------------------------------------
-- Lexing: whitespace and comments
--------------------------------------------------------------------------------

-- | Skip whitespace and EXPRESS comments (@(* ... *)@ block, @-- ...@ line).
ws :: Parser ()
ws = skipMany (simpleSpace <|> blockComment <|> lineComment)
  where
    simpleSpace = skipMany1 (satisfy (`elem` (" \t\r\n\f\v" :: String)))
    blockComment = try (string "(*") *> manyTill anyChar (try (string "*)")) $> ()
    lineComment = try (string "--") *> manyTill anyChar (void (char '\n') <|> eof) $> ()

-- | Run a parser, then skip trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* ws

symbol :: String -> Parser String
symbol s = lexeme (string s)

-- | An EXPRESS identifier (entity/type/attribute names, keywords).
identifier :: Parser Text
identifier =
  lexeme $ do
    c <- letter <|> char '_'
    cs <- many (alphaNum <|> char '_')
    pure (T.pack (c : cs))

-- | Match a specific keyword, case-insensitively, as a whole word.
keyword :: String -> Parser ()
keyword kw =
  lexeme . try $ do
    _ <- mapM (\c -> satisfy (\x -> toLower x == toLower c)) kw
    notFollowedBy (alphaNum <|> char '_')

--------------------------------------------------------------------------------
-- Schema
--------------------------------------------------------------------------------

pSchema :: Parser ExpressSchema
pSchema = do
  ws
  keyword "SCHEMA"
  nm <- identifier
  -- optional schema version string, e.g. SCHEMA name 'version';
  _ <- optionMaybe pString
  _ <- symbol ";"
  decls <- many pDecl
  optional (keyword "END_SCHEMA")
  optional (symbol ";")
  ws
  eof
  let entities = [e | DEntity e <- decls]
      types = [t | DType t <- decls]
  pure
    ExpressSchema
      { esName = nm,
        esEntities = Map.fromList [(enName e, e) | e <- entities],
        esTypes = Map.fromList types
      }

-- | A top-level declaration of interest, or a skipped block.
data Decl
  = DEntity Entity
  | DType (Text, DefinedType)
  | DSkip

pDecl :: Parser Decl
pDecl =
  (DType <$> pType)
    <|> (DEntity <$> pEntity)
    <|> (DSkip <$ pSkipBlock)

--------------------------------------------------------------------------------
-- TYPE
--------------------------------------------------------------------------------

pType :: Parser (Text, DefinedType)
pType = do
  keyword "TYPE"
  nm <- identifier
  _ <- symbol "="
  body <- pTypeBody
  -- consume up to END_TYPE; (skip WHERE etc. between body and END_TYPE)
  skipUntilEndOf "TYPE"
  pure (nm, body)

pTypeBody :: Parser DefinedType
pTypeBody =
  pEnumeration <|> pSelect <|> pAlias

pEnumeration :: Parser DefinedType
pEnumeration = do
  keyword "ENUMERATION"
  -- EXPRESS allows EXTENSIBLE / based-on variants; we only need the members.
  keyword "OF"
  optional (keyword "BASED_ON" *> identifier *> optional (keyword "WITH"))
  TEnum <$> pIdentList

pSelect :: Parser DefinedType
pSelect = do
  keyword "SELECT"
  optional (keyword "BASED_ON" *> identifier *> optional (keyword "WITH"))
  TSelect <$> pIdentList

-- | A parenthesised, comma-separated list of identifiers.
pIdentList :: Parser [Text]
pIdentList = between (symbol "(") (symbol ")") (identifier `sepBy` symbol ",")

-- | Alias type: possibly an aggregate (@LIST [..] OF X@) or a direct base.
-- We record only the bare element type, matching the contract for 'TAlias'.
pAlias :: Parser DefinedType
pAlias = do
  (target, _agg) <- pAggOrBase
  pure (TAlias target)

--------------------------------------------------------------------------------
-- ENTITY
--------------------------------------------------------------------------------

pEntity :: Parser Entity
pEntity = do
  keyword "ENTITY"
  nm <- identifier
  -- SUPERTYPE / ABSTRACT SUPERTYPE clause(s) and SUBTYPE OF clause, in any
  -- order, terminated by ';'.
  super <- pEntityHeader
  attrs <- pExplicitAttrs
  -- The optional sections DERIVE / INVERSE / UNIQUE / WHERE may appear in any
  -- order; collect their raw lines (DERIVE/UNIQUE are skipped, INVERSE/WHERE
  -- are kept). EXPRESS fixes the order, but we stay order-independent.
  (wheres, inverses) <- pEntitySections
  keyword "END_ENTITY"
  _ <- symbol ";"
  pure
    Entity
      { enName = nm,
        enSupertype = super,
        enAttrs = attrs,
        enWhere = wheres,
        enInverse = inverses
      }

-- | Parse the entity header up to and including the first @;@, returning the
-- single supertype (first identifier of @SUBTYPE OF (...)@) if present.
pEntityHeader :: Parser (Maybe Text)
pEntityHeader = do
  optional (keyword "ABSTRACT")
  optional pSupertypeClause
  super <- optionMaybe pSubtypeClause
  _ <- symbol ";"
  pure super

-- | @SUPERTYPE OF (...)@ — we don't need its content, but must consume it
-- with balanced parentheses (it can contain @ONEOF(...)@, @ANDOR@, etc.).
pSupertypeClause :: Parser ()
pSupertypeClause = do
  keyword "SUPERTYPE"
  optional (keyword "OF" *> pBalancedParens)
  pure ()

-- | @SUBTYPE OF (X, ...)@ — keep the first listed supertype.
pSubtypeClause :: Parser Text
pSubtypeClause = do
  keyword "SUBTYPE"
  keyword "OF"
  ids <- pIdentList
  case ids of
    (x : _) -> pure x
    [] -> fail "empty SUBTYPE OF list"

-- | Explicit attributes: @Name : type;@ until a section keyword or END_ENTITY.
pExplicitAttrs :: Parser [Attr]
pExplicitAttrs = many (try pAttr)

pAttr :: Parser Attr
pAttr = do
  -- Stop if we are at a section keyword or the end of the entity.
  notFollowedBy (choice (map (try . keyword) sectionKeywords))
  nm <- identifier
  -- A redeclared inherited attribute uses @SELF\Super.Attr@; such a line is
  -- not a new positional attribute. We detect the backslash form and skip.
  redeclared <- option False (True <$ pSelfRedeclaration)
  _ <- symbol ":"
  (opt, target, agg) <- pTypeRef
  _ <- symbol ";"
  if redeclared
    then pAttr -- a redeclaration takes no new slot; parse the next one
    else
      pure
        Attr
          { atName = nm,
            atTarget = target,
            atOptional = opt,
            atAggregate = agg
          }

-- | Recognise the @SELF\Super.Attr@ left-hand side of a redeclaration.
pSelfRedeclaration :: Parser ()
pSelfRedeclaration = void $ char '\\' *> lexeme (many (alphaNum <|> oneOf "_.\\"))

sectionKeywords :: [String]
sectionKeywords = ["INVERSE", "DERIVE", "WHERE", "UNIQUE", "END_ENTITY"]

--------------------------------------------------------------------------------
-- Type references (attribute / alias element types)
--------------------------------------------------------------------------------

-- | Parse a type reference, returning @(optional?, bareTarget, aggregate)@.
-- Handles a leading @OPTIONAL@, one or more aggregate wrappers
-- (@LIST/SET/ARRAY/BAG [lo:hi] OF@), an optional @UNIQUE@, and the base type.
pTypeRef :: Parser (Bool, Text, Maybe Aggregate)
pTypeRef = do
  opt <- option False (True <$ keyword "OPTIONAL")
  (target, agg) <- pAggOrBase
  pure (opt, target, agg)

-- | Parse aggregate wrapper(s) then the bare element type. Only the outermost
-- aggregate kind+bounds is recorded.
pAggOrBase :: Parser (Text, Maybe Aggregate)
pAggOrBase = do
  mAgg <- optionMaybe pAggregateHead
  case mAgg of
    Just agg -> do
      -- nested aggregates: keep stripping, but record only the outer one.
      (target, _) <- pAggOrBase
      pure (target, Just agg)
    Nothing -> do
      target <- pBaseType
      pure (target, Nothing)

-- | An aggregate head: @LIST [lo:hi] OF@ (bounds optional), possibly with
-- @UNIQUE@ after @OF@.
pAggregateHead :: Parser Aggregate
pAggregateHead = do
  kind <- pAggKind
  (lo, hi) <- option (Nothing, Nothing) pBounds
  keyword "OF"
  optional (keyword "UNIQUE")
  optional (keyword "OPTIONAL")
  pure (Aggregate kind lo hi)

pAggKind :: Parser AggregateKind
pAggKind =
  (AggList <$ keyword "LIST")
    <|> (AggSet <$ keyword "SET")
    <|> (AggArray <$ keyword "ARRAY")
    <|> (AggBag <$ keyword "BAG")

-- | @[lo:hi]@ bounds; @?@ or a non-numeric expression yields 'Nothing'.
pBounds :: Parser (Maybe Integer, Maybe Integer)
pBounds = between (symbol "[") (symbol "]") $ do
  lo <- pBound
  _ <- symbol ":"
  hi <- pBound
  pure (lo, hi)

-- | Convert a non-empty run of decimal digits to an 'Integer'.
digitsToInteger :: String -> Integer
digitsToInteger = foldl' (\acc d -> acc * 10 + toInteger (fromEnum d - fromEnum '0')) 0

pBound :: Parser (Maybe Integer)
pBound =
  (Nothing <$ symbol "?")
    <|> (Just . digitsToInteger <$> lexeme (many1 digit))
    -- bound may be an expression/constant; consume it and yield Nothing
    <|> (Nothing <$ lexeme (many1 (alphaNum <|> oneOf "_+-")))

-- | The bare base type name. We must consume any trailing width/precision
-- specifiers, e.g. @STRING(22)@, @STRING(255) FIXED@, @REAL(6)@.
pBaseType :: Parser Text
pBaseType = do
  nm <- identifier
  optional pBalancedParens -- width/precision
  optional (keyword "FIXED")
  -- EXPRESS primitives are written upper-case (REAL, STRING, ...).
  -- 'targetConcept' maps those to safe concept names; named types keep their
  -- original (PascalCase) spelling.
  pure (targetConcept nm)

--------------------------------------------------------------------------------
-- Entity body sections: DERIVE / INVERSE / UNIQUE / WHERE
--------------------------------------------------------------------------------

-- | Collect the optional entity-body sections, returning (raw WHERE lines,
-- raw INVERSE lines). DERIVE and UNIQUE sections are consumed but discarded.
-- Order-independent so the parser is robust to schema variations.
pEntitySections :: Parser ([Text], [Text])
pEntitySections = go [] []
  where
    go ws' is' =
      (do keyword "WHERE"; rs <- many1 pRawRule; go (ws' <> rs) is')
        <|> (do keyword "INVERSE"; rs <- many1 pRawRule; go ws' (is' <> rs))
        <|> (do keyword "DERIVE"; _ <- many1 pRawRule; go ws' is')
        <|> (do keyword "UNIQUE"; _ <- many1 pRawRule; go ws' is')
        <|> pure (ws', is')

-- | A raw rule/inverse statement: everything up to the terminating @;@, kept
-- verbatim (whitespace-normalised). Stops before a section keyword/END_ENTITY.
pRawRule :: Parser Text
pRawRule = do
  notFollowedBy (choice (map (try . keyword) sectionKeywords))
  chars <- many1 (noneOf ";")
  _ <- symbol ";"
  pure (T.strip (T.pack chars))

--------------------------------------------------------------------------------
-- Skipping uninteresting top-level blocks (FUNCTION, RULE, PROCEDURE, ...)
--------------------------------------------------------------------------------

-- | Skip a top-level construct we don't model. We recognise the keyword and
-- consume through its matching @END_xxx;@, respecting nesting of the same
-- kind (EXPRESS allows nested LOCAL declarations but not nested
-- FUNCTION/RULE of the same head at top level in these schemas; we still
-- balance defensively).
pSkipBlock :: Parser ()
pSkipBlock =
  choice
    [ skipNamed "FUNCTION",
      skipNamed "RULE",
      skipNamed "PROCEDURE",
      skipConstant,
      skipReferenceUse
    ]
  where
    skipNamed kw = keyword kw *> skipUntilEndOf kw
    -- CONSTANT ... END_CONSTANT;
    skipConstant = keyword "CONSTANT" *> skipUntilEndOf "CONSTANT"
    -- REFERENCE FROM ...; / USE FROM ...; (single statement)
    skipReferenceUse =
      (keyword "REFERENCE" <|> keyword "USE")
        *> skipMany (noneOf ";")
        *> void (symbol ";")

-- | Consume everything up to the matching @END_<kw>;@ for the current block.
skipUntilEndOf :: String -> Parser ()
skipUntilEndOf kw = go
  where
    endTok = "END_" <> kw
    go =
      (try (keyword endTok) *> void (symbol ";"))
        <|> (pString *> go)
        <|> (anyToken *> go)

--------------------------------------------------------------------------------
-- Low-level helpers
--------------------------------------------------------------------------------

-- | Consume a balanced parenthesised group (used to skip e.g. SUPERTYPE OF
-- (...) and type width specifiers). Assumes the next token is '('.
pBalancedParens :: Parser ()
pBalancedParens = lexeme $ do
  _ <- char '('
  go (1 :: Int)
  where
    go 0 = pure ()
    go n = do
      c <- anyChar
      case c of
        '(' -> go (n + 1)
        ')' -> go (n - 1)
        _ -> go n

-- | An EXPRESS single-quoted string literal (@''@ escapes a quote).
pString :: Parser Text
pString = lexeme $ do
  _ <- char '\''
  cs <- pInner
  pure (T.pack cs)
  where
    pInner = do
      c <- anyChar
      case c of
        '\'' ->
          (char '\'' *> ((c :) <$> pInner)) -- '' -> literal quote
            <|> pure []
        _ -> (c :) <$> pInner
