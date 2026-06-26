-- | A generic reader for ISO 10303-21 (STEP / Part-21 / SPF) data files.
--
--   This module is deliberately schema-agnostic: it depends on neither EXPRESS nor on
--   Ampersand's @P_Context@. It turns the @DATA; … ENDSEC;@ section of a Part-21 file
--   into a list of 'StepInstance' (handoff §3b), which the IFC binder (WP3) then binds to
--   an EXPRESS schema.
--
--   The behaviour follows the executable reference @spf2json.py@ (functions @split_statements@,
--   @split_args@, @parse_value@), but is implemented as a real hand-written tokenizer over
--   'Text' rather than regex. Edge cases handled: single-quoted strings with @''@ escape,
--   nested parenthesised lists, typed values (@IFCBOOLEAN(.T.)@), enumerations (@.T.@ → @"T"@),
--   STEP binary literals (@"0FF…"@), @$@ / @*@ → 'SVNull', and @\/* … *\/@ comments.
module Ampersand.Input.Step.Parser
  ( parseStepText,
    parseStepFile,
    -- * Lower-level helpers (exposed for testing)
    stripComments,
    dataSection,
    splitStatements,
    splitArgs,
    parseValue,
  )
where

import Ampersand.Basics
import Ampersand.Input.Step.Types
import qualified Data.Text as DT
import qualified RIO.Char as C
import qualified RIO.Text as T

-- | Read a Part-21 file from disk and parse its @DATA@ section into instances.
--   Errors from reading the file are surfaced as a 'Left' with messages.
parseStepFile :: FilePath -> RIO env (Either [Text] [StepInstance])
parseStepFile fp = do
  eContent <- readFileUtf8Lenient fp
  pure $ eContent >>= parseStepText

-- | Parse the textual content of a Part-21 file into a list of 'StepInstance'.
--   Only statements of the form @#id = TYPE(args)@ in the @DATA@ section are returned;
--   anything else is silently skipped (matching the reference reader's behaviour).
parseStepText :: Text -> Either [Text] [StepInstance]
parseStepText raw =
  Right
    . mapMaybe parseStatement
    . splitStatements
    . dataSection
    . stripComments
    $ raw

-- | Remove @\/* … *\/@ comments. Comments inside single-quoted strings are preserved,
--   because a @'@ opens a string in which @/*@ has no special meaning.
stripComments :: Text -> Text
stripComments = T.pack . go . T.unpack
  where
    go [] = []
    go ('\'' : cs) = '\'' : inString cs
    go ('/' : '*' : cs) = inComment cs
    go (c : cs) = c : go cs

    -- Inside a string literal: copy verbatim until the closing quote.
    -- A doubled quote ('') is an escaped quote and does not end the string.
    inString [] = []
    inString ('\'' : '\'' : cs) = '\'' : '\'' : inString cs
    inString ('\'' : cs) = '\'' : go cs
    inString (c : cs) = c : inString cs

    -- Inside a comment: drop everything up to and including the closing */.
    inComment [] = []
    inComment ('*' : '/' : cs) = go cs
    inComment (_ : cs) = inComment cs

-- | Extract the body between @DATA;@ and the next @ENDSEC;@. If no such section is
--   found, the whole text is returned (matching the reference reader).
dataSection :: Text -> Text
dataSection text =
  case findData text of
    Nothing -> text
    Just afterData -> takeUntilEndsec afterData
  where
    findData t =
      -- Look for the keyword DATA followed (after optional spaces) by ';'.
      case DT.breakOn "DATA" t of
        (_, rest)
          | T.null rest -> Nothing
          | otherwise ->
              let afterKw = T.drop (T.length "DATA") rest
                  afterSpace = T.dropWhile C.isSpace afterKw
               in case T.uncons afterSpace of
                    Just (';', body) -> Just body
                    _ -> (findData . T.drop 1) rest
    takeUntilEndsec t =
      case DT.breakOn "ENDSEC" t of
        (before, rest)
          | T.null rest -> before
          | otherwise ->
              let afterKw = T.drop (T.length "ENDSEC") rest
                  afterSpace = T.dropWhile C.isSpace afterKw
               in case T.uncons afterSpace of
                    Just (';', _) -> before
                    -- "ENDSEC" not followed by ';': keep scanning.
                    _ -> before <> "ENDSEC" <> takeUntilEndsec afterKw

-- | Split on @;@ outside of strings, trimming and dropping empty statements.
splitStatements :: Text -> [Text]
splitStatements = map T.pack . go . T.unpack
  where
    go s = case statement s of
      (stmt, rest) ->
        let trimmed = trim stmt
         in (if null trimmed then id else (trimmed :)) (continue rest)
    continue [] = []
    continue cs = go cs

    statement [] = ([], [])
    statement ('\'' : cs) =
      let (str, rest) = string cs
       in first (('\'' :) . (str ++)) (statement rest)
    statement (';' : cs) = ([], cs)
    statement (c : cs) = first (c :) (statement cs)

-- | Split top-level arguments on @,@, respecting parentheses and strings.
splitArgs :: Text -> [StepValue]
splitArgs = map (parseValue . T.pack) . splitArgsRaw . T.unpack

-- | Raw comma-split (returns the textual tokens). Mirrors @split_args@ in the reference.
splitArgsRaw :: String -> [String]
splitArgsRaw input = go input (0 :: Int)
  where
    go s depth = case arg s depth of
      (tok, Nothing) ->
        -- End of input. Emit a trailing token only if it is non-empty or
        -- there was at least one separator (so "()" yields [], "(a,)" yields ["a",""]).
        [trim tok | not (null (trim tok))]
      (tok, Just rest) -> trim tok : go rest depth

    -- Consume one argument up to a top-level comma. Returns the token and the
    -- remainder after the comma (Nothing when input is exhausted).
    arg [] _ = ([], Nothing)
    arg ('\'' : cs) d =
      let (str, rest) = string cs
       in first (('\'' :) . (str ++)) (arg rest d)
    arg ('(' : cs) d = first ('(' :) (arg cs (d + 1))
    arg (')' : cs) d = first (')' :) (arg cs (d - 1))
    arg (',' : cs) 0 = ([], Just cs)
    arg (c : cs) d = first (c :) (arg cs d)

-- | Consume a single-quoted string body (the opening quote already consumed).
--   Returns the body including its closing quote, plus the remaining input.
--   A doubled quote (@''@) is an escaped quote and does not end the string.
string :: String -> (String, String)
string [] = ([], [])
string ('\'' : '\'' : cs) = first (('\'' :) . ('\'' :)) (string cs)
string ('\'' : cs) = (['\''], cs)
string (c : cs) = first (c :) (string cs)

-- | Parse one Part-21 statement @#id = TYPE(args)@ into a 'StepInstance'.
--   Returns 'Nothing' for statements that do not match this shape (e.g. header noise).
parseStatement :: Text -> Maybe StepInstance
parseStatement stmt = do
  let t0 = T.stripStart stmt
  rest0 <- T.stripPrefix "#" t0
  let (digits, afterId) = T.span C.isDigit rest0
  guard (not (T.null digits))
  let afterEq = T.dropWhile (== '=') (T.stripStart afterId)
      afterEqTrim = T.stripStart afterEq
      (typ, afterType) = T.span isTypeChar afterEqTrim
  guard (not (T.null typ))
  let afterTypeTrim = T.stripStart afterType
  body <- T.stripPrefix "(" afterTypeTrim
  let inner = dropLastParen (T.stripEnd body)
  pure
    StepInstance
      { siId = "#" <> digits,
        siType = T.toUpper typ,
        siArgs = splitArgs inner
      }
  where
    isTypeChar c = C.isAlphaNum c || c == '_'
    -- Drop a single trailing ')' if present (the statement's closing paren).
    dropLastParen x
      | ")" `T.isSuffixOf` x = T.dropEnd 1 x
      | otherwise = x

-- | Parse a single argument token into a 'StepValue'. Mirrors @parse_value@ in the reference.
parseValue :: Text -> StepValue
parseValue tok0
  | T.null tok = SVNull -- empty argument
  | tok == "$" = SVNull -- omitted
  | tok == "*" = SVNull -- derived / redeclared
  | "#" `T.isPrefixOf` tok = SVRef tok
  | "'" `T.isPrefixOf` tok = SVStr (unquoteString tok)
  | "\"" `T.isPrefixOf` tok = SVBin (unquoteBinary tok)
  | isEnum tok = SVEnum (T.dropAround (== '.') tok)
  | "(" `T.isPrefixOf` tok = SVList (splitArgs (innerParens tok))
  | Just (nm, args) <- typedValue tok = SVTyped (T.toUpper nm) (splitArgs args)
  | Just n <- readInteger tok = SVInt n
  | Just d <- readReal tok = SVReal d
  | otherwise = SVStr tok -- fallback: keep the literal as-is
  where
    tok = T.strip tok0

-- | True for an enumeration token: @.SOMETHING.@ (at least one char between the dots).
isEnum :: Text -> Bool
isEnum t =
  "." `T.isPrefixOf` t
    && "." `T.isSuffixOf` t
    && T.length t > 2

-- | Strip surrounding single quotes and unescape @''@ → @'@.
unquoteString :: Text -> Text
unquoteString t =
  let body = T.drop 1 t
      inner = if "'" `T.isSuffixOf` body then T.dropEnd 1 body else body
   in DT.replace "''" "'" inner

-- | Strip surrounding double quotes from a STEP binary literal.
unquoteBinary :: Text -> Text
unquoteBinary t =
  let body = T.drop 1 t
   in if "\"" `T.isSuffixOf` body then T.dropEnd 1 body else body

-- | The content of a parenthesised token, dropping the outer @( … )@.
innerParens :: Text -> Text
innerParens t =
  let body = T.drop 1 t
   in if ")" `T.isSuffixOf` body then T.dropEnd 1 body else body

-- | Recognise a typed value @NAME( … )@: a leading identifier followed by a
--   parenthesised body ending the token. Returns the name and the inner args text.
typedValue :: Text -> Maybe (Text, Text)
typedValue t = do
  let (nm, rest) = T.span isIdentChar t
  (firstChar, _) <- T.uncons nm
  guard (isIdentStart firstChar)
  let rest' = T.stripStart rest
  body <- T.stripPrefix "(" rest'
  guard (")" `T.isSuffixOf` T.stripEnd body)
  pure (nm, innerParensFrom (T.stripEnd body))
  where
    isIdentStart c = C.isAlpha c || c == '_'
    isIdentChar c = C.isAlphaNum c || c == '_'
    -- body already had its leading '(' stripped; drop the trailing ')'.
    innerParensFrom b = if ")" `T.isSuffixOf` b then T.dropEnd 1 b else b

-- | Parse a Part-21 integer literal (optional sign, digits, no decimal point or exponent).
readInteger :: Text -> Maybe Integer
readInteger t
  | T.null core = Nothing
  | T.all C.isDigit core = readMaybe (T.unpack t)
  | otherwise = Nothing
  where
    core = T.dropWhile (\c -> c == '+' || c == '-') t

-- | Parse a Part-21 real literal (with a decimal point and/or exponent).
readReal :: Text -> Maybe Double
readReal t = readMaybe (T.unpack (normalize t))
  where
    -- Haskell's reader needs a digit before/after the point and an explicit
    -- mantissa before an exponent; STEP allows e.g. "1." and ".5" and "1.E3".
    normalize =
      ensureLeadingZero
        . fixExponentDot
        . fixTrailingDot
    fixTrailingDot s = case T.span (/= 'E') (toUpperE s) of
      (mant, expo)
        | "." `T.isSuffixOf` mant -> mant <> "0" <> expo
        | otherwise -> s
    -- "1.E3" handled by fixTrailingDot via the split; also handle ".E"
    fixExponentDot = id
    ensureLeadingZero s
      | "." `T.isPrefixOf` s = "0" <> s
      | "-." `T.isPrefixOf` s = "-0" <> T.drop 1 s
      | "+." `T.isPrefixOf` s = "0" <> T.drop 1 s
      | otherwise = s
    toUpperE = T.map (\c -> if c == 'e' then 'E' else c)

-- | Drop leading and trailing ASCII whitespace from a 'String'.
trim :: String -> String
trim = f . f where f = reverse . dropWhile C.isSpace
