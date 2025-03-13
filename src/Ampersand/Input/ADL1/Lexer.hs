{-# LANGUAGE ScopedTypeVariables #-}

module Ampersand.Input.ADL1.Lexer
  ( keywords,
    operators,
    symbols,
    lexer,
    -- LexerMessage
    LexerError (..),
    LexerErrorInfo (..),
    LexerWarning (..),
    LexerWarningInfo (..),
    keepOneTabWarning,
    showLexerErrorInfo,
    showLexerWarningInfo,
    -- LexerToken
    Token (..),
    Lexeme (..),
    lexemeText,
    initPos,
    FilePos (..),
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.LexerMessage
import Ampersand.Input.ADL1.LexerMonad
import Ampersand.Input.ADL1.LexerToken
import Numeric
import RIO.Char hiding (isSymbol)
import qualified RIO.Char.Partial as Partial (chr)
import qualified RIO.List as L
import qualified RIO.Text as T
import RIO.Time

-- | Retrieves a list of keywords accepted by the Ampersand language
keywords ::
  -- | The keywords
  [Text1]
keywords =
  fmap toText1Unsafe
    . L.nub
    $ [ "CONTEXT",
        "ENDCONTEXT",
        "IN"
      ]
    ++ [ T.toUpper $ tshow x | x :: Lang <- [minBound ..]
       ]
    ++ [ "INCLUDE",
         "META",
         "PATTERN",
         "ENDPATTERN",
         "CONCEPT",
         "LABEL",
         -- Keywords for Relation-statements
         "RELATION",
         "PRAGMA",
         "MEANING",
         "ASY",
         "INJ",
         "BIJ",
         "IRF",
         "RFX",
         "SUR",
         "SYM",
         "TOT",
         "TRN",
         "UNI",
         "MAP",
         "PROP",
         "VALUE",
         "EVALPHP",
         "POPULATION",
         "CONTAINS",
         -- Keywords for rules
         "RULE",
         "MESSAGE",
         "VIOLATION",
         "TXT"
       ]
    ++ [ T.toUpper $ tshow x | x :: SrcOrTgt <- [minBound ..]
       ]
    ++ [ "I",
         "V",
         "ONE",
         "ROLE",
         "MAINTAINS",
         -- Keywords for purposes
         "PURPOSE",
         "REF"
       ]
    ++ [ T.toUpper $ tshow x | x :: PandocFormat <- [minBound ..]
       ]
    ++
    -- Keywords for interfaces
    [ "INTERFACE",
      "FOR",
      "LINKTO",
      "API",
      "BOX",
      -- Keywords for identitys
      "IDENT",
      -- Keywords for views
      "VIEW",
      "ENDVIEW",
      "DEFAULT",
      "TEMPLATE",
      "HTML",
      -- Keywords for generalisations:
      "CLASSIFY",
      "ISA",
      "IS",
      -- Keywords for TType:
      "REPRESENT",
      "TYPE"
    ]
    ++ [ T.toUpper $ tshow tt | tt :: TType <- [minBound ..], tt /= TypeOfOne
       ]
    ++
    -- Keywords for values of atoms:
    [ "TRUE",
      "FALSE", -- for booleans
      -- Experimental stuff:
      "SERVICE",
      -- Enforce statement:
      "ENFORCE" -- TODO: "BY", "INVARIANT" (See issue #1204)
    ]

-- | Retrieves a list of operators accepted by the Ampersand language
operators ::
  -- | The operators
  [Text1]
operators =
  toText1Unsafe
    <$> [ "|-",
          "-",
          "->",
          "<-",
          "=",
          "~",
          "+",
          "*",
          ";",
          "!",
          "#",
          "::",
          ":",
          "\\/",
          "/\\",
          "\\",
          "/",
          "<>",
          "..",
          ".",
          ":=",
          ">:",
          ":<",
          ">",
          "<",
          ">=",
          "<="
        ]

-- | Retrieves the list of symbols accepted by the Ampersand language
symbols ::
  -- | The list of symbol characters / [Char]
  String
symbols = "()[],{}"

-- | Runs the lexer
lexer ::
  -- | The file name, used for error messages
  FilePath ->
  -- | The content of the file
  String ->
  -- | Either an error or a list of tokens and warnings
  Either LexerError ([Token], [LexerWarning])
lexer file input = runLexerMonad file (mainLexer (initPos file) input)

-----------------------------------------------------------
-- Help functions
-----------------------------------------------------------

skipLine :: String -> String
skipLine = dropWhile (/= '\n')

-----------------------------------------------------------
-- Lexer definition
-----------------------------------------------------------

type Lexer = FilePos -> String -> LexerMonad [Token]

mainLexer :: Lexer
-----------------------------------------------------------
-- Removing unnecessary text artifacts (comment, spaces,...)
-----------------------------------------------------------

mainLexer _ [] = return []
mainLexer p ('-' : '-' : s) = mainLexer p (skipLine s)
mainLexer p (c : s)
  | isSpace c =
      let (spc, next) = span isSpaceNoTab s
          isSpaceNoTab x = isSpace x && (not . isTab) x
          isTab = ('\t' ==)
       in do
            when (isTab c) (lexerWarning TabCharacter p)
            mainLexer (foldl' updatePos p (c : spc)) next
mainLexer p ('{' : '-' : s) = lexNestComment mainLexer (addPos 2 p) s
mainLexer p ('{' : '+' : s) = lexMarkup mainLexer (addPos 2 p) s
mainLexer p ('"' : ss) =
  let (s, swidth, rest) = scanString ss
   in case rest of
        ('"' : xs) -> returnToken (LexDubbleQuotedString (T.pack s)) p mainLexer (addPos (swidth + 2) p) xs
        _ -> lexerError (NonTerminatedString s) p
-----------------------------------------------------------
-- looking for keywords - operators - special chars
-----------------------------------------------------------

mainLexer p cs@(c : s)
  | isSafeIdChar True c =
      let (name', p', s') = scanIdent (addPos 1 p) s
          name'' = Text1 c $ T.pack name'
          tokt
            | iskeyword name'' = LexKeyword name''
            | otherwise = LexSafeID name''
       in returnToken tokt p mainLexer p' s'
  | prefixIsOperator (T.pack cs) =
      let (name', s') = getOp cs
       in returnToken (LexOperator name') p mainLexer (foldl' updatePos p (T.unpack . text1ToText $ name')) (T.unpack s')
  | isSymbol c = returnToken (LexSymbol c) p mainLexer (addPos 1 p) s
  | isDigit c =
      case getDateTime cs of
        Just (Right (tk, _, width, s')) -> returnToken tk p mainLexer (addPos width p) s'
        Just (Left msg) -> lexerError msg p
        Nothing ->
          case getDate cs of
            Just (tk, _, width, s') -> returnToken tk p mainLexer (addPos width p) s'
            Nothing ->
              let (tk, _, width, s') = getNumber cs
               in returnToken tk p mainLexer (addPos width p) s'
  -- Ignore unexpected characters in the beginning of the file because of the UTF-8 BOM marker.
  -- TODO: Find out the right way of handling the BOM marker.
  | beginFile p = do lexerWarning UtfChar p; mainLexer p s
  | otherwise = lexerError (UnexpectedChar c) p
  where
    beginFile (FilePos _ 1 1) = True
    beginFile _ = False
    -- Finds the longest prefix of cs occurring in keywordsops
    getOp :: String -> (Text1, Text)
    getOp cs' = case s' of
      [] -> (fatal "prefixIsOperator should have been tested in the only place where getOp is used.", T.pack str)
      (h : tl) -> (Text1 h $ T.pack tl, T.pack str)
      where
        (s', str) = findOper (map (T.unpack . text1ToText) operators) cs' ""
        findOper :: [String] -> String -> String -> (String, String)
        findOper [] _ _ = ("", cs')
        findOper _ [] op = (op, [])
        findOper ops (c' : rest) op =
          if null found
            then (op, c' : rest)
            else findOper found rest (op ++ [c'])
          where
            found = [s'' | o : s'' <- ops, c' == o]

-----------------------------------------------------------
-----------------------------------------------------------
-- Supporting functions for MainLexer
-----------------------------------------------------------
-----------------------------------------------------------

-----------------------------------------------------------
-- Check on keywords - operators - special chars
-----------------------------------------------------------

iskeyword :: Text1 -> Bool
iskeyword str = str `elem` keywords

isSymbol :: Char -> Bool
isSymbol c = c `elem` symbols

prefixIsOperator :: Text -> Bool
prefixIsOperator str = any ((`T.isPrefixOf` str) . text1ToText) operators

-- scan ident receives a file position and the resting contents, returning the scanned identifier, the file location and the remaining contents.
scanIdent :: FilePos -> String -> (String, FilePos, String)
scanIdent p s =
  let (nm, rest) = span (isSafeIdChar False) s
   in (nm, addPos (length nm) p, rest)

-----------------------------------------------------------
-- String clean-up functions / comments
-----------------------------------------------------------

lexNestComment :: Lexer -> Lexer
lexNestComment c p ('-' : '}' : s) = c (addPos 2 p) s
lexNestComment c p ('{' : '-' : s) = lexNestComment (lexNestComment c) (addPos 2 p) s
lexNestComment c p (x : s) = lexNestComment c (updatePos p x) s
lexNestComment _ p [] = lexerError UnterminatedComment p

lexMarkup :: Lexer -> Lexer
lexMarkup = lexMarkup' ""
  where
    lexMarkup' :: Text -> t -> FilePos -> [Char] -> LexerMonad [Token]
    lexMarkup' str _ p ('+' : '}' : s) = returnToken (LexMarkup str) p mainLexer (addPos 2 p) s
    lexMarkup' str c p (x : s) = lexMarkup' (str <> T.singleton x) c (updatePos p x) s
    lexMarkup' _ _ p [] = lexerError UnterminatedMarkup p

-----------------------------------------------------------
-- iso 8601 date / time
-----------------------------------------------------------
-- Returns tuple with the parsed lexeme, the UTCTime, the amount of read characters and the rest of the text
getDateTime :: String -> Maybe (Either LexerErrorInfo (Lexeme, UTCTime, Int, String))
getDateTime cs =
  case getDate cs of
    Nothing -> Nothing
    Just (_, day, ld, rd) -> case getTime rd of
      Nothing -> case rd of
        'T' : _ -> Just . Left $ ProblematicISO8601DateTime
        _ -> getDateTime' cs -- Here we try the ohter notation of time
      Just (timeOfDay, tzoneOffset, lt, rt) ->
        let ucttime = addUTCTime tzoneOffset (UTCTime day timeOfDay)
         in Just . Right $ (LexDateTime ucttime, ucttime, ld + lt, rt)

getTime :: String -> Maybe (DiffTime, NominalDiffTime, Int, String)
getTime cs =
  case cs of
    'T' : h1 : h2 : ':' : m1 : m2 : rest ->
      if all isDigit [h1, h2, m1, m2]
        then
          let hours = case getNumber [h1, h2] of
                (_, Left val, _, _) -> val
                _ -> fatal "Impossible, for h1 and h2 are digits"
              minutes = case getNumber [m1, m2] of
                (_, Left val, _, _) -> val
                _ -> fatal "Impossible, for m1 and m2 are digits"
              (seconds, ls, rs) = getSeconds rest
           in case getTZD rs of
                Nothing -> Nothing
                Just (offset, lo, ro) ->
                  if hours < 24 && minutes < 60 && seconds < 60
                    then
                      Just
                        ( fromRational
                            . toRational
                            $ ( fromIntegral hours
                                  * 60
                                  + fromIntegral minutes
                              )
                            * 60
                            + seconds,
                          offset,
                          1 + 5 + ls + lo,
                          ro
                        )
                    else Nothing
        else Nothing
    _ -> Nothing

getSeconds :: String -> (Float, Int, String)
getSeconds cs =
  case cs of
    (':' : s1 : s2 : rest) ->
      if all isDigit [s1, s2]
        then
          let (fraction, lf, rf) = getFraction (s1 : s2 : rest)
           in (fraction, 1 + lf, rf)
        else (0, 0, cs)
    _ -> (0, 0, cs)

getFraction :: String -> (Float, Int, String)
getFraction cs =
  case readFloat cs of
    [(a, str)] -> (a, length cs - length str, str) -- TODO: Make more efficient.
    _ -> (0, 0, cs)

getTZD :: String -> Maybe (NominalDiffTime, Int, String)
getTZD cs = case cs of
  'Z' : rest -> Just (0, 1, rest)
  '+' : h1 : h2 : ':' : m1 : m2 : rest -> mkOffset [h1, h2] [m1, m2] rest (+)
  '-' : h1 : h2 : ':' : m1 : m2 : rest -> mkOffset [h1, h2] [m1, m2] rest (-)
  _ -> Nothing
  where
    mkOffset :: String -> String -> String -> (Int -> Int -> Int) -> Maybe (NominalDiffTime, Int, String)
    mkOffset hs ms rest op =
      let hours = case getNumber hs of
            (_, Left val, _, _) -> val
            _ -> fatal "Impossible, for h1 and h2 are digits"
          minutes = case getNumber ms of
            (_, Left val, _, _) -> val
            _ -> fatal "Impossible, for m1 and m2 are digits"
          total = hours * 60 + minutes
       in if hours <= 24 && minutes < 60
            then
              Just
                ( fromRational . toRational $ 0 `op` total,
                  6,
                  rest
                )
            else Nothing

getDateTime' :: String -> Maybe (Either LexerErrorInfo (Lexeme, UTCTime, Int, String))
getDateTime' cs = case readUniversalTime cs of
  Nothing -> Nothing
  Just (time, rest) -> Just . Right $ (LexDateTime time, time, length cs - length rest, rest)
  where
    readUniversalTime :: String -> Maybe (UTCTime, String)
    readUniversalTime s = best (reads s)
    best :: [(UTCTime, String)] -> Maybe (UTCTime, String)
    best candidates = case reverse . L.sortBy myOrdering $ candidates of
      [] -> Nothing
      ((tim, rst) : _) -> case rst of
        ' ' : 'U' : 'T' : 'C' : x -> Just (tim, x)
        _ -> Just (tim, rst)
    myOrdering :: (Show a) => (a, b) -> (a, b) -> Ordering
    myOrdering (x, _) (y, _) = compare (length . show $ x) (length . show $ y)

getDate :: String -> Maybe (Lexeme, Day, Int, String)
getDate cs =
  case cs of
    y1 : y2 : y3 : y4 : '-' : m1 : m2 : '-' : d1 : d2 : rest ->
      if all isDigit [y1, y2, y3, y4, m1, m2, d1, d2]
        then case fromGregorianValid (toInteger year) month day of
          Nothing -> Nothing
          Just d -> Just (LexDate d, d, 10, rest)
        else Nothing
      where
        year = case getNumber [y1, y2, y3, y4] of
          (_, Left val, _, _) -> val
          _ -> fatal "Impossible, for [y1, y2, y3, y4] are digits"
        month = case getNumber [m1, m2] of
          (_, Left val, _, _) -> val
          _ -> fatal "Impossible, for m1 and m2 are digits"
        day = case getNumber [d1, d2] of
          (_, Left val, _, _) -> val
          _ -> fatal "Impossible, for d1 and d2 are digits"
    _ -> Nothing

-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
-- Returns tuple with the parsed lexeme, the integer, the amount of read characters and the rest of the text
getNumber :: String -> (Lexeme, Either Int Double, Int, String)
getNumber str =
  case readDec str of
    [(_, '.' : _)] -> case readFloat str of
      [(flt, rest)] -> (LexFloat flt, Right flt, length str - length rest, rest)
      _ -> fatal "Unexpected: can read decimal, but not float???"
    [(dec, rest)] -> (LexDecimal dec, Left dec, length str - length rest, rest)
    _ -> fatal $ "No number to read!\n  " <> T.take 40 (T.pack str)

-- getNumber :: String -> (Lexeme, (Either Int Double), Int, String)
-- getNumber [] = fatal "getNumber"
-- getNumber cs@(c:s)
--  | c /= '0'         = num10
--  | null s           = const0
--  | hs `elem` "xX"   = num16
--  | hs `elem` "oO"   = num8
--  | otherwise        = num10
--  where (hs:ts) = s
--        const0 = (LexDecimal 0, Left 0, 1, s)
--        num10 :: (Lexeme, (Either Int Double), Int, String)
--        num10  = let (n, rs) = span isDigit cs
--                     (isWhole,readed,rest) =
--                        case rs of
--                          '.':cs' -> let (n',rs') = span isDigit cs'
--                                         wholeNumberString = n++"."++n'
--                                     in (all (=='0') n',wholeNumberString,rs')
--                          _       -> (True,n, rs)
--                     nrInt = read n
--                 in if isWhole
--                    then (LexDecimal nrInt , Left nrInt, length readed,rest)
--                    else let x = fst.head.readFloat $ readed
--                         in (LexFloat x, Right x, length readed,rest)
--        num16   = readIntNum isHexaDigit  16 LexHex
--        num8    = readIntNum isOctDigit 8  LexOctal
--        readIntNum :: (Char -> Bool) -> Int -> (Int -> Lexeme) -> (Lexeme, Either Int a , Int, String)
--        readIntNum p base lx
--          = let (n, rs) = span p ts
--            in  if null n
--                then const0
--                else let nr = readn base n
--                     in (lx nr, Left nr, 2 + length n, rs)

-----------------------------------------------------------
-- characters / strings
-----------------------------------------------------------
scanString :: String -> (String, Int, String)
scanString = scanUpto ['"']

-- | scan to some given character. The end char is scanned away too
scanUpto ::
  String -> -- non-empty list of ending characters
  String ->
  (String, Int, String)
scanUpto echrs s =
  case s of
    xs ->
      let (ch, cw, cr) = getchar echrs xs
          (str, w, r) = scanUpto echrs cr
       in maybe ("", 0, xs) (\c -> (c : str, cw + w, r)) ch

getchar ::
  String -> -- non-empty list of ending characters
  String -> -- string to get the character from
  (Maybe Char, Int, String)
getchar echrs s =
  case s of
    [] -> (Nothing, 0, [])
    ('\n' : _) -> (Nothing, 0, s)
    ('\t' : _) -> (Nothing, 0, s)
    ('\\' : '&' : xs) ->
      let (str, w, r) = getchar echrs xs -- Special case is required because an escaped & is equal to empty string in Haskell
       in (str, w + 2, r)
    ('\\' : xs) ->
      let (c, l, r) = getEscChar xs
       in (c, l + 1, r)
    (x : xs)
      | x `elem` echrs -> (Nothing, 0, s)
      | otherwise -> (Just x, 1, xs)

getEscChar :: String -> (Maybe Char, Int, String)
getEscChar [] = (Nothing, 0, [])
getEscChar s@(x : xs)
  | isDigit x = case readDec s of
      [(val, rest)]
        | val >= 0 && val <= ord (maxBound :: Char) -> (Just (Partial.chr val), length s - length rest, rest)
        | otherwise -> (Nothing, 1, rest)
      _ -> fatal $ "Impossible! first char is a digit.. " <> T.take 40 (T.pack s)
  | x `elem` ['\"', '\''] = (Just x, 2, xs)
  | otherwise = case x `lookup` cntrChars of
      Nothing -> (Nothing, 0, s)
      Just c -> (Just c, 1, xs)
  where
    cntrChars =
      [ ('a', '\a'),
        ('b', '\b'),
        ('f', '\f'),
        ('n', '\n'),
        ('r', '\r'),
        ('t', '\t'),
        ('v', '\v'),
        ('\\', '\\')
      ]

-----------------------------------------------------------
-- Token creation function
-----------------------------------------------------------

returnToken :: Lexeme -> FilePos -> Lexer -> Lexer
returnToken lx fpos continue posi input = do
  let token = Tok lx fpos
  tokens <- continue posi input
  return (token : tokens)
