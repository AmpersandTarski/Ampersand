module Ampersand.Input.ADL1.LexerToken
  ( Token (..),
    Lexeme (..),
    lexemeText,
    initPos,
    FilePos (..),
  )
where

import Ampersand.Basics
import Ampersand.Input.ADL1.FilePos (FilePos (..), initPos)
import RIO.Time

-- | The Ampersand token
data Token = Tok
  { -- | The lexeme
    tokLex :: Lexeme,
    -- | The position where the lexeme was found
    tokPos :: FilePos
  }

instance Show Token where
  show (Tok lx p) = show lx ++ " " ++ show p

-- | The Ampersand Lexemes
data Lexeme
  = -- | A symbol
    LexSymbol !Char
  | -- | An operator
    LexOperator !String
  | -- | A keyword
    LexKeyword !String
  | -- | A single quoted (possibly empty) string
    LexSingleQuotedString !String
  | -- | A double quoted (possibly empty) string
    LexDubbleQuotedString !String
  | -- | A markup (string to be parsed by Pandoc)
    LexMarkup !String
  | -- | A decimal number
    LexDecimal !Int
  | -- | A decimal floating point thing
    LexFloat !Double
  | -- | An octal number
    LexOctal !Int
  | -- | A hexadecimal number
    LexHex !Int
  | -- | An identifier that is safe to be used as a name in a database. It must contain only alphanumeric (UTF8) characters and underscore `_`. It may not begin with a numeric character or an underscore.
    LexSafeID !String
  | -- | A date-time
    LexDateTime !UTCTime
  | -- | A date
    LexDate !Day
  deriving (Eq, Ord)

instance Show Lexeme where
  show x = case x of
    LexSymbol val -> "symbol " ++ "'" ++ [val] ++ "'"
    LexOperator val -> "operator " ++ "'" ++ val ++ "'"
    LexKeyword val -> "keyword " ++ show val
    LexSingleQuotedString val -> "single quoted string " ++ "'" ++ val ++ "'"
    LexDubbleQuotedString val -> "double quoted string " ++ "\"" ++ val ++ "\""
    LexMarkup val -> "markup " ++ "{+" ++ val ++ "+}"
    LexDecimal _ -> "integer " ++ lexemeText x
    LexFloat _ -> "float " ++ lexemeText x
    LexOctal _ -> "octal " ++ lexemeText x
    LexHex _ -> "hexadecimal " ++ lexemeText x
    LexSafeID val -> "identifier " ++ val
    LexDateTime _ -> "iso 8601 date time " ++ lexemeText x
    LexDate _ -> "iso 8601 date " ++ lexemeText x

-- A Stream instance is responsible for maintaining the "position within the stream" in the stream state (Token).
-- This is trivial unless you are using the monad in a non-trivial way.
-- instance (Monad m) => Stream [Token] m Char where
--  uncons :: (Monad m) => [Token] -> m (Maybe (Char, [Token]))
--    uncons []     = return $ Nothing
--    uncons (t:ts) = return $ Just (t,ts)

-- | Retrieves the text from a lexeme
lexemeText ::
  -- | The lexeme
  Lexeme ->
  -- | The text contained in the lexeme
  String
lexemeText l = case l of
  LexSymbol val -> [val]
  LexOperator val -> val
  LexKeyword val -> val
  LexSingleQuotedString val -> val
  LexDubbleQuotedString val -> val
  LexMarkup val -> val
  LexDecimal val -> show val
  LexFloat val -> show val
  LexOctal val -> "0o" ++ toBase 8 val
  LexHex val -> "0x" ++ toBase 16 val
  LexSafeID val -> val
  LexDateTime val -> show val
  LexDate val -> show val

toBase :: Integral a => Show a => a -> a -> String
toBase b x = conv x ""
  where
    conv 0 str = str
    conv n str = conv (n `div` b) (show (n `mod` b) ++ str)
