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
import qualified RIO.Text as T
import RIO.Time

-- | The Ampersand token
data Token = Tok
  { -- | The lexeme
    tokLex :: !Lexeme,
    -- | The position where the lexeme was found
    tokPos :: !FilePos
  }

instance Show Token where
  show (Tok lx p) = show lx ++ " " ++ show p

-- | The Ampersand Lexemes
data Lexeme
  = -- | A symbol
    LexSymbol !Char
  | -- | An operator
    LexOperator !Text1
  | -- | A keyword
    LexKeyword !Text1
  | -- | A single quoted (possibly empty) string
    LexSingleQuotedString !Text
  | -- | A double quoted (possibly empty) string
    LexDubbleQuotedString !Text
  | -- | A markup (string to be parsed by Pandoc)
    LexMarkup !Text
  | -- | A decimal number
    LexDecimal !Int
  | -- | A decimal floating point thing
    LexFloat !Double
  | -- | An octal number
    LexOctal !Int
  | -- | A hexadecimal number
    LexHex !Int
  | -- | An identifier that is safe to be used as a name in a database. It must contain only alphanumeric (UTF8) characters and underscore `_`. It may not begin with a numeric character or an underscore.
    LexSafeID !Text1
  | -- | A date-time
    LexDateTime !UTCTime
  | -- | A date
    LexDate !Day
  deriving (Eq, Ord)

instance Show Lexeme where
  show x = case x of
    LexSymbol val -> "symbol " ++ "'" ++ [val] ++ "'"
    LexOperator val -> "operator " ++ "'" ++ T.unpack (text1ToText val) ++ "'"
    LexKeyword val -> "keyword " ++ show val
    LexSingleQuotedString val -> "single quoted string " ++ "'" ++ T.unpack val ++ "'"
    LexDubbleQuotedString val -> "double quoted string " ++ "\"" ++ T.unpack val ++ "\""
    LexMarkup val -> "markup " ++ "{+" ++ T.unpack val ++ "+}"
    LexDecimal _ -> "integer " ++ show (lexemeText x)
    LexFloat _ -> "float " ++ show (lexemeText x)
    LexOctal _ -> "octal " ++ show (lexemeText x)
    LexHex _ -> "hexadecimal " ++ show (lexemeText x)
    LexSafeID val -> "identifier " ++ T.unpack (text1ToText val)
    LexDateTime _ -> "iso 8601 date time " ++ show (lexemeText x)
    LexDate _ -> "iso 8601 date " ++ show (lexemeText x)

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
  Text
lexemeText l = case l of
  LexSymbol val -> T.cons val ""
  LexOperator val -> text1ToText val
  LexKeyword val -> text1ToText val
  LexSingleQuotedString val -> val
  LexDubbleQuotedString val -> val
  LexMarkup val -> val
  LexDecimal val -> tshow val
  LexFloat val -> tshow val
  LexOctal val -> T.pack $ "0o" ++ toBase 8 val
  LexHex val -> T.pack $ "0x" ++ toBase 16 val
  LexSafeID val -> text1ToText val
  LexDateTime val -> tshow val
  LexDate val -> tshow val

toBase :: Integral a => Show a => a -> a -> String
toBase b x = conv x ""
  where
    conv 0 str = str
    conv n str = conv (n `div` b) (show (n `mod` b) ++ str)
