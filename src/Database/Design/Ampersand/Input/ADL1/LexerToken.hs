module Database.Design.Ampersand.Input.ADL1.LexerToken (
    Token(..), Lexeme(..), lexemeText,
    Pos(..), Line, Column, Filename,
    noPos, initPos, Origin(..), FilePos(..)
) where

import Database.Design.Ampersand.Input.ADL1.FilePos (Origin(..), FilePos(..))
import Text.Parsec.Pos(SourcePos)
import Text.Parsec()

type Line = Int
type Column = Int

--TODO: Use either Pos or SourcePos, why both?
data Pos = Pos{line:: !Line, column:: !Column} deriving (Eq, Ord, Show)

initPos :: Pos
initPos = Pos 1 1

noPos :: Pos
noPos = Pos 1 0

type Filename   = String

data Token = Tok { tokLex :: Lexeme
                 , tokPos :: SourcePos }

instance Show Token where
  show (Tok lx p) = show lx ++ " " ++ show p

data Lexeme  = LexSymbol      Char
             | LexOperator    String
             | LexKeyword     String
             | LexString      String
             | LexExpl        String
             | LexAtom        String
             | LexDecimal     Int
             | LexOctal       Int
             | LexHex         Int
             | LexConId       String
             | LexVarId       String
  deriving (Eq, Ord)

instance Show Lexeme where
    show x = case x of
         LexSymbol   val -> "Symbol "                ++ "'"  ++     [val] ++ "'"
         LexOperator val -> "Operator "              ++ "'"  ++      val  ++ "'"
         LexKeyword  val -> "Keyword "               ++         show val
         LexString   val -> "String "                ++ "\"" ++      val  ++ "\""
         LexExpl     val -> "Explanation "           ++ "{+" ++      val  ++ "+}"
         LexAtom     val -> "Atom "                  ++ "'"  ++      val  ++ "'"
         LexDecimal   _  -> "Integer "               ++   lexemeText  x
         LexOctal     _  -> "Octal "                 ++   lexemeText  x
         LexHex       _  -> "Hexadecimal "           ++   lexemeText  x
         LexVarId    val -> "Lower case identifier " ++              val
         LexConId    val -> "Upper case identifier " ++              val

-- A Stream instance is responsible for maintaining the "position within the stream" in the stream state (Token).
-- This is trivial unless you are using the monad in a non-trivial way.
-- instance (Monad m) => Stream [Token] m Char where
--  uncons :: (Monad m) => [Token] -> m (Maybe (Char, [Token]))
--    uncons []     = return $ Nothing
--    uncons (t:ts) = return $ Just (t,ts)

lexemeText :: Lexeme -> String
lexemeText l = case l of
         LexSymbol   val -> [val]
         LexOperator val -> val
         LexKeyword  val -> val
         LexString   val -> val
         LexExpl     val -> val
         LexAtom     val -> val
         LexDecimal  val -> show val
         LexOctal    val -> "0o" ++ toBase 8  val
         LexHex      val -> "0x" ++ toBase 16 val
         LexConId    val -> val
         LexVarId    val -> val

toBase :: Integral a => Show a => a -> a -> String
toBase b x = conv x ""
       where conv 0 str = str
             conv n str = conv (n `div` b) (show (n `mod` b) ++ str)
