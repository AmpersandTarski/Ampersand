module Database.Design.Ampersand.Input.ADL1.LexerToken (
    Token(..), Lexeme(..),
    get_tok_val, get_lex_val, get_tok_pos, get_tok_val_pos,
    Pos(..), Line, Column, Filename,
    noPos, initPos, Origin(..), FilePos(..)
) where

import Database.Design.Ampersand.Input.ADL1.FilePos (Origin(..), FilePos(..))
import Text.Parsec.Pos(SourcePos, sourceName)
import Text.Parsec()

type Line = Int
type Column = Int

--TODO: Use either Pos or SourcePos, why both?
data Pos = Pos{line:: !Line, column:: !Column} deriving (Eq, Ord, Show)

initPos :: Pos
initPos = Pos 1 1

noPos :: Pos
noPos = Pos 0 0

type Filename   = String

data Token = Tok { tok_lex :: Lexeme
                  ,tok_pos :: SourcePos }

instance Show Token where
  show (Tok lx p) = show lx ++ " " ++ show p

data Lexeme  = LexSymbol      Char
             | LexOperator    String
             | LexKeyword     String
             | LexString      String
             | LexExpl        String
             | LexAtom        String
             --TODO: Integers are not used! Ask Stef if we can remove them.
             | LexInteger     Int
             | LexConId       String
             | LexVarId       String
  deriving (Eq, Ord)

instance Show Lexeme where
    show x = case x of
         LexSymbol   val -> "Symbol "         ++ "'"  ++    [val] ++ "'"
         LexOperator val -> "Operator "       ++ "'"  ++      val ++ "'"
         LexKeyword  val -> "Keyword "        ++         show val
         LexString   val -> "String "         ++ "\"" ++      val ++ "\""
         LexExpl     val -> "Explanation "    ++ "{+" ++      val ++ "+}"
         LexAtom     val -> "Atom "           ++ "'"  ++      val ++ "'"
         LexInteger  val -> "Integer "        ++         show val
         LexVarId    val -> "Var identifier " ++              val
         LexConId    val -> "Con identifier " ++              val

-- A Stream instance is responsible for maintaining the "position within the stream" in the stream state (Token).
-- This is trivial unless you are using the monad in a non-trivial way.
-- instance (Monad m) => Stream [Token] m Char where
--  uncons :: (Monad m) => [Token] -> m (Maybe (Char, [Token]))
--    uncons []     = return $ Nothing
--    uncons (t:ts) = return $ Just (t,ts)

get_tok_val :: Token -> String
get_tok_val (Tok l _) = get_lex_val l

get_lex_val :: Lexeme -> String
get_lex_val l = case l of
         LexSymbol   val -> [val]
         LexOperator val -> val
         LexKeyword  val -> val
         LexString   val -> val
         LexExpl     val -> val
         LexAtom     val -> val
         LexInteger  val -> show val
         LexConId    val -> val
         LexVarId    val -> val

-- Gets the location of the token in the file
get_tok_pos :: Token -> Origin
get_tok_pos (Tok t p) = FileLoc(FilePos (sourceName p, p, show t))

-- Gets the location of the token in the file and it's value
get_tok_val_pos :: Token -> (String, Origin)
get_tok_val_pos tok = (show tok, get_tok_pos tok)
