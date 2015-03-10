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

data Lexeme  = LexSymbol      String -- TODO: we miss a token for special characters (see pSpec). Is this a LexSymbol Char?
             -- TODO: Rename to LexOperator
             | LexOp          String
             | LexKeyword     String
             | LexString      String
             | LexExpl        String
             | LexAtom        String
             | LexChar        Char
             | LexInteger     Int
             --TODO: Either rename this to conId/varId or rename con/var to upper/lower
             | LexUpperId     String
             | LexLowerId     String
  deriving (Eq, Ord)

instance Show Lexeme where
    show x = case x of
 		 LexSymbol    val -> "Symbol "                ++ " '"  ++      val ++ "'"
 		 LexOp        val -> "Operator "              ++ " '"  ++      val ++ "'"
		 LexKeyword   val -> "Keyword"                ++          show val
		 LexString    val -> "String "                ++ " \"" ++      val ++ "\""
		 LexExpl      val -> "Explanation  "          ++ " {+" ++      val ++ "+}"
		 LexAtom      val -> "Atom  "                 ++ " '"  ++      val ++ "'"
		 LexChar      val -> "Character "             ++ " '"  ++ show val ++ "'"
		 LexInteger   val -> "Integer "               ++          show val
		 LexLowerId   val -> "Lower case identifier " ++               val
		 LexUpperId   val -> "Upper case identifier " ++               val

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
 		 LexSymbol    val -> val
 		 LexOp        val -> val
		 LexKeyword   val -> val
		 LexString    val -> val
		 LexExpl      val -> val
		 LexAtom      val -> val
		 LexChar      val -> [val]
		 LexInteger   val -> show val
		 LexUpperId   val -> val
		 LexLowerId   val -> val

-- Gets the location of the token in the file
get_tok_pos :: Token -> Origin
get_tok_pos (Tok t p) = FileLoc(FilePos (sourceName p ,p, show t))

-- Gets the location of the token in the file and it's value
get_tok_val_pos :: Token -> (String, Origin)
get_tok_val_pos tok = (show tok, get_tok_pos tok)
