module Database.Design.Ampersand.Input.ADL1.LexerToken (
    Token(..), Lexeme(..),
    get_tok_val, get_lex_val, get_tok_pos, get_tok_val_pos,
    Pos(..), Line, Column, Filename,
    noPos, initPos, lexemeLength,
    Origin(..), FilePos(..)
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

data Token = Tok  { tok_lex :: Lexeme
                   ,tok_pos :: SourcePos }

-- TODO: Make use of show Lexeme
instance Show Token where
  showsPrec _ token'
    = showString
       (case token' of
        (Tok (LexSymbol val)    p) -> "symbol "                ++      val             ++ show p
        (Tok (LexOp val)        p) -> "operator "              ++      val             ++ show p
        (Tok (LexKeyword val)   p) ->                             show val             ++ show p
        (Tok (LexString val)    p) -> "string \""              ++      val     ++ "\"" ++ show p
        (Tok (LexExpl val)      p) -> "explanation {+"         ++      val     ++ "-}" ++ show p
        (Tok (LexAtom val)      p) -> "atom '"                 ++      val     ++ "'"  ++ show p
        (Tok (LexChar val)      p) -> "character '"            ++ show val     ++ "'"  ++ show p
        (Tok (LexInteger val)   p) -> "decimal Integer "       ++ show val             ++ show p
        (Tok (LexLowerId val)   p) -> "lower case identifier " ++      val             ++ show p
        (Tok (LexUpperId val)   p) -> "upper case identifier " ++      val             ++ show p
        (Tok (LexTextName val)  p) -> "text name "             ++      val             ++ show p
        (Tok (LexTextLine val)  p) -> "text line "             ++      val             ++ show p
        (Tok (LexSpace)         p) -> "spaces "                                        ++ show p
       )

data Lexeme  = LexSymbol      String -- TODO: we miss a token for special characters (see pSpec). Is this a LexSymbol Char?
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
             --TODO: The lexemes below are probably unnecessary
             | LexTextName    String
             | LexTextLine    String
             | LexSpace
  deriving (Eq, Ord)

instance Show Lexeme where
    show x = case x of
 		 LexSymbol    val        -> "symbol "                           ++ " '"  ++      val      ++ "'"
 		 LexOp        val        -> "operator "                         ++ " '"  ++      val      ++ "'"
		 LexKeyword   val        -> "keyword"                           ++          show val
		 LexString    val        -> "string "                           ++ " \"" ++      val      ++ "\""
		 LexExpl      val        -> "Explanation  "                     ++ " {+" ++      val      ++ "+}" 		
		 LexAtom      val        -> "Atom  "                            ++ " '"  ++      val      ++ "'"		
		 LexChar      val        -> "character "                        ++ " '"  ++ show val      ++ "'" 		
		 LexInteger   val        -> "decimal Integer "                  ++          show val
		 LexUpperId   val        -> "upper case identifier "            ++               val
		 LexLowerId   val        -> "lower case identifier "            ++               val
		 LexTextName  val        -> "text name "                        ++ " '" ++       val      ++ "'"
		 LexTextLine  val        -> "text name "                        ++ " '" ++       val      ++ "'"
		 LexSpace                -> "spaces "

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
		 LexTextName  val -> val
		 LexTextLine  val -> val
		 LexSpace         -> " "

-- Gets the location of the token in the file
get_tok_pos :: Token -> Origin
get_tok_pos (Tok t p) = FileLoc(FilePos (sourceName p ,p, show t))

-- Gets the location of the token in the file and it's value
get_tok_val_pos :: Token -> (String, Origin)
get_tok_val_pos tok = (show tok, get_tok_pos tok)

-- TODO: Check the lenghts. This is taken from Helium, but is it necessary? It can never be precise...
lexemeLength :: Lexeme -> Int
lexemeLength l = case l of
 		 LexSymbol    val -> length val
 		 LexOp        val -> length val
		 LexKeyword   val -> length val
		 LexString    val -> length val + 2 -- including quotes
		 LexExpl      val -> length val + 4 -- including quotes
		 LexAtom      val -> length val
		 LexChar      _   -> 3 -- including quotes
		 LexInteger   val -> val `div` 10
		 LexUpperId   val -> length val
		 LexLowerId   val -> length val
		 LexTextName  val -> length val
		 LexTextLine  val -> length val
		 LexSpace         -> 0
