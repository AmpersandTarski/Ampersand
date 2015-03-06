module Database.Design.Ampersand.Input.ADL1.LexerToken (
    Token(..), Lexeme(..),
    makeGenToken, GenToken, returnOutputToken,
    get_tok_val, get_lex_val, get_tok_pos, get_tok_val_pos,
    Pos(..), Line, Column, Filename,
    GenTokenType(..), noPos, initPos, errGenToken, lexemeLength,
    Origin(..), FilePos(..)
) where

import Database.Design.Ampersand.Input.ADL1.FilePos (Origin(..), FilePos(..))
import Text.Parsec.Pos(SourcePos, newPos, sourceName)
import Text.Parsec()

--Generic types used in all token types
--

type Line = Int
type Column = Int

data Pos = Pos{line:: !Line, column:: !Column} deriving (Eq, Ord, Show)

initPos :: Pos
initPos = Pos 1 1

noPos :: Pos
noPos = Pos 0 0

type Filename   = String

-- GenToken out of which different token formats can be generated
-- The generic token abtracts from the main lexer logic and the output format to make the scanner easy maintainable when another TokenType is needed
-- TODO: Remove this generic token, generate the definite tokens
data GenTokenType
  = GtkSymbol
  | GtkVarid
  | GtkConid
  | GtkKeyword
  | GtkOp
  | GtkString
  | GtkExpl
  | GtkAtom
  | GtkChar
  | GtkInteger8
  | GtkInteger10
  | GtkInteger16
  | GtkTextnm
  | GtkTextln
  | GtkSpace
  | GtkError
  deriving (Eq, Ord)

data GenToken = GenTok { gtokt       ::  GenTokenType
                       , gvalue      ::  String
                       , gpos        :: !Pos
                       , gfilen      :: !Filename
                       }

instance Show GenToken where
  showsPrec _ gentoken'
    = showString
       (case gentoken' of
        (GenTok GtkSymbol    s2 i fn)  -> "symbol "                ++ s2         ++ maybeshow i fn
        (GenTok GtkOp        s2 i fn)  -> "operator "              ++ s2         ++ maybeshow i fn
        (GenTok GtkKeyword   s2 i fn)  ->                        show s2         ++ maybeshow i fn
        (GenTok GtkString    s2 i fn)  -> "string \""              ++ s2 ++ "\"" ++ maybeshow i fn
        (GenTok GtkExpl      s2 i fn)  -> "explanation {+"         ++ s2 ++ "-}" ++ maybeshow i fn
        (GenTok GtkAtom      s2 i fn)  -> "atom '"                 ++ s2 ++ "'"  ++ maybeshow i fn
        (GenTok GtkChar      s2 i fn)  -> "character '"            ++ s2 ++ "'"  ++ maybeshow i fn
        (GenTok GtkInteger8  s2 i fn)  -> "octal integer "         ++ s2         ++ maybeshow i fn
        (GenTok GtkInteger10 s2 i fn)  -> "decimal Integer "       ++ s2         ++ maybeshow i fn
        (GenTok GtkInteger16 s2 i fn)  -> "hexadecimal integer "   ++ s2         ++ maybeshow i fn
        (GenTok GtkVarid     s2 i fn)  -> "lower case identifier " ++ s2         ++ maybeshow i fn
        (GenTok GtkConid     s2 i fn)  -> "upper case identifier " ++ s2         ++ maybeshow i fn
        (GenTok GtkTextnm    s2 i fn)  -> "text name "             ++ s2         ++ maybeshow i fn
        (GenTok GtkTextln    s2 i fn)  -> "text line "             ++ s2         ++ maybeshow i fn
        (GenTok GtkSpace     _  i fn)  -> "spaces "                              ++ maybeshow i fn
        (GenTok GtkError     s2 i fn)  -> "error in scanner: "     ++ s2         ++ maybeshow i fn
       )

maybeshow :: Pos -> Filename -> String
maybeshow (Pos 0 0) _  =  ""
maybeshow (Pos l c) fn =  " at line " ++ show l
                       ++ ", column " ++ show c
                       ++ " of file " ++ show fn

makeGenToken :: GenTokenType -> String -> Pos -> Filename -> GenToken
makeGenToken tokt val pos filen = GenTok tokt val pos filen
					
errGenToken :: String -> Pos -> Filename -> GenToken
errGenToken errorstr pos filen = GenTok GtkError errorstr pos filen
					   
-- Original Token structure that will be replaced with the new ParsecT structure 
-- Functions based on the old Token structure are beneath the data and type declarations

   
-- Parsec Token structure is introduced as a replacement of the original Token structure
-- 

--TODO: Rename the functions, the first conflicts with the Parsec function, the second is difficult to understand.
data Token = Tok  { lexeme  :: Lexeme
                   , sp      :: SourcePos
                   }

-- TODO: Make use of show Lexeme
instance Show Token where
  showsPrec _ token'
    = showString
       (case token' of
        (Tok (LexSymbol val)    sp )  -> "symbol "                ++      val             ++ show sp 
        (Tok (LexOp val)        sp )  -> "operator "              ++      val             ++ show sp 
        (Tok (LexKeyword val)   sp )  ->                             show val             ++ show sp 
        (Tok (LexString val)    sp )  -> "string \""              ++      val     ++ "\"" ++ show sp 
        (Tok (LexExpl val)      sp )  -> "explanation {+"         ++      val     ++ "-}" ++ show sp 
        (Tok (LexAtom val)      sp )  -> "atom '"                 ++      val     ++ "'"  ++ show sp 
        (Tok (LexChar val)      sp )  -> "character '"            ++ show val     ++ "'"  ++ show sp
        (Tok (LexInteger val)   sp )  -> "decimal Integer "       ++ show val             ++ show sp
        (Tok (LexLowerId val)   sp )  -> "lower case identifier " ++      val             ++ show sp 
        (Tok (LexUpperId val)   sp )  -> "upper case identifier " ++      val             ++ show sp 
        (Tok (LexTextName val)  sp )  -> "text name "             ++      val             ++ show sp
        (Tok (LexTextLine val)  sp )  -> "text line "             ++      val             ++ show sp 
        (Tok (LexSpace)         sp )  -> "spaces "                                        ++ show sp 
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
get_tok_pos (Tok lex p) = FileLoc(FilePos (sourceName p ,p, show lex))

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
		 LexChar      val -> 3 -- including quotes
		 LexInteger   val -> val `div` 10
		 LexUpperId   val -> length val
		 LexLowerId   val -> length val
		 LexTextName  val -> length val
		 LexTextLine  val -> length val
		 LexSpace         -> 0

-- New Lexeme Token format
returnOutputToken :: [GenToken] -> [Token]
returnOutputToken []     = []
returnOutputToken (x:xs) =  conv x : returnOutputToken xs
      where conv (GenTok GtkSymbol    val (Pos l c) fn) = Tok (LexSymbol val)         (newPos  fn l c)
            conv (GenTok GtkOp        val (Pos l c) fn) = Tok (LexOp val)             (newPos  fn l c)
            conv (GenTok GtkKeyword   val (Pos l c) fn) = Tok (LexKeyword val)        (newPos  fn l c)
            conv (GenTok GtkString    val (Pos l c) fn) = Tok (LexString val)         (newPos  fn l c)
            conv (GenTok GtkExpl      val (Pos l c) fn) = Tok (LexExpl val)           (newPos  fn l c)
            conv (GenTok GtkAtom      val (Pos l c) fn) = Tok (LexAtom val)           (newPos  fn l c)
            conv (GenTok GtkInteger8  val (Pos l c) fn) = Tok (LexInteger (read val)) (newPos  fn l c)
            conv (GenTok GtkInteger10 val (Pos l c) fn) = Tok (LexInteger (read val)) (newPos  fn l c)
            conv (GenTok GtkInteger16 val (Pos l c) fn) = Tok (LexInteger (read val)) (newPos  fn l c)
            conv (GenTok GtkVarid     val (Pos l c) fn) = Tok (LexLowerId val)        (newPos  fn l c)
            conv (GenTok GtkConid     val (Pos l c) fn) = Tok (LexUpperId val)        (newPos  fn l c)
            conv (GenTok GtkTextnm    val (Pos l c) fn) = Tok (LexTextName val)       (newPos  fn l c)
            conv (GenTok GtkTextln    val (Pos l c) fn) = Tok (LexTextLine val)       (newPos  fn l c)
            conv (GenTok GtkSpace     val (Pos l c) fn) = Tok LexSpace                (newPos  fn l c)
