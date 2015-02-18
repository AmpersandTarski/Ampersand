module LexerToken
(Token, Tok)
where

import Text.Parsec.Pos(SourcePos)

--type Token = (SourcePos,Tok,String)

-- Original Token structure that will be replaced with the new ParsecT structure 
-- 

type Line = Int
type Column = Int

data Pos = Pos{line:: !Line, column:: !Column} deriving (Eq, Ord, Show)
type Filename   = String

data TokenType
  = TkSymbol
  | TkVarid
  | TkConid
  | TkKeyword
  | TkOp
  | TkString
  | TkExpl
  | TkAtom
  | TkChar
  | TkInteger8
  | TkInteger10
  | TkInteger16
  | TkTextnm
  | TkTextln
  | TkSpace
  | TkError
  deriving (Eq, Ord)

data Token = Tok { tp' :: TokenType
                 , val1 :: String
                 , val2 :: String
                 , pos :: !Pos
                 , file :: !Filename
                 }
				 
instance Show Token where
  showsPrec _ token'
    = showString
       (case token' of
        (Tok TkSymbol    _  s2 i fn)  -> "symbol "                ++ s2         ++ maybeshow i fn
        (Tok TkOp        _  s2 i fn)  -> "operator "              ++ s2         ++ maybeshow i fn
        (Tok TkKeyword   _  s2 i fn)  ->                        show s2         ++ maybeshow i fn
        (Tok TkString    _  s2 i fn)  -> "string \""              ++ s2 ++ "\"" ++ maybeshow i fn
        (Tok TkExpl      _  s2 i fn)  -> "explanation {+"         ++ s2 ++ "-}" ++ maybeshow i fn
        (Tok TkAtom      _  s2 i fn)  -> "atom '"                 ++ s2 ++ "'"  ++ maybeshow i fn
        (Tok TkChar      _  s2 i fn)  -> "character '"            ++ s2 ++ "'"  ++ maybeshow i fn
        (Tok TkInteger8  _  s2 i fn)  -> "octal integer "         ++ s2         ++ maybeshow i fn
        (Tok TkInteger10 _  s2 i fn)  -> "decimal Integer "       ++ s2         ++ maybeshow i fn
        (Tok TkInteger16 _  s2 i fn)  -> "hexadecimal integer "   ++ s2         ++ maybeshow i fn
        (Tok TkVarid     _  s2 i fn)  -> "lower case identifier " ++ s2         ++ maybeshow i fn
        (Tok TkConid     _  s2 i fn)  -> "upper case identifier " ++ s2         ++ maybeshow i fn
        (Tok TkTextnm    _  s2 i fn)  -> "text name "             ++ s2         ++ maybeshow i fn
        (Tok TkTextln    _  s2 i fn)  -> "text line "             ++ s2         ++ maybeshow i fn
        (Tok TkSpace     _  _  i fn)  -> "spaces "                              ++ maybeshow i fn
        (Tok TkError     _  s2 i fn)  -> "error in scanner: "     ++ s2         ++ maybeshow i fn
       )

	   

-- Parsec Token structure is introduced as a replacement of the original Token structure
-- 

data TokenP = Tokv { lexeme  :: Lexeme
                   , sp      :: SourcePos
                   , val     :: String
                   }

	
instance Show TokenP where
  showsPrec _ token'
    = showString
       (case token' of
        (Tokv (TkSymbol val)    sp tval)  -> "symbol "                ++ val         ++ show sp ++ tval
        (Tokv (TkOp val)        sp tval)  -> "operator "              ++ val         ++ show sp ++ tval
        (Tokv (TkKeyword val)   sp tval)  ->                        show val         ++ show sp ++ tval
        (Tokv (TkString val)    sp tval)  -> "string \""              ++ val ++ "\"" ++ show sp ++ tval
        (Tokv (TkExpl val)      sp tval)  -> "explanation {+"         ++ val ++ "-}" ++ show sp ++ tval
        (Tokv (TkAtom val)      sp tval)  -> "atom '"                 ++ val ++ "'"  ++ show sp ++ tval
        (Tokv (TkChar val)      sp tval)  -> "character '"            ++ show val ++ "'"  ++ show sp ++ tval
        (Tokv (TkInteger val)   sp tval)  -> "decimal Integer "       ++ show val    ++ show sp ++ tval
        (Tokv (TkLowerId val)   sp tval)  -> "lower case identifier " ++ val         ++ show sp ++ tval
        (Tokv (TkUpperId val)   sp tval)  -> "upper case identifier " ++ val         ++ show sp ++ tval
        (Tokv (TkTextName val)  sp tval)  -> "text name "             ++ val         ++ show sp ++ tval
        (Tokv (TkTextLine val)  sp tval)  -> "text line "             ++ val         ++ show sp ++ tval
        (Tokv (TkSpace)         sp tval)  -> "spaces "                               ++ show sp ++ tval
       )

data Lexeme  = TkSymbol String
             | TkOp String
             | TkKeyword String
             | TkString String
             | TkExpl String
             | TkAtom String
             | TkChar Char
             | TkInteger Int
             | TkUpperId String
             | TkLowerId String
             | TkTextName String
             | TkTextLine String
             | TkSpace
        deriving (Show, Eq)
	
	
instance Show lexeme where 
    show x = case x of 
         LexChar    c        -> Texts.parserCharacterLiteral      ++ " '" ++ c      ++ "'" 
         LexString  s        -> Texts.parserStringLiteral         ++ " \""++ s      ++ "\"" 
         LexInt     i        -> Texts.parserIntegerLiteral        ++ " '" ++ i      ++ "'" 
         LexFloat   f        -> Texts.parserFloatLiteral          ++ " '" ++ f      ++ "'" 
 
 
         LexVar     n        -> Texts.parserVariable              ++ " '" ++ n      ++ "'" 
         LexVarSym  o        -> Texts.parserOperator              ++ " '" ++ o      ++ "'" 
         LexCon     c        -> Texts.parserConstructor           ++ " '" ++ c      ++ "'" 
         LexConSym  o        -> Texts.parserConstructorOperator   ++ " '" ++ o      ++ "'" 
          
         LexKeyword kwd      -> Texts.parserKeyword ++ " '" ++ kwd ++ "'" 
         LexResVarSym s      -> "'" ++ s ++ "'" 
         LexResConSym s      -> "'" ++ s ++ "'" 
         LexSpecial c        -> "'" ++ [c] ++ "'" 
          
         LexInsertedOpenBrace  -> Texts.parserInsertedLBrace  
         LexInsertedCloseBrace -> Texts.parserEndOfBlock 
         LexInsertedSemicolon  -> Texts.parserNextInBlock 
                          
         LexEOF              -> Texts.parserEndOfFile 
