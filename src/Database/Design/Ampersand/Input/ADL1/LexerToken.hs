module Database.Design.Ampersand.Input.ADL1.LexerToken
(Token, Tok)
where

import Text.Parsec.Pos(SourcePos)

-- Original Token structure that will be replaced with the new ParsecT structure 
-- Functions based on the old Token structure are beneath the data and type declarations

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

maybeshow :: Pos -> Filename -> String
maybeshow (Pos 0 0) _  =  ""
maybeshow (Pos l c) fn =  " at line " ++ show l
                       ++ ", column " ++ show c
                       ++ " of file " ++ show fn	   

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
        (Tokv (LexSymbol val)    sp tval)  -> "symbol "                ++      val             ++ show sp ++ tval
        (Tokv (LexOp val)        sp tval)  -> "operator "              ++      val             ++ show sp ++ tval
        (Tokv (LexKeyword val)   sp tval)  ->                             show val             ++ show sp ++ tval
        (Tokv (LexString val)    sp tval)  -> "string \""              ++      val     ++ "\"" ++ show sp ++ tval
        (Tokv (LexExpl val)      sp tval)  -> "explanation {+"         ++      val     ++ "-}" ++ show sp ++ tval
        (Tokv (LexAtom val)      sp tval)  -> "atom '"                 ++      val     ++ "'"  ++ show sp ++ tval
        (Tokv (LexChar val)      sp tval)  -> "character '"            ++ show val     ++ "'"  ++ show sp ++ tval
        (Tokv (LexInteger val)   sp tval)  -> "decimal Integer "       ++ show val             ++ show sp ++ tval
        (Tokv (LexLowerId val)   sp tval)  -> "lower case identifier " ++      val             ++ show sp ++ tval
        (Tokv (LexUpperId val)   sp tval)  -> "upper case identifier " ++      val             ++ show sp ++ tval
        (Tokv (LexTextName val)  sp tval)  -> "text name "             ++      val             ++ show sp ++ tval
        (Tokv (LexTextLine val)  sp tval)  -> "text line "             ++      val             ++ show sp ++ tval
        (Tokv (LexSpace)         sp tval)  -> "spaces "                                        ++ show sp ++ tval
       )

data Lexeme  = LexSymbol      String
             | LexOp          String
             | LexKeyword     String
             | LexString      String
             | LexExpl        String
             | LexAtom        String
             | LexChar        Char
             | LexInteger     Int
             | LexUpperId     String
             | LexLowerId     String
             | LexTextName    String
             | LexTextLine    String
             | LexSpace
        deriving (Eq)
	
	
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
