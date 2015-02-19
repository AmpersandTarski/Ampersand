module LexerToken
(Token, Lexeme, makeGenToken, GenToken)
where

import Text.Parsec.Pos(SourcePos)

--Generic types used in all token types
--

type Line = Int
type Column = Int

data Pos = Pos{line:: !Line, column:: !Column} deriving (Eq, Ord, Show)
type Filename   = String

-- GenToken out of which different token formats can be generated
-- The generic token abtracts from the main lexer logic and the output format to make the scanner easy maintainable when another TokenType is needed
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
					   
-- Original Token structure that will be replaced with the new ParsecT structure 
-- Functions based on the old Token structure are beneath the data and type declarations


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

data TokenL = TokL { lexeme  :: Lexeme
                   , sp      :: SourcePos
                   , val     :: String
                   }

	
instance Show TokenL where
  showsPrec _ token'
    = showString
       (case token' of
        (TokL (LexSymbol val)    sp tval)  -> "symbol "                ++      val             ++ show sp ++ tval
        (TokL (LexOp val)        sp tval)  -> "operator "              ++      val             ++ show sp ++ tval
        (TokL (LexKeyword val)   sp tval)  ->                             show val             ++ show sp ++ tval
        (TokL (LexString val)    sp tval)  -> "string \""              ++      val     ++ "\"" ++ show sp ++ tval
        (TokL (LexExpl val)      sp tval)  -> "explanation {+"         ++      val     ++ "-}" ++ show sp ++ tval
        (TokL (LexAtom val)      sp tval)  -> "atom '"                 ++      val     ++ "'"  ++ show sp ++ tval
        (TokL (LexChar val)      sp tval)  -> "character '"            ++ show val     ++ "'"  ++ show sp ++ tval
        (TokL (LexInteger val)   sp tval)  -> "decimal Integer "       ++ show val             ++ show sp ++ tval
        (TokL (LexLowerId val)   sp tval)  -> "lower case identifier " ++      val             ++ show sp ++ tval
        (TokL (LexUpperId val)   sp tval)  -> "upper case identifier " ++      val             ++ show sp ++ tval
        (TokL (LexTextName val)  sp tval)  -> "text name "             ++      val             ++ show sp ++ tval
        (TokL (LexTextLine val)  sp tval)  -> "text line "             ++      val             ++ show sp ++ tval
        (TokL (LexSpace)         sp tval)  -> "spaces "                                        ++ show sp ++ tval
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
