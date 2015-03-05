module LexerToken
(Token, Lexeme, makeGenToken, GenToken, Pos (..), Line, Column, Filename, GenTokenType (..), noPos, initPos, errGenToken)
where

import Text.Parsec.Pos(SourcePos, newPos)

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
        (Tok TkSymbol    _  s2 pos fn)  -> "symbol "                ++ s2         ++ maybeshow pos fn
        (Tok TkOp        _  s2 pos fn)  -> "operator "              ++ s2         ++ maybeshow pos fn
        (Tok TkKeyword   _  s2 pos fn)  ->                        show s2         ++ maybeshow pos fn
        (Tok TkString    _  s2 pos fn)  -> "string \""              ++ s2 ++ "\"" ++ maybeshow pos fn
        (Tok TkExpl      _  s2 pos fn)  -> "explanation {+"         ++ s2 ++ "-}" ++ maybeshow pos fn
        (Tok TkAtom      _  s2 pos fn)  -> "atom '"                 ++ s2 ++ "'"  ++ maybeshow pos fn
        (Tok TkChar      _  s2 pos fn)  -> "character '"            ++ s2 ++ "'"  ++ maybeshow pos fn
        (Tok TkInteger8  _  s2 pos fn)  -> "octal integer "         ++ s2         ++ maybeshow pos fn
        (Tok TkInteger10 _  s2 pos fn)  -> "decimal Integer "       ++ s2         ++ maybeshow pos fn
        (Tok TkInteger16 _  s2 pos fn)  -> "hexadecimal integer "   ++ s2         ++ maybeshow pos fn
        (Tok TkVarid     _  s2 pos fn)  -> "lower case identifier " ++ s2         ++ maybeshow pos fn
        (Tok TkConid     _  s2 pos fn)  -> "upper case identifier " ++ s2         ++ maybeshow pos fn
        (Tok TkTextnm    _  s2 pos fn)  -> "text name "             ++ s2         ++ maybeshow pos fn
        (Tok TkTextln    _  s2 pos fn)  -> "text line "             ++ s2         ++ maybeshow pos fn
        (Tok TkSpace     _  _  pos fn)  -> "spaces "                              ++ maybeshow pos fn
        (Tok TkError     _  s2 pos fn)  -> "error in scanner: "     ++ s2         ++ maybeshow pos fn
       )
   
instance Eq Token where
  (Tok ttypel     stringl _ _ _) ==  (Tok ttyper     stringr _ _ _) =  ttypel == ttyper && stringl == stringr
   
instance  Ord Token where
  compare x y | x==y      = EQ
              | x<=y      = LT
              | otherwise = GT
  (Tok ttypel     stringl _ _ _ ) <= (Tok ttyper    stringr _ _  _ )
      =     ttypel <  ttyper
        || (ttypel == ttyper && stringl <= stringr)
   
-- Parsec Token structure is introduced as a replacement of the original Token structure
-- 

data TokenL = TokL { lexeme  :: Lexeme
                   , sp      :: SourcePos
                   }

	
instance Show TokenL where
  showsPrec _ token'
    = showString
       (case token' of
        (TokL (LexSymbol val)    sp )  -> "symbol "                ++      val             ++ show sp 
        (TokL (LexOp val)        sp )  -> "operator "              ++      val             ++ show sp 
        (TokL (LexKeyword val)   sp )  ->                             show val             ++ show sp 
        (TokL (LexString val)    sp )  -> "string \""              ++      val     ++ "\"" ++ show sp 
        (TokL (LexExpl val)      sp )  -> "explanation {+"         ++      val     ++ "-}" ++ show sp 
        (TokL (LexAtom val)      sp )  -> "atom '"                 ++      val     ++ "'"  ++ show sp 
        (TokL (LexChar val)      sp )  -> "character '"            ++ show val     ++ "'"  ++ show sp
        (TokL (LexInteger val)   sp )  -> "decimal Integer "       ++ show val             ++ show sp
        (TokL (LexLowerId val)   sp )  -> "lower case identifier " ++      val             ++ show sp 
        (TokL (LexUpperId val)   sp )  -> "upper case identifier " ++      val             ++ show sp 
        (TokL (LexTextName val)  sp )  -> "text name "             ++      val             ++ show sp
        (TokL (LexTextLine val)  sp )  -> "text line "             ++      val             ++ show sp 
        (TokL (LexSpace)         sp )  -> "spaces "                                        ++ show sp 
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

{- Old token format

returnOutputToken :: [GenToken] -> [Token]
returnOutputToken []                                  = []
returnOutputToken ((GenTok GtkSymbol    s2 i fn):xs)  = (Tok TkSymbol    ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkOp        s2 i fn):xs)  = (Tok TkOp        ""  s2 i fn):returnOutputToken xs 
returnOutputToken ((GenTok GtkKeyword   s2 i fn):xs)  = (Tok TkKeyword   ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkString    s2 i fn):xs)  = (Tok TkString    ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkExpl      s2 i fn):xs)  = (Tok TkExpl      ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkAtom      s2 i fn):xs)  = (Tok TkAtom      ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkInteger8  s2 i fn):xs)  = (Tok TkInteger8  ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkInteger10 s2 i fn):xs)  = (Tok TkInteger10 ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkInteger16 s2 i fn):xs)  = (Tok TkInteger16 ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkVarid     s2 i fn):xs)  = (Tok TkVarid     ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkConid     s2 i fn):xs)  = (Tok TkConid     ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkTextnm    s2 i fn):xs)  = (Tok TkTextnm    ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkTextln    s2 i fn):xs)  = (Tok TkTextln    ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkSpace     s2 i fn):xs)  = (Tok TkSpace     ""  s2 i fn):returnOutputToken xs
returnOutputToken ((GenTok GtkError     s2 i fn):xs)  = (Tok TkError     ""  s2 i fn):returnOutputToken xs
-}

-- New Lexeme Token format

returnOutputToken :: [GenToken] -> [TokenL]
returnOutputToken []                                          = []
returnOutputToken ((GenTok GtkSymbol    val (Pos l c) fn):xs)  = (TokL (LexSymbol val)               (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkOp        val (Pos l c) fn):xs)  = (TokL (LexOp val)                   (newPos  fn l c)):returnOutputToken xs 
returnOutputToken ((GenTok GtkKeyword   val (Pos l c) fn):xs)  = (TokL (LexKeyword val)              (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkString    val (Pos l c) fn):xs)  = (TokL (LexString val)               (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkExpl      val (Pos l c) fn):xs)  = (TokL (LexExpl val)                 (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkAtom      val (Pos l c) fn):xs)  = (TokL (LexAtom val)                 (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkInteger8  val (Pos l c) fn):xs)  = (TokL (LexInteger (read val))       (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkInteger10 val (Pos l c) fn):xs)  = (TokL (LexInteger (read val))       (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkInteger16 val (Pos l c) fn):xs)  = (TokL (LexInteger (read val))       (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkVarid     val (Pos l c) fn):xs)  = (TokL (LexLowerId val)              (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkConid     val (Pos l c) fn):xs)  = (TokL (LexUpperId val)              (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkTextnm    val (Pos l c) fn):xs)  = (TokL (LexTextName val)             (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkTextln    val (Pos l c) fn):xs)  = (TokL (LexTextLine val)             (newPos  fn l c)):returnOutputToken xs
returnOutputToken ((GenTok GtkSpace     val (Pos l c) fn):xs)  = (TokL LexSpace                      (newPos  fn l c)):returnOutputToken xs
