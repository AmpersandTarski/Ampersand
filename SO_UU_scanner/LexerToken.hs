module LexerToken
(Token, Tok)
where

import Text.Parsec.Pos(SourcePos)

--type Token = (SourcePos,Tok,String)

data Token = Tokv { tokenT  :: Tok
                 , sp      :: SourcePos
                 , val     :: String
                 }

data Tok = TkSymbol String
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
	
instance Show Token where
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
