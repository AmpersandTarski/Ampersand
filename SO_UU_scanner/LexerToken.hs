module LexerToken
(Token, Tok)
where

import Text.Parsec.Pos(SourcePos)

--type Token = (SourcePos,Tok,String)

data Token = Tok { tokenT  :: Tok
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
        (Tok TkSymbol    tval sp val)  -> "symbol "                ++ tval         ++ sp ++ val
        (Tok TkOp        sp val)  -> "operator "              ++ val         ++ sp
        (Tok TkKeyword   sp val)  ->                        show val         ++ sp
        (Tok TkString    sp val)  -> "string \""              ++ val ++ "\"" ++ sp
        (Tok TkExpl      sp val)  -> "explanation {+"         ++ val ++ "-}" ++ sp
        (Tok TkAtom      sp val)  -> "atom '"                 ++ val ++ "'"  ++ sp
        (Tok TkChar      sp val)  -> "character '"            ++ val ++ "'"  ++ sp
        (Tok TkInteger   sp val)  -> "decimal Integer "       ++ val         ++ sp
        (Tok TkLowerId   sp val)  -> "lower case identifier " ++ val         ++ sp
        (Tok TkUpperId   sp val)  -> "upper case identifier " ++ val         ++ sp
        (Tok TkTextName  sp val)  -> "text name "             ++ val         ++ sp
        (Tok TkTextLine  sp val)  -> "text line "             ++ val         ++ sp
        (Tok TkSpace     sp val)  -> "spaces "                               ++ sp
       )
