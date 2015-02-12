module Database.Design.Ampersand.Input.ADL1.LexerToken
(Token, Tok, SourcePos)
where

import Text.Parsec.Pos(SourcePos)

type Token = (SourcePos,Tok)

data Tok = TkSymbol String
         | TkOp String
         | TkKeyword String
         | TkString String
         | TkExpl String
         | TkAtom String
         | TkChar String
         | TkInteger8 String
         | TkInteger10 String
         | TkInteger16 String
         | TkUpperId String
         | TkLowerId String
         | TkTextName String
         | TkTextLine String
         | TkSpace
    deriving Show