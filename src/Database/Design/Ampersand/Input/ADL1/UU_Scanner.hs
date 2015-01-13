{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash #-}
module Database.Design.Ampersand.Input.ADL1.UU_Scanner 
         ( scan,initPos,Pos(..)
         , Token(..),TokenType(..),noPos
         )
where

import Data.Char hiding(isSymbol)
import Data.List
import Data.Maybe
import Database.Design.Ampersand.Input.ADL1.UU_BinaryTrees(tab2tree,btLocateIn)
---- import UU.Parsing(Symbol(..),IsParser,pSym,(<$>))
import Database.Design.Ampersand.Basics (fatalMsg)
fatal :: Int -> String -> a
fatal = fatalMsg "Scanner"

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

type Line = Int
type Column = Int

data Pos = Pos{line:: !Line, column:: !Column} deriving (Eq, Ord, Show)
type Filename   = String

data Token = Tok { tp' :: TokenType
                 , val1 :: String
                 , val2 :: String
                 , pos :: !Pos
                 , file :: !Filename
                 }

instance Eq Token where
  (Tok ttypel     stringl _ _ _) ==  (Tok ttyper     stringr _ _ _) =  ttypel == ttyper && stringl == stringr

instance   Ord Token where
  compare x y | x==y      = EQ
              | x<=y      = LT
              | otherwise = GT
  (Tok ttypel     stringl _ _ _ ) <= (Tok ttyper    stringr _ _  _ )
      =     ttypel <  ttyper
        || (ttypel == ttyper && stringl <= stringr)

maybeshow :: Pos -> Filename -> String
maybeshow (Pos 0 0) _  =  ""
maybeshow (Pos l c) fn =  " at line " ++ show l
                       ++ ", column " ++ show c
                       ++ " of file " ++ show fn

initPos :: Pos
initPos = Pos 1 1

noPos :: Pos
noPos = Pos 0 0

advl ::  Line -> Pos ->Pos
advl i (Pos l _) = Pos (l+i) 1

advc :: Column -> Pos ->  Pos
advc i (Pos l c) = Pos l (c+i)

adv :: Pos -> Char -> Pos
adv pos' c = case c of
  '\t' -> advc (tabWidth (column pos')) pos'
  '\n' -> advl 1 pos'
  _    -> advc 1 pos'

tabWidth :: Column -> Int
tabWidth c = 8 - ((c-1) `mod` 8)

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

scan :: [String] -> [String] -> String -> String -> String -> Pos -> String -> [Token]
scan keywordstxt keywordsops specchars opchars fn pos' input
  = []
