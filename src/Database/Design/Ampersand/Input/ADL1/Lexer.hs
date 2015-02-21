{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Database.Design.Ampersand.Input.ADL1.Lexer (
    keywords, operators, special_chars
    )
where

import Database.Design.Ampersand.Input.ADL1.LexerToken
--import LexerMonad
--import Text.Parsec.Char
--import Text.Parsec.Combinator
import Database.Design.Ampersand.Input.ADL1.LexerBinaryTrees
--import Text.Parsec.Pos
--import Text.Parsec.Prim
--import Text.Parsec.Token
import Control.Monad.Identity (Identity)
import Data.Char (isUpper)
import Data.Maybe
import Data.List (sort)


--  The Ampersand scanner takes the file name (String) for documentation and error messaging.
--   scanner :: String -> String -> [Token]
--   scanner fn str = scan keywordstxt keywordsops specialchars opchars fn initPos str

keywords :: [String]
keywords      = [ "INCLUDE"
                , "CONTEXT", "ENDCONTEXT", "EXTENDS", "THEMES"
                , "META"
                , "PATTERN", "ENDPATTERN"
                , "PROCESS", "ENDPROCESS"
                , "INTERFACE", "CLASS", "FOR", "BOX", "ROWS", "TABS", "COLS", "INITIAL", "SQLPLUG", "PHPPLUG", "TYPE"
                , "POPULATION", "CONTAINS"
                , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX", "IRF", "AUT", "PROP", "ALWAYS"
                , "RULE", "MESSAGE", "VIOLATION", "SRC", "TGT", "TEST"
                , "RELATION", "MEANING", "CONCEPT", "IDENT"
                , "VIEW", "TXT", "PRIMHTML"
                , "KEY" -- HJO, 20130605: Obsolete. Only usefull as long as the old prototype generator is still in use.
                , "IMPORT", "SPEC", "ISA", "IS", "I", "V"
                , "CLASSIFY"
                , "PRAGMA", "PURPOSE", "IN", "REF", "ENGLISH", "DUTCH"
                , "REST", "HTML", "LATEX", "MARKDOWN"
                , "ONE"
                , "BYPLUG"
                , "ROLE", "EDITS", "MAINTAINS"
                ]

operators :: [String]
operators = [ "|-", "-", "->", "<-", ">", "=", "~", "+", "*", ";", "!", "#",
              "::", ":", "\\/", "/\\", "\\", "/", "<>" , "..", "." , "0", "1"]

special_chars :: [Char]
special_chars = "()[],{}"

-- Main Lexer function
-- Steps:
--       * mainLexer fitlers input string to remove all irrelevant data such as comments, spaces,...
--       * runLexerMonad takes the


type Lexer = Pos -> [Char] -> [GenToken]

mainLexer :: Lexer

-----------------------------------------------------------
-- Removing unnecessary text artifacts (comment, spaces,...)
-----------------------------------------------------------


mainLexer p [] = []
mainLexer p ('-':'-':s) = mainLexer p (dropWhile (/= '\n') s)

mainLexer p (c:s) | isSpace c = let (sp,next) = span isSpace s
                                in  mainLexer (foldl adv p (c:sp)) next
								
								
{-}
todo:
   mainLexer p ('-':'+':s)  = 
   mainLexer p ('{':'-':s)  = 
-}

-----------------------------------------------------------
-- looking for keywords - operators - special chars
-----------------------------------------------------------

mainLexer p cs@(c:s)
     | isSymbol c = keyToken TkSymbol [c] p fn
                  : doScan(advc 1 p) s
     | isIdStart c || isUpper c
         = let (name', p', s')    = scanIdent (advc 1 p) s
               name               = c:name'
               tok    | iskw name = keyToken TkKeyword name p fn
                      | null name' && isSymbol c
                                  = keyToken TkSymbol [c] p fn
                      | otherwise = token (if isIdStart c then TkVarid else TkConid) name p fn
           in tok :  doScan p' s'
     | isOpsym c = let (name, s') = getOp cs   -- was:      span isOpsym cs
                       tok | isop name = keyToken TkKeyword name p fn
                           | otherwise = keyToken TkOp name p fn
                   in tok : doScan (foldl adv p name) s'
     | isDigit c = let (tktype,number,width,s') = getNumber cs
                   in  token tktype number p fn : doScan (advc width p) s'
     | otherwise = errToken ("Unexpected character " ++ show c) p fn
                 : doScan (adv p c) s


	

-----------------------------------------------------------
-----------------------------------------------------------
-- Supporting functions for MainLexer
-----------------------------------------------------------
-----------------------------------------------------------

-----------------------------------------------------------
-- Check on keywords - operators - special chars
-----------------------------------------------------------

locatein :: Ord a => [a] -> a -> Bool
locatein es = isJust . btLocateIn compare (tab2tree (sort es))
iskw     = locatein keywords
isop     = locatein operators
isSymbol = locatein special_chars


--scanIdent p s = let (name,rest) = span isIdChar s
--                in (name,advc (length name) p,rest)

-----------------------------------------------------------
-- String clean-up functions
-----------------------------------------------------------

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\n' || c == '\t' || c == '\r'
				
				
-----------------------------------------------------------
-- position handling
-----------------------------------------------------------

tabWidth :: Column -> Int
tabWidth c = 8 - ((c-1) `mod` 8)

advl ::  Line -> Pos -> Pos
advl i (Pos l _) = Pos (l+i) 1

advc :: Column -> Pos ->  Pos
advc i (Pos l c) = Pos l (c+i)


adv :: Pos -> Char -> Pos
adv pos' c = case c of
  '\t' -> advc (tabWidth (column pos')) pos'
  '\n' -> advl 1 pos'
  '\r' -> advl 1 pos'
  _    -> advc 1 pos'