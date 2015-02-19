{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Lexer (
    keywords, operators, special_chars,
    pKey, pConid, pString, pSpec, pExpl, pVarid, pAtom, pComma, pSemi,
    SourceName
)
where

import LexerToken(Token, Lexeme)
--import LexerMonad
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Token
import Control.Monad.Identity (Identity)
import Data.Char (isUpper)

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
-- lexer :: String -> [Char] -> Either LexerError ([Token], [LexerWarning])
--lexer fileName input = runLexerMonad fileName (mainLexer input)

--temporary lexer to start with the building of the mainLexer function
lexer :: [Char] -> Either LexerError ([Token], [LexerWarning])
lexer input = mainLexer input

type Lexer = [Char] -> LexerMonad [Token]

mainLexer :: Lexer
--mainLexer [] = do
--    checkBracketsAtEOF
--    pos <- getPos
--    return [(incSourceLine (setSourceColumn pos 0) 1, LexEOF)]

mainLexer ('-':'-':cs) 
    | not (nextCharSatisfy isSymbol rest) = do
        incPos (2 + length minuses)
        lexOneLineComment rest
    where
        (minuses, rest) = span (== '-') cs
        
--mainLexer ('{':'-':cs) = do 
--    pos <- getPos 
--    incPos 2
--    lexMultiLineComment [pos] 0 cs 



