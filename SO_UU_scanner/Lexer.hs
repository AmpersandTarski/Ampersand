{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Lexer (
    keywords, operators, special_chars,
    pKey, pConid, pString, pSpec, pExpl, pVarid, pAtom, pComma, pSemi,
    SourceName
)
where

import LexerToken(Token, Tok)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Token
import Control.Monad.Identity (Identity)
import Data.Char (isUpper)

type AmpT a = ParsecT String [Token] Identity a

lexer :: TokenParser [Token]
lexer = makeTokenParser langDef

langDef :: LanguageDef [Token]
langDef = LanguageDef {
        commentStart = "{-",
        commentEnd = "-}",
        commentLine = "--",
        nestedComments = True,
        identStart = letter <|> char '_',
        identLetter = alphaNum <|> char '_',
        opStart = oneOf $ map head operators,
        opLetter = oneOf $ concat $ map tail operators,
        reservedNames = keywords,
        reservedOpNames = operators,
        caseSensitive = True
    }

--Temprary function to test the parser in ghci - to be removed before deploy
runP :: Show a => AmpT a -> String -> IO ()
runP p input 
        = case (runParserT p [] a0 input ) of
                Left err -> do{ putStr "parse error at "
                              ; print err
                              }
                Right x -> print x --}
	
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

--Remark: description is different from the type check in GHCI!
-- scan :: [String] -> [String] -> String -> String -> String -> SourcePos -> String -> [Token]
-- scan = 

--Remark: description is different from the type check in GHCI!
initPos :: SourceName -> SourcePos
initPos = initialPos

-- The lexing
pKey :: String -> AmpT ()
pKey = reserved lexer

--- Conid ::= UpperChar (Char | '_')*
pConid :: AmpT String
pConid = lexeme lexer $ try $
        do name <- identifier lexer
           if isUpper $ head name
           then return name
           else unexpected ("Expected upper case identifier but got " ++ show name)

--- String ::= '"' Any* '"'
--- StringListSemi ::= String (';' String)*
pString :: AmpT String
pString = stringLiteral lexer

-- Spec just matches the given character so it has no EBNF
pSpec :: Char -> AmpT String
pSpec x = do { y <- char x; return [y] }

--- Expl ::= '{+' Any* '-}'
pExpl :: AmpT String
pExpl = do try (string "{+")
           inExpl
        where inExpl =  do{ try (string "+}")            ; return "explanation" }
                    <|> do{ skipMany1 (noneOf "+}")      ; inExpl } -- TODO: We shouldn't skip them of course
                    <?> "end of comment"

--- Varid ::= (LowerChar | '_') (Char | '_')*
pVarid :: AmpT String
pVarid = lexeme lexer $ try $
        do name <- identifier lexer
           if isUpper $ head name
           then unexpected ("Expected lower case identifier but got " ++ show name)
           else return name

-- TODO: does not escape, i.e. 'Mario\'s Pizzas' will fail to parse
pAtom :: AmpT String
pAtom   = lexeme lexer (
             do between (char '\'')
                        (char '\'' <?> "end of atom")
                        (many $ satisfy isLetter)
                <?> "atom")
            where isLetter c = (c /= '\'') && (c /= '\\') && (c > '\026')

--- Comma ::= ','
pComma :: AmpT String
pComma  = pSpec ','

--- Semi ::= ';'
pSemi :: AmpT String
pSemi = pSpec ';'


