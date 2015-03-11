{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Database.Design.Ampersand.Input.ADL1.Lexer (
    keywords, operators, symbols, lexer
) where

import Database.Design.Ampersand.Input.ADL1.LexerToken
import Database.Design.Ampersand.Input.ADL1.LexerMonad
import Database.Design.Ampersand.Input.ADL1.LexerMessage
import Database.Design.Ampersand.Input.ADL1.LexerBinaryTrees
import Text.Parsec.Pos hiding (Line, Column)
import Data.Char hiding(isSymbol, isSpace)
import Data.Maybe
import Data.List (sort)
import Database.Design.Ampersand.Basics (fatalMsg)
import Database.Design.Ampersand.Misc

fatal :: Int -> String -> a
fatal = fatalMsg "Lexer"

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
operators = [ "|-", "-", "->", "<-", "=", "~", "+", "*", ";", "!", "#",
              "::", ":", "\\/", "/\\", "\\", "/", "<>" , "..", "." , "0", "1"]

symbols :: [Char]
symbols = "()[],{}<>"

lexer :: [Options] -> Filename -> String -> Either LexerError ([Token], [LexerWarning])
lexer opt file input = case runLexerMonad opt file (mainLexer noPos file input) of
                               Left err -> Left err
                               Right (ts, ws) -> Right (ts, ws)

-----------------------------------------------------------
-- Help functions
-----------------------------------------------------------

skipLine :: String -> String
skipLine = dropWhile (/= '\n')

takeLine :: String -> String
takeLine = takeWhile (/= '\n')

-----------------------------------------------------------
-- Lexer definition
-----------------------------------------------------------

type Lexer = Pos -> Filename  -> String  -> LexerMonad [Token]

mainLexer :: Lexer

-----------------------------------------------------------
-- Removing unnecessary text artifacts (comment, spaces,...)
-----------------------------------------------------------

mainLexer _ _ [] =  return []
mainLexer p fn ('-':'-':s) = mainLexer p fn (skipLine s)

mainLexer p fn (c:s) | isSpace c = let (spc,next) = span isSpace s
                                in  mainLexer (foldl adv p (c:spc)) fn next

mainLexer p fn ('-':'+':s)  = returnToken lx p mainLexer p fn rest
                where lx   = LexExpl $ dropWhile isSpace (takeLine s)
                      rest = skipLine s

mainLexer p fn ('{':'-':s)  = lexNest mainLexer (advc 2 p) fn s
mainLexer p fn ('{':'+':s)  = lexExpl mainLexer (advc 2 p) fn s
mainLexer p fn ('"':ss) =  let (s,swidth,rest) = scanString ss
                           in if null rest || head rest /= '"'
                              then lexerError (NonTerminatedChar (Just(s))) (initialPos fn)
                              else returnToken (LexString s) p mainLexer (advc (swidth+2) p) fn (tail rest)

{- In Ampersand, atoms may be promoted to singleton relations by single-quoting them. For this purpose, we treat
   single quotes exactly as the double quote for strings. That substitutes the scanner code for character literals. -}
mainLexer p fn ('\'':ss)
     = let (s,swidth,rest) = scanAtom ss
       in if null rest || head rest /= '\''
             then lexerError UnterminatedAtom (initialPos fn)
             else returnToken (LexAtom s) p mainLexer (advc (swidth+2) p) fn (tail rest)

-----------------------------------------------------------
-- Handling infix operators
-----------------------------------------------------------

--TODO: What are infix operators? Isn't this only for Haskell?
mainLexer p fn ('`':ss)
     = case ss of
         []    -> lexerError UnterminatedInfix (initialPos fn)
         (c:s) -> let res | isIdStart c || isUpper c =
                                   let (name,p1,rest) = scanIdent (advc 2 p) s
                                       ident = c:name
                                       tokens | null rest ||
                                                head rest /= '`' = lexerError UnterminatedInfix (initialPos fn)
                                              | iskw ident       = lexerError (UnexpectedInfixKeyword ident) (initialPos fn)
                                              | otherwise        = returnToken (LexOperator ident) p mainLexer (advc 1 p1) fn (tail rest)
                                   in tokens
                          | otherwise =  lexerError (UnexpectedInfixChar c) (initialPos fn)
                  in res

-----------------------------------------------------------
-- looking for keywords - operators - special chars
-----------------------------------------------------------

mainLexer p fn cs@(c:s)
     | isIdStart c || isUpper c
         = let (name', p', s')    = scanIdent (advc 1 p) s
               name               = c:name'
               tokt   | iskw name = LexKeyword name
                      | otherwise = if isIdStart c
                                    then LexVarId name
                                    else LexConId name
           in returnToken tokt p mainLexer p' fn s'
     | isOperatorBegin c
         = let (name, s') = getOp cs
           in returnToken (LexOperator name) p mainLexer (foldl adv p name) fn s'
     | isSymbol c = returnToken (LexSymbol c) p mainLexer (advc 1 p) fn s
     | isDigit c
         = let (tk,width,s') = getNumber cs
           in  returnToken tk p mainLexer (advc width p) fn s'
     | otherwise  = lexerError (UnexpectedChar c) (initialPos fn)

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

iskw :: String -> Bool
iskw = locatein keywords

isSymbol :: Char -> Bool
isSymbol = locatein symbols

isOperatorBegin :: Char -> Bool
isOperatorBegin  = locatein (map head operators)

isIdStart :: Char -> Bool
isIdStart c = isLower c || c == '_'

isIdChar :: Char -> Bool
isIdChar c =  isAlphaNum c || c == '_'

-- Finds the longest prefix of cs occurring in keywordsops
getOp :: String -> (String, String)
getOp cs = findOper operators cs ""
    where findOper :: [String] -> String -> String -> (String,String)
          findOper []  _     _  = ("", cs)
          findOper _   []    op = (op,[])
          findOper ops (c:rest) op =
              if null found then (op, c:rest)
              else findOper found rest (op ++ [c])
              where found = [s' | o:s'<-ops, c==o]

scanIdent :: Pos -> String -> (String, Pos, String)
scanIdent p s = let (name,rest) = span isIdChar s
                in (name,advc (length name) p,rest)


scanAtom :: String -> (String,Int,String)
scanAtom []              = ("",0,[])
scanAtom ('\\':'&':xs)   = let (str,w,r) = scanAtom xs
                           in (str,w+2,r)
scanAtom ('"':xs)        = let (str,w,r) = scanAtom xs
                           in ('"': str,w+1,r)
scanAtom xs   = let (ch,cw,cr) = getchar xs
                    (str,w,r)  = scanAtom cr
--                    str' = maybe "" (:str) ch
                in maybe ("",0,xs) (\c -> (c:str,cw+w,r)) ch

-----------------------------------------------------------
-- String clean-up functions / comments
-----------------------------------------------------------

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

lexNest :: Lexer -> Lexer
lexNest c p fn ('-':'}':s) = c (advc 2 p) fn s
lexNest c p fn ('{':'-':s) = lexNest (lexNest c) (advc 2 p) fn s
lexNest c p fn (x:s)       = lexNest c (adv p x) fn s
lexNest _ _ fn []          = lexerError UnterminatedComment (initialPos fn)

--TODO: Also accept {+ ... +} as delimiters
lexExpl :: Lexer -> Lexer
lexExpl cont pos' file inp = lexExpl' "" cont pos' file inp
 where lexExpl' str _ p fn ('-':'}':s) = returnToken (LexExpl str) p mainLexer (advc 2 p)  fn s
       lexExpl' str c p fn ('{':'-':s) = lexNest (lexExpl' str c) (advc 2 p) fn s
       lexExpl' str c p fn ('-':'-':s) = lexExpl' str c  p fn (dropWhile (/= '\n') s)
       lexExpl' str c p fn (x:s)       = lexExpl' (str++[x]) c (adv p x) fn s
       lexExpl' _   _ _ fn []          = lexerError UnterminatedPurpose (initialPos fn)

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

-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------

-- Returns tuple with the parsed lexeme, the amount of read characters and the rest of the text
getNumber :: String -> (Lexeme, Int, String)
getNumber [] = fatal 294 "getNumber"
getNumber cs@(c:s)
  | c /= '0'         = num10
  | null s           = const0
  | hs `elem` "xX"   = num16
  | hs `elem` "oO"   = num8
  | otherwise        = num10
  where (hs:ts) = s
        const0 = (LexInteger 0, 1, s)
        num10  = let (n,r) = span isDigit cs
                 in (LexInteger (read n), length n, r)
        num16   = readNum isHexaDigit  16 ts
        num8    = readNum isOctalDigit 8 ts
        readNum p base ts'
          = let (n,rs) = span p ts'
            in  if null n
                then const0
                --TODO: Are the numbers being read correctly?
                else (LexInteger (readn base n), 2 + length n, rs)

isHexaDigit :: Char -> Bool
isHexaDigit  d = isDigit d || (d >= 'A' && d <= 'F') || (d >= 'a' && d <= 'f')

isOctalDigit :: Char -> Bool
isOctalDigit d = d >= '0' && d <= '7'

value :: Char -> Int
value c | isDigit c = ord c - ord '0'
        | isUpper c = ord c - ord 'A' + 10
        | isLower c = ord c - ord 'a' + 10
        | otherwise = fatal 321 ("value undefined for '"++ show c++"'")

-----------------------------------------------------------
-- characters / strings
-----------------------------------------------------------

scanString :: String -> (String, Int, String)
scanString []            = ("",0,[])
scanString ('\\':'&':xs) = let (str,w,r) = scanString xs  -- TODO: why do we ignore \& ?
                           in (str,w+2,r)
scanString ('\\':'\'':xs) = let (str,w,r) = scanString xs -- escaped single quote: \'  (redundant, but allowed in most languages, and it makes escaping generated code a lot easier.)
                           in ('\'': str,w+2,r)
scanString ('\'':xs)     = let (str,w,r) = scanString xs  -- single quote: '
                           in ('\'': str,w+1,r)
scanString xs = let (ch,cw,cr) = getchar xs
                    (str,w,r)  = scanString cr
                in maybe ("",0,xs) (\c -> (c:str,cw+w,r)) ch

getchar :: String -> (Maybe Char, Int, String)
getchar []          = (Nothing,0,[])
getchar s@('\n':_ ) = (Nothing,0,s)
getchar s@('\t':_ ) = (Nothing,0,s)
getchar s@('\'':_ ) = (Nothing,0,s)
getchar s@('"' :_ ) = (Nothing,0,s)
getchar   ('\\':xs) = let (c,l,r) = getEscChar xs
                      in (c,l+1,r)
getchar (x:xs)      = (Just x,1,xs)

getEscChar :: String -> (Maybe Char, Int, String)
getEscChar [] = (Nothing,0,[])
getEscChar s@(x:xs) | isDigit x = let (LexInteger val, len, rest) = getNumber s
                                  in  if val >= 0 && val <= 255
                                         then (Just (chr val),len, rest)
                                         else (Nothing, 1, rest)
                    | otherwise = case x `lookup` cntrChars of
                                 Nothing -> (Nothing,0,s)
                                 Just c  -> (Just c,1,xs)
  where cntrChars = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t')
                    ,('v','\v'),('\\','\\'),('"','\"')]

readn :: Int -> String -> Int
readn base = foldl (\r x  -> value x + base * r) 0

-----------------------------------------------------------
-- Token creation function
-----------------------------------------------------------

--TODO: Can we simplify this? The complete signature is too complicated! See:
--returnToken :: Lexeme -> Pos -> Pos -> Filename  -> String  -> LexerMonad [Token] -> Pos -> Filename  -> String  -> LexerMonad [Token]
returnToken :: Lexeme -> Pos -> Lexer -> Lexer
returnToken lx (Pos ln col) continue posi fn input = do
    let token = Tok lx (newPos fn ln col)
    tokens <- continue posi fn input
    return (token:tokens)
