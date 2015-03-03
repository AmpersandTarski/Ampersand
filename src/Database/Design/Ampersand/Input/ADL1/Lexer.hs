{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Database.Design.Ampersand.Input.ADL1.Lexer (
    keywords, operators, special_chars
    )
where

import Database.Design.Ampersand.Input.ADL1.LexerToken
import Database.Design.Ampersand.Input.ADL1.LexerMonad
import Database.Design.Ampersand.Input.ADL1.LexerMessage
import Database.Design.Ampersand.Input.ADL1.LexerBinaryTrees
import Text.Parsec.Pos hiding (Line, Column)
import Control.Monad.Identity (Identity)
import Data.Char hiding(isSymbol, isSpace)
import Data.Maybe
import Data.Either
import Data.List (sort, nub)
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
operators = [ "|-", "-", "->", "<-", ">", "=", "~", "+", "*", ";", "!", "#",
              "::", ":", "\\/", "/\\", "\\", "/", "<>" , "..", "." , "0", "1"]

special_chars :: [Char]
special_chars = "()[],{}"

opchars :: String
opchars = nub (sort (concat operators))

runLexer :: [Options] -> [Char] -> Filename -> [GenToken]
runLexer opt fn input = case (lexer opt fn input) of
                          Left error  -> []
                          Right (x,y) -> x
                          
lexer :: [Options] -> String -> [Char] -> Either LexerError ([GenToken], [LexerWarning])
lexer opt fileName input = runLexerMonad opt fileName (mainLexer noPos fileName input)

type Lexer = Pos -> Filename  -> [Char]  -> LexerMonad [GenToken]

mainLexer :: Lexer

-----------------------------------------------------------
-- Removing unnecessary text artifacts (comment, spaces,...)
-----------------------------------------------------------


mainLexer p fn [] =  return []
mainLexer p fn ('-':'-':s) = mainLexer p fn (dropWhile (/= '\n') s)

mainLexer p fn (c:s) | isSpace c = let (sp,next) = span isSpace s
                                in  mainLexer (foldl adv p (c:sp)) fn next
								
mainLexer p fn ('-':'+':s)  = returnGenToken GtkExpl (dropWhile isSpace (takeWhile (/= '\n') s)) p mainLexer p fn (dropWhile (/= '\n') s)


mainLexer p fn ('{':'-':s)  = lexNest mainLexer (advc 2 p) fn s
mainLexer p fn ('{':'+':s)  = lexExpl mainLexer (advc 2 p) fn s
mainLexer p fn ('"':ss) =  let (s,swidth,rest) = scanString ss
                           in if null rest || head rest /= '"'
                              then lexerError (NonTerminatedChar (Just(s))) (initialPos fn)
                              else returnGenToken GtkString s p mainLexer (advc (swidth+2) p) fn (tail rest)

{- In Ampersand, atoms may be promoted to singleton relations by single-quoting them. For this purpose, we treat
   single quotes exactly as the double quote for strings. That substitutes the scanner code for character literals. -}
mainLexer p fn ('\'':ss)
     = let (s,swidth,rest) = scanAtom ss
       in if null rest || head rest /= '\''
             then lexerError UnterminatedAtom (initialPos fn)
             else returnGenToken GtkAtom s p mainLexer (advc (swidth+2) p) fn (tail rest)

-----------------------------------------------------------
-- Handling infix operators
-----------------------------------------------------------

mainLexer p fn ('`':ss)
     = case ss of
         []    -> lexerError UnterminatedInfix (initialPos fn)
         (c:s) -> let res | isIdStart c || isUpper c =
                                   let (name,p1,rest) = scanIdent (advc 2 p) s
                                       ident = c:name
                                       tokens | null rest ||
                                                head rest /= '`' = lexerError UnterminatedInfix (initialPos fn)
                                              | iskw ident       = lexerError (UnexpectedInfixKeyword ident) (initialPos fn)
                                              | otherwise        = returnGenToken GtkOp ident p mainLexer (advc 1 p1) fn (tail rest)
                                   in tokens
                          | otherwise =  lexerError (UnexpectedInfixChar c) (initialPos fn)
                  in res

-----------------------------------------------------------
-- looking for keywords - operators - special chars
-----------------------------------------------------------

mainLexer p fn cs@(c:s)
     | isSymbol c = returnGenToken GtkSymbol [c] p mainLexer (advc 1 p) fn s
     | isIdStart c || isUpper c
         = let (name', p', s')    = scanIdent (advc 1 p) s
               name               = c:name'
               tokt   | iskw name = GtkKeyword
                      | null name' && isSymbol c  
                                  =  GtkSymbol 
                      | otherwise = if isIdStart c then GtkVarid else GtkConid
               val    | null name' && isSymbol c 
                                  = [c]
                      | otherwise = name				  
           in returnGenToken tokt val p mainLexer p' fn s'
     | isOpsym c = let (name, s') = getOp cs
                       tokt | isop name = GtkKeyword
                            | otherwise =  GtkOp
                   in returnGenToken tokt name p mainLexer (foldl adv p name) fn s'
     | isDigit c = let (tktype,number,width,s') = getNumber cs
                   in  returnGenToken tktype number p mainLexer (advc width p) fn s'
     | otherwise = lexerError (UnexpectedChar c) (initialPos fn)

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
isOpsym  = locatein opchars

isIdStart c = isLower c || c == '_'
isIdChar c =  isAlphaNum c || c == '_'

getOp cs -- the longest prefix of cs occurring in keywordsops
    = f operators cs ""
      where
       f ops (e:s) op = if null [s' | o:s'<-ops, e==o] then (op,e:s) --was: f ops (e:s) op = if and (map null ops) then (op,e:s) --b.joosten
                        else f [s' | o:s'<-ops, e==o] s (op++[e])
       f []  _     _  = ("",cs)
       f _   []    op = (op,[])


scanIdent p s = let (name,rest) = span isIdChar s
                in (name,advc (length name) p,rest)
				
				
scanAtom :: [Char] -> ([Char],Int,[Char])
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
lexNest cont pos' fn inp = lexNest' cont pos' fn inp
 where lexNest' c p fn ('-':'}':s) = c (advc 2 p) fn s
       lexNest' c p fn ('{':'-':s) = lexNest' (lexNest' c) (advc 2 p) fn s
       lexNest' c p fn (x:s)       = lexNest' c (adv p x) fn s
       lexNest' _ _ _ []           = lexerError UnterminatedComment (initialPos fn)

lexExpl :: Lexer -> Lexer
lexExpl cont pos' fn inp = lexExpl' "" cont pos' fn inp
 where lexExpl' str c p fn ('-':'}':s) = returnGenToken GtkExpl str p mainLexer (advc 2 p)  fn s
       lexExpl' str c p fn ('{':'-':s) = lexNest (lexExpl' str c) (advc 2 p) fn s
       lexExpl' str c p fn ('-':'-':s) = lexExpl' str c  p fn (dropWhile (/= '\n') s)
       lexExpl' str c p fn (x:s)       = lexExpl' (str++[x]) c (adv p x) fn s
       lexExpl' _   _ _ _  []          = lexerError UnterminatedPurpose (initialPos fn)
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
  
getNumber :: [Char] -> (GenTokenType, [Char], Int, [Char])
getNumber [] = fatal 294 "getNumber" 
getNumber cs@(c:s)
  | c /= '0'         = num10
  | null s           = const0
  | hs `elem` "xX"   = num16
  | hs `elem` "oO"   = num8
  | otherwise        = num10
  where (hs:ts) = s
        const0 = (GtkInteger10, "0",1,s)
        num10  = let (n,r) = span isDigit cs
                 in (GtkInteger10,n,length n,r)
        num16   = readNum isHexaDigit  ts GtkInteger16
        num8    = readNum isOctalDigit ts GtkInteger8
        readNum p ts' tk
          = let (n,rs) = span p ts'
            in  if null n then const0
                          else (tk         , n, 2+length n,rs)
						  
						  
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

scanString :: [Char] -> ([Char],Int,[Char])
scanString []            = ("",0,[])
scanString ('\\':'&':xs) = let (str,w,r) = scanString xs  -- TODO: why do we ignore \& ?  
                           in (str,w+2,r)
scanString ('\\':'\'':xs) = let (str,w,r) = scanString xs -- escaped single quote: \'  (redundant, but allowed in most languages, and it makes escaping generated code a lot easier.)    
                           in ('\'': str,w+2,r)
scanString ('\'':xs)     = let (str,w,r) = scanString xs  -- single quote: '
                           in ('\'': str,w+1,r)
scanString xs = let (ch,cw,cr) = getchar xs
                    (str,w,r)  = scanString cr
--                    str' = maybe "" (:str) ch
                in maybe ("",0,xs) (\c -> (c:str,cw+w,r)) ch

getchar :: [Char] -> (Maybe Char, Int, [Char])
getchar []          = (Nothing,0,[])
getchar s@('\n':_ ) = (Nothing,0,s)
getchar s@('\t':_ ) = (Nothing,0,s)
getchar s@('\'':_ ) = (Nothing,0,s)
getchar s@('"' :_ ) = (Nothing,0,s)
getchar   ('\\':xs) = let (c,l,r) = getEscChar xs
                      in (c,l+1,r)
getchar (x:xs)      = (Just x,1,xs)

getEscChar :: [Char] -> (Maybe Char, Int, [Char])
getEscChar [] = (Nothing,0,[])
getEscChar s@(x:xs) | isDigit x = let (tp,n,len,rest) = getNumber s
                                      val = case tp of
                                              GtkInteger8  -> readn 8  n
                                              GtkInteger16 -> readn 16 n
                                              GtkInteger10 -> readn 10 n
                                              _           -> fatal 279 "getExcChar: unknown tokentype."
                                  in  if val >= 0 && val <= 255
                                         then (Just (chr val),len, rest)
                                         else (Nothing,1,rest)
                    | otherwise = case x `lookup` cntrChars of
                                 Nothing -> (Nothing,0,s)
                                 Just c  -> (Just c,1,xs)
  where cntrChars = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t')
                    ,('v','\v'),('\\','\\'),('"','\"')]
					
					
readn :: Int -> [Char] -> Int
readn base = foldl (\r x  -> value x + base * r) 0

-----------------------------------------------------------
-- Generic LexerMonad Token Maker
-----------------------------------------------------------

returnGenToken :: GenTokenType -> String -> Pos -> Lexer -> Lexer
returnGenToken gtokt val post continue posi fn input = do
    let token = makeGenToken gtokt val post fn
    tokens <- continue posi fn input
    return (token:tokens)
