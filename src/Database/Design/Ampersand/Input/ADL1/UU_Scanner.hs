{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, MagicHash #-}
module Database.Design.Ampersand.Input.ADL1.UU_Scanner 
         ( scan,initPos,Pos(..)
         , Token(..),TokenType(..),noPos
         , pKey,pConid,pString,pSpec,pExpl,pVarid,pComma,pInteger,pSemi)
where

import Data.Char hiding(isSymbol)
import Data.List
import Data.Maybe
import Database.Design.Ampersand.Input.ADL1.UU_BinaryTrees(tab2tree,btLocateIn)
import UU.Parsing(Symbol(..),IsParser,pSym,(<$>))
import Database.Design.Ampersand.Basics (fatalMsg)
fatal :: Int -> String -> a
fatal = fatalMsg "UU_Scanner"

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
  --(Tok TkOp       ""      l _ _) ==  (Tok TkOp       ""      r _ _) =  l == r
  --(Tok TkOp       ""      l _ _) ==  (Tok TkOp       r       _ _ _) =  l == r
  --(Tok TkOp       l       _ _ _) ==  (Tok TkOp       ""      r _ _) =  l == r
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

instance  Symbol Token where
  deleteCost (Tok TkKeyword _ _ _ _) = 10#
  deleteCost _                       = 5#

keyToken,token :: TokenType -> String -> Pos -> Filename -> Token
keyToken tp key  = Tok tp key key
token  tp  = Tok tp ""

errToken :: String -> Pos -> Filename -> Token
errToken = token TkError

scan :: [String] -> [String] -> String -> String -> String -> Pos -> String -> [Token]
scan keywordstxt keywordsops specchars opchars fn pos' input
  = doScan pos' input

 where
   locatein :: Ord a => [a] -> a -> Bool
   locatein es = isJust . btLocateIn compare (tab2tree (sort es))
   iskw     = locatein keywordstxt
   isop     = locatein keywordsops
   isSymbol = locatein specchars
   isOpsym  = locatein opchars

   isIdStart c = isLower c || c == '_'

   isIdChar c =  isAlphaNum c
--               || c == '\''   -- character literals are not used in Ampersand. Since this scanner was used for Haskell-type languages, this alternative is commented out...
              || c == '_'

   scanIdent p s = let (name,rest) = span isIdChar s
                   in (name,advc (length name) p,rest)
   doScan _ [] = []
   doScan p (c:s)        | isSpace c = let (sp,next) = span isSpace s
                                       in  doScan (foldl adv p (c:sp)) next

   doScan p ('-':'-':s)  = doScan p (dropWhile (/= '\n') s)
   doScan p ('-':'+':s)  = token TkExpl (dropWhile isSpace (takeWhile (/= '\n') s)) p fn
                           : doScan p (dropWhile (/= '\n') s)
   doScan p ('{':'-':s)  = lexNest fn doScan (advc 2 p) s
   doScan p ('{':'+':s)  = lexExpl fn doScan (advc 2 p) s
   doScan p ('"':ss)
     = let (s,swidth,rest) = scanString ss
       in if null rest || head rest /= '"'
             then errToken "Unterminated string literal" p fn : doScan (advc swidth p) rest
             else token TkString s p fn : doScan (advc (swidth+2) p) (tail rest)
{- In Ampersand, atoms may be promoted to singleton relations by single-quoting them. For this purpose, we treat
   single quotes exactly as the double quote for strings. That substitutes the scanner code for character literals. -}
   doScan p ('\'':ss)
     = let (s,swidth,rest) = scanAtom ss
       in if null rest || head rest /= '\''
             then errToken "Unterminated atom literal" p fn : doScan (advc swidth p) rest
             else token TkAtom s p fn : doScan (advc (swidth+2) p) (tail rest)


   -- In Haskell infix identifiers consist of three separate tokens(two backquotes + identifier)
   doScan p ('`':ss)
     = case ss of
         []    -> [errToken "Unterminated infix identifier" p fn]
         (c:s) -> let res | isIdStart c || isUpper c =
                                   let (name,p1,rest) = scanIdent (advc 2 p) s
                                       ident = c:name
                                       tokens | null rest ||
                                                head rest /= '`' = errToken "Unterminated infix identifier" p fn
                                                                 : doScan p1 rest
                                              | iskw ident       = errToken ("Keyword used as infix identifier: " ++ ident) p fn
                                                                 : doScan (advc 1 p1) (tail rest)
                                              | otherwise        = token TkOp ident p fn
                                                                 : doScan (advc 1 p1) (tail rest)
                                   in tokens
                          | otherwise = errToken ("Unexpected character in infix identifier: " ++ show c) p fn
                                      : doScan (adv p c) s
                  in res
   doScan p cs@(c:s)
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

   getOp cs -- the longest prefix of cs occurring in keywordsops
    = f keywordsops cs ""
      where
       f ops (e:s) op = if null [s' | o:s'<-ops, e==o] then (op,e:s) --was: f ops (e:s) op = if and (map null ops) then (op,e:s) --b.joosten
                        else f [s' | o:s'<-ops, e==o] s (op++[e])
       f []  _     _  = ("",cs)
       f _   []    op = (op,[])

lexNest :: Filename -> (Pos -> [Char] -> [Token]) -> Pos -> [Char] -> [Token]
lexNest fn cont pos' inp = lexNest' cont pos' inp
 where lexNest' c p ('-':'}':s) = c (advc 2 p) s
       lexNest' c p ('{':'-':s) = lexNest' (lexNest' c) (advc 2 p) s
       lexNest' c p (x:s)       = lexNest' c (adv p x) s
       lexNest' _ _ []          = [ errToken "Unterminated nested comment" pos' fn ]

lexExpl :: Filename -> (Pos -> [Char] -> [Token]) -> Pos -> [Char] -> [Token]
lexExpl fn cont pos' inp = lexExpl' "" cont pos' inp
 where lexExpl' str c p ('-':'}':s) = token TkExpl str p fn: c (advc 2 p) s
       lexExpl' str c p ('{':'-':s) = lexNest fn (lexExpl' str c) (advc 2 p) s
       lexExpl' str c p ('-':'-':s) = lexExpl' str c  p (dropWhile (/= '\n') s)
       lexExpl' str c p (x:s)       = lexExpl' (str++[x]) c (adv p x) s
       lexExpl' _ _ _ []            = [ errToken "Unterminated PURPOSE section" pos' fn ]

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

getchar :: [Char] -> (Maybe Char, Int, [Char])
getchar []          = (Nothing,0,[])
getchar s@('\n':_ ) = (Nothing,0,s )
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
                                              TkInteger8  -> readn 8  n
                                              TkInteger16 -> readn 16 n
                                              TkInteger10 -> readn 10 n
                                              _           -> fatal 279 "getExcChar: unknown tokentype."
                                  in  if val >= 0 && val <= 255
                                         then (Just (chr val),len, rest)
                                         else (Nothing,1,rest)
                    | otherwise = case x `lookup` cntrChars of
                                 Nothing -> (Nothing,0,s)
                                 Just c  -> (Just c,1,xs)
  where cntrChars = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t')
                    ,('v','\v'),('\\','\\'),('"','\"')]
-- character literals are not used in Ampersand. Since this scanner was used for Haskell-type languages, ('\'','\'') has been removed from cntrChars...

readn :: Int -> [Char] -> Int
readn base = foldl (\r x  -> value x + base * r) 0

getNumber :: [Char] -> (TokenType, [Char], Int, [Char])
getNumber [] = fatal 294 "getNumber" 
getNumber cs@(c:s)
  | c /= '0'         = num10
  | null s           = const0
  | hs `elem` "xX"   = num16
  | hs `elem` "oO"   = num8
  | otherwise        = num10
  where (hs:ts) = s
        const0 = (TkInteger10, "0",1,s)
        num10  = let (n,r) = span isDigit cs
                 in (TkInteger10,n,length n,r)
        num16   = readNum isHexaDigit  ts TkInteger16
        num8    = readNum isOctalDigit ts TkInteger8
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

-- Gets the value of a given token
get_tok_val :: Token -> String
get_tok_val (Tok _ _ s _ _) = s

-- Matches a token of a given type
gsym :: IsParser p Token => TokenType -> String -> String -> p String
gsym kind val val2' = get_tok_val <$> pSym (Tok kind val val2' noPos "")
-- Key has no EBNF because in EBNF it's just the given keyword.
pKey :: IsParser p Token => String -> p String
pKey  keyword  =   gsym TkKeyword   keyword   keyword
-- Spec just matches the given character so it has no EBNF
pSpec :: IsParser p Token => Char -> p String
pSpec s        =   gsym TkSymbol    [s]       [s]

pString, pExpl, pInteger10, pVarid, pConid,
  pInteger :: IsParser p Token => p String
--- String ::= '"' Any* '"'
--- StringListSemi ::= String (';' String)*
pString        =   gsym TkString    ""        ""
--- Expl ::= '{+' Any* '-}'
pExpl          =   gsym TkExpl      ""        ""
--- Atom ::= "'" Any* "'"
pAtom          =   gsym TkAtom      ""        ""
--- Digit ::= ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')
--- Integer10 ::= ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') Digit*
--- Integer8 ::= ('00' | '0o') Digit+
--- Integer16 ::= ('0x' | '0X') Digit+
pInteger10     =   gsym TkInteger10 ""        "1"
--- Varid ::= (LowerChar | '_') (Char | '_')*
pVarid         =   gsym TkVarid     ""        "?lc?"
--- Conid ::= UpperChar (Char | '_')*
pConid         =   gsym TkConid     ""        "?uc?"

--- Integer ::= Integer10 | Integer8 | Integer16
pInteger       =   pInteger10

pComma, pSemi :: IsParser p Token => p String
--- Comma ::= ','
pComma  = pSpec ','
--- Semi ::= ';'
pSemi   = pSpec ';'

--- UpperChar ::= 'A'
--- LowerChar ::= 'A'
--- Char ::= 'A'
--- Lower ::= 'A'
--- Any ::= 'a'