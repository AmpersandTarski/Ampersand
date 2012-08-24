{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
module DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner where

   import Data.Char
   import Data.List
   import Data.Maybe
   import DatabaseDesign.Ampersand.Input.ADL1.UU_BinaryTrees(tab2tree,btLocateIn)
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing(Symbol(..),IsParser,pSym,(<$>),pListSep,pPacked)

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

   data Pos = Pos{line:: !Line, column:: !Column} deriving (Eq, Ord)
   type Filename   = String

   data Token = Tok { tp :: TokenType
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
   maybeshow (Pos 0 0) fn =  ""
   maybeshow (Pos l c) fn =  " at line " ++ show l
                          ++ ", column " ++ show c
                          ++ " of file " ++ show fn

   initPos :: Pos
   initPos = Pos 1 1

   noPos :: Pos
   noPos = Pos 0 0

   advl ::  Line -> Pos ->Pos
   advl i (Pos l c) = Pos (l+i) 1

   advc :: Column -> Pos ->  Pos
   advc i (Pos l c) = Pos l (c+i)

   adv :: Pos -> Char -> Pos
   adv pos c = case c of
     '\t' -> advc (tabWidth (column pos)) pos
     '\n' -> advl 1 pos
     _    -> advc 1 pos

   tabWidth :: Column -> Int
   tabWidth c = 8 - ((c-1) `mod` 8)

   instance Show Token where
     showsPrec _ token
       = showString
          (case token of
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
           (Tok TkSpace     _  s2 i fn)  -> "spaces "                              ++ maybeshow i fn
           (Tok TkError     _  s2 i fn)  -> "error in scanner: "     ++ s2         ++ maybeshow i fn
          )

   instance  Symbol Token where
     deleteCost (Tok TkKeyword _ _ _ _) = 10
     deleteCost _                       = 5

   keyToken,token :: TokenType -> String -> Pos -> Filename -> Token
   keyToken tp key  = Tok tp key key
   token  tp  = Tok tp ""  

   errToken :: String -> Pos -> Filename -> Token
   errToken = token TkError

   skipline s = let (_,rest) = span (/='\n') s
                in  rest

   scan :: [String] -> [String] -> String -> String -> String -> Pos -> String -> [Token]
   scan keywordstxt keywordsops specchars opchars fn pos input
     = doScan pos input

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
      doScan p [] = []
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

{- character literals are not used in Ampersand.  doScan p ('\'':ss) is commented out to make room for singleton atoms.
      doScan p ('\'':ss)
        = let (mc,cwidth,rest) = scanChar ss
          in case mc of
               Nothing -> errToken "Error in character literal" p fn : doScan (advc cwidth p) rest
               Just c  -> if null rest || head rest /= '\''
                             then errToken "Unterminated character literal" p fn : doScan (advc (cwidth+1) p) rest
                             else token TkChar [c] p fn : doScan (advc (cwidth+2) p) (tail rest)
-}

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
          f ops (e:s) op = if null [s | o:s<-ops, e==o] then (op,e:s) --was: f ops (e:s) op = if and (map null ops) then (op,e:s) --b.joosten
                           else f [s | o:s<-ops, e==o] s (op++[e])
          f [] es op     = ("",cs)
          f ops [] op    = (op,[])



   lexNest fn cont pos inp = lexNest' cont pos inp
    where lexNest' c p ('-':'}':s) = c (advc 2 p) s
          lexNest' c p ('{':'-':s) = lexNest' (lexNest' c) (advc 2 p) s
          lexNest' c p (x:s)       = lexNest' c (adv p x) s
          lexNest' _ _ []          = [ errToken "Unterminated nested comment" pos fn ]

   lexExpl fn cont pos inp = lexExpl' "" cont pos inp
    where lexExpl' str c p ('-':'}':s) = token TkExpl str p fn: c (advc 2 p) s
          lexExpl' str c p ('{':'-':s) = lexNest fn (lexExpl' str c) (advc 2 p) s
          lexExpl' str c p ('-':'-':s) = lexExpl' str c  p (dropWhile (/= '\n') s)
          lexExpl' str c p (x:s)       = lexExpl' (str++[x]) c (adv p x) s
          lexExpl' _ _ _ []            = [ errToken "Unterminated EXPLAIN section" pos fn ]

   scanString []            = ("",0,[])
   scanString ('\\':'&':xs) = let (str,w,r) = scanString xs
                              in (str,w+2,r)
   scanString ('\'':xs)     = let (str,w,r) = scanString xs
                              in ('\'': str,w+1,r)
   scanString xs = let (ch,cw,cr) = getchar xs
                       (str,w,r)  = scanString cr
--                       str' = maybe "" (:str) ch
                   in maybe ("",0,xs) (\c -> (c:str,cw+w,r)) ch

   scanAtom []              = ("",0,[])
   scanAtom ('\\':'&':xs)   = let (str,w,r) = scanAtom xs
                              in (str,w+2,r)
   scanAtom ('"':xs)        = let (str,w,r) = scanAtom xs
                              in ('"': str,w+1,r)
   scanAtom xs   = let (ch,cw,cr) = getchar xs
                       (str,w,r)  = scanAtom cr
--                       str' = maybe "" (:str) ch
                   in maybe ("",0,xs) (\c -> (c:str,cw+w,r)) ch

   scanChar ('"' :xs) = (Just '"',1,xs)
   scanChar xs        = getchar xs

   getchar []          = (Nothing,0,[])
   getchar s@('\n':_ ) = (Nothing,0,s )
   getchar s@('\t':_ ) = (Nothing,0,s)
   getchar s@('\'':_ ) = (Nothing,0,s)
   getchar s@('"' :_ ) = (Nothing,0,s)
   getchar   ('\\':xs) = let (c,l,r) = getEscChar xs
                         in (c,l+1,r)
   getchar (x:xs)      = (Just x,1,xs)

   getEscChar [] = (Nothing,0,[])
   getEscChar s@(x:xs) | isDigit x = let (tp,n,len,rest) = getNumber s
                                         val = case tp of
                                                 TkInteger8  -> readn 8  n
                                                 TkInteger16 -> readn 16 n
                                                 TkInteger10 -> readn 10 n
                                     in  if val >= 0 && val <= 255
                                            then (Just (chr val),len, rest)
                                            else (Nothing,1,rest)
                       | otherwise = case x `lookup` cntrChars of
                                    Nothing -> (Nothing,0,s)
                                    Just c  -> (Just c,1,xs)
     where cntrChars = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t')
                       ,('v','\v'),('\\','\\'),('"','\"')]
-- character literals are not used in Ampersand. Since this scanner was used for Haskell-type languages, ('\'','\'') has been removed from cntrChars...

   readn base = foldl (\r x  -> value x + base * r) 0

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
           readNum p ts tk
             = let nrs@(n,rs) = span p ts
               in  if null n then const0
                             else (tk         , n, 2+length n,rs)

   isHexaDigit  d = isDigit d || (d >= 'A' && d <= 'F') || (d >= 'a' && d <= 'f')
   isOctalDigit d = d >= '0' && d <= '7'

   value c | isDigit c = ord c - ord '0'
           | isUpper c = ord c - ord 'A' + 10
           | isLower c = ord c - ord 'a' + 10





   get_tok_val (Tok _ _ s _ _) = s

   gsym :: IsParser p Token => TokenType -> String -> String -> p String
   gsym kind val val2 = get_tok_val <$> pSym (Tok kind val val2 noPos "")
   pString, pExpl, pAtom, pChar, pInteger8, pInteger10, pInteger16, pVarid, pConid,
     pTextnm, pTextln, pInteger :: IsParser p Token => p String
   pOper name     =   gsym TkOp        name      name
   pKey  keyword  =   gsym TkKeyword   keyword   keyword
   pSpec s        =   gsym TkSymbol    [s]       [s]

   pString        =   gsym TkString    ""        ""
   pExpl          =   gsym TkExpl      ""        ""
   pAtom          =   gsym TkAtom      ""        ""
   pChar          =   gsym TkChar      ""        "\NUL"
   pInteger8      =   gsym TkInteger8  ""        "1"
   pInteger10     =   gsym TkInteger10 ""        "1"
   pInteger16     =   gsym TkInteger16 ""        "1"
   pVarid         =   gsym TkVarid     ""        "?lc?"
   pConid         =   gsym TkConid     ""        "?uc?"
   pTextnm        =   gsym TkTextnm    ""        ""
   pTextln        =   gsym TkTextln    ""        ""

   pInteger       =   pInteger10

   pComma, pSemi, pOParen, pCParen, pOBrack, pCBrack, pOCurly, pCCurly :: IsParser p Token => p String
   pComma  = pSpec ','
   pSemi   = pSpec ';'
   pOParen = pSpec '('
   pCParen = pSpec ')'
   pOBrack = pSpec '['
   pCBrack = pSpec ']'
   pOCurly = pSpec '{'
   pCCurly = pSpec '}'

   pCommas ::  IsParser p Token => p a -> p [a]
   pSemics ::  IsParser p Token => p a -> p [a]
   pParens ::  IsParser p Token => p a -> p a
   pBracks ::  IsParser p Token => p a -> p a
   pCurly ::  IsParser p Token => p a -> p a

   pCommas  = pListSep pComma
   pSemics  = pListSep pSemi
   pParens  = pPacked pOParen pCParen
   pBracks  = pPacked pOBrack pCBrack
   pCurly   = pPacked pOCurly pCCurly

   pParens_pCommas :: IsParser p Token => p a -> p [a]
   pBracks_pCommas :: IsParser p Token => p a -> p [a]
   pCurly_pSemics :: IsParser p Token => p a -> p [a]

   pParens_pCommas = pParens.pCommas
   pBracks_pCommas = pBracks.pCommas
   pCurly_pSemics  = pCurly .pSemics
   
