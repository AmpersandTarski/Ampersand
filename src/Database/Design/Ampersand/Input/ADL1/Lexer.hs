{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Database.Design.Ampersand.Input.ADL1.Lexer
    ( keywords
    , operators
    , symbols
    , lexer
    -- LexerMessage
    , LexerError(..)
    , LexerErrorInfo(..)
    , LexerWarning(..)
    , LexerWarningInfo(..)
    , keepOneTabWarning
    , showLexerErrorInfo
    , showLexerWarningInfo
    -- LexerToken
    , Token(..)
    , Lexeme(..)
    , lexemeText
    , initPos
    , FilePos(..)
) where

import Database.Design.Ampersand.Input.ADL1.FilePos(updatePos)
import Database.Design.Ampersand.Input.ADL1.LexerToken
import Database.Design.Ampersand.Input.ADL1.LexerMonad
import Database.Design.Ampersand.Input.ADL1.LexerMessage
import Data.Char hiding(isSymbol)
import Data.Set (member, fromList)
import Database.Design.Ampersand.Basics (fatal)
import Database.Design.Ampersand.Misc
import Data.Time.Calendar
import Data.Time.Clock
import Numeric

-- | Retrieves a list of keywords accepted by the ampersand language
keywords :: [String] -- ^ The keywords
keywords      = [ "INCLUDE"
                , "CONTEXT", "ENDCONTEXT", "EXTENDS", "THEMES"
                , "META"
                , "PATTERN", "ENDPATTERN"
                , "PROCESS", "ENDPROCESS"
                , "INTERFACE", "CLASS", "FOR", "BOX", "ROWS", "TABS", "COLS", "INITIAL", "SQLPLUG", "PHPPLUG"
                , "REPRESENT", "TYPE", "LINKTO"
                , "POPULATION", "CONTAINS"
                , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX", "IRF", "AUT", "PROP", "ALWAYS"
                , "RULE", "MESSAGE", "VIOLATION", "SRC", "TGT", "TEST"
                , "RELATION", "MEANING", "CONCEPT", "IDENT"
                , "VIEW", "ENDVIEW", "DEFAULT", "TXT", "PRIMHTML", "TEMPLATE"
                , "IMPORT", "SPEC", "ISA", "IS", "I", "V"
                , "CLASSIFY"
                , "PRAGMA", "PURPOSE", "IN", "REF", "ENGLISH", "DUTCH"
                , "REST", "HTML", "LATEX", "MARKDOWN"
                , "ONE"
                , "BYPLUG"
                , "ROLE", "SERVICE", "EDITS", "MAINTAINS"
                -- Keywords for TType:
                , "ALPHANUMERIC", "BIGALPHANUMERIC", "HUGEALPHANUMERIC", "PASSWORD"
                , "BINARY", "BIGBINARY", "HUGEBINARY"
                , "DATE", "DATETIME", "BOOLEAN", "INTEGER", "FLOAT", "AUTOINCREMENT"
                -- Keywords for values of atoms:
                , "TRUE", "FALSE" --for booleans
                ]

-- | Retrieves a list of operators accepted by the ampersand language
operators :: [String] -- ^ The operators
operators = [ "|-", "-", "->", "<-", "=", "~", "+", "*", ";", "!", "#",
              "::", ":", "\\/", "/\\", "\\", "/", "<>" , "..", "."]

-- | Retrieves the list of symbols accepted by the ampersand language
symbols :: String -- ^ The list of symbol characters / [Char]
symbols = "()[],{}<>"

--TODO: Options should be one item, not a list
-- | Runs the lexer
lexer :: [Options]  -- ^ The command line options
      -> FilePath   -- ^ The file name, used for error messages
      -> String     -- ^ The content of the file
      -> Either LexerError ([Token], [LexerWarning]) -- ^ Either an error or a list of tokens and warnings
lexer opt file input = case runLexerMonad opt file (mainLexer (initPos file) input) of
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

type Lexer = FilePos -> String  -> LexerMonad [Token]

mainLexer :: Lexer

-----------------------------------------------------------
-- Removing unnecessary text artifacts (comment, spaces,...)
-----------------------------------------------------------

mainLexer _ [] =  return []

mainLexer p ('-':'-':s) = mainLexer p (skipLine s) --TODO: Test if we should increase line number and reset the column number

mainLexer p (c:s) | isSpace c = let (spc,next) = span isSpace s
                                in  mainLexer (foldl updatePos p (c:spc)) next

mainLexer p ('-':'+':s)  = returnToken lx p mainLexer p rest
                where lx   = LexExpl $ dropWhile isSpace (takeLine s)
                      rest = skipLine s

mainLexer p ('{':'-':s) = lexNest mainLexer (addPos 2 p) s
mainLexer p ('{':'+':s) = lexExpl mainLexer (addPos 2 p) s
mainLexer p ('"':ss) =
    let (s,swidth,rest) = scanString ss
    in if null rest || head rest /= '"'
                              then lexerError (NonTerminatedString s) p
                              else returnToken (LexString s) p mainLexer (addPos (swidth+2) p) (tail rest)

{- In Ampersand, atoms may be promoted to singleton relations by single-quoting them. For this purpose, we treat
   single quotes exactly as the double quote for strings. That substitutes the scanner code for character literals. -}
mainLexer p ('\'':ss)
     = let (s,swidth,rest) = scanSingletonInExpression ss
       in if null rest || head rest /= '\''
             then lexerError UnterminatedAtom p
             else returnToken (LexSingleton s) p mainLexer (addPos (swidth+2) p) (tail rest)

-----------------------------------------------------------
-- looking for keywords - operators - special chars
-----------------------------------------------------------

-- Special case for < since it's the beginning of operators but also a symbol when alone
mainLexer p ('<':d:s) = if isOperator ['<',d]
                        then returnToken (LexOperator ['<',d]) p mainLexer (addPos 2 p) s
                        else returnToken (LexSymbol    '<')    p mainLexer (addPos 1 p) (d:s)

mainLexer p cs@(c:s)
     | isIdStart c || isUpper c
         = let (name', p', s')    = scanIdent (addPos 1 p) s
               name               = c:name'
               tokt   | iskw name = LexKeyword name
                      | otherwise = if isIdStart c
                                    then LexVarId name
                                    else LexConId name
           in returnToken tokt p mainLexer p' s'
     | isOperatorBegin c
         = let (name, s') = getOp cs
           in returnToken (LexOperator name) p mainLexer (foldl updatePos p name) s'
     | isSymbol c = returnToken (LexSymbol c) p mainLexer (addPos 1 p) s
     | isDigit c
         = case  getDateTime cs of
            Just (Right (tk,_,width,s')) -> returnToken tk p mainLexer (addPos width p) s'
            Just (Left msg) -> lexerError msg p
            Nothing 
              -> case getDate cs of
                  Just (tk,_,width,s') -> returnToken tk p mainLexer (addPos width p) s'
                  Nothing -> let (tk,_,width,s') = getNumber cs
                             in  returnToken tk p mainLexer (addPos width p) s'
       -- Ignore unexpected characters in the beginning of the file because of the UTF-8 BOM marker.
       -- TODO: Find out the right way of handling the BOM marker.
     | beginFile p = do { lexerWarning UtfChar p; mainLexer p s }
     | otherwise  = lexerError (UnexpectedChar c) p
    where beginFile (FilePos _ 1 1) = True
          beginFile _ = False

-----------------------------------------------------------
-----------------------------------------------------------
-- Supporting functions for MainLexer
-----------------------------------------------------------
-----------------------------------------------------------

-----------------------------------------------------------
-- Check on keywords - operators - special chars
-----------------------------------------------------------

locatein :: Ord a => [a] -> a -> Bool
locatein es e = member e (fromList es)

iskw :: String -> Bool
iskw = locatein keywords

isSymbol :: Char -> Bool
isSymbol = locatein symbols

isOperator :: String -> Bool
isOperator  = locatein operators

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

-- scan ident receives a file position and the resting contents, returning the scanned identifier, the file location and the resting contents.
scanIdent :: FilePos -> String -> (String, FilePos, String)
scanIdent p s = let (name,rest) = span isIdChar s
                in (name,addPos (length name) p,rest)


-----------------------------------------------------------
-- String clean-up functions / comments
-----------------------------------------------------------

lexNest :: Lexer -> Lexer
lexNest c p ('-':'}':s) = c (addPos 2 p) s
lexNest c p ('{':'-':s) = lexNest (lexNest c) (addPos 2 p) s
lexNest c p (x:s)       = lexNest c (updatePos p x) s
lexNest _ p []          = lexerError UnterminatedComment p

--TODO: Also accept {+ ... +} as delimiters
lexExpl :: Lexer -> Lexer
lexExpl = lexExpl' ""
 where lexExpl' str _ p ('-':'}':s) = returnToken (LexExpl str) p mainLexer (addPos 2 p)  s
       lexExpl' str c p ('{':'-':s) = lexNest (lexExpl' str c) (addPos 2 p) s
       lexExpl' str c p ('-':'-':s) = lexExpl' str c p (dropWhile (/= '\n') s)
       lexExpl' str c p (x:s)       = lexExpl' (str++[x]) c (updatePos p x) s
       lexExpl' _   _ p []          = lexerError UnterminatedPurpose p

-----------------------------------------------------------
-- iso 8601 date / time
-----------------------------------------------------------
-- Returns tuple with the parsed lexeme, the UTCTime, the amount of read characters and the rest of the text
getDateTime :: String -> Maybe (Either LexerErrorInfo (Lexeme, UTCTime, Int, String) )
getDateTime cs = 
  case getDate cs of
   Nothing -> Nothing
   Just (_,day,ld,rd) -> 
      case getTime rd of
        Nothing -> case rd of
                    'T':_ -> Just . Left $ ProblematicISO8601DateTime
                    _     -> Nothing
        Just (timeOfDay, tzoneOffset,lt,rt) -> 
            let ucttime = addUTCTime tzoneOffset (UTCTime day timeOfDay)
            in Just . Right $ 
                    ( LexDateTime ucttime
                    , ucttime
                    , ld + lt
                    , rt
                    )
getTime :: String -> Maybe (DiffTime, NominalDiffTime, Int, String)
getTime cs =
  case cs of
   'T':h1:h2:':':m1:m2:rest 
    -> if (all isDigit [h1,h2,m1,m2])
       then let (_,Left hours,_,_) = getNumber [h1,h2]
                (_,Left minutes,_,_) = getNumber [m1,m2]
                (seconds,ls,rs) = getSeconds rest
            in case getTZD rs of
                 Nothing -> Nothing
                 Just (offset,lo,ro)
                   -> if hours < 24 && minutes < 60 && seconds < 60
                      then Just (fromRational . toRational $
                                   ( fromIntegral hours*60
                                    +fromIntegral minutes
                                   )*60+ seconds
                                ,offset
                                ,1+5+ls+lo
                                ,ro)
                      else Nothing
       else Nothing
   _ -> Nothing
   
getSeconds :: String -> (Float,Int,String)
getSeconds cs =
 case cs of
  (':':s1:s2:rest) ->  
     if all isDigit [s1,s2]
     then let (fraction,lf,rf) = getFraction (s1:s2:rest)
          in (fraction,1+lf,rf)
     else (0,0,cs)
  _ -> (0,0,cs)
getFraction :: String -> (Float,Int,String)
getFraction cs = 
  case readFloat cs of
   [(a,str)] -> (a, length cs - length str, str) --TODO: Make more efficient.
   _ -> (0,0,cs)
   


getTZD :: String -> Maybe (NominalDiffTime, Int,String)
getTZD cs = 
 case cs of
  'Z':rest -> Just (0,1,rest)
  '+':h1:h2:':':m1:m2:rest -> mkOffset [h1,h2] [m1,m2] rest (+)
  '-':h1:h2:':':m1:m2:rest -> mkOffset [h1,h2] [m1,m2] rest (-)
  _ -> Nothing
  where
   mkOffset :: String -> String -> String -> (Int -> Int -> Int) -> Maybe (NominalDiffTime, Int,String)
   mkOffset hs ms rest op = 
    let (_,Left hours  ,_,_) = getNumber hs
        (_,Left minutes,_,_) = getNumber ms
        total = hours*60+minutes
    in if hours <= 24 && minutes < 60
       then Just (fromRational . toRational $ 0 `op` total
                 ,6,rest) 
       else Nothing 
getDate :: String -> Maybe (Lexeme, Day, Int, String)
getDate cs =
  case cs of
   y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:rest ->
      if all isDigit [y1,y2,y3,y4,m1,m2,d1,d2] 
      then case fromGregorianValid (toInteger year) month day of
             Nothing -> Nothing 
             Just d -> Just (LexDate d, d, 10, rest)
      else Nothing
     where (_,Left year ,_,_) = getNumber [y1,y2,y3,y4]
           (_,Left month,_,_) = getNumber [m1,m2]
           (_,Left day  ,_,_) = getNumber [d1,d2]
   _  -> Nothing
     
-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
-- Returns tuple with the parsed lexeme, the integer, the amount of read characters and the rest of the text
getNumber :: String -> (Lexeme, (Either Int Double), Int, String)
getNumber str =
  case readDec str of
    [(_,('.':_))] -> case readFloat str of
                           [(flt,rest)] -> (LexFloat flt, Right flt, length str - length rest,rest)
                           _            -> fatal 342 "Unexpected: can read decimal, but not float???"
    [(dec,rest)]  -> (LexDecimal dec , Left dec, length str - length rest,rest)
    _  -> fatal 343 $ "No number to read!\n  " ++ take 40 str                    
--getNumber :: String -> (Lexeme, (Either Int Double), Int, String)
--getNumber [] = fatal 294 "getNumber"
--getNumber cs@(c:s)
--  | c /= '0'         = num10
--  | null s           = const0
--  | hs `elem` "xX"   = num16
--  | hs `elem` "oO"   = num8
--  | otherwise        = num10
--  where (hs:ts) = s
--        const0 = (LexDecimal 0, Left 0, 1, s)
--        num10 :: (Lexeme, (Either Int Double), Int, String)
--        num10  = let (n, rs) = span isDigit cs
--                     (isWhole,readed,rest) = 
--                        case rs of
--                          '.':cs' -> let (n',rs') = span isDigit cs'
--                                         wholeNumberString = n++"."++n'
--                                     in (all (=='0') n',wholeNumberString,rs')
--                          _       -> (True,n, rs)
--                     nrInt = read n
--                 in if isWhole
--                    then (LexDecimal nrInt , Left nrInt, length readed,rest)
--                    else let x = fst.head.readFloat $ readed
--                         in (LexFloat x, Right x, length readed,rest)
--        num16   = readIntNum isHexaDigit  16 LexHex
--        num8    = readIntNum isOctDigit 8  LexOctal
--        readIntNum :: (Char -> Bool) -> Int -> (Int -> Lexeme) -> (Lexeme, Either Int a , Int, String)
--        readIntNum p base lx
--          = let (n, rs) = span p ts
--            in  if null n
--                then const0
--                else let nr = readn base n
--                     in (lx nr, Left nr, 2 + length n, rs)

-----------------------------------------------------------
-- characters / strings
-----------------------------------------------------------
scanString :: String -> (String, Int, String)
scanString = scanUpto False ['"']

scanSingletonInExpression :: String -> (String, Int, String)
scanSingletonInExpression = scanUpto True ['\'']

-- | scan to some given character. The end char is scanned away too
scanUpto :: Bool    -- Special case for Ampersand Atomvalues? (if so, both singlequote and doublequote must be escaped)
         -> [Char]  -- non-empty list of ending characters
         -> String 
         -> (String, Int, String)
scanUpto isAtomScan echrs s = 
 case s of
   xs       -> let (ch,cw,cr) = getchar isAtomScan echrs xs
                   (str,w,r)  = scanUpto isAtomScan echrs cr
               in maybe ("",0,xs) (\c -> (c:str,cw+w,r)) ch

getchar :: Bool    -- Special case for Ampersand Atomvalues? (if so, both singlequote and doublequote must be escaped)
        -> [Char]  -- non-empty list of ending characters
        -> String  -- string to get the character from
        -> (Maybe Char, Int, String)
getchar isAtomScan echrs s =
  case s of
   []          -> (Nothing,0,[])
   ('\n':_ )   -> (Nothing,0,s)
   ('\t':_ )   -> (Nothing,0,s)
   ('\\':'&':xs) -> let (str,w,r) = getchar isAtomScan echrs xs -- Special case is required because an escaped & is equal to empty string in Haskell
                    in (str,w+2,r)
   ('\\':xs)   -> let (c,l,r) = getEscChar xs
                  in (c,l+1,r)
   (x:xs)      
    | x `elem` echrs   -> (Nothing,0,s)
    | isAtomScan && x `elem`[doubleQuote, singleQuote]  -> (Nothing,0,s) 
   --- | isAtomScan && ec == singleQuote && x == doubleQuote -> (Nothing,0,s) 
    | otherwise -> (Just x,1,xs)
  where
    (doubleQuote,singleQuote) = ('\"','\'')
getEscChar :: String -> (Maybe Char, Int, String)
getEscChar [] = (Nothing,0,[])
getEscChar s@(x:xs) | isDigit x = case readDec s of
                                    [(val,rest)]
                                      | val >= 0 && val <= 255 -> (Just (chr val),length s - length rest, rest)
                                      | otherwise -> (Nothing, 1, rest)
                                    _  -> fatal 432 $ "Impossible! first char is a digit.. "++take 40 s
                    | x `elem` ['\"','\''] = (Just x,2,xs)
                    | otherwise = case x `lookup` cntrChars of
                                 Nothing -> (Nothing,0,s)
                                 Just c  -> (Just c,1,xs)
  where cntrChars = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t')
                    ,('v','\v'),('\\','\\')]

-----------------------------------------------------------
-- Token creation function
-----------------------------------------------------------

returnToken :: Lexeme -> FilePos -> Lexer -> Lexer
returnToken lx pos continue posi input = do
    let token = Tok lx pos
    tokens <- continue posi input
    return (token:tokens)
