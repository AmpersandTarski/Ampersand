{-# OPTIONS_GHC -Wall #-}
module Data.Ampersand.Parsing.ADLScanner 
      ( adlScanFile, showTokens
      )where
import Char(isLower, isUpper, isSpace, isAlphaNum, isDigit, chr, ord)
import List(sort)
import Maybe(isJust)
import UU.Util.BinaryTrees(tab2tree,btLocateIn)
import UU.Scanner.Token -- (Token, EnumValToken(..), valueToken, reserved, errToken)
import UU.Scanner.GenToken
import UU.Scanner.Position(Pos, initPos, advc, adv)
import Options
import Data.Set (toList,fromList)
import UTF8  
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
import Data.List

adlScanFile :: Options -> FilePath -> IO [Token]
adlScanFile flags fn =
        do txt <- readFile fn
           return (adlScan flags (initPos fn) txt) 

adlScan :: Options   
        -> Pos
        -> String
        -> [Token]
adlScan flags pos input 
 = myscan keywordstxt keywordsops specialchars opchars pos input
 where
  -- | The keywords of the ADL language
  keywordstxt :: [String]
  keywordstxt = removeDoubles 
           (    [ "CONTEXT", "ENDCONTEXT" ]    -- Context
             ++ [ "IN", "DUTCH", "ENGLISH" ]   -- LangDef
             ++ notBasic [ "BINDING", "BIND", "TOPHP" ] -- Binding
             ++ [ "USE", "PATTERN" ]           -- UsePattern
             ++ [ "PATTERN", "ENDPATTERN" ]    -- PatternDef
             ++ [ "PURPOSE", "REF" ]           -- Explain
             ++ [ "CONCEPT", "RELATION", "RULE", "KEY", "SERVICE", "PATTERN", "CONTEXT" ]  -- ExplObj
             ++ [ "POPULATION", "CONTAINS" ]   -- Population
             ++ [ "RULE", "MEANING" ]          -- RuleDef
             ++ [ "GEN", "ISA" ]               -- Gen
             ++ [ "I", "V" ]                   -- Morphism 
             ++ [ "SELF" ]                     -- Concept
             ++ [ "CONCEPT", "BYPLUG", "REF" ] -- ConceptDef
             ++ [ "KEY" ]                      -- KeyDef
             ++ [ "ROLE", "USES", "EDITS" ]    -- RoleService / RoleRelation
             ++ notBasic [ "SQLPLUG" ]         -- SqlPlug
             ++ notBasic [ "PHPPLUG" ]         -- PhpPlug
             ++ [ "SERVICE" ]                  -- Service
             ++ [ "ALWAYS" , "UNI", "TOT", "PROP" ] -- Obj
             ++ notBasic [ "BYPLUG" ]          -- Declaration
             ++ [ "UNI", "INJ", " SUR", "TOT", "SYM", "ASY", "TRN", "RFX" ] -- Prop
             ++ [ "PRAGMA" ]                   -- Pragma
           )
  -- | The operation literals of the ADL language 
  keywordsops :: [String]
  keywordsops  = removeDoubles
           (    [ ":" ]                        -- Context
             ++ notBasic [ "," ]               -- Binding
             ++ [ "|-", "-|", "=" ]            -- RuleDef
             ++ [ ";" , "!" ]                  -- Term1
             ++ [ "\\", "/"]                   -- Term2
             ++ [ "\\/", "/\\", "-" ]          -- Term3
             ++ [ "-" ]                        -- PreTerm
             ++ [ "~", "+", "*" ]              -- PostTerm
             ++ [ "*" ]              -- Morphism
             ++ [ "'" ]                        -- Atom
             ++ [ ",", ":" ]         -- LabelProps
             ++ [ "," ]              -- KeyDef
             ++ [ "," ]                        -- RoleService, RoleRelation
             ++ [ ",", ":", "=" ] -- Service
             ++ [ "=", "," ]         -- Obj
             ++ [ "::", "*", "->", "." ]       -- Declaration
             ++ [ ";" ]              -- Content
             ++ [ "," ]              -- Record
             ++ [ "," ]              -- Props
           ) 
    
  
  -- special characters
  specialchars :: String
  specialchars      = "[]{}()~" 
  
  opchars :: String
  opchars           = removeDoubles (concat keywordsops)
  
  removeDoubles :: Ord a => [a] -> [a]
  removeDoubles = toList . fromList 

  notBasic :: [a] -> [a]
  notBasic xs = case theme flags of
                   StudentTheme -> []
                   DefaultTheme -> xs
                   ProofTheme   -> xs            
           
myscan :: [String] -> [String] -> String -> String -> Pos -> String -> [Token]
myscan keywordstxt keywordsops specchars opchars pos input
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
              || c == '\''
              || c == '_'

   scanIdent p s = let (name,rest) = span isIdChar s
                   in (name,advc (length name) p,rest)


   doScan _ [] = []
   doScan p (c:s)        | isSpace c = let (sp,next) = span isSpace s
                                       in  doScan (foldl adv p (c:sp)) next

   doScan p ('-':'-':s)  = doScan p (dropWhile (/= '\n') s)
   doScan p ('{':'-':s)  = lexNest doScan (advc 2 p) s
   doScan p ('"':ss)
     = let (s,swidth,rest) = scanString ss
       in if null rest || head rest /= '"'
             then errToken "Unterminated string literal" p : doScan (advc swidth p) rest
             else valueToken TkString s p : doScan (advc (swidth+2) p) (tail rest)

   doScan p ('\'':ss)
     = let (mc,cwidth,rest) = scanChar ss
       in case mc of
            Nothing -> errToken "Error in character literal" p : doScan (advc cwidth p) rest
            Just c  -> if null rest || head rest /= '\''
                          then errToken "Unterminated character literal" p : doScan (advc (cwidth+1) p) rest
                          else valueToken TkChar [c] p : doScan (advc (cwidth+2) p) (tail rest)

   doScan p cs@(c:s)
     | isSymbol c = reserved [c] p
                  : doScan(advc 1 p) s
     | isIdStart c || isUpper c
         = let (name', p', s')    = scanIdent (advc 1 p) s
               name               = c:name'
               tok                = if iskw name
                                    then reserved name p
                                    else if null name' && isSymbol c
                                    then reserved [c] p
                                    else valueToken (if isIdStart c then TkVarid else TkConid) name p
           in tok :  doScan p' s'
     | isOpsym c = let (name, s') = span isOpsym cs
                       tok | isop name = reserved name p
                           | c==':'    = valueToken TkConOp name p
                           | otherwise = valueToken TkOp name p
                   in tok : doScan (foldl adv p name) s'
--     | isOpsym c = let (longest, _) = span isOpsym cs
--                       name = last (filter (\x -> x `elem` keywordsops) (inits longest))
--                       s'   = drop (length name) cs
--                       tok | isop name = reserved name p
--                           | otherwise = errToken ("Unexpected character " ++ show c) p
--                   in tok : doScan (foldl adv p name) s'
     | isDigit c = let (tktype,number,width,s') = getNumber cs
                   in  valueToken tktype number p : doScan (advc width p) s'
     | otherwise = errToken ("Unexpected character " ++ show c) p
                 : doScan (adv p c) s

lexNest :: (Pos -> String -> [Token]) 
        -> Pos 
        -> String 
        -> [Token]
lexNest cont pos inp = lexNest' cont pos inp
 where lexNest' c p ('-':'}':s) = c (advc 2 p) s
       lexNest' c p ('{':'-':s) = lexNest' (lexNest' c) (advc 2 p) s
       lexNest' c p (x:s)       = lexNest' c (adv p x) s
       lexNest' _ _ []          = [ errToken "Unterminated nested comment" pos]

scanString :: String -> (String,Int,String)
scanString []            = ("",0,[])
scanString ('\\':'&':xs) = let (str,w,r) = scanString xs
                           in (str,w+2,r)
scanString ('\'':xs)     = let (str,w,r) = scanString xs
                           in ('\'': str,w+1,r)
scanString xs = let (ch,cw,cr) = getchar xs
                    (str,w,r)  = scanString cr
                    str' = maybe "" (:str) ch
                in maybe ("",0,xs) (\c -> (c:str,cw+w,r)) ch

scanChar :: [Char] -> (Maybe Char,Int,[Char])
scanChar ('"' :xs) = (Just '"',1,xs)
scanChar xs        = getchar xs

getchar :: [Char] -> (Maybe Char,Int,[Char])
getchar []          = (Nothing,0,[])
getchar s@('\n':_ ) = (Nothing,0,s )
getchar s@('\t':_ ) = (Nothing,0,s)
getchar s@('\'':_ ) = (Nothing,0,s)
getchar s@('\"' :_ ) = (Nothing,0,s)
getchar   ('\\':xs) = let (c,l,r) = getEscChar xs
                      in (c,l+1,r)
getchar (x:xs)      = (Just x,1,xs)

getEscChar :: [Char] -> (Maybe Char,Int,[Char])
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
                    ,('v','\v'),('\\','\\'),('\"','\"'),('\'','\'')]

readn :: Int -> [Char] -> Int
readn base n = foldl (\r x  -> value x + base * r) 0 n

getNumber :: [Char] -> (EnumValToken,[Char],Int,[Char])
getNumber cs@(c:s)
  | c /= '0'               = num10
  | null s                 = const0
  | hs == 'x' || hs == 'X' = num16
  | hs == 'o' || hs == 'O' = num8
  | otherwise              = num10
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

isHexaDigit :: Char -> Bool
isHexaDigit  d = isDigit d || (d >= 'A' && d <= 'F') || (d >= 'a' && d <= 'f')

isOctalDigit :: Char -> Bool
isOctalDigit d = d >= '0' && d <= '7'

value :: Char -> Int
value c | isDigit c = ord c - ord '0'
        | isUpper c = ord c - ord 'A' + 10
        | isLower c = ord c - ord 'a' + 10
        

-----------------------------------------------------
showTokens :: [Token] -> String
showTokens ts = intercalate "\n" (map showToken ts)

showToken :: Token -> String
showToken t  = "GenToken " ++ show enumVT
   where
     enumVT = case t of
         Reserved key _ -> key
         ValToken tp val _ -> showtp++" "++ show val
           where
             showtp = case tp of
                  TkVarid      -> "TkVarid"
                  TkConid      -> "TkConid"
                  TkString     -> "TkString"
                  TkChar       -> "TkChar"
                  TkInteger8   -> "TkInteger8"
                  TkInteger10  -> "TkInteger10"
                  TkInteger16  -> "TkInteger16"
                  TkFraction   -> "TkFraction"
                  TkTextnm     -> "TkTextnm"
                  TkTextln     -> "TkTextln" 
                  TkOp         -> "TkOp" 
                  TkConOp      -> "TkConOp" 
                  TkError      -> "TkError" 
  

  

