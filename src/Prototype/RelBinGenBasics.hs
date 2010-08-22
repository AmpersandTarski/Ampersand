{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module Prototype.RelBinGenBasics(phpIdentifier,naming,commentBlock,strReplace
 ,addSlashes,zipnum,Concatable(..),(+++)
 ,indentBlock,phpShow,addToLast
 ,pDebug,noCollide,indentBlockBetween,quote
 ,cChain,isPrefixOf,filterEmpty 
 ) where
   import Char(isDigit,digitToInt,intToDigit,isAlphaNum)
   import Strings (chain)
   import List(isPrefixOf)
   import Collection (Collection(rd))
   import Auxiliaries (naming)
   import Data.Maybe

   zipnum :: [b] -> [(Int, b)]
   zipnum = zip [(0::Int)..]
   
   pDebug :: Bool
   pDebug = True
   
   class Concatable a where
     toM :: a -> Maybe String
     (+<+) :: a -> String -> a
     (+>+) :: String -> a -> a
     (+|+) :: a -> a -> a
   instance Concatable [Char] where
     toM a = Just a
     (+<+) = (++)
     (+>+) = (++)
     (+|+) = (++)
   instance Concatable (Maybe [Char]) where
     toM a = a
     (+<+) = (+++)
     (+>+) = (+++)
     (+|+) = (+++)
   
   infixr 4 +++
   infixr 4 +>+
   infixr 4 +<+
   infixr 4 +|+
   
   (+++) :: (Concatable a,Concatable b) => a->b->Maybe String
   (+++) a b = listToMaybe [a'++b'|Just a'<-[toM a],Just b'<-[toM b]]
   
   filterEmpty :: (Eq a) => [Maybe [a]] -> [Maybe [a]]
   filterEmpty = (filter (\x->not ((==) (Just []) x)))
   
   
   cChain :: (Concatable t, Concatable a) => a -> [t] -> Maybe [Char]
   cChain _ [] = Just []
   cChain _ [b] = toM b
   cChain a (f:fs) = f+++a+++(cChain a fs)

   quote :: String->String
   quote [] = []
   quote ('`':s) = ('`':s)
   quote s = "`"++s++"`"
   
   commentBlock :: [String]->[String]
   commentBlock ls = ["/*"++take lnth (repeat '*')++"*\\"]
                        ++ ["* "++(strReplace "*/" "**" line)++take (lnth - length line) (repeat ' ')++" *" | line <- ls]
                        ++ ["\\*"++take lnth (repeat '*')++"*/"]
      where
        lnth = foldl max 0 (map length ls)
   indentBlock :: Int -> [String] -> [String]
   indentBlock i = map ((++) (take i (repeat ' ')))
   
   -- | will put the block after the first string, and put the second after the block
   -- | If the block is just 1 line, indentBlockBetween will return just 1 line as well
   indentBlockBetween :: String -- ^ precedes the block
                      -> String -- ^ comes at the end of the block
                      -> [String] -- ^ the block itself, (will be indented)
                      -> String -- ^ result
   indentBlockBetween pre post [] = pre++post
   indentBlockBetween pre post [s] = pre++s++post
   indentBlockBetween pre post block
    = chain (phpIndent (length pre)) ((pre++head block):(init rest++[last rest++post]))
    where rest = tail block
   
   strReplace :: String -> String -> String -> String
   strReplace _ _ "" = ""
   strReplace "" _ str = str
   strReplace src dst inp
       = process inp
         where
           n = length src
           process "" = ""
           process st@(c:cs)
             | src `isPrefixOf` st = dst ++ process (drop n st)
             | otherwise           = c:process cs

   -- | does the same as noCollide, but ensures that all names used have `quotes` around them (for mySQL)
   noCollide' :: [String] -> String -> String
   noCollide' nms nm = quote$noCollide (map unquote nms) (unquote nm)
   unquote :: String->String
   unquote ('`':xs) = init xs
   unquote xs = xs
   -- | changes its second argument by appending a digit, such that it does not occur in its first argument 
   noCollide :: [String] -- ^ forbidden names
             -> String -- ^ preferred name
             -> String -- ^ a unique name (does not occur in forbidden names)
   noCollide names nm | nm `elem` names = noCollide names (namepart (reverse nm) ++ changeNr (numberpart (reverse nm)))
                      | otherwise = nm
    where
      namepart str   = reverse (dropWhile isDigit str)
      numberpart str = reverse (takeWhile isDigit str)
      changeNr x     = int2string (string2int x+1)
      --  changeNr x = show (read x +1)
      string2int :: String -> Int
      string2int  = enc.reverse
       where enc "" = 0
             enc (c:cs) = digitToInt c + 10* enc cs
      int2string :: Int -> String
      int2string 0 = "0"
      int2string n = if n `div` 10 == 0 then [intToDigit (n `rem` 10)|n>0] else int2string (n `div` 10)++[intToDigit (n `rem` 10)]

   selectExists' :: (Concatable a,Concatable b) => Int -> a -> b -> (Maybe String)
   selectExists' i tbl whr
    = ("SELECT *" ++
       phpIndent i ++ "  FROM ") +++ tbl +++
      (phpIndent i ++ " WHERE ") +++ whr
   selectGeneric :: (Concatable a) =>
                    Int             -- ^ indentation
                 -> (String,String) -- ^ (source field,source table)
                 -> (String,String) -- ^ (target field,target table)
                 -> a               -- ^ tables
                 -> a               -- ^ the WHERE clause
                 -> a
   selectGeneric i src trg tbl whr
    = selectcl ++
      phpIndent i ++ "  FROM " +>+ 
      (if toM whr==Just "1" then tbl else tbl+|+(phpIndent i ++ " WHERE "+>+whr))
      where selectcl | snd src=="" && snd trg=="" = error ("!Fatal (module RelBinGenBasics 461): Source and target are \"\", use selectExists' for this purpose")
                     | snd src==snd trg  = "SELECT DISTINCT " ++ selectSelItem src
                     | snd src==""   = "SELECT DISTINCT " ++ selectSelItem trg
                     | snd trg==""   = "SELECT DISTINCT " ++ selectSelItem src
                     | otherwise = "SELECT DISTINCT " ++ selectSelItem src ++", "++selectSelItem trg
   selectSelItem :: (String, String) -> String
   selectSelItem (att,alias)
     | unquote (afterPoint att) == unquote alias = att
     | otherwise                                 = att++" AS "++alias
    where myafterPoint ('.':xs) = xs
          myafterPoint ( _ :xs) = myafterPoint xs
          myafterPoint []       = []
          afterPoint s = if (myafterPoint s == "") then s else myafterPoint s
   
   phpIndent :: Int -> [Char]
   phpIndent i = "\n"++take i (repeat ' ')

   phpIdentifier :: String -> String
   phpIdentifier (s:str) | isDigit s = "I"++phpIdentifier (s:str)
   phpIdentifier str = [c| c<-str, isAlphaNum c]

   phpShow :: String -> String
   phpShow str = "'" ++ addSlashes str ++ "'"

   addSlashes :: String -> String
   addSlashes ('\'': cs) = "\\'"++addSlashes cs
   addSlashes ('"': cs) = "\\\""++addSlashes cs
   addSlashes ('\\': cs) = "\\\\"++addSlashes cs
   addSlashes (c:cs) = c:addSlashes cs
   addSlashes "" = ""
   

   addToLast :: [a] -> [[a]] -> [[a]]
   addToLast _ [] = error "!Fatal (module RelBinGenBasics 645): addToLast: empty list"
   addToLast s as = (init as)++[last as++s]

