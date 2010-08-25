{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module Prototype.RelBinGenBasics(phpIdentifier,naming,commentBlock,strReplace
 ,addSlashes,zipnum,Concatable(..),(+++)
 ,indentBlock,phpShow,addToLast
 ,pDebug,indentBlockBetween,quote
 ,cChain,filterEmpty,phpIndent
 ) where
   import Char(isAlphaNum,isDigit)
   import Strings (chain)
   import List(isPrefixOf)
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
