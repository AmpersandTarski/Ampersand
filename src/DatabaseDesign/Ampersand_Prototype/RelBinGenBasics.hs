{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.RelBinGenBasics
    (phpIdentifier,commentBlock,strReplace
 ,addSlashes
 ,indentBlock,addToLast
 ,pDebug,indentBlockBetween,quote,sqlAtomQuote
 ,phpIndent
 ) where
   import Data.Char(isAlphaNum,isDigit)
   import Data.List
   import DatabaseDesign.Ampersand_Prototype.Version 

   fatal :: Int -> String -> a
   fatal = fatalMsg "RelBinGenBasics"

   pDebug :: Bool
   pDebug = True

   quote :: String->String
   quote [] = []
   quote ('`':s) = '`':s  -- do nothing if already quoted
   quote s = "`"++s++"`"
   quote s = "`"++quot s++"`"
    where quot ('`':s) = "\\`" ++ quot s
          quot (c:s)   = c: quot s
          quot []      = []                          

   sqlAtomQuote :: String->String
   sqlAtomQuote s = "'"++sAQ s++"'"
    where sAQ ('\'':s) = "\\'" ++ sAQ s
          sAQ (c:s)    = c: sAQ s
          sAQ []       = []                          

   commentBlock :: [String]->[String]
   commentBlock ls = ["/*"++replicate lnth '*'++"*\\"]
                        ++ ["* "++strReplace "*/" "**" line++replicate (lnth - length line) ' '++" *" | line <- ls]
                        ++ ["\\*"++replicate lnth '*'++"*/"]
      where
        lnth = foldl max 0 (map length ls)
   indentBlock :: Int -> [String] -> [String]
   indentBlock i = map (replicate i ' ' ++)
   
   -- | will put the block after the first string, and put the second after the block
   -- | If the block is just 1 line, indentBlockBetween will return just 1 line as well
   indentBlockBetween :: String -- ^ precedes the block
                      -> String -- ^ comes at the end of the block
                      -> [String] -- ^ the block itself, (will be indented)
                      -> String -- ^ result
   indentBlockBetween pre post [] = pre++post
   indentBlockBetween pre post [s] = pre++s++post
   indentBlockBetween pre post block
    = intercalate (phpIndent (length pre)) ((pre++head block):(init rest++[last rest++post]))
    where  rest = tail block
   
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
   
   phpIndent :: Int -> String
   phpIndent i 
    | i < 0     = " " --space instead of \n
    | otherwise = '\n':replicate i ' '

   -- | guarantees a valid identifier name. The function is NOT injective! 
   phpIdentifier :: String -> String
   phpIdentifier [] = []
   phpIdentifier (s:str)
    | isDigit s = 'I':s:g str
    | otherwise = g (s:str)
      where g xs = [c | c<-xs, isAlphaNum c]

   addSlashes :: String -> String
   addSlashes ('\'': cs) = "\\'"++addSlashes cs
   addSlashes ('"': cs) = "\\\""++addSlashes cs
   addSlashes ('\\': cs) = "\\\\"++addSlashes cs
   addSlashes (c:cs) = c:addSlashes cs
   addSlashes "" = ""
   

   addToLast :: [a] -> [[a]] -> [[a]]
   addToLast _ [] = fatal 109 "addToLast: empty list"
   addToLast s as = init as++[last as++s]
