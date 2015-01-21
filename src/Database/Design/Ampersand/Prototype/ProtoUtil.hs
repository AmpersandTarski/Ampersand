{-# LANGUAGE FlexibleInstances #-}
module Database.Design.Ampersand.Prototype.ProtoUtil
         ( phpIdentifier,commentBlock,strReplace
         , addSlashes
         , indentBlock,addToLast
         , pDebug,indentBlockBetween,quote,sqlAtomQuote
         , phpIndent,showPhpStr,escapePhpStr,showPhpBool
         ) where
 
import Data.Char(isAlphaNum,isDigit)
import Data.List
import Database.Design.Ampersand

fatal :: Int -> String -> a
fatal = fatalMsg "ProtoUtil"

pDebug :: Bool
pDebug = True

quote :: String->String
quote [] = []
quote ('`':s) = '`':s  -- do nothing if already quoted
quote s = "`"++s++"`"
--   quote s = "`"++quo s++"`"
--    where quo ('`':s')  = "\\`" ++ quo s'
--          quo ('\\':s') = "\\\\" ++ quo s'
--          quo (c:s')    = c: quo s'
--          quo []       = []
-- See http://stackoverflow.com/questions/11321491/when-to-use-single-quotes-double-quotes-and-backticks
sqlAtomQuote :: String->String
sqlAtomQuote s = "'"++sAQ s++"'"
 where sAQ ('\'':s') = "\\'" ++ sAQ s'
       sAQ ('\\':s') = "\\\\" ++ sAQ s'
       sAQ (c:s')    = c: sAQ s'
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

showPhpStr :: String -> String
showPhpStr str = "'"++escapePhpStr str++"'"

-- NOTE: we assume a single quote php string, so $ and " are not escaped
escapePhpStr :: String -> String
escapePhpStr ('\'':s) = "\\'" ++ escapePhpStr s
escapePhpStr ('\\':s) = "\\\\" ++ escapePhpStr s
escapePhpStr (c:s)    = c: escapePhpStr s
escapePhpStr []       = []
-- todo: escape everything else (unicode, etc)

showPhpBool :: Bool -> String
showPhpBool b = if b then "true" else "false"

