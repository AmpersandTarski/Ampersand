{-# LANGUAGE FlexibleInstances #-}
module Database.Design.Ampersand.Prototype.ProtoUtil
         ( writePrototypeFile, getGenericsDir
         , copyDirRecursively, getProperDirectoryContents
         , phpIdentifier,commentBlock,strReplace
         , addSlashes
         , indentBlock,addToLast
         , indentBlockBetween,quote,sqlAtomQuote
         , phpIndent,showPhpStr,escapePhpStr,showPhpBool
         ) where
 
import Prelude hiding (putStrLn, readFile, writeFile)
import Control.Monad
import Data.Char(isAlphaNum,isDigit)
import Data.List
import System.Directory
import System.FilePath
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.Misc.Options (verboseLn) -- TODO: verboseLn shouldn't be in Options
import qualified Database.Design.Ampersand.Misc.Options as Opts

fatal :: Int -> String -> a
fatal = fatalMsg "ProtoUtil"

writePrototypeFile :: FSpec -> String -> String -> IO ()
writePrototypeFile fSpec relFilePath content =
 do { verboseLn (getOpts fSpec) ("  Generating "++relFilePath)
    ; let filePath = getGenericsDir fSpec </> relFilePath
    ; createDirectoryIfMissing True (takeDirectory filePath)
    ; writeFile filePath content
    }

getGenericsDir :: FSpec -> String
getGenericsDir fSpec = 
  let protoDir = Opts.dirPrototype (getOpts fSpec)
  in  if (Opts.newFrontend $ getOpts fSpec) then protoDir </> "generics" else protoDir

-- Copy entire directory tree from srcBase/ to tgtBase/, overwriting existing files, but not emptying existing directories.
copyDirRecursively :: FSpec -> FilePath -> FilePath -> IO ()
copyDirRecursively fSpec srcBase tgtBase = copy ""
  where copy fileOrDirPth = 
         do { let srcPath = srcBase </> fileOrDirPth
                  tgtPath = tgtBase </> fileOrDirPth
            ; isDir <- doesDirectoryExist srcPath
            ; if isDir then 
               do { tgtExists <- doesDirectoryExist tgtPath
                  ; when (not tgtExists) $
                      createDirectory tgtPath
                  ; fOrDs <- getProperDirectoryContents srcPath
                  ; mapM_ (\fOrD -> copy $ fileOrDirPth </> fOrD) fOrDs
                  }
              else
               do { fileContents <- readFile srcPath 
                  ; verboseLn (getOpts fSpec) $ "Copying: " ++ srcPath
                  ; writeFile tgtPath fileContents
                  }
            }

getProperDirectoryContents :: FilePath -> IO [String]
getProperDirectoryContents pth = fmap (filter (`notElem` [".","..",".svn"])) $
                                   getDirectoryContents pth


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
phpIdentifier str = prefix ++ [ if isAlphaNum c then c else '_' | c <- str ]
  where prefix | (c:_) <- str, isDigit c = "n"
               | otherwise               = ""

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

