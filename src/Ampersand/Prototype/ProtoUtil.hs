{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.ProtoUtil
         ( getGenericsDir
         , writePrototypeAppFile
         , copyDirRecursively, removeAllDirectoryFiles, getProperDirectoryContents
         , escapeIdentifier,commentBlock,strReplace
         , addSlashes
         , indentBlock
         , phpIndent,showPhpStr,escapePhpStr
         ) where
 
import           Ampersand.Basics
import           Ampersand.Misc.Defaults (defaultDirPrototype)
import           Ampersand.Misc.HasClasses
import qualified RIO.List as L
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath


writePrototypeAppFile :: (HasDirPrototype env, HasLogFunc env) =>
                         FilePath -> Text -> RIO env ()
writePrototypeAppFile relFilePath content = do
  env <- ask
  logDebug $ "  Generating "<>display (T.pack relFilePath)
  let filePath = getAppDir env </> relFilePath
  liftIO $ createDirectoryIfMissing True (takeDirectory filePath)
  writeFileUtf8 filePath content
     
  
-- Copy entire directory tree from srcBase/ to tgtBase/, overwriting existing files, but not emptying existing directories.
-- NOTE: tgtBase specifies the copied directory target, not its parent
-- NOTE: directories with extention .proto are excluded. This would compromise regression tests, 
--       where '.proto' is the default output directory (if not specified)
copyDirRecursively :: (HasLogFunc env) =>
                      FilePath -> FilePath -> RIO env ()
copyDirRecursively srcBase tgtBase 
  | srcBase == tgtBase = mapM_ logError
        [ "Are you kidding me? I got the instruction to copy "
        , "     "<>display (T.pack srcBase)
        , "  to itself!"
        ]
  | otherwise = do
        srcBaseA <- liftIO $ makeAbsolute srcBase 
        tgtBaseA <- liftIO $ makeAbsolute tgtBase 
        mapM_ logDebug 
          [ "Recursively copying " 
          , "     " <> display (T.pack srcBaseA)
          , "  to " <> display (T.pack tgtBaseA)
          ]
        copy ("." </> tgtBase) ""
  where copy shouldSkip fileOrDirPth = do
          let srcPath = srcBase </> fileOrDirPth
              tgtPath = tgtBase </> fileOrDirPth
          isDir <- liftIO $ doesDirectoryExist srcPath
          if isDir then 
            if srcPath == shouldSkip
              then do
                logDebug $ "Skipping "<>display (T.pack srcPath)<>" because it is the target directory of the recursive copy action."
              else 
                if takeExtension srcPath == defaultDirPrototype 
                  then do  
                    logDebug $ "Skipping "<>display (T.pack srcPath)<>" because its extention is excluded by design" --This is because of regression tests. (See what happend at https://travis-ci.org/AmpersandTarski/Ampersand/jobs/621565925 )
                  else do
                    logDebug $ " Copying dir... " <> display (T.pack srcPath)
                    logDebug $ "      to dir... " <> display (T.pack tgtPath)
                    fOrDs <- getProperDirectoryContents srcPath
                    liftIO $ createDirectoryIfMissing True tgtPath
                    mapM_ (\fOrD -> copy shouldSkip $ fileOrDirPth </> fOrD) fOrDs
          else do
              logDebug $ "  file... " <> display (T.pack fileOrDirPth)
              liftIO $ copyFile srcPath tgtPath
             

-- Remove all files in directory dirPath, but don't enter subdirectories (for which a warning is emitted.)
removeAllDirectoryFiles :: HasLogFunc env =>
                           FilePath -> RIO env ()
removeAllDirectoryFiles dirPath = do
    dirContents <- getProperDirectoryContents dirPath
    mapM_ removeDirectoryFile dirContents 
  where removeDirectoryFile path = 
         do { let absPath = dirPath </> path
            ; isDir <- liftIO $ doesDirectoryExist absPath
            ; if isDir then
                logInfo $ "WARNING: directory '"<>display (T.pack dirPath)<>"' contains a subdirectory '"<>display (T.pack path)<>"' which is not cleared."
              else
                liftIO $ removeFile absPath
            }
     
getProperDirectoryContents :: FilePath -> RIO env [String]
getProperDirectoryContents pth = 
    filter (`notElem` [".","..",".svn"]) 
       <$> (liftIO $ getDirectoryContents pth)

commentBlock :: [String]->[String]
commentBlock ls = ["/*"<>replicate lnth '*'<>"*\\"]
                     <> ["* "<>strReplace "*/" "**" line<>replicate (lnth - length line) ' '<>" *" | line <- ls]
                     <> ["\\*"<>replicate lnth '*'<>"*/"]
   where
     lnth = L.foldl max 0 (map length ls)
indentBlock :: Int -> [String] -> [String]
indentBlock i = map (replicate i ' ' <>)

strReplace :: String -> String -> String -> String
strReplace _ _ "" = ""
strReplace "" _ str = str
strReplace src dst inp
    = process inp
      where
        n = length src
        process "" = ""
        process st@(c:cs)
          | src `L.isPrefixOf` st = dst <> process (drop n st)
          | otherwise           = c:process cs

phpIndent :: Int -> Text
phpIndent i
 | i < 0     = T.pack " " --space instead of \n
 | otherwise = T.pack $ '\n':replicate i ' '


addSlashes :: Text -> Text
addSlashes = T.pack . addSlashes' . T.unpack
  where
    addSlashes' ('\'': cs) = "\\'"<>addSlashes' cs
    addSlashes' ('"': cs) = "\\\""<>addSlashes' cs
    addSlashes' ('\\': cs) = "\\\\"<>addSlashes' cs
    addSlashes' (c:cs) = c:addSlashes' cs
    addSlashes' "" = ""

showPhpStr :: Text -> Text
showPhpStr txt = q<>(escapePhpStr txt)<>q
  where q = T.singleton '\''

-- NOTE: we assume a single quote php string, so $ and " are not escaped
escapePhpStr :: Text -> Text
escapePhpStr txt = 
   case T.uncons txt of
     Nothing -> mempty
     Just ('\'',s) -> "\\'" <> escapePhpStr s
     Just ('\\',s) -> "\\\\" <> escapePhpStr s
     Just (c,s)    -> T.cons c $ escapePhpStr s

