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
         , phpIndent,showPhpStr,escapePhpStr,showPhpBool, showPhpMaybeBool
         , installComposerLibs
         ) where
 
import           Ampersand.Basics
import           Ampersand.Misc.HasClasses
import qualified RIO.List as L
import qualified RIO.Text as T
import           System.Directory
import qualified System.Exit as SE (ExitCode(ExitSuccess,ExitFailure))
import           System.FilePath
import           System.Process(CreateProcess(..),readCreateProcessWithExitCode
                               ,CmdSpec(..),StdStream(..))




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
--       where foo.adl.proto is used to output the prototype of foo.adl
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
                if takeExtension srcPath == ".proto" 
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
showPhpStr str = q<>T.pack (escapePhpStr (T.unpack str))<>q
  where q = T.pack "'"

-- NOTE: we assume a single quote php string, so $ and " are not escaped
escapePhpStr :: String -> String
escapePhpStr ('\'':s) = "\\'" <> escapePhpStr s
escapePhpStr ('\\':s) = "\\\\" <> escapePhpStr s
escapePhpStr (c:s)    = c: escapePhpStr s
escapePhpStr []       = []
-- todo: escape everything else (unicode, etc)

showPhpBool :: Bool -> String
showPhpBool b = if b then "true" else "false"

showPhpMaybeBool :: Maybe Bool -> String
showPhpMaybeBool Nothing = "null"
showPhpMaybeBool (Just b) = showPhpBool b


installComposerLibs :: (HasLogFunc env) => 
                       FilePath -> RIO env ()
installComposerLibs installTarget = do
    curPath <- liftIO $ getCurrentDirectory
    logDebug $ "current directory: "<>display (T.pack curPath)
    logDebug "  Trying to download and install Composer libraries..."
    (exit_code, stdout', stderr') <- liftIO $ readCreateProcessWithExitCode myProc ""
    case exit_code of
      SE.ExitSuccess   -> do logDebug $
                              " Succeeded." <> (if null stdout' then " (stdout is empty)" else "") 
                             logDebug $ display (T.pack stdout')
      SE.ExitFailure _ -> failOutput (exit_code, stdout', stderr')

   where
     myProc :: CreateProcess
     myProc = CreateProcess 
       { cmdspec = ShellCommand $ "composer install --prefer-dist --no-dev --profile --working-dir="<> installTarget
       , cwd = Nothing
       , env = Nothing
       , std_in = Inherit
       , std_out = Inherit
       , std_err = Inherit
       , close_fds = False
       , create_group = False
       , delegate_ctlc = True
       , detach_console = False
       , create_new_console = False
       , new_session = False
       , child_group = Nothing
       , child_user = Nothing
       , use_process_jobs = False
       }
     failOutput :: (ExitCode, String, String) -> RIO env ()
     failOutput (exit_code, out, err) = do
        exitWith . FailedToInstallComposer  $
            [ "Failed!"
            , "composerTargetPath: "++installTarget
            , "Exit code of trying to install Composer: "<>show exit_code<>". "
            ] ++ 
            (if null out then [] else "stdout:" : lines out) ++
            (if null err then [] else "stderr:" : lines err) ++
            [ "Possible solutions to fix your prototype:"
            , "  1) Make sure you have composer installed. (Details can be found at https://getcomposer.org/download/)"
            , "  2) Make sure you have an active internet connection."
            , "  3) If you previously built another Ampersand prototype succesfully, you could try to copy the lib directory from it into you prototype manually."
            ]