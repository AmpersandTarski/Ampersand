{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Ampersand.Misc
import qualified RIO.List as L
import qualified RIO.Text as T
import           System.Directory
import qualified System.Exit as SE (ExitCode(ExitSuccess,ExitFailure))
import           System.FilePath
import           System.Process


getGenericsDir :: Options -> String
getGenericsDir Options{..} = 
  dirPrototype </> "generics" 

writePrototypeAppFile :: (HasOptions env, HasVerbosity  env, HasHandles env) =>
                         String -> String -> RIO env ()
writePrototypeAppFile relFilePath content = do
  verboseLn $ "  Generating "<>relFilePath 
  env <- ask
  let filePath = getAppDir (getOptions env) </> relFilePath
  liftIO $ createDirectoryIfMissing True (takeDirectory filePath)
  liftIO $ writeFile filePath content
     
getAppDir :: Options -> String
getAppDir Options{..} =
  dirPrototype </> "public" </> "app" </> "project"
  
-- Copy entire directory tree from srcBase/ to tgtBase/, overwriting existing files, but not emptying existing directories.
-- NOTE: tgtBase specifies the copied directory target, not its parent
copyDirRecursively :: (HasVerbosity  env, HasHandles env) =>
                      FilePath -> FilePath -> RIO env ()
copyDirRecursively srcBase tgtBase = copy ""
  where copy fileOrDirPth = do
          let srcPath = srcBase </> fileOrDirPth
              tgtPath = tgtBase </> fileOrDirPth
          isDir <- liftIO $ doesDirectoryExist srcPath
          if isDir then do
              liftIO $ createDirectoryIfMissing True tgtPath
              verboseLn $ " Copying dir... " ++ srcPath
              fOrDs <- getProperDirectoryContents srcPath
              mapM_ (\fOrD -> copy $ fileOrDirPth </> fOrD) fOrDs
          else do
              verboseLn $ "  file... " ++ fileOrDirPth
              liftIO $ copyFile srcPath tgtPath
             

-- Remove all files in directory dirPath, but don't enter subdirectories (for which a warning is emitted.)
removeAllDirectoryFiles :: (HasOptions env, HasVerbosity  env, HasHandles env) =>
                           FilePath -> RIO env ()
removeAllDirectoryFiles dirPath = do
    dirContents <- getProperDirectoryContents dirPath
    mapM_ removeDirectoryFile dirContents 
  where removeDirectoryFile path = 
         do { let absPath = dirPath </> path
            ; isDir <- liftIO $ doesDirectoryExist absPath
            ; if isDir then
                putStrLn $ "WARNING: directory '"<>dirPath<>"' contains a subdirectory '"<>path<>"' which is not cleared."
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

phpIndent :: Int -> T.Text
phpIndent i
 | i < 0     = T.pack " " --space instead of \n
 | otherwise = T.pack $ '\n':replicate i ' '


addSlashes :: T.Text -> T.Text
addSlashes = T.pack . addSlashes' . T.unpack
  where
    addSlashes' ('\'': cs) = "\\'"<>addSlashes' cs
    addSlashes' ('"': cs) = "\\\""<>addSlashes' cs
    addSlashes' ('\\': cs) = "\\\\"<>addSlashes' cs
    addSlashes' (c:cs) = c:addSlashes' cs
    addSlashes' "" = ""

showPhpStr :: T.Text -> T.Text
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


installComposerLibs :: (HasOptions env, HasVerbosity  env, HasHandles env) =>
                       RIO env ()
installComposerLibs = do
    env <- ask
    curPath <- liftIO $ getCurrentDirectory
    verboseLn $ "current directory: "++curPath
    verbose "  Trying to download and install Composer libraries..."
    (exit_code, stdout', stderr') <- liftIO $ readCreateProcessWithExitCode (myProc $ getOptions env)""
    case exit_code of
      SE.ExitSuccess   -> do verboseLn $
                              " Succeeded." <> (if null stdout' then " (stdout is empty)" else "") 
                             verboseLn stdout'
      SE.ExitFailure _ -> failOutput (exit_code, stdout', stderr')

   where
     myProc :: Options -> CreateProcess
     myProc opts = CreateProcess 
       { cmdspec = ShellCommand $ "composer install --prefer-dist --no-dev --profile --working-dir="<>composerTargetPath opts
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
     composerTargetPath = dirPrototype 
     failOutput :: (HasOptions env) =>
                   (ExitCode, String, String) -> RIO env ()
     failOutput (exit_code, stdout', stderr') = do
        env <- ask
        exitWith . FailedToInstallComposer  $
            [ "Failed!"
            , "composerTargetPath: "++composerTargetPath (getOptions env)
            , "Exit code of trying to install Composer: "<>show exit_code<>". "
            ] ++ 
            (if null stdout' then [] else "stdout:" : lines stdout') ++
            (if null stderr' then [] else "stderr:" : lines stderr') ++
            [ "Possible solutions to fix your prototype:"
            , "  1) Make sure you have composer installed. (Details can be found at https://getcomposer.org/download/)"
            , "  2) Make sure you have an active internet connection."
            , "  3) If you previously built another Ampersand prototype succesfully, you could try to copy the lib directory from it into you prototype manually."
            ]