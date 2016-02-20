module Main (main) where

import Data.Char(toUpper)
import System.FilePath ((</>),takeExtension)
import Control.Monad --(filterM, forM_, foldM,when)
import System.IO.Error (tryIOError)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import Control.Monad.Trans.Class (lift)
import Data.Conduit


main :: IO ()
main = walkRegressionTestSets

data DirContent = DirList [FilePath] [FilePath]  -- files and directories in a directory
                | DirError IOError               
data DirData = DirData FilePath DirContent       -- path and content of a directory
--data DirInfo = DirInfo [FilePath] TestInfo       -- list of testscripts and information on how to test them

walkRegressionTestSets :: IO ()
walkRegressionTestSets
 = do 
    walk baseDir $$ myVisitor
 where
    baseDir = "." </> "testing"

-- Produces directory data
walk :: FilePath -> Source IO DirData
walk path = do 
    result <- lift $ tryIOError listdir
    case result of
        Right dl
            -> case dl of 
                DirList subdirs _
                 -> do
                     yield (DirData path dl)
                     forM_ subdirs (walk . (path </>))
                DirError err 
                 -> yield (DirData path (DirError err))
        Left err
            -> yield (DirData path (DirError err))

  where
    listdir = do
        entries <- getDirectoryContents path >>= filterHidden
        subdirs <- filterM isDir entries
        files <- filterM isFile entries
        return $ DirList subdirs files
        where 
            isFile entry = doesFileExist (path </> entry)
            isDir entry = doesDirectoryExist (path </> entry)
            filterHidden paths = return $ filter (not.isHidden) paths
            isHidden dir = head dir == '.'
            
-- Consume directories
myVisitor :: Sink DirData IO ()
myVisitor = addCleanup (\_ -> putStrLn "Finished.") $ loop 1
  where
    loop :: Int -> ConduitM DirData a IO ()
    loop n = do
        lift $ putStr $ ">> " ++ show n ++ ". "
        mr <- await
        case mr of
            Nothing     -> return ()
            Just r      -> lift (process r) >> loop (n + 1)
    process (DirData path dirContent) =
      case dirContent of
        DirError err     -> do
                putStrLn $ "I've tried to look in " ++ path ++ "."
                putStrLn $ "    There was an error: "
                putStrLn $ "       " ++ show err

        DirList dirs files -> do
                putStrLn $ path ++ ". ("++ show (length dirs) ++ " directorie(s) and " ++ show (length files) ++ " file(s):"
                forM_ files doTheFile
      where
        doTheFile :: FilePath -> IO()
        doTheFile file = putStrLn $ file

