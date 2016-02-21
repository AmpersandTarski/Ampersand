module Main (main) where

import Conduit
--import qualified Data.Conduit.List as CL
--import qualified Data.Conduit.Binary as CB

import System.FilePath ((</>))
import Control.Monad --(filterM, forM_, foldM,when)
import System.IO.Error (tryIOError)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
--import Control.Monad.Trans.Class (lift)
--import Data.Conduit
import System.Exit --(ExitCode, exitFailure, exitSuccess)


main :: IO ExitCode
main = do 
    totalfails <- walk baseDir $$ myVisitor =$ sumarize
    failWhenNotZero totalfails 
  where 
    baseDir = "." </> "testing"
    failWhenNotZero :: Int -> IO ExitCode
    failWhenNotZero x 
      | x==0 =
          do putStrLn $ "Regression test of all scripts succeeded."
             exitSuccess
      | otherwise = 
          do putStrLn $ "Regression test failed! ("++show x++" tests failed.)"
             exitFailure




data DirContent = DirList [FilePath] [FilePath]  -- files and directories in a directory
                | DirError IOError               
data DirData = DirData FilePath DirContent       -- path and content of a directory
--data DirInfo = DirInfo [FilePath] TestInfo       -- list of testscripts and information on how to test them

    

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
            
-- Convert a DirData into an ExitCode
myVisitor :: Conduit DirData IO Int
myVisitor = loop 1
  where
    loop :: Int -> Conduit DirData IO Int
    loop n = do
        lift $ putStr $ ">> " ++ show n ++ ". "
        mdird <- await
        case mdird of
            Nothing     -> return()
            Just dird   -> do x <- liftIO $ process dird 
                              yield x
                              loop (n + 1) 
    process :: DirData -> IO Int 
    process (DirData path dirContent) =
      case dirContent of
        DirError err     -> do
                putStrLn $ "I've tried to look in " ++ path ++ "."
                putStrLn $ "    There was an error: "
                putStrLn $ "       " ++ show err
                return 1
        DirList dirs files -> do
                putStr $ path ++" : "
                doTestSet files
                

doTestSet :: [FilePath] -> IO Int
doTestSet fs 
  | "testinfo.yaml" `elem` fs = 
       do putStrLn $ "yaml file present."
          return 0
  | otherwise =
       do putStrLn $ "no yaml file present."
          return 0
sumarize :: Sink Int IO Int
sumarize = gensum 0 
  where
    gensum :: Int -> Sink Int IO Int
    gensum i = do
      await >>= maybe (return i) 
                      (\x -> gensum (i+x))


