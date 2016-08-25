module Main (main) where

import Conduit
import System.FilePath ((</>))
import Control.Monad --(filterM, forM_, foldM,when)
import System.IO.Error (tryIOError)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.Exit --(ExitCode, exitFailure, exitSuccess)
import Ampersand.Test.Regression(DirContent(..),DirData(..),process)

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
            
-- Convert a DirData into an Int that contains the number of failed tests
myVisitor :: Conduit DirData IO Int
myVisitor = loop 1
  where
    loop :: Int -> Conduit DirData IO Int
    loop n = 
        awaitForever 
          (\dird -> do lift $ putStr $ ">> " ++ show n ++ ". "
                       x <- liftIO $ process 4 dird 
                       yield x
                       loop (n + 1)
          ) 
                

sumarize :: Sink Int IO Int
sumarize = loop 0 
  where
    loop :: Int -> Sink Int IO Int
    loop i = 
      await >>= maybe (return i) 
                      (\x -> loop $! (i+x))


