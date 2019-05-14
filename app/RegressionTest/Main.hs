module Main (main) where

import Ampersand.Basics
import Ampersand.Test.Regression(DirContent(..),DirData(..),process)
import Conduit
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist, makeAbsolute)
import System.Exit --(ExitCode, exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO.Error (tryIOError)

main :: IO ExitCode
main = do 
    putStrLn $ "Starting regression test."
    baseDir <- makeAbsolute $ "." </> "testing"
    totalfails <- runConduit $ walk baseDir .| myVisitor .| sumarize
    failWhenNotZero totalfails 
  where 
    failWhenNotZero :: Int -> IO ExitCode
    failWhenNotZero x 
      | x==0 =
          do putStrLn $ "Regression test of all scripts succeeded."
             exitSuccess
      | otherwise = 
          do putStrLn $ "Regression test failed! ("++show x++" tests failed.)"
             exitFailure
   

-- Produces directory data
walk :: FilePath -> ConduitT () DirData IO ()
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
            isHidden ('.':_) = True
            isHidden _       = False
            
-- Convert a DirData into an Int that contains the number of failed tests
myVisitor :: ConduitT DirData Int IO ()
myVisitor = loop 1
  where
    loop :: Int -> ConduitT DirData Int IO ()
    loop n = 
        awaitForever 
          (\dird -> do lift $ putStr $ ">> " ++ show n ++ ". "
                       x <- liftIO $ process 4 dird 
                       yield x
                       loop (n + 1)
          ) 
                

sumarize :: ConduitT Int Void IO Int
sumarize = loop 0 
  where
    loop :: Int -> ConduitT Int Void IO Int
    loop i = 
      await >>= maybe (return i) 
                      (\x -> loop $! (i+x))


