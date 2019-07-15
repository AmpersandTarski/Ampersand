{-# LANGUAGE RecordWildCards #-}
module MainApps(
    IO
  , ampersand
  , preProcessor
  , mainTest
  , regressionTest
  , defEnv 
) where

import           Ampersand
import           Ampersand.Input.PreProcessor
import           Conduit
import qualified Data.List.NonEmpty as NEL (toList)
import qualified RIO.List as L
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist, makeAbsolute)
import           System.Environment    (getArgs, getProgName)
import           System.FilePath ((</>))
import           System.IO.Error (tryIOError)

defEnv :: IO App
defEnv = do
  logOptions' <- logOptionsHandle stderr False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    opts <- getOptionsIO
    return App
          { appLogFunc = logFunc
          , options' = opts 
          , appHandle = stdout
          }

  

ampersand :: IO ()
ampersand = do
  env <- defEnv
  runRIO env ampersand'
ampersand' :: RIO App ()
ampersand' = do
    opts'@Options{..} <- view optionsL
    sequence_ . map snd . filter fst $ actionsWithoutScript opts'-- There are commands that do not need a single filename to be speciied
    case fileName of
      Just _ -> do -- An Ampersand script is provided that can be processed
            sayLn "Processing your model..."
            gMulti <- createMulti
            case gMulti of
              Errors err    -> 
                 exitWith . NoValidFSpec . L.intersperse  (replicate 30 '=') 
               . fmap show . NEL.toList $ err
              Checked multi ws -> do
                 mapM_  sayLn . concatMap (lines . show) $ ws
                 generateAmpersandOutput opts' multi
                 sayLn "Finished processing your model"
                 sayLn . ("Your script has no errors " ++) $
                    case ws of
                      []  -> "and no warnings"
                      [_] -> ", but one warning was found"
                      _   -> ", but "++show (length ws)++" warnings were found"
           
      Nothing -> -- No Ampersand script is provided 
         if or (map fst $ actionsWithoutScript opts')
         then sayWhenLoudLn "No further actions, because no ampersand script is provided"
         else liftIO $ do 
            args     <- getArgs
            progName <- getProgName
            exitWith . NoAmpersandScript $
                 [ "No ampersand script provided. Use --help for usage information"
                 , "   " <> progName <> (concat $ fmap (" " <>) args) ]

 where
   actionsWithoutScript :: Options -> [(Bool, RIO App ())]
   actionsWithoutScript opts@Options{..} = 
      [ ( test                     , sayLn $ "Executable: " ++ show dirExec )
      , ( showVersion  || verbosity == Loud , sayLn $ versionText opts)
      , ( genSampleConfigFile      , liftIO writeConfigFile)
      , ( showHelp                 , sayLn $ usageInfo' opts)
      , ( runAsDaemon              , runDaemon)
      ]
   
   versionText :: Options -> String
   versionText opts = preVersion opts ++ ampersandVersionStr ++ postVersion opts

preProcessor :: IO()
preProcessor = 
   runRIO stdout preProcessor' 

preProcessor' :: (HasHandle env) => RIO env ()
preProcessor' =
  do
    args <- liftIO getArgs;
    case args of 
      []  -> fatal "No arguments given"
      filename:defs -> do
        result  <- readUTF8File filename
        content <- do
             case result of
               Left err   -> exitWith $ ReadFileError $ "Error while reading input file.\n" : err
               Right cont -> return cont
        say $ either show id (preProcess' filename (Set.fromList defs) (T.unpack content)) ++ "\n"

mainTest :: IO ()
mainTest = do
   env <- defEnv
   runRIO env mainTest'

mainTest' :: RIO App ()
mainTest' = do 
    sayLn "Starting Quickcheck tests."
    funcs <- testFunctions
    testAmpersandScripts
    tests funcs
  where 
      tests :: (HasHandle env) => [([String], RIO env Bool)] -> RIO env ()
      tests [] = pure ()
      tests ((msg,tst):xs) = do
          mapM_ sayLn msg
          success <- tst
          if success then tests xs
          else exitWith (SomeTestsFailed ["*** Some tests failed***"])
               

      testFunctions :: RIO App [([String], RIO App Bool)]
      testFunctions = do
          scr <- getTestScripts
          (parserCheckResult, msg) <- parserQuickChecks
          return [ (["Parsing " ++ show (length scr) ++ " scripts."], parseScripts scr)
                 , ( if parserCheckResult  
                     then ["Parser & prettyprinter test PASSED."]
                     else (  ["QuickCheck found errors in the roundtrip in parsing/prettyprinting for the following case:"]
                           ++map ("\n   "++) (lines msg)
                          )
                   , return parserCheckResult
                   )
                 ]

regressionTest :: IO ()
regressionTest = do
   env <- defEnv
   runRIO env regressionTest'


regressionTest' :: RIO App ()
regressionTest' = do 
    sayLn $ "Starting regression test."
    baseDir <- liftIO . makeAbsolute $ "." </> "testing"
    totalfails <- runConduit $ walk baseDir .| myVisitor .| sumarize
    if totalfails == 0
    then sayLn $ "Regression test of all scripts succeeded."
    else exitWith (SomeTestsFailed ["Regression test failed! ("++show totalfails++" tests failed.)"])
  where   

    -- Produces directory data
    walk :: FilePath -> ConduitT () DirData (RIO env) ()
    walk path = do 
        result <- liftIO $ tryIOError (liftIO listdir)
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
    myVisitor :: ConduitT DirData Int (RIO App) ()
    myVisitor = loop 1
      where
        loop :: Int -> ConduitT DirData Int (RIO App) ()
        loop n = awaitForever $
            (\dird -> do 
                lift $ say $ ">> " ++ show n ++ ". "
                x <- lift $ process 4 dird     
                yield x
                loop (n + 1)
            ) 
                    

    sumarize :: ConduitT Int Void (RIO env) Int
    sumarize = loop 0 
      where
        loop :: Int -> ConduitT Int Void (RIO env) Int
        loop i = 
          await >>= maybe (return i) 
                          (\x -> loop $! (i+x))


