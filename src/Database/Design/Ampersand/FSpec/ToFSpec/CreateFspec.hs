module Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec 
  (createFSpec,getPopulationsFrom)
  
where
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.ADL1.P2A_Converters
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.ShowMeatGrinder
import Database.Design.Ampersand.Input
import Database.Design.Ampersand.FSpec.ToFSpec.ADL2Plug (showPlug)
import Database.Design.Ampersand.FSpec.ToFSpec.ADL2FSpec
import Data.List
import System.Directory
import System.FilePath
import Control.Monad
import Data.Traversable (sequenceA)

fatal :: Int -> String -> a
fatal = fatalMsg "Parsing"

-- | create an FSpec, based on the provided command-line options.
createFSpec :: Options  -- ^The options derived from the command line
            -> IO(Guarded FSpec)
createFSpec opts =
  do userCtx <- parseADL opts (fileName opts)
     bothCtx <- if includeRap opts
                then do let rapFile = ampersandDataDir opts </> "FormalAmpersand" </> "FormalAmpersand.adl"
                        exists <- doesFileExist rapFile
                        when (not exists) (fatal 39 $ "Ampersand isn't installed properly. Formal specification of Ampersand expected at:"
                                                    ++"\n  "++show rapFile
                                                    ++"\n  (You might want to re-install ampersand...)")
                        rapCtx <- parseADL opts rapFile
                        popsCtx <- popsCtxOf userCtx
                        case userCtx of 
                          Errors err   -> return (Errors err)
                          Checked uCtx -> case sequenceA [rapCtx, popsCtx] of
                                            Errors err -> return (Errors err)
                                            Checked ctxs -> return (Checked $ foldr mergeContexts uCtx ctxs)
                else return userCtx
     case bothCtx of
        Errors err -> return (Errors err)
        Checked pCtx
           -> do let (gaCtx) = pCtx2aCtx opts pCtx
                 case gaCtx of
                   (Errors  err ) -> return (Errors err)
                   (Checked aCtx) -> 
                    do { let fSpec = makeFSpec opts aCtx
                       ; when (development (getOpts fSpec)) $
                          do { putStrLn "Table structure for internal plugs:\n"
                             ; putStrLn $ (unlines . concat) [showPlug plug | InternalPlug plug <- plugInfos fSpec]
                             }
                                      
                       ; return $ Checked fSpec
                       }
  where
    popsCtxOf :: Guarded P_Context ->IO(Guarded P_Context)
    popsCtxOf gp =
     (case gp of
       Errors _ -> return (Errors []) -- The errors are already in the error list
       Checked pCtx
         -> case pCtx2aCtx opts pCtx of
              (Errors  err ) -> return (Errors err)
              (Checked aCtx)
                 -> do let fSpec = makeFSpec opts aCtx
                           (popFilePath,popContents) = meatGrinder fSpec
                       when (genMeat opts) $
                          do let outputFile = combine (dirOutput opts) $ replaceExtension popContents ".adl"
                             writeFile outputFile popContents
                             verboseLn opts $ "Meta population written into " ++ outputFile ++ "."
                          
                       case parseCtx popFilePath popContents of
                         (Errors  err) -> fatal 64 ("MeatGrinder has errors!"
                                                 ++ intercalate "\n"(map showErr err))
                         (Checked (pctx,[])) -> return (Checked pctx)
                         (Checked _ )        -> fatal 67 "Meatgrinder returns included file????"
     )

     
getPopulationsFrom :: Options -> FilePath -> IO (Guarded [Population])
getPopulationsFrom opts filePath =
 do { gpCtx <- parseADL opts filePath
    ; return $ case gpCtx of 
                 Errors err    -> Errors err
                 Checked pCtx  -> case pCtx2aCtx opts pCtx of
                                    (Errors  err ) -> Errors err
                                    (Checked aCtx) -> Checked $ initialPops (makeFSpec opts aCtx)
                      
    }
     
     