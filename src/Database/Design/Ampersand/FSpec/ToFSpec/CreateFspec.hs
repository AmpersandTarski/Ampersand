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
import Database.Design.Ampersand.FSpec.ToFSpec.ADL2FSpec
import System.Directory
import System.FilePath
import Data.Traversable (sequenceA)
import Control.Applicative

fatal :: Int -> String -> a
fatal = fatalMsg "Parsing"

-- | create an FSpec, based on the provided command-line options.
createFSpec :: Options  -- ^The options derived from the command line
            -> IO(Guarded FSpec)
createFSpec opts =
  do userCtx <- parseADL opts (fileName opts)
     let userFspec = pCtx2Fspec userCtx
     if includeRap opts
     then do rapCtx <- getRap
             let populatedRapCtx = 
                   (merge.sequenceA) [grind <?> userFspec, rapCtx]
             return $ pCtx2Fspec populatedRapCtx
     else return userFspec
  where
    getRap :: IO (Guarded P_Context)
    getRap 
     = do let rapFile = ampersandDataDir opts </> "FormalAmpersand" </> "FormalAmpersand.adl"
          exists <- doesFileExist rapFile
          if exists then parseADL opts rapFile
          else fatal 98 $ unlines
                 [ "Ampersand isn't installed properly. Formal specification of Ampersand expected at:"
                 , "  "++show rapFile
                 , "  (You might need to re-install ampersand...)"
                 ]
    toFspec :: A_Context -> Guarded FSpec
    toFspec = pure . makeFSpec opts
    pCtx2Fspec :: Guarded P_Context -> Guarded FSpec
    pCtx2Fspec c = toFspec <?> ((pCtx2aCtx opts) <?> c)
    merge :: Guarded [P_Context] -> Guarded P_Context
    merge ctxs = fmap f ctxs
      where
       f []     = fatal 77 $ "merge must not be applied to an empty list"
       f (c:cs) = foldr mergeContexts c cs
    grind :: FSpec -> Guarded P_Context
    grind fSpec
      = fmap fstIfNoIncludes $ parseCtx f c
      where (f,c) = meatGrinder fSpec 
            fstIfNoIncludes (a,includes)
             = case includes of 
               [] -> a
               _  -> fatal 83 "Meatgrinder returns included file. That shouldn't be possible!"
            
     
getPopulationsFrom :: Options -> FilePath -> IO (Guarded [Population])
getPopulationsFrom opts filePath =
 do gpCtx <- parseADL opts filePath
    return (f <?> gpCtx) 
   where
     f :: P_Context -> Guarded [Population]
     f pCtx = pure . initialPops . makeFSpec opts
          <?> (pCtx2aCtx opts pCtx)
     
     