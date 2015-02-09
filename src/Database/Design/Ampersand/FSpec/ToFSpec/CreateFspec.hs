module Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec 
  (createFSpec,getPopulationsFrom,createFSpecForGenerics)
  
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
import Database.Design.Ampersand.Core.ToMeta

fatal :: Int -> String -> a
fatal = fatalMsg "CreateFspec"

data FSpecType = Plain | Generics | Rap deriving(Eq)

createFSpec :: Options -> IO (Guarded FSpec)
createFSpec opts = createFSpec' (if includeRap opts then Rap else Plain) opts

createFSpecForGenerics :: Options -> IO (Guarded FSpec)
createFSpecForGenerics = createFSpec' Generics

-- | create an FSpec, based on the provided command-line options.
createFSpec' :: FSpecType -- The requested type of FSpec?
            -> Options  -- ^The options derived from the command line
            -> IO(Guarded FSpec)
createFSpec' fType opts =
  do userCtx <- parseADL opts (fileName opts) -- the P_Context of the user's sourceFile
     let userFspec = pCtx2Fspec userCtx
     case fType of
       Plain 
         -> return userFspec --no magical Meta Mystery 'Meuk', so a 'normal' fSpec is returned.
       _ 
         -> do rapCtx <- getFormalFile -- the P_Context of the 
               let rapCtxMeta = (pure . toMeta) <?> rapCtx
                   grindedUserCtx = (pure . toMeta) <?> (grind <?> userFspec)
               let populatedRapCtx = --the P_Context of the user is transformed with the meatgrinder to a
                                     -- P_Context, that contains all 'things' specified in the user's file 
                                     -- as populations in RAP. These populations are the only contents of 
                                     -- the returned P_Context. 
                     (merge.sequenceA) [grindedUserCtx, rapCtxMeta] -- Both p_Contexts are merged into a single P_Context
               return $ pCtx2Fspec populatedRapCtx -- the RAP specification that is populated with the user's 'things' is returned.
     where
    
      
    getFormalFile :: IO(Guarded P_Context)
    getFormalFile 
     = do let rapFile = ampersandDataDir opts 
                    </> "FormalAmpersand" 
                    </> (case fType of
                           Plain  -> fatal 57 "Plain does not need to read anything."
                           Generics -> "Generics.adl"
                           Rap -> "FormalAmpersand.adl")
          exists <- doesFileExist rapFile
          if exists then parseADL opts rapFile
          else fatal 98 $ unlines
                 [ "Ampersand isn't installed properly. Couldn't read:"
                 , "  "++show rapFile
                 , "  (Make sure you have the latest content of Ampersand data. You might need to re-install ampersand...)"
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
     
     