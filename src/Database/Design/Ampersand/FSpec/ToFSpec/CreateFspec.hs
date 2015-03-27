module Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec 
  (createFSpec,getPopulationsFrom)
  
where
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.ADL1.P2A_Converters
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



-- | create an FSpec, based on the provided command-line options.
createFSpec :: Options  -- ^The options derived from the command line
            -> IO(Guarded FSpec)
createFSpec opts =
  do userCtx <- parseADL opts (fileName opts) -- the P_Context of the user's sourceFile
     let userFspec = pCtx2Fspec userCtx
     case whatToCreateExtra of
       Nothing 
         -> return userFspec --no magical Meta Mystery 'Meuk', so a 'normal' fSpec is returned.
       Just mType
         -> do rapCtx <- getFormalFile mType -- the P_Context of the 
               let rapCtxMeta = unguard $ (pure . toMeta) <$> rapCtx
                   grindedUserCtx = unguard $ pure . toMeta <$> (unguard $ grind mType <$> userFspec)
               let populatedRapCtx = --the P_Context of the user is transformed with the meatgrinder to a
                                     -- P_Context, that contains all 'things' specified in the user's file 
                                     -- as populations in RAP. These populations are the only contents of 
                                     -- the returned P_Context. 
                     (merge.sequenceA) [grindedUserCtx, rapCtxMeta] -- Both p_Contexts are merged into a single P_Context
               return $ pCtx2Fspec populatedRapCtx -- the RAP specification that is populated with the user's 'things' is returned.
     where
    
    whatToCreateExtra :: Maybe MetaType
    whatToCreateExtra 
       | genASTTables opts     || genASTFile opts      = Just AST
       | genGenericTables opts || genGenericsFile opts = Just Generics
       | otherwise = Nothing
    getFormalFile :: MetaType -> IO(Guarded P_Context)
    getFormalFile mType
     = do let file = ampersandDataDir opts 
                    </> "FormalAmpersand" 
                    </> (case mType of
                           Generics -> "Generics.adl"
                           AST -> "FormalAmpersand.adl")
          exists <- doesFileExist file
          if exists then parseADL opts file
          else fatal 98 $ unlines
                 [ "Ampersand isn't installed properly. Couldn't read:"
                 , "  "++show file
                 , "  (Make sure you have the latest content of Ampersand data. You might need to re-install ampersand...)"
                 ]
    
    
    toFspec :: A_Context -> Guarded FSpec
    toFspec = pure . makeFSpec opts
    pCtx2Fspec :: Guarded P_Context -> Guarded FSpec
    pCtx2Fspec c = unguard $ toFspec <$> (unguard $ pCtx2aCtx opts <$> c)
    merge :: Guarded [P_Context] -> Guarded P_Context
    merge ctxs = fmap f ctxs
      where
       f []     = fatal 77 $ "merge must not be applied to an empty list"
       f (c:cs) = foldr mergeContexts c cs
    grind :: MetaType -> FSpec -> Guarded P_Context
    grind mType fSpec
      = fmap fstIfNoIncludes $ parseCtx f c
      where (f,c) = makeMetaPopulationFile mType fSpec
            fstIfNoIncludes (a,includes)
             = case includes of 
               [] -> a
               _  -> fatal 83 "Meatgrinder returns included file. That shouldn't be possible!"
            
     
getPopulationsFrom :: Options -> FilePath -> IO (Guarded [Population])
getPopulationsFrom opts filePath =
 do gpCtx <- parseADL opts filePath
    return (unguard $ f <$> gpCtx) 
   where
     f :: P_Context -> Guarded [Population]
     f pCtx = unguard $ 
                pure . initialPops . makeFSpec opts
                 <$> pCtx2aCtx opts pCtx


 