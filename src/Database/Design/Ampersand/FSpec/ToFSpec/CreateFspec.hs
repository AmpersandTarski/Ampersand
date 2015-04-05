module Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec 
  (createFSpec,getPopulationsFrom)
  
where
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.ADL1.P2A_Converters
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.FSpec.ShowMeatGrinder
import Database.Design.Ampersand.Input
import Database.Design.Ampersand.FSpec.ToFSpec.ADL2FSpec
import System.Directory
import System.FilePath
import Data.Traversable (sequenceA)
import Control.Applicative
import Database.Design.Ampersand.Core.ToMeta
import Control.Monad

fatal :: Int -> String -> a
fatal = fatalMsg "CreateFspec"



-- | create an FSpec, based on the provided command-line options.
createFSpec :: Options  -- ^The options derived from the command line
            -> IO(Guarded FSpec)
createFSpec opts =
  do userP_Ctx <- parseADL opts (fileName opts) -- the P_Context of the user's sourceFile
     let 
     genFiles userP_Ctx >> genTables userP_Ctx
   where
    genFiles :: Guarded P_Context -> IO(Guarded ())
    genFiles uCtx 
      = case pCtx2Fspec uCtx of
          Errors es -> return(Errors es)
          Checked uFspec
            ->   when (genASTFile opts) (doGenMetaFile AST uFspec)
              >> when (genGenericsFile opts) (doGenMetaFile Generics uFspec)
              >> return (Checked ())
     
       
    genTables :: Guarded P_Context -> IO(Guarded FSpec)
    genTables uCtx= case whatTablesToCreateExtra of
       Nothing 
--         -> return (pCtx2Fspec uCtx) --no magical Meta Mystery 'Meuk', so a 'normal' fSpec is returned.
         -> do rapP_Ctx <- getFormalFile AST
               let rapP_CtxMeta = unguard $ pure . (toMeta (opts {metaTablesHaveUnderscore=True})) <$> rapP_Ctx
               return (pCtx2Fspec rapP_CtxMeta) 
       Just mType
         -> do rapP_Ctx <- getFormalFile mType -- the P_Context of the 
               let rapP_CtxMeta = unguard $ pure . (toMeta opts) <$> rapP_Ctx
                   metaPop = unguard ( pure . toMeta opts <$> (unguard ( grind mType <$> pCtx2Fspec uCtx)))
               let populatedRapCtx = --the P_Context of the user is transformed with the meatgrinder to a
                                     -- P_Context, that contains all 'things' specified in the user's file 
                                     -- as populations in RAP. These populations are the only contents of 
                                     -- the returned P_Context. 
                     (merge.sequenceA) [ --metaPop, 
                                         rapP_CtxMeta, uCtx]
               return $ pCtx2Fspec populatedRapCtx -- the RAP specification that is populated with the user's 'things' is returned.
     
       
    
    whatTablesToCreateExtra :: Maybe MetaType
    whatTablesToCreateExtra 
       | genASTTables opts     = Just AST
       | genGenericTables opts = Just Generics
       | otherwise             = Nothing
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
               _  -> fatal 83 "Meatgrinder returns included file. That isn't anticipated."
            
     
getPopulationsFrom :: Options -> FilePath -> IO (Guarded [Population])
getPopulationsFrom opts filePath =
 do gpCtx <- parseADL opts filePath
    return (unguard $ f <$> gpCtx) 
   where
     f :: P_Context -> Guarded [Population]
     f pCtx = unguard $ 
                pure . initialPops . makeFSpec opts
                 <$> pCtx2aCtx opts pCtx

doGenMetaFile :: MetaType -> FSpec -> IO()
doGenMetaFile mType fSpec =
 do { verboseLn (getOpts fSpec) $ "Generating "++show mType++" meta file for "++name fSpec
    ; writeFile outputFile contents
    ; verboseLn (getOpts fSpec) $ show mType++" written into " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput (getOpts fSpec)) $ fpath
       (fpath,contents) = makeMetaPopulationFile mType fSpec
 