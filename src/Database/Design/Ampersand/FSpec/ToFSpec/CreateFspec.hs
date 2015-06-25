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
import Control.Monad
import Data.GraphViz

fatal :: Int -> String -> a
fatal = fatalMsg "CreateFspec"



-- | create an FSpec, based on the provided command-line options.
createFSpec :: Options  -- ^The options derived from the command line
            -> IO(Guarded FSpec)
createFSpec opts =
  do userP_Ctx <- parseADL opts (fileName opts) -- the P_Context of the user's sourceFile
     genTypeGraphs userP_Ctx   -- Type graphs must be generated from the P-Structure, in order to visualize type errors.
     genFiles userP_Ctx >> genTables userP_Ctx
   where
-- For educational purposes, the switch "--typeGraphs" can be used. It executes genTypeGraphs (below), which prints two graphs.
-- For an explanation of those graphs, consult the corresponding paper (Joosten&Joosten, Ramics 2015).
-- Use only for very small scripts to obtain informative results.
-- For the large scripts that are used in projects, the program may abort due to insufficient resources.
    genTypeGraphs :: Guarded P_Context -> IO(Guarded ())
    genTypeGraphs userP_Ctx
      = case typeGraphs opts of
          True -> do { let (stTypeGraph, condensedGraph) = computeTypeGraphs userP_Ctx
                     ; condensedGraphPath<-runGraphvizCommand Dot condensedGraph Png (replaceExtension ("Condensed_Graph_of_"++baseName opts) ".png")
                     ; verboseLn opts (condensedGraphPath++" written.")
                     ; stDotGraphPath<-runGraphvizCommand Dot stTypeGraph Png (replaceExtension ("stGraph_of_"++baseName opts) ".png")
                     ; verboseLn opts (stDotGraphPath++" written.")
                     ; return (Checked ())
                     }
          _    -> return (Checked ())

    computeTypeGraphs :: Guarded P_Context -> (DotGraph String, DotGraph String)
    computeTypeGraphs _ = fatal 41 "TODO typeGraphs"

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
         -> return (pCtx2Fspec uCtx)
       Just mType
         -> do rapP_Ctx <- getFormalFile mType -- the P_Context of the 
               let populationPctx       = unguard ( grind mType <$> pCtx2Fspec uCtx)
                   populatedRapPctx     = merge.sequenceA $ [rapP_Ctx,populationPctx]
                   metaPopulatedRapPctx = toMeta opts <$> populatedRapPctx
                   allCombinedPctx      = merge.sequenceA $ [uCtx, metaPopulatedRapPctx]
               return $ pCtx2Fspec allCombinedPctx -- the RAP specification that is populated with the user's 'things' is returned.

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
    ; verboseLn (getOpts fSpec) $ show mType++" written into " ++ outputFile ++ ""
    }
 where outputFile = combine (dirOutput (getOpts fSpec)) $ fpath
       (fpath,contents) = makeMetaPopulationFile mType fSpec
 