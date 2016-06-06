module Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec
  (createFSpec)

where
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.ADL1.P2A_Converters
import Database.Design.Ampersand.Input
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.ShowMeatGrinder
import Database.Design.Ampersand.FSpec.ArchiAnalyze
import Database.Design.Ampersand.FSpec.ToFSpec.ADL2FSpec
import System.FilePath
import Database.Design.Ampersand.Core.ToMeta
import Control.Monad

-- | create an FSpec, based on the provided command-line options.
--   Without the command-line switches "--ast-tables" or "--ast-file", 
--   Ampersand compiles its script (userP_Ctx) straightforwardly in first order relation algebra.
--   This is useful for simple scripts and the compilation process is easy to understand.
--
--   With "--ast-tables" switched on, Ampersand does more.
--   These switches are useful for higher order Ampersand,
--   in which the user can work with the rules, relations and concepts of the model inside the model.
--   Besides the user script, userP_Ctx, Ampersand creates its own metamodel, rapP_Ctx, which is generated from "AST.adl"
--   This metamodel is populated with the result of grinding userP_Ctx, being populationPctx.
--   Grinding means to analyse the script down to the binary relations that constitute the metamodel.
--   The combination of model and populated metamodel results in the Guarded FSpec,
--   which is the result of createFSpec.
createFSpec :: Options  -- ^The options derived from the command line
            -> IO(Guarded FSpec)
createFSpec opts =
  do userP_Ctx <- parseADL opts (Left (fileName opts)) -- the P_Context of the user's sourceFile
     genFiles userP_Ctx >> genTables userP_Ctx
   where
    genFiles :: Guarded P_Context -> IO(Guarded ())
    genFiles uCtx
      = case pCtx2Fspec uCtx of
          Errors es -> return(Errors es)
          Checked uFspec
            ->   when (genASTFile      opts) (doGenMetaFile AST      uFspec)  -- for the meatgrinder
              >> when (genGenericsFile opts) (doGenMetaFile Generics uFspec)  -- for JSON-based communication to the front-end
              >> return (Checked ())

    genTables :: Guarded P_Context -> IO(Guarded FSpec)
    genTables uCtx= case whatTablesToCreateExtra of
       Nothing
         -> return (pCtx2Fspec uCtx)
       Just mType
         -> do rapP_Ctx <- getFormalFile mType -- the P_Context of the
               let populationPctx       = join ( grind mType <$> pCtx2Fspec uCtx)
                   populatedRapPctx     = (merge.sequenceA) [rapP_Ctx,populationPctx]
                   metaPopulatedRapPctx = toMeta opts <$> populatedRapPctx
                   allCombinedPctx      = (merge.sequenceA) [uCtx, metaPopulatedRapPctx]
               return (pCtx2Fspec allCombinedPctx) -- the RAP specification that is populated with the user's 'things' is returned.

    whatTablesToCreateExtra :: Maybe MetaType
    whatTablesToCreateExtra
       | genASTTables opts     = Just AST        -- for the meatgrinder
       | genGenericTables opts = Just Generics   -- for JSON-based communication to the front-end
       | otherwise             = Nothing

    getFormalFile :: MetaType -> IO(Guarded P_Context)
    getFormalFile mType
     = do parseADL opts (Right mType)


    toFspec :: A_Context -> Guarded FSpec
    toFspec = pure . makeFSpec opts
    pCtx2Fspec :: Guarded P_Context -> Guarded FSpec
    pCtx2Fspec c = join $ toFspec <$> (join $ pCtx2aCtx opts <$> c)
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



doGenMetaFile :: MetaType -> FSpec -> IO()
doGenMetaFile mType fSpec =
 do { verboseLn (getOpts fSpec) $ "Generating "++show mType++" meta file for "++name fSpec
    ; writeFile outputFile contents
    ; verboseLn (getOpts fSpec) $ show mType++" written into " ++ outputFile ++ ""
    }
 where outputFile = combine (dirOutput (getOpts fSpec)) $ fpath
       (fpath,contents) = makeMetaPopulationFile mType fSpec
