module Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec
  (createFSpec)

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
import System.FilePath
import Database.Design.Ampersand.Core.ToMeta
import Control.Monad

-- | create an FSpec, based on the provided command-line options.
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
            ->   when (genASTFile opts) (doGenMetaFile AST uFspec)
              >> when (genGenericsFile opts) (doGenMetaFile Generics uFspec)
              >> return (Checked ())

    genTables :: Guarded P_Context -> IO(Guarded FSpec)
    genTables uCtx= case whatTablesToCreateExtra of
       Nothing
         -> return (pCtx2Fspec uCtx)
       Just mType
         -> do rapP_Ctx <- getFormalFile mType -- the P_Context of the
               let populationPctx       = join ( grind mType <$> pCtx2Fspec uCtx)
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
