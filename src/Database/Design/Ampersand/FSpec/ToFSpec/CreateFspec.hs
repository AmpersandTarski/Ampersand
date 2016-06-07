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
import Database.Design.Ampersand.FSpec.ArchiAnalyze
import Database.Design.Ampersand.FSpec.ToFSpec.ADL2FSpec
import System.FilePath
import Control.Monad

-- | create an FSpec, based on the provided command-line options.
--   Without the command-line switch "--meta-tables", 
--   Ampersand compiles its script (userP_Ctx) straightforwardly in first order relation algebra.
--   This is useful for simple scripts and the compilation process is easy to understand.
--
--   With "--meta-tables" switched on, Ampersand does more.
--   This switch is useful for higher order Ampersand,
--   in which the user can work with the rules, relations and concepts of the model inside the model.
--   Besides the user script, userP_Ctx, Ampersand creates its own metamodel, rapP_Ctx, which is generated from "AST.adl"
--   This metamodel is populated with the result of grinding userP_Ctx, being populationPctx.
--   Grinding means to analyse the script down to the binary relations that constitute the metamodel.
--   The combination of model and populated metamodel results in the Guarded FSpec,
--   which is the result of createFSpec.
createFSpec :: Options  -- ^The options derived from the command line
            -> IO(Guarded FSpec)
createFSpec opts =
  do userP_Ctx <- parseADL opts (fileName opts) -- the P_Context of the user's sourceFile
     genFiles userP_Ctx >> genTables userP_Ctx
   where
    genFiles :: Guarded P_Context -> IO(Guarded ())
    genFiles uCtx
      = case pCtx2Fspec uCtx of
          Errors es -> return(Errors es)
          Checked uFspec
            ->   when (genASTFile opts) (doGenMetaFile uFspec)
              >> return (Checked ())

    genTables :: Guarded P_Context -> IO(Guarded FSpec)
    genTables uCtx= case genASTTables opts of
       False
         -> return (pCtx2Fspec uCtx)
       True
         -> do rapP_Ctx <- parseMeta opts -- the P_Context of the
               let populationPctx       = join ( grind <$> pCtx2Fspec uCtx)
                   populatedRapPctx     = merge.sequenceA $ [rapP_Ctx,populationPctx]
                   metaPopulatedRapPctx = populatedRapPctx
                   allCombinedPctx      = merge.sequenceA $ [uCtx, metaPopulatedRapPctx]
               return $ pCtx2Fspec allCombinedPctx -- the RAP specification that is populated with the user's 'things' is returned.



    toFspec :: A_Context -> Guarded FSpec
    toFspec = pure . makeFSpec opts
    pCtx2Fspec :: Guarded P_Context -> Guarded FSpec
    pCtx2Fspec c = join $ toFspec <$> (join $ pCtx2aCtx opts <$> c)
    merge :: Guarded [P_Context] -> Guarded P_Context
    merge ctxs = fmap f ctxs
      where
       f []     = fatal 77 $ "merge must not be applied to an empty list"
       f (c:cs) = foldr mergeContexts c cs
    grind :: FSpec -> Guarded P_Context
    grind fSpec
      = fmap fstIfNoIncludes $ parseCtx f c
      where (f,c) = makeMetaPopulationFile fSpec
            fstIfNoIncludes (a,includes)
             = case includes of
               [] -> a
               _  -> fatal 83 "Meatgrinder returns included file. That isn't anticipated."



doGenMetaFile :: FSpec -> IO()
doGenMetaFile fSpec =
 do { verboseLn (getOpts fSpec) $ "Generating meta file for "++name fSpec
    ; writeFile outputFile contents
    ; verboseLn (getOpts fSpec) $ "Meatgrinder output written into " ++ outputFile ++ ""
    }
 where outputFile = combine (dirOutput (getOpts fSpec)) $ fpath
       (fpath,contents) = makeMetaPopulationFile fSpec
