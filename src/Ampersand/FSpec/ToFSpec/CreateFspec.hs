module Ampersand.FSpec.ToFSpec.CreateFspec
  (createFSpec)

where
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Ampersand.Basics
import Ampersand.Misc
import Ampersand.ADL1
import Ampersand.ADL1.P2A_Converters
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.ShowMeatGrinder
import Ampersand.Input
import Ampersand.FSpec.ToFSpec.ADL2FSpec
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
     let gFSpec = pCtx2Fspec userP_Ctx
     if genMetaTables opts
     then do rapP_Ctx <- parseMeta opts -- the P_Context of the formalAmpersand metamodel
             case makeMetaPopulationFile <$> gFSpec of
               Checked (filePath,metaContents)
                 -> do writeMetaFile (filePath,metaContents)
                       return $ genMeta userP_Ctx gFSpec rapP_Ctx
               Errors errs   -> return (Errors errs)
     else return gFSpec        
   where
    writeMetaFile :: (FilePath,String) -> IO ()
    writeMetaFile (filePath,metaContents) = do
        verboseLn opts ("Generating meta file in path "++dirOutput opts)
        writeFile (dirOutput opts </> filePath) metaContents      
        verboseLn opts ("\""++filePath++"\" written")

    -- | Combine the original user's context with
    genMeta :: Guarded P_Context -> Guarded FSpec -> Guarded P_Context -> Guarded FSpec
    genMeta gUserCtx gFSpec gRapP_Ctx
       = pCtx2Fspec . merge . sequenceA $ [gUserCtx, gRapP_Ctx, join (grind <$> gFSpec)]
         
    toFspec :: A_Context -> Guarded FSpec
    toFspec = pure . makeFSpec opts
    pCtx2Fspec :: Guarded P_Context -> Guarded FSpec
    pCtx2Fspec c = join $ toFspec <$> (join $ pCtx2aCtx opts <$> c)
    merge :: Guarded [P_Context] -> Guarded P_Context
    merge ctxs = f <$> ctxs
      where
       f []     = fatal 77 $ "merge must not be applied to an empty list"
       f (c:cs) = foldr mergeContexts c cs
    grind :: FSpec -> Guarded P_Context
    grind fSpec = f <$> parseCtx filePath metaContents
      where (filePath,metaContents) = makeMetaPopulationFile fSpec
            f (a,[]) = a
            f _      = fatal 83 "Meatgrinder returns included file. That isn't anticipated."
