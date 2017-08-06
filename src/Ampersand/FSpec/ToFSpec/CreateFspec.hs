{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.FSpec.ToFSpec.CreateFspec
  (createMulti)

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
import Data.List
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
--   which is the result of createMulti.
createMulti :: Options  -- ^The options derived from the command line
            -> IO(Guarded MultiFSpecs)
createMulti opts =
  do fAmpP_Ctx :: Guarded P_Context <-
        if genMetaFile opts ||
           genMetaTables opts ||
           genRapPopulationOnly opts 
        then parseMeta opts  -- the P_Context of the formalAmpersand metamodel
        else return --Not very nice way to do this, but effective. Don't try to remove the return, otherwise the fatal could be evaluated... 
               $ fatal "With the given switches, the formal ampersand model is not supposed to play any part."
     userP_Ctx:: Guarded P_Context <- parseADL opts (fileName opts) -- the P_Context of the user's sourceFile
     let fAmpFSpec :: FSpec
         fAmpFSpec = case pCtx2Fspec fAmpP_Ctx of
                       Checked f -> f
                       Errors err-> fatal ("The FormalAmpersand ADL scripts are not type correct:\n" ++
                                           intercalate (replicate 30 '=') (map showErr err))
         userGFSpec :: Guarded FSpec
         userGFSpec = pCtx2Fspec userP_Ctx              -- the FSpec resuting from the user's souceFile
     when (genMetaFile opts) (dumpMetaFile fAmpFSpec userGFSpec)
     if genMetaTables opts || genRapPopulationOnly opts
     then case userGFSpec of 
            Errors err -> return $ Errors err  
            Checked usrFSpec
                      -> do let grinded :: P_Context
                                grinded = grind fAmpFSpec usrFSpec -- the user's sourcefile grinded, i.e. a P_Context containing population in terms of formalAmpersand.
                                metaPopPCtx :: Guarded P_Context
                                metaPopPCtx = mergeContexts grinded <$> fAmpP_Ctx
                                metaPopFSpec :: Guarded FSpec
                                metaPopFSpec = pCtx2Fspec metaPopPCtx
                            return $ MultiFSpecs <$> (pCtx2Fspec $ mergeContexts <$> userP_Ctx <*> pure grinded)
                                                 <*> (Just <$> metaPopFSpec)
     else return $ MultiFSpecs <$> userGFSpec <*> pure Nothing
   where
    dumpMetaFile :: FSpec -> Guarded FSpec -> IO()
    dumpMetaFile faSpec a = case a of
              Checked fSpec -> writeMetaFile $ dumpGrindFile faSpec fSpec
              Errors err    -> fatal ("The FormalAmpersand ADL scripts are not type correct:\n" ++
                                           intercalate (replicate 30 '=') (map showErr err))
      where
        writeMetaFile :: (FilePath,String) -> IO ()
        writeMetaFile (filePath,metaContents) = do
            verboseLn opts ("Generating meta file in path "++dirOutput opts)
            writeFile (dirOutput opts </> filePath) metaContents      
            verboseLn opts ("\""++filePath++"\" written")

    pCtx2Fspec :: Guarded P_Context -> Guarded FSpec
    pCtx2Fspec c = makeFSpec opts <$> join (pCtx2aCtx opts <$> c)
