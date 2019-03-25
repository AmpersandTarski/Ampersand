{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.FSpec.ToFSpec.CreateFspec
  (createMulti)

where
import           Ampersand.ADL1
import           Ampersand.ADL1.P2A_Converters
import           Ampersand.Basics
import           Ampersand.Core.A2P_Converters
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ShowMeatGrinder
import           Ampersand.FSpec.ToFSpec.ADL2FSpec
import           Ampersand.FSpec.Transformers 
import           Ampersand.Input
import           Ampersand.Misc
import           Control.Monad
import           Data.List
import qualified Data.List.NonEmpty as NEL (toList)
import qualified Data.Set as Set
import           System.FilePath

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
createMulti opts@Options{..} =
  do fAmpP_Ctx :: Guarded P_Context <-
        if genMetaFile ||
           genRapPopulationOnly ||
           addSemanticMetamodel
        then parseMeta opts -- the P_Context of the formalAmpersand metamodel
        else return --Not very nice way to do this, but effective. Don't try to remove the return, otherwise the fatal could be evaluated... 
               $ fatal "With the given switches, the formal ampersand model is not supposed to play any part."
     userP_Ctx:: Guarded P_Context <- 
        case fileName of
          Just x -> parseADL opts x -- the P_Context of the user's sourceFile
          Nothing -> exitWith . WrongArgumentsGiven $ ["Please supply the name of an ampersand file"]
    
     systemP_Ctx:: Guarded P_Context <- parseSystemContext opts

     let fAmpModel :: MetaFSpec
         fAmpModel = MetaFSpec
            { metaModelFileName = "FormalAmpersand.adl"
            , model             = 
                case pCtx2Fspec fAmpP_Ctx of
                  Checked f _ -> f
                  Errors errs -> fatal . unlines $
                      "The FormalAmpersand ADL scripts are not type correct:"
                    : (intersperse (replicate 30 '=') . fmap showErr . NEL.toList $ errs)
            , transformers  = transformersFormalAmpersand
            }
         sysCModel :: MetaFSpec
         sysCModel = MetaFSpec
            { metaModelFileName = "SystemContext.adl"
            , model             = 
                case pCtx2Fspec systemP_Ctx of
                  Checked f _ -> f
                  Errors errs -> fatal . unlines $
                      "The SystemContext ADL scripts are not type correct:"
                    : (intersperse (replicate 30 '=') . fmap showErr . NEL.toList $ errs)
            , transformers  = transformersSystemContext
            }
         userP_CtxPlus :: Guarded P_Context
         userP_CtxPlus =
              if addSemanticMetamodel 
              then addSemanticModel (model fAmpModel) <$> userP_Ctx
              else userP_Ctx
         
         -- | When the semantic model of a metamodel is added to the user's model, we add
         --   the relations as wel as the generalisations to it, so they are available to the user
         --   in an implicit way. We want other things, like Idents, Views and REPRESENTs available too.
         addSemanticModel :: FSpec -> P_Context -> P_Context
         addSemanticModel metamodel pCtx =
            pCtx { ctx_pos    = ctx_pos    pCtx
                 , ctx_lang   = ctx_lang   pCtx
                 , ctx_markup = ctx_markup pCtx
                 , ctx_pats   = ctx_pats   pCtx `uni` map aPattern2pPattern     (Set.toList . instances $ metamodel)
                 , ctx_rs     = ctx_rs     pCtx `uni` map aRule2pRule           (Set.toList . instances $ metamodel)
                 , ctx_ds     = ctx_ds     pCtx `uni` map aRelation2pRelation   (Set.toList . instances $ metamodel)
                 , ctx_cs     = ctx_cs     pCtx `uni` map id                    (Set.toList . instances $ metamodel)
                 , ctx_ks     = ctx_ks     pCtx `uni` map aIdentityDef2pIdentityDef (Set.toList . instances $ metamodel)
                 , ctx_rrules = ctx_rrules pCtx `uni` map aRoleRule2pRoleRule   (Set.toList . instances $ metamodel)
                 , ctx_rrels  = ctx_rrels  pCtx
                 , ctx_reprs  = ctx_reprs  pCtx `uni` (reprList . fcontextInfo $ metamodel)
                 , ctx_vs     = ctx_vs     pCtx `uni` map aViewDef2pViewDef     (Set.toList . instances $ metamodel)
                 , ctx_gs     = ctx_gs     pCtx `uni` map aClassify2pClassify   (Set.toList . instances $ metamodel)
                 , ctx_ifcs   = ctx_ifcs   pCtx `uni` map aInterface2pInterface (Set.toList . instances $ metamodel)
                 , ctx_ps     = ctx_ps     pCtx 
                 , ctx_pops   = ctx_pops   pCtx `uni` map aPopulation2pPopulation (Set.toList . instances $ metamodel)
                 , ctx_metas  = ctx_metas  pCtx
                 }
           where
            uni :: Eq a => [a] -> [a] -> [a]
            uni xs ys = nub (xs ++ ys)
         userGFSpec :: Guarded FSpec
         userGFSpec = 
            pCtx2Fspec $ 
              if useSystemContext
              then mergeContexts <$> userPlus <*> (grind opts sysCModel <$> pCtx2Fspec userP_Ctx)  
              else userP_Ctx
           where 
            userPlus :: Guarded P_Context
            userPlus = addSemanticModel (model sysCModel) <$> userP_Ctx
         result :: Guarded MultiFSpecs
         result = 
           if genRapPopulationOnly
           then case userGFSpec of 
                  Errors err -> Errors err  
                  Checked usrFSpec _
                           -> let grinded :: P_Context
                                  grinded = grind opts fAmpModel usrFSpec -- the user's sourcefile grinded, i.e. a P_Context containing population in terms of formalAmpersand.
                                  metaPopFSpec :: Guarded FSpec
                                  metaPopFSpec =  pCtx2Fspec $ mergeContexts <$> pure grinded  <*> fAmpP_Ctx
                              in MultiFSpecs <$> (pCtx2Fspec $ mergeContexts <$> userP_CtxPlus <*> pure grinded)
                                             <*> (Just <$> metaPopFSpec)
           else MultiFSpecs <$> userGFSpec <*> pure Nothing
     res <- if genMetaFile
            then writeMetaFile fAmpModel userGFSpec
            else return $ pure ()
     return (res >> result)
  where
    useSystemContext :: Bool
    useSystemContext = genPrototype
    writeMetaFile :: MetaFSpec -> Guarded FSpec -> IO (Guarded ())
    writeMetaFile metaModel userSpec = 
       case makeMetaFile opts metaModel <$> userSpec of
        Checked (filePath,metaContents) ws -> 
                  do verboseLn $ "Generating meta file in path "++dirOutput
                     writeFile (dirOutput </> filePath) metaContents      
                     verboseLn $ "\"" ++ filePath ++ "\" written"
                     return $ Checked () ws
        Errors err -> return (Errors err)

    pCtx2Fspec :: Guarded P_Context -> Guarded FSpec
    pCtx2Fspec c = makeFSpec opts <$> join (pCtx2aCtx opts <$> c)
