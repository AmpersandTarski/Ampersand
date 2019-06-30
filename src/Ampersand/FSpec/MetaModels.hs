{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.FSpec.MetaModels
  ( MetaModel(..)
  , mkGrindInfo
  , GrindInfo
  , grind
  , addSemanticModel
  , pCtx2Fspec
  )

where
import           Ampersand.ADL1
import           Ampersand.ADL1.P2A_Converters
import           Ampersand.Basics
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ShowMeatGrinder
import           Ampersand.FSpec.ToFSpec.ADL2FSpec
import           Ampersand.FSpec.Transformers 
import           Ampersand.Input
import           Ampersand.Misc
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NEL

parser :: MetaModel -> RIO App (Guarded P_Context)
parser FormalAmpersand = parseFormalAmpersand
parser SystemContext   = parseSystemContext 
parser FADocumented    = parseFormalAmpersandDocumented

pCtx2Fspec :: Options -> P_Context -> Guarded FSpec
pCtx2Fspec opts c = makeFSpec opts <$> pCtx2aCtx opts c


mkGrindInfo :: Options -> MetaModel -> RIO App GrindInfo
mkGrindInfo opts metamodel = do
    c <- parser metamodel
    return $ build c
  where
    build :: Guarded P_Context -> GrindInfo
    build pCtx = GrindInfo
            { metaModel    = metamodel
            , pModel       = case pCtx of
                  Errors errs -> fatal . unlines $
                          ("The ADL scripts of "++name metamodel++" cannot be parsed:")
                        : (L.intersperse (replicate 30 '=') . fmap show . NEL.toList $ errs)
                  Checked x [] -> x
                  Checked _ ws -> fatal . unlines $
                          ("The ADL scripts of "++name metamodel++" are not free of warnings:")
                        : (L.intersperse (replicate 30 '=') . fmap show $ ws)
            , fModel       = 
                case join $ pCtx2Fspec opts <$> pCtx of
                  Errors errs -> fatal . unlines $
                          ("The ADL scripts of "++name metamodel++" cannot be parsed:")
                        : (L.intersperse (replicate 30 '=') . fmap show . NEL.toList $ errs)
                  Checked x [] -> x
                  Checked _ ws -> fatal . unlines $
                          ("The ADL scripts of "++name metamodel++" are not free of warnings:")
                        : (L.intersperse (replicate 30 '=') . fmap show $ ws)
            , transformers = case metamodel of
                                FormalAmpersand -> transformersFormalAmpersand
                                FADocumented    -> transformersFormalAmpersand
                                SystemContext   -> transformersSystemContext
            }


-- | When the semantic model of a metamodel is added to the user's model, we add
--   the relations as wel as the generalisations to it, so they are available to the user
--   in an implicit way. We want other things, like Idents, Views and REPRESENTs available too.
addSemanticModel :: MetaModel -> P_Context -> RIO App P_Context
addSemanticModel metamodel pCtx = do
  env <- ask
  let opts = getOptions env
  grindInfo <- mkGrindInfo opts metamodel
  let pCtxOfMetaModel = pModel grindInfo
  return PCtx    
        { ctx_nm     = ctx_nm     pCtx
        , ctx_pos    = ctx_pos    pCtx
        , ctx_lang   = ctx_lang   pCtx
        , ctx_markup = ctx_markup pCtx
        , ctx_pats   = ctx_pats   pCtx `uni` ctx_pats   pCtxOfMetaModel
        , ctx_rs     = ctx_rs     pCtx `uni` ctx_rs     pCtxOfMetaModel
        , ctx_ds     = ctx_ds     pCtx `uni` ctx_ds     pCtxOfMetaModel
        , ctx_cs     = ctx_cs     pCtx `uni` ctx_cs     pCtxOfMetaModel
        , ctx_ks     = ctx_ks     pCtx `uni` ctx_ks     pCtxOfMetaModel
        , ctx_rrules = ctx_rrules pCtx `uni` ctx_rrules pCtxOfMetaModel
        , ctx_rrels  = ctx_rrels  pCtx
        , ctx_reprs  = ctx_reprs  pCtx `uni` ctx_reprs  pCtxOfMetaModel
        , ctx_vs     = ctx_vs     pCtx `uni` ctx_vs     pCtxOfMetaModel
        , ctx_gs     = ctx_gs     pCtx `uni` ctx_gs     pCtxOfMetaModel
        , ctx_ifcs   = ctx_ifcs   pCtx `uni` ctx_ifcs   pCtxOfMetaModel
        , ctx_ps     = ctx_ps     pCtx 
        , ctx_pops   = ctx_pops   pCtx `uni` ctx_pops   pCtxOfMetaModel
        , ctx_metas  = ctx_metas  pCtx
        }
           where
            uni :: Eq a => [a] -> [a] -> [a]
            uni xs ys = L.nub (xs ++ ys)
    --      userGFSpec :: Guarded FSpec
    --      userGFSpec = 
    --         pCtx2Fspec opts $ 
    --           if useSystemContext opts
    --           then mergeContexts <$> userPlus
    --                              <*> (grind opts sysCModel <$> pCtx2Fspec opts userPlus)
    --           else userP_Ctx
    --        where 
    --         userPlus :: Guarded P_Context
    --         userPlus = addSemanticModel (model sysCModel) <$> userP_Ctx
    --      result :: Guarded FSpecKinds
    --      result = 
    --        if genRapPopulationOnly
    --        then case userGFSpec of 
    --               Errors err -> Errors err  
    --               Checked usrFSpec _
    --                        -> let grinded :: P_Context
    --                               grinded = grind opts fAmpModel usrFSpec -- the user's sourcefile grinded, i.e. a P_Context containing population in terms of formalAmpersand.
    --                               metaPopPCtx :: Guarded P_Context
    --                               metaPopPCtx = mergeContexts grinded <$> fAmpP_Ctx
    --                               metaPopFSpec :: Guarded FSpec
    --                               metaPopFSpec = pCtx2Fspec opts metaPopPCtx
    --                           in FSpecKinds <$> (pCtx2Fspec opts $ mergeContexts <$> userP_CtxPlus <*> pure grinded)
    --                                          <*> (Just <$> metaPopFSpec)
    --        else FSpecKinds <$> userGFSpec <*> pure Nothing
    --  res <- if genMetaFile
    --         then writeMetaFile fAmpModel userGFSpec
    --         else return $ pure ()
    --  return (res >> result)
  -- where
  --   useSystemContext :: Options -> Bool
  --   useSystemContext = genPrototype
  --   writeMetaFile :: (HasOptions env , HasVerbosity env, HasHandles env) => GrindInfo -> Guarded FSpec -> RIO env (Guarded ())
  --   writeMetaFile metaModel userSpec = do
  --      env <- ask
  --      let opts@Options{..} = getOptions env
  --      case makeMetaFile opts metaModel <$> userSpec of
  --       Checked (filePath,metaContents) ws -> 
  --                 do verboseLn $ "Generating meta file in path "++dirOutput
  --                    liftIO $ writeFile (dirOutput </> filePath) metaContents      
  --                    verboseLn $ "\"" ++ filePath ++ "\" written"
  --                    return $ Checked () ws
  --       Errors err -> return (Errors err)

