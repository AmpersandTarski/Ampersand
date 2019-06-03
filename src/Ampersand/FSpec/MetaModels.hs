{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.FSpec.MetaModels
  ( Grinder(..)
  , grindWith
  , addSemanticModel)

where
import           Ampersand.ADL1
import           Ampersand.ADL1.P2A_Converters
import           Ampersand.Basics
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ShowMeatGrinder
import           Ampersand.FSpec.ToFSpec.ADL2FSpec
import           Ampersand.FSpec.Transformers 
import           Ampersand.Input
import           Ampersand.Core.A2P_Converters
import           Ampersand.Misc
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NEL
import qualified RIO.Set as Set

data Grinder = FormalAmpersand | FADocumented | SystemContext
instance Named Grinder where
  name FormalAmpersand = "Formal Ampersand"
  name FADocumented    = "Formal Ampersand (documented)"
  name SystemContext   = "System context"

parser :: Grinder -> RIO App (Guarded P_Context)
parser FormalAmpersand = parseFormalAmpersand
parser SystemContext   = parseSystemContext 
parser FADocumented    = parseFormalAmpersandDocumented

grindWith :: Grinder -> FSpec -> RIO App (Guarded P_Context)
grindWith grinder userSpec = do
    pCtx <- parser grinder
    env <- ask
    let opts = getOptions env
        gMetaModel = pCtx >>= mkModel opts grinder
    case gMetaModel of 
        Errors errs -> fatal . unlines $
                 ("The ADL scripts of "++name grinder++" cannot be parsed:")
               : (L.intersperse (replicate 30 '=') . fmap show . NEL.toList $ errs)
        Checked metaModel [] -> do
               let result = pure $ grind opts metaModel userSpec
               pure result
        Checked _         ws -> fatal . unlines $
                 ("The ADL scripts of "++name grinder++" cannot be parsed:")
               : (L.intersperse (replicate 30 '=') . fmap show $ ws)

pCtx2Fspec :: Options -> P_Context -> Guarded FSpec
pCtx2Fspec opts c = makeFSpec opts <$> pCtx2aCtx opts c


mkModel :: Options -> Grinder -> P_Context -> Guarded MetaModel
mkModel opts grinder c = build <$> pCtx2Fspec opts c
  where
    build :: FSpec -> MetaModel
    build fSpec = MetaModel
            { metaModelName = name grinder
            , fModel        = fSpec
            , transformers  = case grinder of
                                FormalAmpersand -> transformersFormalAmpersand
                                FADocumented    -> transformersFormalAmpersand
                                SystemContext   -> transformersSystemContext
            }


-- | When the semantic model of a metamodel is added to the user's model, we add
--   the relations as wel as the generalisations to it, so they are available to the user
--   in an implicit way. We want other things, like Idents, Views and REPRESENTs available too.
addSemanticModel :: Options -> Grinder -> P_Context -> P_Context
addSemanticModel opts grinder pCtx =
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
            metamodel = case mkModel opts grinder pCtx of
               Errors err -> fatal $ "Should not be possible. \n"++ show err 
               Checked mm _ -> fModel mm
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
    --      result :: Guarded MultiFSpecs
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
    --                           in MultiFSpecs <$> (pCtx2Fspec opts $ mergeContexts <$> userP_CtxPlus <*> pure grinded)
    --                                          <*> (Just <$> metaPopFSpec)
    --        else MultiFSpecs <$> userGFSpec <*> pure Nothing
    --  res <- if genMetaFile
    --         then writeMetaFile fAmpModel userGFSpec
    --         else return $ pure ()
    --  return (res >> result)
  -- where
  --   useSystemContext :: Options -> Bool
  --   useSystemContext = genPrototype
  --   writeMetaFile :: (HasOptions env , HasVerbosity env, HasHandles env) => MetaModel -> Guarded FSpec -> RIO env (Guarded ())
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

