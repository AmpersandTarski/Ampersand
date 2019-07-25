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

parser :: (HasLogFunc env, HasExcellOutputOptions env) => MetaModel -> RIO env (Guarded P_Context)
parser FormalAmpersand = parseFormalAmpersand
parser SystemContext   = parseSystemContext 
parser FADocumented    = parseFormalAmpersandDocumented

pCtx2Fspec :: (HasSqlBinTables env, HasDefaultCrud env, HasGenInterfaces env, HasSqlBinTables env, HasNamespace env, HasOutputLanguage env) => env -> P_Context -> Guarded FSpec
pCtx2Fspec env c = makeFSpec env <$> pCtx2aCtx env c


mkGrindInfo :: (HasOutputLanguage env, HasNamespace env, HasSqlBinTables env, HasGenInterfaces env, HasDefaultCrud env, HasLogFunc env, HasExcellOutputOptions env) => MetaModel -> RIO env GrindInfo
mkGrindInfo metamodel = do
    env <- ask 
    c <- parser metamodel
    return $ build env c 
  where
    build :: (HasDefaultCrud env, HasGenInterfaces env, HasSqlBinTables env, HasNamespace env, HasOutputLanguage env) =>
        env -> Guarded P_Context -> GrindInfo
    build env pCtx = GrindInfo
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
                case join $ pCtx2Fspec env <$> pCtx of
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


