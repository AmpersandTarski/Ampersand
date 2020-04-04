{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.FSpec.MetaModels
  ( MetaModel(..)
  , mkGrindInfo
  , GrindInfo
  , grind
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
import           Ampersand.Misc.HasClasses
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

parser :: (HasLogFunc env, HasFSpecGenOpts env) => MetaModel -> RIO env (Guarded P_Context)
parser FormalAmpersand = parseFormalAmpersand
parser PrototypeContext   = parsePrototypeContext 

pCtx2Fspec :: (HasFSpecGenOpts env) => env -> P_Context -> Guarded FSpec
pCtx2Fspec env c = do
    fSpec <- makeFSpec env <$> pCtx2aCtx env c
    checkInvariants fSpec
  where 
    checkInvariants :: FSpec -> Guarded FSpec
    checkInvariants fSpec =
      if view allowInvariantViolationsL env 
        then 
          pure fSpec
        else 
          case violationsOfInvariants fSpec of
            [] -> pure fSpec
            h:tl -> Errors $ fmap mkInvariantViolationsError $ h NE.:| tl




mkGrindInfo :: (HasFSpecGenOpts env, HasLogFunc env) => MetaModel -> RIO env GrindInfo
mkGrindInfo metamodel = do
    env <- ask 
    build env <$> parser metamodel
  where
    build :: (HasFSpecGenOpts env) =>
        env -> Guarded P_Context -> GrindInfo
    build env pCtx = GrindInfo
            { metaModel    = metamodel
            , pModel       = case pCtx of
                  Errors errs -> fatal $ showWithHeader
                          ("The ADL scripts of "<>name metamodel<>" cannot be parsed:") errs
                  Checked x [] -> x
                  Checked _ (h:tl) -> fatal $ showWithHeader
                          ("The ADL scripts of "<>name metamodel<>" are not free of warnings:") (h NE.:|tl)
            , fModel       = 
                case join $ pCtx2Fspec env <$> pCtx of
                  Errors errs -> fatal $ showWithHeader
                          ("The ADL scripts of "<>name metamodel<>" cannot be parsed:") errs
                  Checked x [] -> x
                  Checked _ (h:tl) -> fatal $ showWithHeader
                          ("The ADL scripts of "<>name metamodel<>" are not free of warnings:") (h NE.:|tl)
            , transformers = case metamodel of
                                FormalAmpersand  -> transformersFormalAmpersand
                                PrototypeContext -> transformersPrototypeContext
            }
      where showWithHeader :: Show a => Text -> NE.NonEmpty a -> Text
            showWithHeader txt xs = T.intercalate "\n" $ txt : (map tshow . NE.toList $ xs)
