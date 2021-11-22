{-# LANGUAGE ScopedTypeVariables #-}

module Ampersand.FSpec.MetaModels
  ( MetaModel(..)
  , pCtx2Fspec
  )

where
import           Ampersand.ADL1
import           Ampersand.ADL1.P2A_Converters
import           Ampersand.Basics
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ShowMeatGrinder
import           Ampersand.FSpec.ToFSpec.ADL2FSpec
import           Ampersand.Input
import           Ampersand.Misc.HasClasses
import qualified RIO.NonEmpty as NE

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
            h:tl -> Errors (fmap (mkInvariantViolationsError (applyViolText fSpec)) (h NE.:| tl))
