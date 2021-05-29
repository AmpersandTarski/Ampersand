{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.FSpec.ToFSpec.CreateFspec
  ( Recipe(..)
--  , BuildStep(Grind)
--  , StartContext(..)
--  , MetaModel(..)
  , createFspec 
--  , script
--  , merge
--  , andThen
  )

where
import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.MetaModels
import           Ampersand.FSpec.ShowMeatGrinder
import           Ampersand.Input
import           Ampersand.Misc.HasClasses

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

createFspec :: (HasFSpecGenOpts env, HasLogFunc env) => 
               Recipe -> RIO env (Guarded FSpec)
createFspec recipe = do 
    env <- ask
    userScript :: Guarded P_Context <- do
         rootFile <- fromMaybe (fatal "No script was given!") <$> view rootFileL
         snd <$> parseFileTransitive rootFile -- the P_Context of the user's sourceFile
    return (cook env recipe userScript)

cook :: (HasFSpecGenOpts env) => 
         env
      -> Recipe
      -> Guarded P_Context
      -> Guarded FSpec
cook env recipe userScript
     = case recipe of
         Standard  -> userGFspec
         Atlas     -> pCtx2Fspec env =<< liftM2 mergeContexts userMetamodel emptyMetaModel
         Prototype -> pCtx2Fspec env =<< liftM2 mergeContexts userMetamodel prototypeMetamodel
       where
       -- typecheck and check the user's script
          userGFspec = pCtx2Fspec env =<< userScript
       -- The following definitions are only used in the atlas.
       -- make a metamodel by grinding
          userMetamodel = grind <$!> userGFspec
       -- create FormalAmpersand without population. Just the metamodel used by the transformers.
          emptyMetaModel = metaModelTransformers <$!> userGFspec
       -- create FormalAmpersand without population. Just the metamodel used by the transformers.
          prototypeMetamodel = metaModelPrototypeContext <$!> userGFspec