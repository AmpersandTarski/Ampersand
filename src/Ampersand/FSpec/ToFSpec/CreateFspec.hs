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
import           Ampersand.FSpec.Transformers
import           Ampersand.FSpec.ShowMeatGrinder
import           Ampersand.Input
import           Ampersand.Misc.HasClasses

-- | creating an FSpec is based on command-line options.
--   It follows a recipe for translating a P_Context (the parsed user script) into an FSpec (the type-checked and enriched result).
--   Ampersand parses its script (userScript) and adds semantics to it, depending on the recipe.
--   Currently, two semantic interpretations are available: FormalAmpersand and PrototypeContext.
--   (There may be more in the future.)
--   There are two relevant operations to take notice of to understand this code:
--    * mergeContexts :: P_Context -> P_Context -> P_Context
--      To merge two contexts means to take the union of the respective sets in the context.
--      mergeContexts behaves almost like a union operator.
--      Its result is a P_Context because the result must be typechecked as a whole.
--    * grind transformers :: FSpec -> P_Context
--      Grinding means to analyse the script down to the binary relations that constitute the metamodel.
--      It assembles these relations to form a valid P_Context.

createFspec :: (HasFSpecGenOpts env, HasLogFunc env) => 
               Recipe -> RIO env (Guarded FSpec)
createFspec recipe =
 do env <- ask
    userScript <- do
         rootFile <- fromMaybe (fatal "No script was given!") <$> view rootFileL
         snd <$> parseFileTransitive rootFile -- the P_Context of the user's sourceFile
    formalAmpersandScript  <- parseFormalAmpersand
    prototypeContextScript <- parsePrototypeContext
    let pContext
          = case recipe of
              Standard  -> userScript
              Atlas     -> do userPCtx  <- userScript
                              let one    = userPCtx `mergeContexts` metaModel FormalAmpersand
                              oneFspec  <- pCtx2Fspec env one
                              let two  = grind transformersFormalAmpersand oneFspec
                              faScript  <- formalAmpersandScript
                              return (one `mergeContexts` two `mergeContexts` faScript)
              Prototype -> do userPCtx  <- userScript
                              let one    = userPCtx `mergeContexts` metaModel PrototypeContext
                              oneFspec  <- pCtx2Fspec env one
                              let two    = grind transformersPrototypeContext oneFspec
                              pcScript  <- prototypeContextScript
                              return (one `mergeContexts` two `mergeContexts` pcScript)
    return (pCtx2Fspec env =<< pContext)
