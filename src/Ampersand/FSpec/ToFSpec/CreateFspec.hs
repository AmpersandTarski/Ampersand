{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ampersand.FSpec.ToFSpec.CreateFspec
  ( Recipe (..),
    --  , BuildStep(Grind)
    --  , StartContext(..)
    --  , MetaModel(..)
    createFspec,
    --  , script
    --  , merge
    --  , andThen
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.MetaModels
import Ampersand.FSpec.ShowMeatGrinder
import Ampersand.FSpec.Transformers
import Ampersand.Input
import Ampersand.Misc.HasClasses

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

--   Explanation Grind:
--     The "Grind" option is used for RAP because the "grinded" user script lands in the RAP-database.
--     The function pCtx2Fspec checks the user script for type errors, yielding 'userFspc'.
--     Grinding provides a minimal metamodel, which is devoid of empty relations.

--   Explanation RAP
--     The entire FormalAmpersand metamodel is called 'faScript' in the code below.
--     It is merged into the result so the RAP-database can hold the grinded results of any valid Ampersand script.
--     The metamodel FormalAmpersand is also incorporated in the database, so
--     RAP can populate this metamodel with grinded user scripts for showing an Atlas.
--     It also incorporates PrototypeContext because RAP is a running application.

--   Explanation Prototype
--     The "Prototype" option is used to generate prototypes. It combines the user script
--     with some meta concepts and relations from the prototype context. Furthermore a few management interfaces are added.
--     That is why 'one' is the combination of the user script (uCtx) and the prototype context (pCtx).
--     NOTE! different than with the RAP recipe, we don't want the metaModel of the PrototypeContext, but the context itself
--     Next step is to grind 'one' using the PrototypeContext transformers. That will give us the meta-population in 'two'
--     We return the combination of 'one' and 'two'
--     The compiler typechecks the combination because a user might inadvertedly use concepts from the prototype context.
--     In that case he is in for a suprise, but at least the system does not land on its back.
createFspec ::
  (HasFSpecGenOpts env, HasLogFunc env) =>
  RIO env (Guarded FSpec)
createFspec =
  do
    env <- ask
    let recipe = view recipeL env
    userScript <- do
      rootFiles <- view rootFileL
      snd <$> parseFilesTransitive rootFiles -- the P_Context of the user's sourceFile
    formalAmpersandScript <- parseFormalAmpersand
    prototypeContextScript <- parsePrototypeContext
    let p_context =
          case recipe of
            Standard -> userScript
            Grind -> do
              userScr <- userScript
              userFspc <- pCtx2Fspec env userScr
              return (grind nameSpaceFormalAmpersand transformersFormalAmpersand userFspc)
            Prototype -> do
              userPCtx <- userScript
              pcScript <- prototypeContextScript
              let one = userPCtx `mergeContexts` pcScript
              oneFspec <- pCtx2Fspec env one -- this is done to typecheck the combination
              let two = grind nameSpacePrototypeContext transformersPrototypeContext oneFspec
              return (one `mergeContexts` two)
            RAP -> do
              rapPCtx <- userScript
              faScript <- formalAmpersandScript
              let one = rapPCtx `mergeContexts` metaModel PrototypeContext `mergeContexts` faScript
              oneFspec <- pCtx2Fspec env one -- this is done to typecheck the combination
              let two = grind nameSpacePrototypeContext transformersPrototypeContext oneFspec
              pcScript <- prototypeContextScript
              return (one `mergeContexts` two `mergeContexts` pcScript)
    return (pCtx2Fspec env =<< p_context)
