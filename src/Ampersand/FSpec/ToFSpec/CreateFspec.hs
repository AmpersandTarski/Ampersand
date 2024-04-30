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
import Ampersand.FSpec.Instances
import Ampersand.FSpec.MetaModels
import Ampersand.FSpec.ShowMeatGrinder
import Ampersand.FSpec.Transformers
import Ampersand.Input
import Ampersand.Misc.HasClasses
import RIO.List (sortOn)
import qualified RIO.Text as T

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
  (HasTrimXLSXOpts env, HasFSpecGenOpts env, HasLogFunc env) =>
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
    let pContext =
          case recipe of
            Standard -> userScript
            Grind -> do
              checkFormalAmpersandTransformers env =<< formalAmpersandScript
              userScr <- userScript
              userFspc <- pCtx2Fspec env userScr
              return (grind transformersFormalAmpersand userFspc)
            Prototype -> do
              userPCtx <- userScript
              pcScript <- prototypeContextScript
              checkPrototypeContextTransformers env pcScript
              let one = userPCtx `mergeContexts` pcScript
              oneFspec <- pCtx2Fspec env one -- this is done to typecheck the combination
              let two = grind transformersPrototypeContext oneFspec
              return (one `mergeContexts` two)
            RAP -> do
              rapPCtx <- userScript
              faScript <- formalAmpersandScript
              let one = rapPCtx `mergeContexts` metaModel PrototypeContext `mergeContexts` faScript
              oneFspec <- pCtx2Fspec env one -- this is done to typecheck the combination
              let two = grind transformersPrototypeContext oneFspec
              pcScript <- prototypeContextScript
              checkPrototypeContextTransformers env pcScript
              return (one `mergeContexts` two `mergeContexts` pcScript)
    return (pCtx2Fspec env =<< pContext)

-- | make sure that the relations defined in formalampersand.adl are in sync with the transformers of formal ampersand.
checkFormalAmpersandTransformers :: (HasFSpecGenOpts env) => env -> P_Context -> Guarded ()
checkFormalAmpersandTransformers env x =
  case pCtx2Fspec env x of
    Errors err ->
      fatal
        . T.intercalate "\n  "
        $ ["Formal Ampersand script does not compile:"]
          <> T.lines (tshow err)
    Checked fSpecOfx _ -> compareSync (transformersFormalAmpersand fSpecOfx) (instanceList fSpecOfx)

-- | make sure that the relations defined in prototypecontext.adl are in sync with the transformers of prototypecontext.
checkPrototypeContextTransformers :: (HasFSpecGenOpts env) => env -> P_Context -> Guarded ()
checkPrototypeContextTransformers env x =
  case pCtx2Fspec env x of
    Errors err ->
      fatal
        . T.intercalate "\n  "
        $ ["PrototypeContext script does not compile:"]
          <> T.lines (tshow err)
    Checked fSpecOfx _ -> compareSync (transformersFormalAmpersand fSpecOfx) (instanceList fSpecOfx)

compareSync :: [Transformer] -> [Relation] -> Guarded ()
compareSync ts rs = case (filter (not . hasmatchingRel) ts, filter (not . hasmatchingTransformer) rs) of
  ([], []) -> addWarnings theWarnings $ pure ()
  (ts', rs') ->
    fatal . T.intercalate "\n  " $
      [ "Error: There are one or more unmatched relations and transformers that are not in sync.",
        "Please report this to the AmpersandTarski team at",
        "https://github.com/AmpersandTarski/Ampersand/issues/new/choose",
        ""
      ]
        <> (snd <$> sortOn fst errorItems)
    where
      errorItems =
        [ (src <> tgt <> nm, nm <> "[" <> src <> "*" <> tgt <> "] is in transformers.hs, but not in .adl")
          | Transformer nm src tgt _ _ <- ts'
        ]
          <> [ (tshow (source rel) <> tshow (target rel) <> name rel, showRel rel <> " is in " <> tshow (origin rel) <> ", but not in transformers.hs")
               | rel <- rs'
             ]
  where
    hasmatchingRel :: Transformer -> Bool
    hasmatchingRel t = any (isMatch t) rs
    hasmatchingTransformer :: Relation -> Bool
    hasmatchingTransformer rel = any (`isMatch` rel) ts
    isMatch :: Transformer -> Relation -> Bool
    isMatch (Transformer nm src tgt _ _) rel =
      name rel == nm
        && name (source rel) == src
        && name (target rel) == tgt
    matches :: [Transformer] -> [(Transformer, Relation)]
    matches ts' = case ts' of
      [] -> []
      h : tl -> (h, rel) : matches tl
        where
          rel = case filter (isMatch h) rs of
            [] -> fatal "This should not be possible, because this case is caught as a fatel in `compareSync`."
            re : _ -> re
    theWarnings = concatMap foo (matches ts)
      where
        foo :: (Transformer, Relation) -> [Warning]
        foo (Transformer _ _ _ ps _, r) = mkUnmatchedPropertiesWarning MeatGrinder r ps