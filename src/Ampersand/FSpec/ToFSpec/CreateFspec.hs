{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ampersand.FSpec.ToFSpec.CreateFspec
  ( Recipe (..),
    --  , BuildStep(Grind)
    --  , StartContext(..)
    --  , MetaModel(..)
    createFspec,
    pCtx2Fspec,
    --  , script
    --  , merge
    --  , andThen
  )
where

import Ampersand.ADL1
import Ampersand.ADL1.P2A_Converters (pCtx2aCtx)
import Ampersand.Basics
import Ampersand.Core.A2P_Converters (aRelation2pRelation)
import Ampersand.Core.ParseTree (mkPConcept)
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Instances
import Ampersand.FSpec.ToFSpec.ADL2FSpec (makeFSpec)
import Ampersand.FSpec.Transformers
import Ampersand.Input
import Ampersand.Misc.HasClasses
import Ampersand.Types.Config (HasRunner)
import RIO.List (sortOn)
import qualified RIO.NonEmpty as NE
import qualified RIO.Partial as Partial
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
  (HasDirOutput env, HasTrimXLSXOpts env, HasFSpecGenOpts env, HasRunner env) =>
  RIO env (Guarded FSpec)
createFspec =
  do
    env <- ask
    let recipe = view recipeL env
    userScript <- do
      rootFiles <- view rootFileL
      snd <$> parseFilesTransitive rootFiles -- the P_Context of the user's sourceFile
    pContext <-
      case recipe of
        Standard -> pure userScript
        Grind -> do
          formalAmpersandScript <- parseFormalAmpersand
          let userFspc = do
                faScript <- formalAmpersandScript
                checkFormalAmpersandTransformers env faScript
                userScr <- userScript
                pCtx2Fspec env userScr
          grindInto FormalAmpersand userFspc
        Prototype -> do
          prototypeContextScript <- parsePrototypeContext
          let guardedOne = do
                userPCtx <- userScript
                pcScript <- prototypeContextScript
                checkPrototypeContextTransformers env pcScript
                pure $ userPCtx `mergeContexts` pcScript -- this is done to typecheck the combination
          let oneFspec = pCtx2Fspec env =<< guardedOne -- this is done to typecheck the combination
          guardedTwo <- grindInto PrototypeContext oneFspec
          pure $ do
            one <- guardedOne
            two <- guardedTwo
            pure (one `mergeContexts` two)
        RAP -> do
          -- combine userscript, formalAmpersand and prototypeContext
          formalAmpersandScript <- parseFormalAmpersand
          prototypeContextScript <- parsePrototypeContext
          let guardedOne = do
                rapPCtx <- userScript
                faScript <- formalAmpersandScript
                pcScript <- prototypeContextScript
                pure $ rapPCtx `mergeContexts` pcScript `mergeContexts` faScript
          let oneFspec = pCtx2Fspec env =<< guardedOne -- this is done to typecheck the combination
          -- build a prototype with the combination
          guardedTwo <- grindInto PrototypeContext oneFspec
          pure $ do
            pcScript <- prototypeContextScript
            one <- guardedOne
            two <- guardedTwo
            checkPrototypeContextTransformers env pcScript
            pure (one `mergeContexts` two `mergeContexts` pcScript)
    pure (pCtx2Fspec env =<< pContext)

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
        $ ["The relations defined in prototypecontext.adl are not in sync with the transformers of prototypecontext:"]
        <> T.lines (tshow err)
    Checked fSpecOfx _ -> compareSync (transformersPrototypeContext fSpecOfx) (instanceList fSpecOfx)

compareSync :: [Transformer] -> [Relation] -> Guarded ()
compareSync ts rs = case (filter (not . hasmatchingRel) ts, filter (not . hasmatchingTransformer) rs) of
  ([], []) -> pure ()
  (ts', rs') ->
    fatal
      . T.intercalate "\n  "
      $ [ "Error: There are one or more unmatched relations and transformers that are not in sync.",
          "Please report this to the AmpersandTarski team at",
          "https://github.com/AmpersandTarski/Ampersand/issues/new/choose",
          ""
        ]
      <> (snd <$> sortOn fst errorItems)
    where
      errorItems :: [(Text, Text)]
      errorItems =
        [ (tshow src <> tshow tgt <> tshow nm, tshow nm <> "[" <> tshow src <> "*" <> tshow tgt <> "] is in transformers.hs, but not in .adl")
          | Transformer nm src tgt _ <- ts'
        ]
          <> [ (tshow (source rel) <> tshow (target rel) <> tshow rel, tshow rel <> " is in " <> tshow (origin rel) <> ", but not in transformers.hs")
               | rel <- rs'
             ]
  where
    hasmatchingRel :: Transformer -> Bool
    hasmatchingRel t = any (isMatch t) rs
    hasmatchingTransformer :: Relation -> Bool
    hasmatchingTransformer rel = any (`isMatch` rel) ts
    isMatch :: Transformer -> Relation -> Bool
    isMatch (Transformer nm src tgt _) rel =
      name rel
        == nm
        && name (source rel)
        == src
        && name (target rel)
        == tgt

data MetaModel = FormalAmpersand | PrototypeContext
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Named MetaModel where
  name FormalAmpersand = mkName ContextName (Partial.fromJust (toNamePart "Formal Ampersand") NE.:| [])
  name PrototypeContext = mkName ContextName (Partial.fromJust (toNamePart "Prototype context") NE.:| [])

transformer2pop :: Transformer -> P_Population
transformer2pop tr =
  P_RelPopu
    { p_src = Nothing {- of moet dit zijn: Just (mkPConcept (tSrc tr)) ?? -},
      p_tgt = Nothing {- of moet dit zijn: Just (mkPConcept (tTrg tr)) ?? -},
      pos = MeatGrinder, -- TODO trace to origin
      p_nmdr =
        PNamedRel
          { pos = MeatGrinder, -- TODO trace to origin
            p_nrnm = tRel tr,
            p_mbSign =
              Just
                ( P_Sign
                    (mkPConcept (tSrc tr))
                    (mkPConcept (tTrg tr))
                )
          },
      p_popps = tPairs tr
    }

-- | The 'grindInto' function lifts a model to the population of a metamodel.
--   The model is "ground" with respect to a metamodel defined in transformersFormalAmpersand,
--   The result is delivered as a (Guarded) P_Context, so it can be merged with other Ampersand results.
grindInto :: (HasDirOutput env, HasTrimXLSXOpts env, HasRunner env, HasFSpecGenOpts env) => MetaModel -> Guarded FSpec -> RIO env (Guarded P_Context)
grindInto metamodel specification = do
  env <- ask
  pContextOfMetaModel <- case metamodel of
    FormalAmpersand -> parseFormalAmpersand
    PrototypeContext -> parsePrototypeContext
  let pCtx = do
        let transformers =
              ( case metamodel of
                  FormalAmpersand -> transformersFormalAmpersand <$> specification
                  PrototypeContext -> transformersPrototypeContext <$> specification
              )
        filtered <- filter (not . null . tPairs) <$> transformers
        guardedFSpecOfMetaModel <- pCtx2Fspec env <$> pContextOfMetaModel
        fSpecOfMetaModel <- guardedFSpecOfMetaModel
        specName <- name <$> specification
        pure
          PCtx
            { ctx_nm = prependToPlainName "Grinded_" specName,
              ctx_lbl = Nothing,
              ctx_pos = [],
              ctx_lang = Nothing,
              ctx_markup = Nothing,
              ctx_pats = [],
              ctx_rs = [],
              ctx_ds = aRelation2pRelation <$> instanceList fSpecOfMetaModel,
              ctx_cs = [],
              ctx_ks = [],
              ctx_rrules = [],
              ctx_reprs = reprList . fcontextInfo $ fSpecOfMetaModel,
              ctx_vs = [],
              ctx_gs = [],
              ctx_ifcs = [],
              ctx_ps = [],
              ctx_pops = map transformer2pop filtered,
              ctx_metas = [],
              ctx_enfs = []
            }
  return pCtx

pCtx2Fspec :: (HasFSpecGenOpts env) => env -> P_Context -> Guarded FSpec
pCtx2Fspec env c = do
  fSpec <- makeFSpec env <$> pCtx2aCtx env c
  checkInvariants fSpec
  where
    checkInvariants :: FSpec -> Guarded FSpec
    checkInvariants fSpec =
      if view allowInvariantViolationsL env
        then pure fSpec
        else case violationsOfInvariants fSpec of
          [] -> pure fSpec
          h : tl -> Errors (fmap (mkInvariantViolationsError (applyViolText fSpec)) (h NE.:| tl))
