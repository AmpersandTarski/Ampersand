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
import Ampersand.Core.A2P_Converters (aProps2Pprops, aRelation2pRelation)
import Ampersand.Core.ParseTree (mkPConcept)
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Instances
import Ampersand.FSpec.ToFSpec.ADL2FSpec (makeFSpec)
import Ampersand.FSpec.Transformers
import Ampersand.Input
import Ampersand.Misc.HasClasses
import RIO.List (sortOn)
import qualified RIO.NonEmpty as NE
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
    pContext <-
      case recipe of
        Standard -> pure userScript
        Grind -> do
          let userFspc = do
                faScript <- formalAmpersandScript
                checkFormalAmpersandTransformers env faScript
                userScr <- userScript
                pCtx2Fspec env userScr
          grindInto FormalAmpersand userFspc
        Prototype -> do
          let oneFspec = do
                userPCtx <- userScript
                pcScript <- prototypeContextScript
                -- checkPrototypeContextTransformers env pcScript
                pCtx2Fspec env (userPCtx `mergeContexts` pcScript) -- this is done to typecheck the combination
          grindInto PrototypeContext oneFspec
        RAP -> do
          let guardedOne = do
                rapPCtx <- userScript
                faScript <- formalAmpersandScript
                return $ rapPCtx `mergeContexts` metaModel PrototypeContext `mergeContexts` faScript
          let oneFspec = pCtx2Fspec env =<< guardedOne -- this is done to typecheck the combination
          guardedTwo <- grindInto PrototypeContext oneFspec
          pure $ do
            pcScript <- prototypeContextScript
            one <- guardedOne
            two <- guardedTwo
            -- checkPrototypeContextTransformers env pcScript
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
    Checked fSpecOfx ws -> addWarnings ws $ compareSync (transformersFormalAmpersand fSpecOfx) (instanceList fSpecOfx)

-- | make sure that the relations defined in prototypecontext.adl are in sync with the transformers of prototypecontext.
_checkPrototypeContextTransformers :: (HasFSpecGenOpts env) => env -> P_Context -> Guarded ()
_checkPrototypeContextTransformers env x =
  case pCtx2Fspec env x of
    Errors err ->
      fatal
        . T.intercalate "\n  "
        $ ["PrototypeContext script does not compile:"]
          <> T.lines (tshow err)
    Checked fSpecOfx ws -> addWarnings ws $ compareSync (transformersFormalAmpersand fSpecOfx) (instanceList fSpecOfx)

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

data MetaModel = FormalAmpersand | PrototypeContext
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Named MetaModel where
  name FormalAmpersand = "Formal Ampersand"
  name PrototypeContext = "Prototype context"

-- | This produces the metamodel of either
--   "FormalAmpersand" or "PrototypeContext" as defined by their transformers.
metaModel :: MetaModel -> P_Context
metaModel mmLabel =
  PCtx
    { ctx_nm = "MetaModel" <> T.pack (show mmLabel),
      ctx_pos = [],
      ctx_lang = Nothing,
      ctx_markup = Nothing,
      ctx_pats = [],
      ctx_rs = [],
      ctx_ds = map metarelation (transformers emptyFSpec),
      ctx_cs = [],
      ctx_ks = [],
      ctx_rrules = [],
      ctx_reprs = [],
      ctx_vs = [],
      ctx_gs = [],
      ctx_ifcs = [],
      ctx_ps = [],
      ctx_pops = [],
      ctx_metas = [],
      ctx_enfs = []
    }
  where
    transformers = case mmLabel of
      FormalAmpersand -> transformersFormalAmpersand
      PrototypeContext -> transformersPrototypeContext

metarelation :: Transformer -> P_Relation
metarelation tr =
  P_Relation
    { dec_nm = tRel tr,
      dec_sign =
        P_Sign
          (mkPConcept (tSrc tr))
          (mkPConcept (tTrg tr)),
      dec_prps = aProps2Pprops $ mults tr,
      dec_defaults = [],
      dec_pragma = Nothing,
      dec_Mean = [],
      pos = MeatGrinder
    }

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
grindInto :: (HasTrimXLSXOpts env, HasLogFunc env, HasFSpecGenOpts env) => MetaModel -> Guarded FSpec -> RIO env (Guarded P_Context)
grindInto metamodel specification = do
  fSpecOfMetaModel <- getFSpecForMetaModel metamodel
  let pCtx = do
        let transformers =
              ( case metamodel of
                  FormalAmpersand -> transformersFormalAmpersand <$> specification
                  PrototypeContext -> transformersPrototypeContext <$> specification
              )
        filtered <- filter (not . null . tPairs) <$> transformers
        fSpecOfMetaModel' <- fSpecOfMetaModel
        specName <- name <$> specification
        pure
          PCtx
            { ctx_nm = "Grinded_" <> specName,
              ctx_pos = [],
              ctx_lang = Nothing,
              ctx_markup = Nothing,
              ctx_pats = [],
              ctx_rs = [],
              ctx_ds = aRelation2pRelation <$> instanceList fSpecOfMetaModel',
              ctx_cs = [],
              ctx_ks = [],
              ctx_rrules = [],
              ctx_reprs = reprList . fcontextInfo $ fSpecOfMetaModel',
              ctx_vs = [],
              ctx_gs = [],
              ctx_ifcs = [],
              ctx_ps = [],
              ctx_pops = map transformer2pop filtered,
              ctx_metas = [],
              ctx_enfs = []
            }
  return pCtx

getFSpecForMetaModel :: (HasTrimXLSXOpts env, HasLogFunc env, HasFSpecGenOpts env) => MetaModel -> RIO env (Guarded FSpec)
getFSpecForMetaModel metamodel = do
  env <- ask
  pStructOfMetaModel <-
    ( case metamodel of
        FormalAmpersand -> parseFormalAmpersand
        PrototypeContext -> parsePrototypeContext
      )
  return (pStructOfMetaModel >>= pCtx2Fspec env)

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
