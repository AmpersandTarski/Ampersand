{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ampersand.FSpec.ShowMeatGrinder
  ( grind,
    metaModel,
    MetaModel (..),
  )
where

import Ampersand.ADL1
import Ampersand.Basics hiding (Name)
import qualified Ampersand.Basics as AB (Name)
import Ampersand.Core.A2P_Converters
import Ampersand.Core.ParseTree
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Transformers
import qualified RIO.NonEmpty as NE

-- import qualified RIO.Set as Set

data MetaModel = FormalAmpersand | PrototypeContext
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | This produces the metamodel of either
--   "FormalAmpersand" or "PrototypeContext" as defined by their transformers.
metaModel :: MetaModel -> P_Context
metaModel mmLabel =
  PCtx
    { ctx_nm = modelName,
      ctx_lbl = Nothing,
      ctx_pos = [],
      ctx_lang = Nothing,
      ctx_markup = Nothing,
      ctx_pats = [],
      ctx_rs = [],
      ctx_ds = map metarelation transformers,
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
    modelName :: AB.Name
    modelName =
      withNameSpace nameSpace
        . mkName ContextName
        $ ( case toNamePart ("MetaModel_" <> tshow mmLabel) of
              Nothing -> fatal "Not a proper NamePart."
              Just np -> np NE.:| []
          )
    transformers = case mmLabel of
      FormalAmpersand -> transformersFormalAmpersand . emptyFSpec $ modelName
      PrototypeContext -> transformersPrototypeContext . emptyFSpec $ modelName
    nameSpace = case mmLabel of
      FormalAmpersand -> nameSpaceFormalAmpersand
      PrototypeContext -> nameSpacePrototypeContext

-- | The 'grind' function lifts a model to the population of a metamodel.
--   The model is "ground" with respect to a metamodel defined in transformersFormalAmpersand,
--   The result is delivered as a P_Context, so it can be merged with other Ampersand results.
grind :: NameSpace -> (FSpec -> [Transformer]) -> FSpec -> P_Context
grind ns fun userFspec =
  PCtx
    { ctx_nm = prependToPlainName "Grinded_" $ withNameSpace ns . mkName ContextName $ plainNameOf1 userFspec NE.:| [],
      ctx_lbl = Nothing,
      ctx_pos = [],
      ctx_lang = Nothing,
      ctx_markup = Nothing,
      ctx_pats = [],
      ctx_rs = [],
      ctx_ds = map metarelation filtered,
      ctx_cs = [],
      ctx_ks = [],
      ctx_rrules = [],
      ctx_reprs = [],
      ctx_vs = [],
      ctx_gs = [],
      ctx_ifcs = [],
      ctx_ps = [],
      ctx_pops = map transformer2pop filtered,
      ctx_metas = [],
      ctx_enfs = []
    }
  where
    transformers = fun userFspec
    filtered :: [Transformer]
    filtered = filter (not . null . tPairs) transformers

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
      pos = OriginUnknown
    }

transformer2pop :: Transformer -> P_Population
transformer2pop tr =
  P_RelPopu
    { p_src = Nothing,
      p_tgt = Nothing,
      pos = OriginUnknown, -- TODO trace to origin
      p_nmdr =
        PNamedRel
          { pos = OriginUnknown, -- TODO trace to origin
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
