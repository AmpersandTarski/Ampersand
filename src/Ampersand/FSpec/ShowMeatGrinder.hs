{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.FSpec.ShowMeatGrinder
  ( grind
  , metaModel
  , MetaModel(..)
  )
where

import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Core.ParseTree
-- import           Ampersand.Core.A2P_Converters
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.Transformers
-- import qualified RIO.Set as Set
import qualified RIO.Text as T

data MetaModel = FormalAmpersand | PrototypeContext
       deriving (Eq, Ord, Enum, Bounded, Show)
instance Named MetaModel where
  name FormalAmpersand = "Formal Ampersand"
  name PrototypeContext = "Prototype context"

-- | This produces the metamodel of either
--   "FormalAmpersand" or "PrototypeContext" as defined by their transformers.
metaModel :: MetaModel -> P_Context
metaModel mmLabel = 
  PCtx{ ctx_nm     = "MetaModel"<>T.pack (show mmLabel)
      , ctx_pos    = []
      , ctx_lang   = Nothing
      , ctx_markup = Nothing
      , ctx_pats   = []
      , ctx_rs     = []
      , ctx_ds     = map metarelation (transformers emptyFSpec)
      , ctx_cs     = []
      , ctx_ks     = []
      , ctx_rrules = []
      , ctx_reprs  = []
      , ctx_vs     = []
      , ctx_gs     = []
      , ctx_ifcs   = []
      , ctx_ps     = []
      , ctx_pops   = []
      , ctx_metas  = []
      }
  where
    transformers = case mmLabel of
      FormalAmpersand  -> transformersFormalAmpersand
      PrototypeContext -> transformersPrototypeContext

-- | The 'grind' function lifts a model to the population of a metamodel.
--   The model is "ground" with respect to a metamodel defined in transformersFormalAmpersand,
--   The result is delivered as a P_Context, so it can be merged with other Ampersand results.
grind :: (FSpec -> [Transformer]) -> FSpec -> P_Context
grind transformers userFspec =
  PCtx{ ctx_nm     = "Grinded_"<>name userFspec
      , ctx_pos    = []
      , ctx_lang   = Nothing
      , ctx_markup = Nothing
      , ctx_pats   = []
      , ctx_rs     = []
      , ctx_ds     = map metarelation filtered
      , ctx_cs     = []
      , ctx_ks     = []
      , ctx_rrules = []
      , ctx_reprs  = []
      , ctx_vs     = []
      , ctx_gs     = []
      , ctx_ifcs   = []
      , ctx_ps     = []
      , ctx_pops   = map transformer2pop filtered
      , ctx_metas  = []
      }
  where
    filtered :: [Transformer]
    filtered = filter (not.null.tPairs) . transformers $ userFspec

metarelation :: Transformer -> P_Relation
metarelation  tr =
  P_Relation { dec_nm     = tRel tr
             , dec_sign   = P_Sign (mkPConcept (tSrc tr))
                                   (mkPConcept (tTrg tr))
             , dec_prps   = mults tr 
             , dec_pragma = []
             , dec_Mean   = []
             , pos   = OriginUnknown 
             }

transformer2pop :: Transformer -> P_Population
transformer2pop tr =
  P_RelPopu { p_src  = Nothing
            , p_tgt  = Nothing
            , pos    = OriginUnknown -- TODO trace to origin
            , p_nmdr = PNamedRel 
                 { pos    = OriginUnknown -- TODO trace to origin
                 , p_nrnm = tRel tr
                 , p_mbSign = Just (P_Sign (mkPConcept (tSrc tr))
                                           (mkPConcept (tTrg tr)))
                 }
            , p_popps = tPairs tr
            }