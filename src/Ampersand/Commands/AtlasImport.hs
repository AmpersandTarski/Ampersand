{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Generate a configuration file for a new project.
module Ampersand.Commands.AtlasImport
  ( atlasImport,
    InitOpts (..),
    HasInitOpts (..),
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree hiding (Object)
import Ampersand.Core.ShowPStruct
import Ampersand.Misc.HasClasses
import Ampersand.Types.Config
import Data.Aeson
import Data.Aeson.Types
import qualified RIO.ByteString.Lazy as B
import qualified RIO.Text as T

-- | Read a file containing the population of an Atlas.
atlasImport ::
  (HasOutputFile env, HasImportFile env, HasRunner env) =>
  RIO env ()
atlasImport = do
  env <- ask
  content <- liftIO $ B.readFile (view importFileL env)
  -- Get JSON data and decode it
  let result = myDecode content
  case result of
    Left msg -> fatal . T.pack $ "Couldn't read " <> view importFileL env <> ": " <> msg
    Right x -> do
      let outputFn = view outputfileL env
      writeFileUtf8 outputFn (showP x)
      logInfo . display . T.pack $ outputFn <> " written"

myDecode :: B.ByteString -> Either String P_Relation
myDecode = eitherDecode

instance FromJSON P_Concept where
  parseJSON :: Value -> Parser P_Concept
  parseJSON (Object v) =
    PCpt <$> v .: "concept"
  parseJSON (String txt) =
    pure (PCpt txt)
  parseJSON invalid =
    prependFailure
      "parsing P_Concept failed, "
      (typeMismatch "Object or String" invalid)

instance FromJSON PProp where
  parseJSON val = case val of
    String x -> case T.toLower x of
      "uni" -> pure P_Uni
      "inj" -> pure P_Inj
      "sur" -> pure P_Sur
      "tot" -> pure P_Tot
      "sym" -> pure P_Sym
      "asy" -> pure P_Asy
      "trn" -> pure P_Trn
      "rfx" -> pure P_Rfx
      "irf" -> pure P_Irf
      "prop" -> pure P_Prop
      _ ->
        unexpected val
    invalid ->
      prependFailure
        "parsing PProp failed, "
        (typeMismatch "String" invalid)

instance FromJSON P_Sign where
  parseJSON val = case val of
    (Object v) ->
      P_Sign <$> v .: "source"
        <*> v .: "target"
    invalid ->
      prependFailure
        "parsing P_Sign failed, "
        (typeMismatch "Object" invalid)

instance FromJSON P_Relation where
  parseJSON val = case val of
    Object v ->
      build <$> v .: "name"
        <*> v .: "sign"
        <*> v .: "properties"
    invalid ->
      prependFailure
        "parsing P_Sign failed, "
        (typeMismatch "Object" invalid)
    where
      build :: Text -> P_Sign -> PProps -> P_Relation
      build nm sig prps =
        P_Relation
          { dec_sign = sig,
            dec_prps = prps,
            dec_pragma = Nothing,
            dec_nm = nm,
            dec_defaults = [],
            dec_Mean = [],
            pos = OriginAtlas
          }

-- parseContext :: B.ByteString -> Guarded P_Context
-- parseContext contents = pure ctx
--   where
--     ctx =
--       PCtx
--         { ctx_vs = [],
--           ctx_rs = [],
--           ctx_rrules = [],
--           ctx_reprs = [],
--           ctx_ps = [],
--           ctx_pos = [],
--           ctx_pops = [],
--           ctx_pats = [],
--           ctx_nm = "Aap",
--           ctx_metas = [],
--           ctx_markup = Nothing,
--           ctx_lang = Nothing,
--           ctx_ks = [],
--           ctx_ifcs = [],
--           ctx_gs = [],
--           ctx_enfs = [],
--           ctx_ds = [],
--           ctx_cs = []
--         }
