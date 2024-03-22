{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
-- Mo: added this myself
{-# LANGUAGE FlexibleInstances #-}
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
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.Parsing (parseTerm)
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

myDecode :: B.ByteString -> Either String P_Context
myDecode = eitherDecode

instance FromJSON P_Context where
  parseJSON :: Value -> Parser P_Context
  parseJSON val = case val of
    Object v ->
      build <$> v .: "context"
        <*> v .: "relations"
        <*> v .: "concepts"
        <*> v .: "rules"
    invalid ->
      prependFailure
        "parsing P_Sign failed, "
        (typeMismatch "Object" invalid)
    where
      build :: Text -> [P_Relation] -> [PConceptDef] -> [P_Rule TermPrim] -> P_Context
      build nm rels cptdef rules =
        PCtx
          { ctx_vs = [],
            ctx_rs = rules,
            ctx_rrules = [],
            ctx_reprs = [],
            ctx_ps = [],
            ctx_pos = [],
            ctx_pops = [],
            ctx_pats = [],
            ctx_nm = nm,
            ctx_metas = [],
            ctx_markup = Nothing,
            ctx_lang = Nothing,
            ctx_ks = [],
            ctx_ifcs = [],
            ctx_gs = [],
            ctx_enfs = [],
            ctx_ds = rels, -- [P_Relation] is already defined as a list
            ctx_cs = cptdef
          }

instance FromJSON PCDDef where
  parseJSON val = case val of
    Object v ->
      --if object
      build <$> v .: "definition"
    String s ->
      -- if string
      pure $ PCDDefNew (PMeaning $ P_Markup Nothing Nothing s)
    Array arr ->
      -- if array
      pure $ PCDDefNew (PMeaning $ P_Markup Nothing Nothing "definition not working")
    invalid ->
      prependFailure
        "parsing PCDDef failed, "
        (typeMismatch "Object" invalid)
    where
      build :: Text -> PCDDef
      build def = PCDDefNew (PMeaning $ P_Markup Nothing Nothing def) -- Here we construct PCDDefNew

instance FromJSON PConceptDef where
  parseJSON val = case val of
    Object v ->
      build <$> v .: "name"
        <*> (v .: "definition" >>= parseJSON) -- Use the PCDDef parser here
    invalid ->
      prependFailure
        "parsing PConceptDef failed, "
        (typeMismatch "Object" invalid)
    where
      build :: Text -> PCDDef -> PConceptDef
      build cpt def =
        PConceptDef
          { cdcpt = cpt,
            cddef2 = def,
            cdmean = [], -- [PMeaning $ P_Markup Nothing Nothing ""] -- Insert a generic meaning / todo: change out with proper meanign
            cdfrom = "", -- Ignored as instructed -- todo: make from
            pos = OriginAtlas
          }

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
        <*> v .: "meaning"
    invalid ->
      prependFailure
        "parsing P_Relation failed, "
        (typeMismatch "Object" invalid)
    where
      build :: Text -> P_Sign -> PProps -> [PMeaning] -> P_Relation
      build nm sig prps mean =
        P_Relation
          { dec_sign = sig,
            dec_prps = prps,
            dec_pragma = Nothing,
            dec_nm = nm,
            dec_defaults = [],
            dec_Mean = mean,
            pos = OriginAtlas
          }

instance FromJSON PMeaning where -- todo: checken of dit werkt
  parseJSON (String txt) =
    pure $ PMeaning $ P_Markup Nothing Nothing txt -- todo: change this so that mLang and mFormat are taken into account
  parseJSON (Object v) =
    PMeaning <$> parseJSON (Object v)
  parseJSON invalid =
    prependFailure
      "parsing PMeaning failed, "
      (typeMismatch "String or Object" invalid)

instance FromJSON PMessage where
  parseJSON (String txt) =
    pure $ PMessage $ P_Markup Nothing Nothing txt -- todo: change this so that mLang and mFormat are taken into account
  parseJSON (Object v) =
    PMessage <$> parseJSON (Object v)
  parseJSON invalid =
    prependFailure
      "parsing PMessage failed, "
      (typeMismatch "String or Object" invalid)

instance FromJSON P_Markup where
  parseJSON (Object v) =
    P_Markup
      <$> pure Nothing -- Ignore mLang
      <*> pure Nothing -- Ignore mFormat
      <*> v .: "meaning"
  parseJSON invalid =
    prependFailure
      "parsing P_Markup failed, "
      (typeMismatch "Object" invalid)

instance FromJSON (P_Rule TermPrim) where -- ToDo: hulp vragen bij Termen
  parseJSON val = case val of
    Object v ->
      build <$> v .: "name"
        <*> v .: "formexp"
        <*> v .: "meaning" -- This should parse an array of `PMeaning`
        <*> v .: "message" -- Assuming `rr_msg` is an empty list for now
    invalid ->
      prependFailure
        "parsing P_Rule failed, "
        (typeMismatch "Object" invalid)
    where
      build :: Text -> Text -> [PMeaning] -> [PMessage] -> P_Rule TermPrim
      build nm formexp mean msg =
        P_Rule
          { pos = OriginAtlas,
            rr_nm = nm,
            rr_exp = case parseTerm ("Json file from Atlas, at a rule named `" <> T.unpack nm <> "`.") formexp of
              Errors err -> fatal ("Parse error in " <> formexp <> ":\n   " <> tshow err)
              Checked term _ -> term,
            rr_mean = mean,
            rr_msg = msg, -- msg
            rr_viol = Nothing
          }

instance FromJSON (Term a) where
  parseJSON :: Value -> Parser (Term a)
  parseJSON val = case val of
    invalid ->
      prependFailure
        "parsing PProp failed, "
        (typeMismatch "String" invalid)
