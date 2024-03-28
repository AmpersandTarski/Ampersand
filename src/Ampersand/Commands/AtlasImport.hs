{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
--import qualified Data.HashMap.Strict as HM
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
      build <$> v .: "name" -- name of the context
        <*> v .: "patterns"
        <*> v .: "purposes" -- purposes within whole CONTEXT
        -- <*> v .: "relations"
        -- <*> v .: "concepts"
        -- <*> v .: "rules"
    invalid ->
      prependFailure
        "parsing P_Context failed, "
        (typeMismatch "Object" invalid)
    where
      -- build :: Text -> [P_Pattern] -> [P_Relation] -> [PConceptDef] -> [P_Rule TermPrim] -> [PPurpose] -> P_Context
      -- build nm pats rels cptdef rules =
      build :: Text -> [P_Pattern] -> [PPurpose] -> P_Context
      build nm pats prps =
        PCtx
          { ctx_vs = [],
            ctx_rs = [], -- rules,
            ctx_rrules = [],
            ctx_reprs = [],
            ctx_ps = prps,
            ctx_pos = [],
            ctx_pops = [],
            ctx_pats = pats,
            ctx_nm = nm,
            ctx_metas = [],
            ctx_markup = Nothing,
            ctx_lang = Nothing,
            ctx_ks = [],
            ctx_ifcs = [],
            ctx_gs = [],
            ctx_enfs = [],
            ctx_ds = [], -- rels,
            ctx_cs = [] -- cptdef
          }

instance FromJSON P_Pattern where
  parseJSON :: Value -> Parser P_Pattern
  parseJSON val = case val of
    Object v ->
      build <$> v .: "name" --name of the patterns
        <*> v .: "relations"
        <*> v .: "concepts"
        <*> v .: "rules"
        <*> v .: "purposes"
    invalid ->
      prependFailure
        "parsing P_PAttern failed, "
        (typeMismatch "Object" invalid)
    where
      build :: Text -> [P_Relation] -> [PConceptDef] -> [P_Rule TermPrim] -> [PPurpose] -> P_Pattern
      build nm rels cptdef rules prps =
        P_Pat
          { pos = OriginAtlas,
            pt_nm = nm,
            pt_rls = rules,
            pt_gns = [],
            pt_dcs = rels,
            pt_RRuls = [],
            pt_cds = cptdef,
            pt_Reprs = [],
            pt_ids = [],
            pt_vds = [],
            pt_xps = prps,
            pt_pop = [],
            pt_end = OriginAtlas,
            pt_enfs = []
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

instance FromJSON (Term a) where -- todo: not sure whether this is still in use
  parseJSON :: Value -> Parser (Term a)
  parseJSON val = case val of
    invalid ->
      prependFailure
        "parsing PProp failed, "
        (typeMismatch "String" invalid)

instance FromJSON PPurpose where
  parseJSON val = case val of
    Object v ->
      build <$> v .: "meaning"
        <*> parseJSON val
    invalid ->
      prependFailure
        "parsing PPurpose failed, "
        (typeMismatch "Object" invalid)
    where
      build :: Text -> PRef2Obj -> PPurpose
      build mrk obj =
        PRef2
          { pos = OriginAtlas, -- Voorbeeldwaarde
            pexObj = obj, -- Je moet bepalen hoe je PRef2Obj wilt invullen
            pexMarkup = P_Markup Nothing Nothing mrk, -- Direct gebruik van `meaning` als pexMarkup
            pexRefIDs = ["test"] -- Voorbeeldlijst
          }

instance FromJSON PRef2Obj where
  parseJSON val = case val of
    Object v ->
      (PRef2ConceptDef <$> parseFirstField v "conceptPurp")
        <|> (v .:? "relationPurp" >>= maybe (fail "Expected a non-empty 'relationPurp' list") (build . listToMaybe))
        <|> (PRef2Rule <$> parseFirstField v "rulePurp")
        <|> (PRef2IdentityDef <$> parseFirstField v "identPurp")
        <|> (PRef2ViewDef <$> parseFirstField v "viewPurp")
        <|> (PRef2Pattern <$> parseFirstField v "patternPurp")
        <|> (PRef2Interface <$> parseFirstField v "interfacePurp")
        <|> (PRef2Context <$> parseFirstField v "contextPurp")
    -- <|> fail "PRef2Obj niet kunnen parsen, geen veld gevonden" --todo: betere fail statement
    invalid ->
      prependFailure
        "parsing PRef2Obj failed, "
        (typeMismatch "Object" invalid)
    where
      build :: Maybe P_NamedRel -> Parser PRef2Obj
      build (Just rel) = pure $ PRef2Relation rel
      build Nothing = fail "relationPurp list is empty"

parseFirstField :: Object -> T.Text -> Parser T.Text
parseFirstField obj key = do
  texts <- obj .:? key .!= []
  case listToMaybe texts of
    Just txt -> return txt -- Als er een waarde is, geef deze terug
    Nothing -> mzero

instance FromJSON P_NamedRel where
  parseJSON val = case val of
    Object v ->
      build <$> v .: "relation"
        <*> v .: "sign"
    -- <*> v .: "reference"
    invalid ->
      prependFailure
        "parsing P_NamedRel failed, "
        (typeMismatch "Object" invalid)
    where
      build :: Text -> Maybe P_Sign -> P_NamedRel
      build txt sgn =
        PNamedRel
          { pos = OriginAtlas,
            p_nrnm = txt, -- name of Relation
            p_mbSign = sgn -- Sign of relation
          }

-- instance FromJSON P_Interface where
--   parseJSON val = case val of
--     Object v ->
--       build <$> v .: "relationName"
--         <*> v .: "sign"
--     -- <*> v .: "reference"
--     invalid ->
--       prependFailure
--         "parsing PPurpose failed, "
--         (typeMismatch "Object" invalid)
--     where
--       build :: Text -> Maybe P_Sign -> P_Interface
--       build txt sgn =
--         P_Ifc
--           { -- | The interface is of type API
--             ifc_IsAPI = Bool,
--             -- | the name of the interface
--             ifc_Name = Text,
--             -- | a list of roles that may use this interface
--             ifc_Roles = [Role],
--             -- | the context term (mostly: I[c])
--             ifc_Obj = P_BoxItemTermPrim,
--             pos = Origin,
--             ifc_Prp = Text
--           }