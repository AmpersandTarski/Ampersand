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
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowPStruct
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.Parsing (parseTerm)
import Ampersand.Misc.HasClasses
import Ampersand.Types.Config
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.IntMap
import qualified RIO.ByteString.Lazy as B
import qualified RIO.NonEmpty as NE
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
myDecode = JSON.eitherDecode

instance JSON.FromJSON P_Context where
  parseJSON :: JSON.Value -> JSON.Parser P_Context
  parseJSON val = case val of
    JSON.Object v ->
      build <$> v JSON..: "name" -- name of the context
        <*> v JSON..: "patterns"
        -- <*> v JSON..: "interfaces"
        <*> v JSON..: "conceptsCtx" -- alle concepten met definitie
        <*> v JSON..: "representationsCtx" -- alle JSON.en
        <*> v JSON..: "rulesCtx"
        <*> v JSON..: "relationsCtx"
        <*> v JSON..: "purposes" -- purposes within whole CONTEXT
        <*> v JSON..:? "language"
        <*> v JSON..: "idents"
    invalid ->
      JSON.prependFailure
        "parsing P_Context failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      -- build :: Text -> [P_Pattern] -> [P_Relation] -> [PConceptDef] -> [P_Rule TermPrim] -> [PPurpose] -> P_Context
      -- build nm pats rels cptdef rules =
      build ::
        Text ->
        [P_Pattern] ->
        [PConceptDef] ->
        [Representation] ->
        [P_Rule TermPrim] ->
        [P_Relation] ->
        [PPurpose] ->
        Maybe Lang ->
        [P_IdentDef] ->
        P_Context
      build nm pats cpts reprs rules rels prps lang ident =
        PCtx
          { ctx_vs = [],
            ctx_rs = rules,
            ctx_rrules = [],
            ctx_reprs = reprs,
            ctx_ps = prps,
            ctx_pos = [],
            ctx_pops = [],
            ctx_pats = pats,
            ctx_nm = nm,
            ctx_metas = [],
            ctx_markup = Nothing,
            ctx_lang = lang,
            ctx_ks = ident, -- IDENT
            ctx_ifcs = [],
            ctx_gs = [],
            ctx_enfs = [],
            ctx_ds = rels, -- rels,
            ctx_cs = cpts -- cptdef
          }

instance JSON.FromJSON P_Pattern where
  parseJSON :: JSON.Value -> JSON.Parser P_Pattern
  parseJSON val = case val of
    JSON.Object v ->
      build <$> v JSON..: "name" --name of the patterns
        <*> v JSON..: "relations"
        <*> v JSON..: "concepts"
        <*> v JSON..: "representations"
        <*> v JSON..: "rules"
        <*> v JSON..: "purposes"
    invalid ->
      JSON.prependFailure
        "parsing P_PAttern failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> [P_Relation] -> [PConceptDef] -> [Representation] -> [P_Rule TermPrim] -> [PPurpose] -> P_Pattern
      build nm rels cptdef reprs rules prps =
        P_Pat
          { pos = OriginAtlas,
            pt_nm = nm,
            pt_rls = rules,
            pt_gns = [],
            pt_dcs = rels,
            pt_RRuls = [],
            pt_cds = cptdef,
            pt_Reprs = reprs,
            pt_ids = [],
            pt_vds = [],
            pt_xps = prps,
            pt_pop = [],
            pt_end = OriginAtlas,
            pt_enfs = []
          }

instance JSON.FromJSON PCDDef where
  parseJSON val = case val of
    JSON.Object v ->
      --if object
      build <$> v JSON..: "definition"
    JSON.String s ->
      -- if string
      pure $ PCDDefNew (PMeaning $ P_Markup Nothing Nothing s)
    JSON.Array arr ->
      -- if array
      pure $ PCDDefNew (PMeaning $ P_Markup Nothing Nothing "definition not working")
    invalid ->
      JSON.prependFailure
        "parsing PCDDef failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> PCDDef
      build def = PCDDefNew (PMeaning $ P_Markup Nothing Nothing def) -- Here we construct PCDDefNew

instance JSON.FromJSON PConceptDef where
  parseJSON val = case val of
    JSON.Object v -> do
      build <$> v JSON..: "name"
        <*> (v JSON..: "definition" >>= JSON.parseJSON)
    invalid ->
      JSON.prependFailure
        "parsing PConceptDef failed, "
        (JSON.typeMismatch "Object" invalid)
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

instance JSON.FromJSON P_Concept where
  parseJSON :: JSON.Value -> JSON.Parser P_Concept
  parseJSON (JSON.Object v) =
    PCpt <$> v JSON..: "concept"
  parseJSON (JSON.String txt) =
    pure (PCpt txt)
  parseJSON invalid =
    JSON.prependFailure
      "parsing P_Concept failed, "
      (JSON.typeMismatch "JSON. or String" invalid)

instance JSON.FromJSON Representation where
  parseJSON :: JSON.Value -> JSON.Parser Representation
  parseJSON val = case val of
    JSON.Object v ->
      build <$> v JSON..: "name"
        <*> v JSON..: "type" -- Use the PCDDef JSON.parser here
    invalid ->
      JSON.prependFailure
        "parsing Representation failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: P_Concept -> TType -> Representation
      build cpt ttype =
        Repr
          { pos = OriginAtlas,
            reprcpts = cpt NE.:| [], -- NE.NonEmpty P_Concept,      -- todo: werkt dit met meerdere statements?
            reprdom = ttype
          }

instance JSON.FromJSON TType where
  parseJSON :: JSON.Value -> JSON.Parser TType
  parseJSON val = case val of
    JSON.String x -> case T.toUpper x of
      "ALPHANUMERIC" -> pure Alphanumeric
      "BIGALPHANUMERIC" -> pure BigAlphanumeric
      "HUGEALPHANUMERIC" -> pure HugeAlphanumeric
      "PASSWORD" -> pure Password
      "BINARY" -> pure Binary
      "BIGBINARY" -> pure BigBinary
      "HUGEBINARY" -> pure HugeBinary
      "DATE" -> pure Date
      "DATETIME" -> pure DateTime
      "BOOLEAN" -> pure Boolean
      "INTEGER" -> pure Integer
      "FLOAT" -> pure Float
      "OBJECT" -> pure Object -- this is a normal concept, but 'Object' is already in use TODO: Han vragen hoe dit op te lossen is
      "TYPEOFONE" -> pure TypeOfOne
      _ -> JSON.unexpected val
    invalid ->
      JSON.prependFailure
        "parsing TType failed, "
        (JSON.typeMismatch "String" invalid)

instance JSON.FromJSON PProp where
  parseJSON :: JSON.Value -> JSON.Parser PProp
  parseJSON val = case val of
    JSON.String x -> case T.toLower x of
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
        JSON.unexpected val
    invalid ->
      JSON.prependFailure
        "parsing PProp failed, "
        (JSON.typeMismatch "String" invalid)

instance JSON.FromJSON P_Sign where
  parseJSON val = case val of
    (JSON.Object v) ->
      P_Sign <$> v JSON..: "source"
        <*> v JSON..: "target"
    invalid ->
      JSON.prependFailure
        "parsing P_Sign failed, "
        (JSON.typeMismatch "Object" invalid)

instance JSON.FromJSON P_Relation where
  parseJSON val = case val of
    JSON.Object v ->
      build <$> v JSON..: "name"
        <*> v JSON..: "sign"
        <*> v JSON..: "properties"
        <*> v JSON..: "meaning"
    invalid ->
      JSON.prependFailure
        "parsing P_Relation failed, "
        (JSON.typeMismatch "Object" invalid)
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

instance JSON.FromJSON PMeaning where -- todo: checken of dit werkt
  parseJSON (JSON.String txt) =
    pure $ PMeaning $ P_Markup Nothing Nothing txt -- todo: change this so that mLang and mFormat are taken into account
  parseJSON (JSON.Object v) =
    PMeaning <$> JSON.parseJSON (JSON.Object v)
  parseJSON invalid =
    JSON.prependFailure
      "parsing PMeaning failed, "
      (JSON.typeMismatch "String or Object" invalid)

instance JSON.FromJSON PMessage where
  parseJSON (JSON.String txt) =
    pure $ PMessage $ P_Markup Nothing Nothing txt -- todo: change this so that mLang and mFormat are taken into account
  parseJSON (JSON.Object v) =
    PMessage <$> JSON.parseJSON (JSON.Object v)
  parseJSON invalid =
    JSON.prependFailure
      "parsing PMessage failed, "
      (JSON.typeMismatch "String or Object" invalid)

instance JSON.FromJSON P_Markup where
  parseJSON (JSON.Object v) =
    P_Markup
      <$> pure Nothing -- Ignore mLang
      <*> pure Nothing -- Ignore mFormat
      <*> v JSON..: "meaning"
  parseJSON invalid =
    JSON.prependFailure
      "parsing P_Markup failed, "
      (JSON.typeMismatch "Object" invalid)

instance JSON.FromJSON (P_Rule TermPrim) where -- ToDo: hulp vragen bij Termen
  parseJSON val = case val of
    JSON.Object v ->
      build <$> v JSON..: "name"
        <*> v JSON..: "formexp"
        <*> v JSON..: "meaning" -- This should parse an array of `PMeaning`
        <*> v JSON..: "message" -- Assuming `rr_msg` is an empty list for now
    invalid ->
      JSON.prependFailure
        "parsing P_Rule failed, "
        (JSON.typeMismatch "Object" invalid)
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

instance JSON.FromJSON PPurpose where
  parseJSON val = case val of
    JSON.Object v ->
      build <$> v JSON..: "meaning"
        <*> JSON.parseJSON val
    invalid ->
      JSON.prependFailure
        "parsing PPurpose failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> PRef2Obj -> PPurpose
      build mrk obj =
        PRef2
          { pos = OriginAtlas, -- Voorbeeldwaarde
            pexObj = obj, -- Je moet bepalen hoe je PRef2Obj wilt invullen
            pexMarkup = P_Markup Nothing Nothing mrk, -- Direct gebruik van `meaning` als pexMarkup
            pexRefIDs = ["test"] -- Voorbeeldlijst
          }

instance JSON.FromJSON PRef2Obj where
  parseJSON val = case val of
    JSON.Object v ->
      (PRef2ConceptDef <$> parseFirstField v "conceptPurp")
        <|> (v JSON..:? "relationPurp" >>= maybe (fail "Expected a non-empty 'relationPurp' list") (build . listToMaybe))
        <|> (PRef2Rule <$> parseFirstField v "rulePurp")
        <|> (PRef2IdentityDef <$> parseFirstField v "identPurp")
        <|> (PRef2ViewDef <$> parseFirstField v "viewPurp")
        <|> (PRef2Pattern <$> parseFirstField v "patternPurp")
        <|> (PRef2Interface <$> parseFirstField v "interfacePurp")
        <|> (PRef2Context <$> parseFirstField v "contextPurp")
        <|> fail "PRef2Obj niet kunnen parsen, geen veld gevonden" --todo: betere fail statement
    invalid ->
      JSON.prependFailure
        "parsing PRef2Obj failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Maybe P_NamedRel -> JSON.Parser PRef2Obj
      build (Just rel) = pure $ PRef2Relation rel
      build Nothing = fail "relationPurp list is empty"

parseFirstField :: JSON.Object -> T.Text -> JSON.Parser T.Text
parseFirstField obj key = do
  texts <- obj JSON..:? key JSON..!= []
  case listToMaybe texts of
    Just txt -> return txt -- Als er een waarde is, geef deze terug
    Nothing -> mzero

instance JSON.FromJSON P_NamedRel where
  parseJSON val = case val of
    JSON.Object v ->
      build <$> v JSON..: "relation"
        <*> v JSON..: "sign"
    -- <*> v JSON..: "reference"
    invalid ->
      JSON.prependFailure
        "parsing P_NamedRel failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> Maybe P_Sign -> P_NamedRel
      build txt sgn =
        PNamedRel
          { pos = OriginAtlas,
            p_nrnm = txt, -- name of Relation
            p_mbSign = sgn -- Sign of relation
          }

instance JSON.FromJSON Lang where
  parseJSON = JSON.withText "Lang" $ \t -> case T.toUpper t of
    "DUTCH" -> pure Dutch
    "ENGLISH" -> pure English
    _ -> fail $ "JSON.Unexpected language: " ++ show t

instance JSON.FromJSON P_IdentDef where
  parseJSON val = case val of
    JSON.Object v ->
      build <$> v JSON..: "name"
        <*> (v JSON..: "concept")
        <*> v JSON..: "ident"
    invalid ->
      JSON.prependFailure
        "parsing P_Rule failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> P_Concept -> P_IdentSegmnt TermPrim -> P_IdentDf TermPrim
      build lbl cpt ident =
        P_Id
          { pos = OriginAtlas,
            ix_lbl = lbl,
            ix_cpt = cpt, -- P_Concept
            ix_ats = ident NE.:| [] -- NE.NonEmpty (P_IdentSegmnt a)
          }

instance (JSON.FromJSON a) => JSON.FromJSON (P_IdentSegmnt a) where
  parseJSON val = P_IdentExp <$> JSON.parseJSON val

instance JSON.FromJSON a => JSON.FromJSON (P_BoxItem a) where
  parseJSON :: JSON.Value -> JSON.Parser (P_BoxItem a)
  parseJSON val = case val of
    JSON.Object v ->
      if has "text" v
        then
          buildTxt
            <$> v JSON..: "name"
              <*> v JSON..: "text"
        else
          buildExpr
            <$> v JSON..: "name"
              <*> v JSON..: "ctx"
              <*> v JSON..:? "crud"
              <*> v JSON..:? "mview"
              <*> v JSON..:? "msub"
    _ -> JSON.typeMismatch "Object" val
    where
      has :: Text -> JSON.Object -> Bool -- todo: Han  is dit een goede manier hiervoor?
      has key obj = case JSON.fromJSON (JSON.Object obj) :: JSON.Result (Maybe Text) of
        JSON.Success _ -> True
        _ -> False

      buildExpr :: Text -> Term a -> Maybe P_Cruds -> Maybe Text -> Maybe (P_SubIfc a) -> P_BoxItem a
      buildExpr nm term crud view sub =
        P_BxExpr
          { obj_nm = nm,
            pos = OriginAtlas,
            obj_ctx = term,
            obj_crud = crud,
            obj_mView = view,
            obj_msub = sub
          }
      buildTxt :: Text -> Text -> P_BoxItem a
      buildTxt nm txt =
        P_BxTxt
          { obj_nm = nm,
            pos = OriginAtlas,
            obj_txt = txt
          }

instance JSON.FromJSON P_Cruds where
  parseJSON :: JSON.Value -> JSON.Parser P_Cruds
  parseJSON (JSON.Object v) =
    P_Cruds <$> pure OriginAtlas
      <*> v JSON..: "crud"
  parseJSON invalid = JSON.typeMismatch "P_Cruds" invalid

instance (JSON.FromJSON a) => JSON.FromJSON (P_SubIfc a) where
  parseJSON :: JSON.Value -> JSON.Parser (P_SubIfc a)
  parseJSON val = case val of
    JSON.Object v ->
      if has "si_str" v
        then
          buildInterfaceRef
            <$> v JSON..: "si_isLink"
            <*> v JSON..: "si_str"
        else
          buildBox
            <$> v JSON..: "si_header"
            <*> v JSON..: "si_box"
    _ -> JSON.typeMismatch "P_SubIfc" val
    where
      has :: Text -> JSON.Object -> Bool
      has key obj = case JSON.fromJSON (JSON.Object obj) :: JSON.Result (Maybe Text) of
        JSON.Success _ -> True
        _ -> False

      buildInterfaceRef :: Bool -> Text -> P_SubIfc a
      buildInterfaceRef isLink str =
        P_InterfaceRef
          { pos = OriginAtlas,
            si_isLink = isLink,
            si_str = str
          }

      buildBox :: BoxHeader -> [P_BoxItem a] -> P_SubIfc a
      buildBox header items =
        P_Box
          { pos = OriginAtlas,
            si_header = header,
            si_box = items
          }

instance JSON.FromJSON BoxHeader where
  parseJSON :: JSON.Value -> JSON.Parser BoxHeader
  parseJSON val = case val of
    JSON.Object v ->
      build <$> v JSON..: "btType"
        <*> v JSON..: "btKeys"
    invalid ->
      JSON.prependFailure
        "parsing BoxHeader failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> [TemplateKeyValue] -> BoxHeader
      build ttype keys =
        BoxHeader
          { pos = OriginAtlas,
            btType = ttype,
            btKeys = keys
          }

instance JSON.FromJSON TemplateKeyValue where
  parseJSON :: JSON.Value -> JSON.Parser TemplateKeyValue
  parseJSON val = case val of
    JSON.Object v ->
      build <$> v JSON..: "tkkey"
        <*> v JSON..:? "tkval"
    invalid ->
      JSON.prependFailure
        "parsing TemplateKeyValue failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> Maybe Text -> TemplateKeyValue
      build ttype kval =
        TemplateKeyValue
          { pos = OriginAtlas,
            tkkey = ttype,
            tkval = kval
          }

instance JSON.FromJSON a => JSON.FromJSON (Term a) where
  parseJSON = JSON.withObject "Term" $ \v -> do
    termType <- v JSON..: "type"
    origin <- pure OriginAtlas
    case termType of
      "Prim" -> Prim <$> v JSON..: "value"
      "PEqu" -> PEqu origin <$> v JSON..: "lhs" <*> v JSON..: "rhs"
      "PInc" -> PInc origin <$> v JSON..: "lhs" <*> v JSON..: "rhs"
      -- Add cases for other constructors
      "PIsc" -> PIsc origin <$> v JSON..: "lhs" <*> v JSON..: "rhs"
      "PUni" -> PUni origin <$> v JSON..: "lhs" <*> v JSON..: "rhs"
      "PDif" -> PDif origin <$> v JSON..: "lhs" <*> v JSON..: "rhs"
      "PLrs" -> PLrs origin <$> v JSON..: "lhs" <*> v JSON..: "rhs"
      "PRrs" -> PRrs origin <$> v JSON..: "lhs" <*> v JSON..: "rhs"
      "PDia" -> PDia origin <$> v JSON..: "lhs" <*> v JSON..: "rhs"
      "PCps" -> PCps origin <$> v JSON..: "lhs" <*> v JSON..: "rhs"
      "PRad" -> PRad origin <$> v JSON..: "lhs" <*> v JSON..: "rhs"
      "PPrd" -> PPrd origin <$> v JSON..: "lhs" <*> v JSON..: "rhs"
      "PKl0" -> PKl0 origin <$> v JSON..: "term"
      "PKl1" -> PKl1 origin <$> v JSON..: "term"
      "PFlp" -> PFlp origin <$> v JSON..: "term"
      "PCpl" -> PCpl origin <$> v JSON..: "term"
      "PBrk" -> PBrk origin <$> v JSON..: "term"
      _ -> fail $ "Unknown term type: " ++ T.unpack termType

instance JSON.FromJSON TermPrim where
  parseJSON = JSON.withObject "TermPrim" $ \v -> do
    constructorType <- v JSON..: "type"
    case constructorType of
      "PI" -> pure $ PI OriginAtlas
      "Pid" -> Pid OriginAtlas <$> v JSON..: "concept"
      "Patm" -> Patm OriginAtlas <$> v JSON..: "atomValue" <*> v JSON..:? "concept"
      "PVee" -> pure $ PVee OriginAtlas
      "Pfull" -> Pfull OriginAtlas <$> v JSON..: "concept1" <*> v JSON..: "concept2"
      "PNamedR" -> PNamedR <$> v JSON..: "namedRel"
      _ -> fail $ "Unknown TermPrim constructor: " ++ constructorType

instance JSON.FromJSON PAtomValue where
  parseJSON = JSON.withObject "PAtomValue" $ \v -> do
    constructorType <- v JSON..: "type"
    case constructorType of
      "PSingleton" -> PSingleton OriginAtlas <$> v JSON..: "text" <*> v JSON..:? "atomValue"
      "ScriptString" -> ScriptString OriginAtlas <$> v JSON..: "text"
      "XlsxString" -> XlsxString OriginAtlas <$> v JSON..: "text"
      "ScriptInt" -> ScriptInt OriginAtlas <$> v JSON..: "integer"
      "ScriptFloat" -> ScriptFloat OriginAtlas <$> v JSON..: "double"
      "XlsxDouble" -> XlsxDouble OriginAtlas <$> v JSON..: "double"
      "ComnBool" -> ComnBool OriginAtlas <$> v JSON..: "bool"
      "ScriptDate" -> ScriptDate OriginAtlas <$> v JSON..: "day"
      "ScriptDateTime" -> ScriptDateTime OriginAtlas <$> v JSON..: "utcTime"
      _ -> fail $ "Unknown PAtomValue constructor: " ++ constructorType

-- instance JSON.FromJSON P_Interface where
--   parseJSON val = case val of
--     Object v ->
--       build <$> v JSON..: "relationName"
--         <*> v JSON..: "sign"
--     -- <*> v JSON..: "reference"
--     invalid ->
--       JSON.prependFailure
--         "parsing PPurpose failed, "
--         (JSON.typeMismatch "Object" invalid)
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