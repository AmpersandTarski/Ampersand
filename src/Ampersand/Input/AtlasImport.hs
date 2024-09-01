{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
-- Mo: added this myself
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# HLINT ignore "Redundant bracket" #-}

-- | Generate a configuration file for a new project.
module Ampersand.Input.AtlasImport
  ( InitOpts (..),
    HasInitOpts (..),
    parseJsonFile,
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
  ( BoxHeader (..),
    DefinitionContainer (..),
    EnforceOperator (..),
    MetaData (..),
    Origin (OriginAtlas),
    PAtomValue (..),
    PCDDef (PCDDefNew),
    PClassify (..),
    PConceptDef (..),
    PMeaning (..),
    PMessage (..),
    PProp (..),
    PProps,
    PPurpose (..),
    PRef2Obj (..),
    P_BoxItem (..),
    P_Concept (PCpt),
    P_Context (..),
    P_Cruds (..),
    P_Enforce (..),
    P_IdentDef,
    P_IdentDf (..),
    P_IdentSegmnt (P_IdentExp),
    P_Markup (P_Markup),
    P_NamedRel (..),
    P_Pattern (..),
    P_Relation (..),
    P_RoleRule (..),
    P_Rule (..),
    P_Sign (P_Sign),
    Representation (..),
    Role (..),
    TType (..),
    TemplateKeyValue (..),
    Term,
    TermPrim (PNamedR),
  )
import Ampersand.Input.ADL1.CtxError (Guarded (..), mkJSONParseError)
import Ampersand.Input.ADL1.Parser (pTerm)
import Ampersand.Input.ADL1.ParsingLib
import Ampersand.Misc.HasClasses
import qualified Data.Aeson as JSON
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.Types as JSON
import qualified RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

parseJsonFile :: FilePath -> RIO env (Guarded P_Context)
parseJsonFile fp = do
  contents <- RIO.readFileBinary fp
  pure . fromAtlas $ contents

fromAtlas :: ByteString -> Guarded P_Context
fromAtlas json =
  case JSON.eitherDecode (BL.fromStrict json) of
    Left msg -> mkJSONParseError OriginAtlas (T.pack msg)
    Right a -> a

instance JSON.FromJSON (Guarded P_Context) where
  parseJSON :: JSON.Value -> JSON.Parser (Guarded P_Context)
  parseJSON val = case val of
    JSON.Object v -> do
      ctxName <- textToNameInJSON ContextName <$> v JSON..: "name"
      build ctxName
        <$> v
        JSON..:? "label" -- the LABEL field
        <*> v
        JSON..: "patterns" -- <*> v JSON..: "interfaces"
        <*> v
        JSON..: "conceptsCtx" -- alle concepten met definitie
        <*> v
        JSON..: "representationsCtx" -- alle JSON.en
        <*> v
        JSON..: "rulesCtx"
        -- <*> v JSON..: "enforceCtx"
        <*> v
        JSON..: "rolerules"
        <*> v
        JSON..: "relationsCtx"
        <*> v
        JSON..: "purposes" -- purposes within whole CONTEXT
        <*> v
        JSON..:? "language"
        <*> v
        JSON..: "idents"
    invalid ->
      JSON.prependFailure
        "parsing P_Context failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      -- build :: Text -> [P_Pattern] -> [P_Relation] -> [PConceptDef] -> [P_Rule TermPrim] -> [PPurpose] -> P_Context
      -- build nm pats rels cptdef rules =
      build ::
        Guarded Name ->
        Maybe Text ->
        [Guarded P_Pattern] ->
        [Guarded (DefinitionContainer -> PConceptDef)] ->
        [Guarded Representation] ->
        [Guarded (P_Rule TermPrim)] ->
        -- [P_Enforce TermPrim] ->
        [Guarded P_RoleRule] ->
        [Guarded P_Relation] ->
        [Guarded PPurpose] ->
        Maybe Lang ->
        [Guarded P_IdentDef] ->
        Guarded P_Context
      build gNm lbl gPats gCpts gReprs gRules gRolerules gRels gPrps lang gIdents = do
        nm <- gNm
        pats <- sequence gPats
        cpts <- sequence gCpts
        reprs <- sequence gReprs
        rules <- sequence gRules
        rolerules <- sequence gRolerules
        rels <- sequence gRels
        prps <- sequence gPrps
        idents <- sequence gIdents
        pure
          PCtx
            { ctx_vs = [],
              ctx_rs = rules,
              ctx_rrules = rolerules,
              ctx_reprs = reprs,
              ctx_ps = prps,
              ctx_pos = [],
              ctx_pops = [], -- niet in RAP
              ctx_pats = pats,
              ctx_nm = nm,
              ctx_lbl = textToLabelInJSON <$> lbl,
              ctx_metas = [], -- staat klaar
              ctx_markup = Nothing,
              ctx_lang = lang,
              ctx_ks = idents, -- IDENT
              ctx_ifcs = [],
              ctx_gs = [], -- staat klaar
              ctx_enfs = [], -- enforce, niet mogelijk met deze versie
              ctx_ds = rels, -- rels,
              ctx_cs = map (\cpt -> cpt (CONTEXT nm)) cpts -- cptdef
            }

instance JSON.FromJSON (Guarded P_Pattern) where
  parseJSON :: JSON.Value -> JSON.Parser (Guarded P_Pattern)
  parseJSON val = case val of
    JSON.Object v -> do
      patName <- textToNameInJSON PatternName <$> v JSON..: "name"
      build patName
        <$> v
        JSON..:? "label" -- name of the patterns
        <*> v
        JSON..: "relations"
        <*> v
        JSON..: "concepts"
        <*> v
        JSON..: "representations"
        <*> v
        JSON..: "rules"
        <*> v
        JSON..: "purposes"
    invalid ->
      JSON.prependFailure
        "parsing P_PAttern failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build ::
        Guarded Name ->
        Maybe Text ->
        [Guarded P_Relation] ->
        [Guarded (DefinitionContainer -> PConceptDef)] ->
        [Guarded Representation] ->
        [Guarded (P_Rule TermPrim)] ->
        [Guarded PPurpose] ->
        Guarded P_Pattern
      build gNm lbl gRels gCpts gReprs gRules gPrps = do
        nm <- gNm
        rels <- sequence gRels
        cpts <- sequence gCpts
        reprs <- sequence gReprs
        rules <- sequence gRules
        prps <- sequence gPrps
        pure
          P_Pat
            { pos = OriginAtlas,
              pt_nm = nm,
              pt_lbl = textToLabelInJSON <$> lbl,
              pt_rls = rules,
              pt_gns = [], -- staat klaar
              pt_dcs = rels,
              pt_RRuls = [], -- not specified in RAP
              pt_cds = map (\cpt -> cpt (PATTERN nm)) cpts,
              pt_Reprs = reprs,
              pt_ids = [],
              pt_vds = [],
              pt_xps = prps,
              pt_pop = [],
              pt_end = OriginAtlas,
              pt_enfs = []
            }

instance JSON.FromJSON (Guarded (DefinitionContainer -> PConceptDef)) where
  parseJSON val = case val of
    JSON.Object v -> do
      build
        <$> v
        JSON..: "name"
        <*> v
        JSON..:? "label"
        <*> (v JSON..: "definition" >>= JSON.parseJSON)
    invalid ->
      JSON.prependFailure
        "parsing PConceptDef failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> Maybe Text -> PCDDef -> Guarded (DefinitionContainer -> PConceptDef)
      build gNm lbl def = do
        nm <- textToNameInJSON ConceptName gNm
        pure $ \x ->
          PConceptDef
            { cdname = nm,
              cdlbl = textToLabelInJSON <$> lbl,
              cddef2 = def,
              cdmean = [], -- [PMeaning $ P_Markup Nothing Nothing ""] -- Insert a generic meaning / todo: change out with proper meanign
              cdfrom = x,
              pos = OriginAtlas
            }

instance JSON.FromJSON PCDDef where
  parseJSON val = case val of
    JSON.Object v -> build <$> v JSON..: "definition"
    JSON.String s -> pure $ PCDDefNew (PMeaning $ P_Markup Nothing Nothing s)
    JSON.Array arr -> case parseArrayToText arr of
      Just txt -> pure $ PCDDefNew (PMeaning $ P_Markup Nothing Nothing txt)
      Nothing -> pure $ PCDDefNew (PMeaning $ P_Markup Nothing Nothing "Definition not provided")
    invalid -> JSON.prependFailure "parsing PCDDef failed, " (JSON.typeMismatch "Object or String" invalid)
    where
      build :: Text -> PCDDef
      build def = PCDDefNew (PMeaning $ P_Markup Nothing Nothing def)
      parseArrayToText :: RIO.Vector JSON.Value -> Maybe Text
      parseArrayToText arr = case listToMaybe (toList arr) of
        Just (JSON.String txt) -> Just txt
        _ -> Nothing

instance JSON.FromJSON (Guarded P_Concept) where
  parseJSON :: JSON.Value -> JSON.Parser (Guarded P_Concept)
  parseJSON (JSON.Object v) = do
    gNm <- textToNameInJSON ConceptName <$> (v JSON..: "name")
    pure
      ( PCpt <$> gNm
      )
  parseJSON invalid =
    JSON.prependFailure
      "parsing P_Concept failed, "
      (JSON.typeMismatch "JSON. or String" invalid)

instance JSON.FromJSON (Guarded Representation) where
  parseJSON :: JSON.Value -> JSON.Parser (Guarded Representation)
  parseJSON val = case val of
    JSON.Object v ->
      build
        <$> v
        JSON..: "name"
        <*> v
        JSON..: "type" -- Use the PCDDef JSON.parser here
    invalid ->
      JSON.prependFailure
        "parsing Representation failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Guarded P_Concept -> TType -> Guarded Representation
      build gCpt ttype = do
        cpt <- gCpt
        pure
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
      "map" -> pure P_Map
      "bij" -> pure P_Bij
      _ ->
        JSON.unexpected val
    invalid ->
      JSON.prependFailure
        "parsing PProp failed, "
        (JSON.typeMismatch "String" invalid)

instance JSON.FromJSON (Guarded P_Sign) where
  parseJSON val = case val of
    (JSON.Object v) ->
      build
        <$> v
        JSON..: "source"
        <*> v
        JSON..: "target"
    invalid ->
      JSON.prependFailure
        "parsing P_Sign failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> Text -> Guarded P_Sign
      build s t = do
        src <- textToNameInJSON ConceptName s
        tgt <- textToNameInJSON ConceptName t
        pure $ P_Sign (PCpt src) (PCpt tgt)

instance JSON.FromJSON (Guarded P_Relation) where
  parseJSON val = case val of
    JSON.Object v ->
      build
        <$> v
        JSON..: "relation"
        <*> v
        JSON..:? "label"
        <*> v
        JSON..: "sign"
        <*> v
        JSON..: "properties"
        <*> v
        JSON..: "meaning"
    invalid ->
      JSON.prependFailure
        "parsing P_Relation failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> Maybe Text -> Guarded P_Sign -> PProps -> [PMeaning] -> Guarded P_Relation
      build txt lbl gSig prps mean = do
        nm <- textToNameInJSON RelationName txt
        sig <- gSig
        pure
          $ P_Relation
            { dec_sign = sig,
              dec_prps = prps,
              dec_pragma = Nothing,
              dec_nm = nm,
              dec_label = textToLabelInJSON <$> lbl,
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
      Nothing -- Ignore mLang
      Nothing -- Ignore mFormat
      <$> v
      JSON..: "meaning"
  parseJSON invalid =
    JSON.prependFailure
      "parsing P_Markup failed, "
      (JSON.typeMismatch "Object" invalid)

instance JSON.FromJSON (Guarded (P_Rule TermPrim)) where
  parseJSON val = case val of
    JSON.Object v ->
      build
        <$> v
        JSON..: "name"
        <*> v
        JSON..:? "label"
        <*> v
        JSON..: "formexp"
        <*> v
        JSON..: "meaning" -- This should parse an array of `PMeaning`
        <*> v
        JSON..: "message" -- Assuming `rr_msg` is an empty list for now
    invalid ->
      JSON.prependFailure
        "parsing P_Rule failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> Maybe Text -> Text -> [PMeaning] -> [PMessage] -> Guarded (P_Rule TermPrim)
      build txt lbl formexp mean msg = do
        nm <- textToNameInJSON RuleName txt
        pure
          P_Rule
            { pos = OriginAtlas,
              rr_nm = nm,
              rr_lbl = textToLabelInJSON <$> lbl,
              rr_exp = case parseTerm ("Json file from Atlas, at a rule named `" <> T.unpack txt <> "`.") formexp of
                Errors err -> fatal ("Parse error in " <> formexp <> ":\n   " <> tshow err)
                Checked term _ -> term,
              rr_mean = mean,
              rr_msg = msg,
              rr_viol = Nothing
            }

parseTerm :: FilePath -> Text -> Guarded (Term TermPrim)
parseTerm = runParser pTerm

instance JSON.FromJSON (Guarded (P_Enforce TermPrim)) where
  parseJSON val = case val of
    JSON.Object v ->
      -- todo: if operator = .. then ...
      build
        <$> v
        JSON..: "relation"
        <*> v
        JSON..: "operator"
        <*> v
        JSON..: "rhs"
    invalid ->
      JSON.prependFailure
        "parsing P_Enforce failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Guarded P_NamedRel -> EnforceOperator -> Text -> Guarded (P_Enforce TermPrim)
      build gRel oper formexp = do
        rel <- gRel
        pure
          $ P_Enforce
            { pos = OriginAtlas,
              penfRel = PNamedR rel,
              penfOp = oper,
              penfExpr = case parseTerm ("Json file from Atlas, at a P_enforce `" <> "` expression .") formexp of
                Errors err -> fatal ("Parse error in " <> formexp <> ":\n   " <> tshow err)
                Checked term _ -> term
            }

-- instance JSON.FromJSON TermPrim where
--   parseJSON = JSON.withObject "relation" $ \v ->
--     -- (PI OriginAtlas <$ (v JSON..: "type" >>= guard . (== "PI")))
--     (Pid OriginAtlas <$> v JSON..: "concept")
--       <|> (Patm OriginAtlas <$> v JSON..: "atomValue" <*> v JSON..:? "concept")
--       -- <|> (PVee OriginAtlas <$ (v JSON..: "type" >>= guard . (== "PVee")))
--       <|> (Pfull OriginAtlas <$> v JSON..: "concept" <*> v JSON..: "concept2")
--       <|> (PNamedR <$> v JSON..: "relation")
--       <|> fail "Unknown or incomplete TermPrim"

instance JSON.FromJSON EnforceOperator where -- werkt nog niet
  parseJSON :: JSON.Value -> JSON.Parser EnforceOperator
  parseJSON val = case val of
    JSON.String x -> case T.toLower x of
      "inclusion" -> pure $ IsSuperSet OriginAtlas -- >:
      "subset" -> pure $ IsSubSet OriginAtlas -- :<
      "sameset" -> pure $ IsSameSet OriginAtlas -- :=
      _ -> JSON.unexpected val
    invalid ->
      JSON.prependFailure
        "parsing EnforceOperator failed, "
        (JSON.typeMismatch "String" invalid)

instance JSON.FromJSON (Guarded PPurpose) where
  parseJSON val = case val of
    JSON.Object v ->
      build
        <$> v
        JSON..: "meaning"
        <*> JSON.parseJSON val
    invalid ->
      JSON.prependFailure
        "parsing PPurpose failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> Guarded PRef2Obj -> Guarded PPurpose
      build mrk gObj = do
        obj <- gObj
        pure
          $ PPurpose
            { pos = OriginAtlas, -- Voorbeeldwaarde
              pexObj = obj, -- Je moet bepalen hoe je PRef2Obj wilt invullen
              pexMarkup = P_Markup Nothing Nothing mrk, -- Direct gebruik van `meaning` als pexMarkup
              pexRefIDs = [] -- geen lijst
            }

instance JSON.FromJSON (Guarded PRef2Obj) where
  parseJSON val = case val of
    JSON.Object v ->
      (foo PRef2ConceptDef ConceptName v "conceptPurp")
        <|> (do v JSON..:? "relationPurp" >>= maybe (fail "Expected a non-empty 'relationPurp' list") (build . listToMaybe))
        <|> (foo PRef2Rule RuleName v "rulePurp")
        <|> (foo PRef2IdentityDef IdentName v "identPurp")
        <|> (foo PRef2ViewDef ViewName v "viewPurp")
        <|> (foo PRef2Pattern PatternName v "patternPurp")
        <|> (foo PRef2Interface InterfaceName v "interfacePurp")
        <|> (foo PRef2Context ContextName v "contextPurp")
        <|> fail "PRef2Obj niet kunnen parsen, geen veld gevonden" -- todo: betere fail statement
    invalid ->
      JSON.prependFailure
        "parsing PRef2Obj failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      foo :: (Name -> PRef2Obj) -> NameType -> JSON.Object -> Text -> JSON.Parser (Guarded PRef2Obj)
      foo constructor typ v key = do
        nm <- parseFirstField typ v key
        pure (constructor <$> nm)
      build :: Maybe (Guarded P_NamedRel) -> JSON.Parser (Guarded PRef2Obj)
      build (Just gRel) = pure $ PRef2Relation <$> gRel
      build Nothing = fail "relationPurp list is empty"

parseFirstField :: NameType -> JSON.Object -> Text -> JSON.Parser (Guarded Name)
parseFirstField typ obj key = do
  let jsonKey = fromText key
  maybeValues <- obj JSON..:? jsonKey
  case maybeValues of
    Just (JSON.Array arr) ->
      case listToMaybe (toList arr) of
        Just (JSON.String txt) -> return $ textToNameInJSON typ txt
        _ -> mzero
    _ -> mzero

instance JSON.FromJSON (Guarded P_NamedRel) where
  parseJSON val = case val of
    JSON.Object v -> do
      sign <- do v JSON..: "sign"
      build
        <$> v
        JSON..: "name"
        <*> pure (sequenceA sign)
    -- <*> v JSON..: "reference"
    -- JSON.Array -- todo: hier komt een array te staan, werkt niet
    invalid ->
      JSON.prependFailure
        "parsing P_NamedRel failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> Guarded (Maybe P_Sign) -> (Guarded P_NamedRel)
      build txt gSgn = do
        nm <- textToNameInJSON RelationName txt
        sgn <- gSgn
        pure
          PNamedRel
            { pos = OriginAtlas,
              p_nrnm = nm, -- name of Relation
              p_mbSign = sgn -- Sign of relation
            }

instance JSON.FromJSON Lang where
  parseJSON = JSON.withText "Lang" $ \t -> case T.toUpper t of
    "DUTCH" -> pure Dutch
    "ENGLISH" -> pure English
    _ -> fail $ "JSON.Unexpected language: " ++ show t

instance JSON.FromJSON (Guarded P_IdentDef) where
  parseJSON val = case val of
    JSON.Object v ->
      build
        <$> (v JSON..: "name")
        <*> (v JSON..:? "label")
        <*> (v JSON..: "concept")
        <*> (v JSON..: "ident")
    invalid ->
      JSON.prependFailure
        "parsing P_Rule failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> Maybe Text -> Guarded P_Concept -> P_IdentSegmnt TermPrim -> (Guarded (P_IdentDf TermPrim))
      build txt lbl gCpt ident = do
        nm <- textToNameInJSON IdentName txt
        cpt <- gCpt
        pure
          P_Id
            { pos = OriginAtlas,
              ix_name = nm,
              ix_label = textToLabelInJSON <$> lbl,
              ix_cpt = cpt,
              ix_ats = ident NE.:| [] -- NE.NonEmpty (P_IdentSegmnt a)
            }

instance JSON.FromJSON (P_IdentSegmnt TermPrim) where
  parseJSON val = P_IdentExp <$> JSON.parseJSON val

instance JSON.FromJSON (P_BoxItem TermPrim) where -- niet in gebruik
  parseJSON val = case val of
    JSON.Object v ->
      if has "text" v
        then
          buildTxt
            <$> v
            JSON..: "name"
            <*> v
            JSON..: "text"
        else
          buildExpr
            <$> v
            JSON..: "name"
            <*> v
            JSON..: "ctx"
            <*> v
            JSON..:? "crud"
            <*> v
            JSON..:? "mview"
    -- <*> v JSON..:? "msub"
    _ -> JSON.typeMismatch "Object" val
    where
      has :: Text -> JSON.Object -> Bool -- todo: Han  is dit een goede manier hiervoor?
      has _key obj = case JSON.fromJSON (JSON.Object obj) :: JSON.Result (Maybe Text) of
        JSON.Success _ -> True
        _ -> False

      -- buildExpr :: Text -> Text -> Maybe P_Cruds -> Maybe Text -> Maybe (P_SubIfc TermPrim) -> P_BoxItem TermPrim
      -- buildExpr nm formexp crud view sub =
      buildExpr :: Text -> Text -> Maybe P_Cruds -> Maybe Text -> P_BoxItem TermPrim
      buildExpr nm formexp crud _view =
        P_BoxItemTerm
          { obj_PlainName = Nothing,
            obj_lbl = Nothing,
            pos = OriginAtlas,
            obj_ctx = case parseTerm ("Json file from Atlas, at a rule named `" <> T.unpack nm <> "`.") formexp of
              Errors err -> fatal ("Parse error in " <> formexp <> ":\n   " <> tshow err)
              Checked term _ -> term,
            obj_crud = crud,
            obj_mView = Nothing, -- todo
            obj_msub = Nothing -- todo
          }
      buildTxt :: Text -> Text -> P_BoxItem a
      buildTxt nm txt =
        P_BxTxt
          { obj_PlainName = case T.uncons nm of
              Nothing -> Nothing
              Just (h, tl) -> Just (Text1 h tl),
            pos = OriginAtlas,
            box_txt = txt
          }

instance JSON.FromJSON P_Cruds where -- niet in gebruik
  parseJSON :: JSON.Value -> JSON.Parser P_Cruds
  parseJSON val = case val of
    JSON.Object v ->
      P_Cruds OriginAtlas
        . toText1Unsafe
        <$> v
        JSON..: "crud"
    invalid ->
      JSON.prependFailure
        "parsing P_Cruds failed, "
        (JSON.typeMismatch "Object" invalid)

-- instance JSON.FromJSON (P_SubIfc a) where
--   parseJSON :: JSON.Value -> JSON.Parser (P_SubIfc a)
--   parseJSON val = case val of
--     JSON.Object v ->
--       if has "si_str" v
--         then
--           buildInterfaceRef
--             <$> v JSON..: "si_isLink"
--             <*> v JSON..: "si_str"
--         else
--           buildBox
--             <$> v JSON..: "si_header"
--             <*> v JSON..: "si_box"
--     _ -> JSON.typeMismatch "P_SubIfc" val
--     where
--       has :: Text -> JSON.Object -> Bool
--       has key obj = case JSON.fromJSON (JSON.Object obj) :: JSON.Result (Maybe Text) of
--         JSON.Success _ -> True
--         _ -> False

--       buildInterfaceRef :: Bool -> Text -> P_SubIfc a
--       buildInterfaceRef isLink str =
--         P_InterfaceRef
--           { pos = OriginAtlas,
--             si_isLink = isLink,
--             si_str = str
--           }
--       buildBox :: BoxHeader -> [P_BoxItem a] -> P_SubIfc a
--       buildBox header items =
--         P_Box
--           { pos = OriginAtlas,
--             si_header = header,
--             si_box = items
--           }

instance JSON.FromJSON BoxHeader where
  parseJSON :: JSON.Value -> JSON.Parser BoxHeader
  parseJSON val = case val of
    JSON.Object v ->
      build
        <$> v
        JSON..: "btType"
        <*> v
        JSON..: "btKeys"
    invalid ->
      JSON.prependFailure
        "parsing BoxHeader failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> [TemplateKeyValue] -> BoxHeader
      build ttype keys =
        BoxHeader
          { pos = OriginAtlas,
            btType = toText1Unsafe ttype, -- TODO:Add Guarded to JSON parser!
            btKeys = keys
          }

instance JSON.FromJSON TemplateKeyValue where
  parseJSON :: JSON.Value -> JSON.Parser TemplateKeyValue
  parseJSON val = case val of
    JSON.Object v ->
      build
        <$> v
        JSON..: "tkkey"
        <*> v
        JSON..:? "tkval"
    invalid ->
      JSON.prependFailure
        "parsing TemplateKeyValue failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> Maybe Text -> TemplateKeyValue
      build ttype kval =
        TemplateKeyValue
          { pos = OriginAtlas,
            tkkey = toText1Unsafe ttype, -- TODO:Add Guarded to JSON parser!
            tkval = kval
          }

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

instance JSON.FromJSON (Guarded PClassify) where
  parseJSON val = case val of
    JSON.Object v ->
      build
        <$> v
        JSON..: "specific"
        <*> v
        JSON..: "generic"
    invalid ->
      JSON.prependFailure
        "parsing PClassify failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Guarded P_Concept -> Guarded P_Concept -> Guarded PClassify
      build gSpec gGen = do
        spec <- gSpec
        gen <- gGen
        pure
          $ PClassify
            { pos = OriginAtlas,
              specific = spec,
              generics = gen NE.:| []
            }

instance JSON.FromJSON MetaData where
  parseJSON val = case val of
    JSON.Object v ->
      build
        <$> v
        JSON..: "name"
        <*> v
        JSON..: "value"
    invalid ->
      JSON.prependFailure
        "parsing MetaData failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: Text -> Text -> MetaData
      build nm value =
        MetaData
          { pos = OriginAtlas,
            mtName = toText1Unsafe nm, -- TODO:Add Guarded to JSON parser!
            mtVal = value
          }

instance JSON.FromJSON (Guarded P_RoleRule) where
  parseJSON val = case val of
    JSON.Object v -> do
      roles <- v JSON..: "roles" -- this is the label of the role --todo (v JSON..: "role" >>= JSON.parseJSON) veranderen???
      rules <- v JSON..: "rules" -- the rule
      case NE.nonEmpty rules of
        Just neRules -> return $ build roles neRules
        Nothing -> fail "The 'rule' array cannot be empty"
    invalid ->
      JSON.prependFailure
        "parsing P_RoleRule failed, "
        (JSON.typeMismatch "Object" invalid)
    where
      build :: NE.NonEmpty Text -> NE.NonEmpty Text -> Guarded P_RoleRule
      build neRoles neRules = do
        roles <- mapM (textToNameInJSON RoleName) neRoles
        ruls <- mapM (textToNameInJSON RuleName) neRules
        pure
          Maintain
            { pos = OriginAtlas,
              mRoles = mkRole <$> roles,
              mRules = ruls
            }
        where
          mkRole :: Name -> Role
          mkRole nm = Role OriginAtlas nm Nothing False

instance JSON.FromJSON (Guarded Role) where
  parseJSON = JSON.withObject "role" $ \v ->
    ( do
        nm <- textToNameInJSON RoleName <$> (v JSON..: "role")
        lbl <- (v JSON..:? "label") <&> fmap textToLabelInJSON
        pure
          $ Role OriginAtlas
          <$> nm
          <*> pure lbl
          <*> pure True
    )
      <|> ( do
              gNm <- textToNameInJSON RoleName <$> (v JSON..: "service")
              gLbl <- (v JSON..:? "label") <&> fmap textToLabelInJSON
              pure
                $ Role OriginAtlas
                <$> gNm
                <*> pure gLbl
                <*> pure False
          )

-- TODO: Wat als geen role of service?

textToLabelInJSON :: Text -> Label
textToLabelInJSON = Label

textToNameInJSON :: NameType -> Text -> Guarded Name
textToNameInJSON a txt =
  case T.words txt of
    [] -> fatal "ERROR parsing JSON: Name must nog be empty"
    [wrd] -> case T.uncons wrd of
      Nothing -> fatal "Impossible! a word cannot be empty"
      Just (h, tl) -> mkName a <$> toNamePart' (Text1 h tl)
        where
          toNamePart' :: Text1 -> Guarded (NonEmpty NamePart)
          toNamePart' x = mapM toNamePart'' (splitOnDots x)
          toNamePart'' :: Text1 -> Guarded NamePart
          toNamePart'' x = case toNamePart1 x of
            Nothing -> mkJSONParseError OriginAtlas ("Not a valid NamePart: " <> tshow x)
            Just np -> Checked np []
    _ -> mkJSONParseError OriginAtlas $ "ERROR parsing JSON: Name must not contain whitespace: `" <> txt <> "`."
