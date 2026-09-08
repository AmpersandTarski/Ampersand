{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An HTTP/JSON server front-end on the Ampersand daemon core.
--
-- It exposes the same parse+typecheck pipeline as @ampersand daemon@
-- (`Ampersand.Daemon.Parser.parseProject`), but driven by HTTP requests
-- instead of file-watching, and returning structured `Load` diagnostics as
-- JSON instead of rendering to a terminal. This is the network front-end of
-- the "one daemon, two front-ends" design (see the RAP architecture doc, §5).
--
-- Endpoints:
--
--   * @GET  \/health@               — liveness probe
--   * @POST \/check@    {script}    — type-check a full ADL script
--   * @POST \/translate@{script,term} — type-check a single term in the script's context
--   * @POST \/fspec@    {dump}      — build the FSpec twin from an Atlas population dump (cap 1)
--   * @POST \/import@   {dump}      — reconstruct an .adl script from an Atlas population dump (workstream A)
module Ampersand.Commands.Serve (runServe) where

import Ampersand.Basics
import Ampersand.Core.ShowPStruct (showP)
import Ampersand.Daemon.Parser (parseProject)
import Ampersand.Daemon.Types (Load (..), Severity (..), isMessage)
import Ampersand.FSpec.ToFSpec.CreateFspec (createFspec)
import Ampersand.Output.ToJSON.ToJson (populationToJSON)
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.AtlasImport (parseJsonFile)
import Ampersand.Misc.HasClasses
import Ampersand.Types.Config (ExtendedRunner, HasRunner)
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.=))
import Data.Text (splitOn)
import Network.HTTP.Types (Status, hContentType, status200, status400, status404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified RIO.ByteString.Lazy as BL
import RIO.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import RIO.FilePath ((</>))
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import System.Environment (lookupEnv)

-- | The environment constraints needed to run the daemon parse pipeline.
type ServeEnv env =
  ( HasDirOutput env,
    HasTrimXLSXOpts env,
    HasDaemonOpts env,
    HasFSpecGenOpts env,
    HasRunner env
  )

-- | Entry point, wired into the CLI as @ampersand serve@. Reuses 'DaemonOpts'
--   (so 'parseProject' is satisfied) and listens on @AMPERSAND_SERVE_PORT@
--   (default 8080).
runServe :: RIO (ExtendedRunner DaemonOpts) ()
runServe = do
  env <- ask
  mPort <- liftIO $ lookupEnv "AMPERSAND_SERVE_PORT"
  let port = fromMaybe 8080 (mPort >>= readMaybe) :: Int
  logInfo $ "Ampersand serve: listening on http://0.0.0.0:" <> display port
  logInfo "  GET /health | POST /check {script} | POST /translate {script,term} | POST /fspec {dump} | POST /import {dump} | POST /population {script}"
  liftIO $ Warp.run port (waiApp env)

waiApp :: (ServeEnv env) => env -> Wai.Application
waiApp env req respond = do
  body <- Wai.strictRequestBody req
  resp <- case (Wai.requestMethod req, Wai.pathInfo req) of
    ("GET", ["health"]) ->
      pure $ jsonResp status200 (JSON.object ["status" .= ("ok" :: Text)])
    ("POST", ["check"]) -> runRIO env (handleCheck body)
    ("POST", ["translate"]) -> runRIO env (handleTranslate body)
    ("POST", ["fspec"]) -> runRIO env (handleFspec body)
    ("POST", ["import"]) -> runRIO env (handleImport body)
    ("POST", ["population"]) -> runRIO env (handlePopulation body)
    _ -> pure $ jsonResp status404 (JSON.object ["error" .= ("not found" :: Text)])
  respond resp

-- Request bodies ------------------------------------------------------------

newtype CheckReq = CheckReq Text

instance JSON.FromJSON CheckReq where
  parseJSON = JSON.withObject "CheckReq" $ \v -> CheckReq <$> v .: "script"

-- | De heenweg neemt naast het script een optionele naam aan. Die naam telt,
--   want de compiler legt de oorsprong van elk element vast en leidt daar
--   gegenereerde namen uit af. Wie dezelfde populatie wil als het commando op
--   een bestand met die naam, geeft haar mee. Zonder naam wordt zij uit de
--   inhoud afgeleid, zodat gelijke scripts gelijke uitvoer geven.
data PopulationReq = PopulationReq Text (Maybe Text)

instance JSON.FromJSON PopulationReq where
  parseJSON = JSON.withObject "PopulationReq" $ \v ->
    PopulationReq <$> v .: "script" <*> v JSON..:? "name"

newtype FspecReq = FspecReq Text

instance JSON.FromJSON FspecReq where
  parseJSON = JSON.withObject "FspecReq" $ \v -> FspecReq <$> v .: "dump"

data TranslateReq = TranslateReq Text Text

instance JSON.FromJSON TranslateReq where
  parseJSON = JSON.withObject "TranslateReq" $ \v ->
    TranslateReq <$> v .: "script" <*> v .: "term"

-- Handlers ------------------------------------------------------------------

handleCheck :: (ServeEnv env) => BL.ByteString -> RIO env Wai.Response
handleCheck = withDecoded $ \(CheckReq script) -> do
  msgs <- checkText ".adl" script
  pure $ resultResp msgs Nothing

handleFspec :: (ServeEnv env) => BL.ByteString -> RIO env Wai.Response
handleFspec = withDecoded $ \(FspecReq dump) -> do
  -- A .json file is dispatched to the Atlas importer by the parser, so this
  -- both validates and builds the twin's P_Context from the population dump.
  msgs <- checkText ".json" dump
  pure $ resultResp msgs Nothing

handleTranslate :: (ServeEnv env) => BL.ByteString -> RIO env Wai.Response
handleTranslate = withDecoded $ \(TranslateReq script term) -> do
  msgs <- checkText ".adl" (spliceProbe script term)
  pure $ resultResp msgs (Just term)

-- | Atlas-import (workstream A): reconstruct an .adl script from an Atlas
--   population dump. Reuses the compiler's AtlasImport (JSON -> P_Context) and
--   the pretty-printer (showP). This makes the daemon the single compiler
--   service for both term-checking (B) and the Atlas-editor round-trip (A).
handleImport :: (ServeEnv env) => BL.ByteString -> RIO env Wai.Response
handleImport = withDecoded $ \(FspecReq dump) -> do
  result <- withTmpScript ".json" dump parseJsonFile
  pure $ case result of
    Checked ctx _ ->
      jsonResp status200 (JSON.object ["ok" .= True, "adl" .= showP ctx])
    Errors errs ->
      jsonResp status200 (JSON.object ["ok" .= False, "diagnostics" .= map tshow (NE.toList errs)])

-- | De heenweg (OK-30): een script als tekst in, de populatie van
--   FormalAmpersand als JSON eruit. Dit is wat RAP tot nu toe deed met een
--   eigen aanroep van het commando @ampersand population --build-recipe Grind@
--   binnen zijn eigen container. Door het hier aan te bieden hoeft de compiler
--   niet meer in de RAP-image te zitten.
handlePopulation :: (ServeEnv env) => BL.ByteString -> RIO env Wai.Response
handlePopulation = withDecoded $ \(PopulationReq script mNaam) ->
  withNamedScript (veiligeNaam mNaam) script $ \fp -> do
    env <- ask
    let metScript =
          set rootFileL (Roots (fp NE.:| []))
            . set recipeL Grind
    result <- local metScript createFspec
    case result of
      Checked fSpec _ ->
        pure . Wai.responseLBS status200 [(hContentType, "application/json")] $
          populationToJSON env fSpec
      Errors errs ->
        pure $
          jsonResp
            status200
            (JSON.object ["ok" .= False, "diagnostics" .= map tshow (NE.toList errs)])

-- Core ----------------------------------------------------------------------

-- | Write @content@ to a unique temp file with extension @ext@, run @action@
--   on it, and clean up the file afterwards.
-- De naam wordt uit de inhoud afgeleid en niet willekeurig gekozen. Dat is geen
-- detail: de compiler legt van elk element de oorsprong vast, inclusief het pad
-- van het bronbestand, en leidt daar gegenereerde namen uit af. Met een
-- willekeurige naam levert hetzelfde script bij elk verzoek een andere populatie
-- op, en dan sluit geen enkele round-trip-maat en werkt geen twin op inhoudshash.
-- Twee gelijktijdige verzoeken met dezelfde inhoud delen dan één bestand met
-- dezelfde inhoud, wat geen kwaad kan.
withTmpScript :: String -> Text -> (FilePath -> RIO env a) -> RIO env a
withTmpScript ext content =
  bracket
    ( do
        let fp = "ampersand-serve-" <> show (abs (hash content)) <> ext
        writeFileUtf8 fp content
        pure fp
    )
    (\fp -> removeFile fp `catchAny` const (pure ()))

-- | Schrijf @content@ onder deze basisnaam in een eigen map, draai @action@, en
--   ruim die map daarna op. De map ligt onder de systeem-tijdelijke map en haar
--   naam volgt uit de inhoud, zodat twee gelijke verzoeken hetzelfde pad krijgen
--   en dus dezelfde populatie opleveren. De dienst schrijft nooit in haar eigen
--   werkmap, zodat een naam van de aanroeper geen bestand van iemand anders raakt.
withNamedScript :: FilePath -> Text -> (FilePath -> RIO env a) -> RIO env a
withNamedScript naam content action =
  bracket maak ruimOp (\(_, fp) -> action fp)
  where
    maak = do
      basis <- liftIO getTemporaryDirectory
      let map' = basis </> ("ampersand-serve-" <> show (abs (hash content)))
      createDirectoryIfMissing True map'
      let fp = map' </> naam
      writeFileUtf8 fp content
      pure (map', fp)
    ruimOp (map', _) =
      removeDirectoryRecursive map' `catchAny` const (pure ())

-- | De naam die de aanroeper meegeeft, teruggebracht tot een veilige basisnaam
--   zonder mappen. Zonder naam heet het script `script.adl`, net als bij RAP.
veiligeNaam :: Maybe Text -> FilePath
veiligeNaam mNaam = case mNaam of
  Just n
    | let kaal = T.unpack (T.takeWhileEnd (`notElem` ("/\\" :: String)) n),
      not (null kaal),
      kaal `notElem` [".", ".."] ->
        kaal
  _ -> "script.adl"

-- | Type-check @content@ (written as a @ext@ file) via the daemon parse
--   pipeline and return the resulting messages.
checkText :: (ServeEnv env) => String -> Text -> RIO env [Load]
checkText ext content =
  withTmpScript ext content (\fp -> filter isMessage . fst <$> parseProject fp)

-- | Splice the term into the script as a probe rule, just before the last
--   @ENDCONTEXT@, so the type-checker reports any error in the term in context.
--   A rule @e |- e@ is well-typed exactly when @e@ is, and pins down its type.
spliceProbe :: Text -> Text -> Text
spliceProbe script term =
  case splitOn "ENDCONTEXT" script of
    [single] -> single <> probe
    parts ->
      T.intercalate "ENDCONTEXT" (initSafe parts)
        <> probe
        <> "ENDCONTEXT"
        <> lastSafe parts
  where
    probe =
      "\nRULE AmpersandDaemonProbe : ("
        <> term
        <> ") |- ("
        <> term
        <> ")\n"
    initSafe xs = case reverse xs of
      (_ : rest) -> reverse rest
      [] -> []
    lastSafe xs = case reverse xs of
      (x : _) -> x
      [] -> ""

-- Helpers -------------------------------------------------------------------

withDecoded ::
  (JSON.FromJSON a) =>
  (a -> RIO env Wai.Response) ->
  BL.ByteString ->
  RIO env Wai.Response
withDecoded action body = case JSON.eitherDecode body of
  Left e -> pure $ jsonResp status400 (JSON.object ["error" .= T.pack e])
  Right a -> action a

-- | @{ "ok": Bool, "diagnostics": [Load], "term"?: Text }@.
--   @ok@ reflects the absence of /errors/; warnings are reported but do not
--   make the result not-ok.
resultResp :: [Load] -> Maybe Text -> Wai.Response
resultResp msgs mTerm =
  jsonResp status200 . JSON.object $
    [ "ok" .= not (any isErrorMsg msgs),
      "diagnostics" .= msgs
    ]
      <> maybe [] (\t -> ["term" .= t]) mTerm

isErrorMsg :: Load -> Bool
isErrorMsg Message {loadSeverity = Error} = True
isErrorMsg _ = False

jsonResp :: Status -> JSON.Value -> Wai.Response
jsonResp st v =
  Wai.responseLBS st [(hContentType, "application/json")] (JSON.encode v)
