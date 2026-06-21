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
module Ampersand.Commands.Serve (runServe) where

import Ampersand.Basics
import Ampersand.Daemon.Parser (parseProject)
import Ampersand.Daemon.Types (Load (..), Severity (..), isMessage)
import Ampersand.Misc.HasClasses
import Ampersand.Types.Config (ExtendedRunner, HasRunner)
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.=))
import Data.Text (splitOn)
import Network.HTTP.Types (Status, hContentType, status200, status400, status404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified RIO.ByteString.Lazy as BL
import RIO.Directory (removeFile)
import qualified RIO.Text as T
import System.Environment (lookupEnv)
import System.IO (openTempFile)

-- | The environment constraints needed to run the daemon parse pipeline.
type ServeEnv env =
  ( HasDirOutput env,
    HasTrimXLSXOpts env,
    HasDaemonOpts env,
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
  logInfo "  GET /health | POST /check {script} | POST /translate {script,term} | POST /fspec {dump}"
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
    _ -> pure $ jsonResp status404 (JSON.object ["error" .= ("not found" :: Text)])
  respond resp

-- Request bodies ------------------------------------------------------------

newtype CheckReq = CheckReq Text

instance JSON.FromJSON CheckReq where
  parseJSON = JSON.withObject "CheckReq" $ \v -> CheckReq <$> v .: "script"

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

-- Core ----------------------------------------------------------------------

-- | Write @content@ to a unique temp file with extension @ext@, run the
--   daemon parse pipeline on it, and return the resulting messages.
checkText :: (ServeEnv env) => String -> Text -> RIO env [Load]
checkText ext content =
  bracket
    ( do
        (fp, h) <- liftIO $ openTempFile "." ("ampersand-serve" <> ext)
        hClose h
        writeFileUtf8 fp content
        pure fp
    )
    (\fp -> removeFile fp `catchAny` const (pure ()))
    (\fp -> filter isMessage . fst <$> parseProject fp)

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
