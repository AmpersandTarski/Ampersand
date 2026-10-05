{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An HTTP/JSON server front-end on the Ampersand daemon core.
--
-- It exposes the same parse+typecheck pipeline as @ampersand daemon@
-- (`Ampersand.Daemon.Parser.parseProject`), but driven by HTTP requests
-- instead of file-watching, and returning structured `Load` diagnostics as
-- JSON instead of rendering to a terminal. The daemon core thus has two
-- front-ends: the file watcher for an editor, and this service for callers
-- such as RAP that have a script in memory rather than on disk.
--
-- Endpoints:
--
--   * @GET  \/health@                     — liveness probe
--   * @POST \/check@      {script}        — type-check a full ADL script
--   * @POST \/translate@  {script,term}   — type-check a single term in the script's context
--   * @POST \/fspec@      {dump}          — validate an Atlas population dump
--   * @POST \/import@     {dump}          — reconstruct an .adl script from an Atlas population dump
--   * @POST \/population@ {script[,name]} — the population of the script in terms of FormalAmpersand
module Ampersand.Commands.Serve
  ( runServe,
    newService,
    Reply,
    spliceProbe,
    safeName,
  )
where

import Ampersand.Basics
import Ampersand.Core.ShowPStruct (showP)
import Ampersand.Daemon.Parser (parseProject)
import Ampersand.Daemon.Types (Load (..), Severity (..), isMessage)
import Ampersand.FSpec.ToFSpec.CreateFspec (createFspec)
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.AtlasImport (parseJsonFile)
import Ampersand.Misc.HasClasses
import Ampersand.Output.ToJSON.ToJson (populationToJSON)
import Ampersand.Types.Config (ExtendedRunner, HasRunner)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as JSON
import Data.Text (splitOn)
import Network.HTTP.Types (Method, Status, hContentType, status200, status400, status404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified RIO.ByteString.Lazy as BL
import RIO.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import RIO.FilePath ((</>))
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
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

-- | The scratch directories that a request is working in right now. A request
--   waits while another request holds the directory it needs; see 'withScript'.
type Busy = TVar (Set FilePath)

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
  service <- liftIO $ newService env
  liftIO $ Warp.run port (waiApp service)

-- | What the service answers to a request: a status and a JSON document.
type Reply = (Status, BL.ByteString)

-- | The service itself: a method, a path and a request body in, a reply out.
--   The web server wraps it, and the test suite asks it without a web server.
newService :: (ServeEnv env) => env -> IO (Method -> [Text] -> BL.ByteString -> IO Reply)
newService env = do
  busy <- newTVarIO Set.empty
  pure $ \method path body -> case (method, path) of
    ("GET", ["health"]) ->
      pure $ jsonResp status200 (JSON.object ["status" .= ("ok" :: Text)])
    ("POST", ["check"]) -> runRIO env (handleCheck busy body)
    ("POST", ["translate"]) -> runRIO env (handleTranslate busy body)
    ("POST", ["fspec"]) -> runRIO env (handleFspec busy body)
    ("POST", ["import"]) -> runRIO env (handleImport busy body)
    ("POST", ["population"]) -> runRIO env (handlePopulation busy body)
    _ -> pure $ jsonResp status404 (JSON.object ["error" .= ("not found" :: Text)])

waiApp :: (Method -> [Text] -> BL.ByteString -> IO Reply) -> Wai.Application
waiApp service req respond = do
  body <- Wai.strictRequestBody req
  (status, reply) <- service (Wai.requestMethod req) (Wai.pathInfo req) body
  respond $ Wai.responseLBS status [(hContentType, "application/json")] reply

-- Request bodies ------------------------------------------------------------

newtype CheckReq = CheckReq Text

instance JSON.FromJSON CheckReq where
  parseJSON = JSON.withObject "CheckReq" $ \v -> CheckReq <$> v .: "script"

-- | Next to the script, @\/population@ takes an optional file name. The name
--   matters, because the compiler records the origin of every element and
--   derives generated names from it. A caller who wants the same population as
--   the command yields on a file of that name, passes the name along.
data PopulationReq = PopulationReq Text (Maybe Text)

instance JSON.FromJSON PopulationReq where
  parseJSON = JSON.withObject "PopulationReq" $ \v ->
    PopulationReq <$> v .: "script" <*> v .:? "name"

newtype FspecReq = FspecReq Text

instance JSON.FromJSON FspecReq where
  parseJSON = JSON.withObject "FspecReq" $ \v -> FspecReq <$> v .: "dump"

data TranslateReq = TranslateReq Text Text

instance JSON.FromJSON TranslateReq where
  parseJSON = JSON.withObject "TranslateReq" $ \v ->
    TranslateReq <$> v .: "script" <*> v .: "term"

-- Handlers ------------------------------------------------------------------

handleCheck :: (ServeEnv env) => Busy -> BL.ByteString -> RIO env Reply
handleCheck busy = withDecoded $ \(CheckReq script) -> do
  msgs <- checkText busy "script.adl" script
  pure $ resultResp msgs Nothing

handleFspec :: (ServeEnv env) => Busy -> BL.ByteString -> RIO env Reply
handleFspec busy = withDecoded $ \(FspecReq dump) -> do
  -- The parser dispatches a .json file to the Atlas importer, so checking the
  -- dump as a .json file validates it as an Atlas population.
  msgs <- checkText busy "script.json" dump
  pure $ resultResp msgs Nothing

handleTranslate :: (ServeEnv env) => Busy -> BL.ByteString -> RIO env Reply
handleTranslate busy = withDecoded $ \(TranslateReq script term) -> do
  msgs <- checkText busy "script.adl" (spliceProbe script term)
  pure $ resultResp msgs (Just term)

-- | Reconstruct an .adl script from an Atlas population dump. Reuses the
--   compiler's AtlasImport (JSON -> P_Context) and the pretty-printer (showP),
--   so the way back from population to text has no second implementation.
handleImport :: Busy -> BL.ByteString -> RIO env Reply
handleImport busy = withDecoded $ \(FspecReq dump) -> do
  result <- withScript busy "script.json" dump parseJsonFile
  pure $ case result of
    Checked ctx _ ->
      jsonResp status200 (JSON.object ["ok" .= True, "adl" .= showP ctx])
    Errors errs ->
      jsonResp status200 (JSON.object ["ok" .= False, "diagnostics" .= map tshow (NE.toList errs)])

-- | A script as text in, its population in terms of FormalAmpersand as JSON
--   out. This is what @ampersand population --build-recipe Grind
--   --output-format json@ writes to a file. Offering it here lets a caller
--   such as RAP obtain the population without a compiler in its own image.
handlePopulation :: (ServeEnv env) => Busy -> BL.ByteString -> RIO env Reply
handlePopulation busy = withDecoded $ \(PopulationReq script mName) ->
  withScript busy (safeName mName) script $ \fp -> do
    env <- ask
    let withThisScript =
          set rootFileL (Roots (fp NE.:| []))
            . set recipeL Grind
    result <- local withThisScript createFspec
    case result of
      Checked fSpec _ ->
        pure (status200, populationToJSON env fSpec)
      Errors errs ->
        pure
          $ jsonResp
            status200
            (JSON.object ["ok" .= False, "diagnostics" .= map tshow (NE.toList errs)])

-- Core ----------------------------------------------------------------------

-- | Write @content@ to a file called @fileName@ in a scratch directory of its own,
--   run @action@ on that file, and remove the directory afterwards.
--
--   The directory lies under the system's temporary directory, never in the
--   working directory of the service, so a name chosen by the caller cannot
--   touch a file that belongs to someone else.
--
--   The name of the directory is derived from the content rather than chosen at
--   random. The compiler records the origin of every element, including the
--   path of the source file, and derives generated names from it. With a random
--   path the same script would yield a different population on every request;
--   with this path, equal requests yield equal results.
--
--   Equal requests therefore share a directory. A request that finds its
--   directory in use waits until the other request has removed it, so no
--   request ever sees its script disappear halfway.
withScript :: Busy -> FilePath -> Text -> (FilePath -> RIO env a) -> RIO env a
withScript busy fileName content action = do
  base <- liftIO getTemporaryDirectory
  let dir = base </> ("ampersand-serve-" <> show (abs (hash content)))
      claim = atomically $ do
        inUse <- readTVar busy
        checkSTM (dir `Set.notMember` inUse)
        writeTVar busy (Set.insert dir inUse)
      release = do
        removeDirectoryRecursive dir `catchAny` const (pure ())
        atomically $ modifyTVar' busy (Set.delete dir)
  bracket_ claim release $ do
    createDirectoryIfMissing True dir
    let fp = dir </> fileName
    writeFileUtf8 fp content
    action fp

-- | The file name that the caller passes, reduced to a base name without
--   directories. Without a usable name the script is called @script.adl@.
safeName :: Maybe Text -> FilePath
safeName mName = case mName of
  Just n
    | let bare = T.unpack (T.takeWhileEnd (`notElem` ("/\\" :: String)) n),
      not (null bare),
      bare `notElem` [".", ".."] ->
        bare
  _ -> "script.adl"

-- | Type-check @content@ (written to a file called @fileName@) via the daemon
--   parse pipeline and return the resulting messages.
checkText :: (ServeEnv env) => Busy -> FilePath -> Text -> RIO env [Load]
checkText busy fileName content =
  withScript busy fileName content (fmap (filter isMessage . fst) . parseProject)

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
  (a -> RIO env Reply) ->
  BL.ByteString ->
  RIO env Reply
withDecoded action body = case JSON.eitherDecode body of
  Left e -> pure $ jsonResp status400 (JSON.object ["error" .= T.pack e])
  Right a -> action a

-- | @{ "ok": Bool, "diagnostics": [Load], "term"?: Text }@.
--   @ok@ reflects the absence of /errors/; warnings are reported but do not
--   make the result not-ok.
resultResp :: [Load] -> Maybe Text -> Reply
resultResp msgs mTerm =
  jsonResp status200
    . JSON.object
    $ [ "ok" .= not (any isErrorMsg msgs),
        "diagnostics" .= msgs
      ]
    <> maybe [] (\t -> ["term" .= t]) mTerm

isErrorMsg :: Load -> Bool
isErrorMsg Message {loadSeverity = Error} = True
isErrorMsg _ = False

jsonResp :: Status -> JSON.Value -> Reply
jsonResp st v = (st, JSON.encode v)
