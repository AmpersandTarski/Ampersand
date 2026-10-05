-- | Tests of the HTTP/JSON service @ampersand serve@.
--
--   Every endpoint is asked through the service itself, with the same
--   environment that the command builds, but without a web server. So the
--   tests see the status and the JSON document that a caller gets.
--
--   Two properties get a test of their own, because a caller relies on them:
--
--   * equal requests yield equal answers, also when they arrive at the same
--     moment (they share a scratch directory, see 'Ampersand.Commands.Serve');
--   * the service writes its scratch files outside its working directory.
module Ampersand.Test.Serve.ServeTest
  ( serveTest,
  )
where

import Ampersand.Basics
import Ampersand.Commands.Serve (Reply, newService, safeName, spliceProbe)
import Ampersand.Misc.HasClasses (DaemonOpts (..))
import Ampersand.Options.FSpecGenOptsParser (defFSpecGenOpts)
import Ampersand.Types.Config (HasRunner, extendWith)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import Network.HTTP.Types (Method, statusCode)
import qualified RIO.ByteString.Lazy as BL
import RIO.Directory (doesDirectoryExist)
import RIO.FilePath (takeDirectory, takeFileName)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

-- | A script that type-checks.
goodScript :: Text
goodScript =
  T.unlines
    [ "CONTEXT Library IN ENGLISH",
      "RELATION owner[Book*Person] [UNI]",
      "RELATION title[Book*Title] [UNI]",
      "POPULATION owner CONTAINS [ (\"b1\",\"ann\") ]",
      "ENDCONTEXT"
    ]

-- | A script with one type error, on line 3: the composition @owner;owner@.
badScript :: Text
badScript =
  T.unlines
    [ "CONTEXT Library IN ENGLISH",
      "RELATION owner[Book*Person]",
      "RULE r : owner;owner |- owner",
      "ENDCONTEXT"
    ]

-- | Run all tests of the service. Returns 'True' when everything passes.
serveTest :: (HasRunner env) => RIO env Bool
serveTest = do
  logInfo "Starting tests of the HTTP/JSON service (ampersand serve)."
  results <- extendWith daemonOpts $ do
    env <- ask
    service <- liftIO $ newService env
    liftIO $ (pureChecks <>) <$> endpointChecks service
  let failed = [failedCheck | (failedCheck, False) <- results]
  mapM_ (\failedCheck -> logError $ "  failed: " <> display failedCheck) failed
  if null failed
    then logInfo $ "✅ Passed: " <> display (length results) <> " checks of the HTTP/JSON service."
    else logError "❗❗❗ Failed: HTTP/JSON service."
  pure (null failed)
  where
    daemonOpts =
      DaemonOpts
        { xdaemonConfig = ".ampersand",
          x2fSpecGenOpts = defFSpecGenOpts ("script.adl" NE.:| []),
          xshowWarnings = True
        }

-- | The checks that need no service: the two pure functions a request passes through.
pureChecks :: [(Text, Bool)]
pureChecks =
  [ ( "spliceProbe puts the probe rule before the closing ENDCONTEXT",
      spliceProbe "CONTEXT C\nENDCONTEXT\n" "r"
        == "CONTEXT C\n\nRULE AmpersandDaemonProbe : (r) |- (r)\nENDCONTEXT\n"
    ),
    ( "spliceProbe uses the last ENDCONTEXT when the word occurs earlier",
      spliceProbe "CONTEXT C -- ENDCONTEXT\nENDCONTEXT" "r"
        == "CONTEXT C -- ENDCONTEXT\n\nRULE AmpersandDaemonProbe : (r) |- (r)\nENDCONTEXT"
    ),
    ( "spliceProbe appends the probe rule to a script without ENDCONTEXT",
      spliceProbe "CONTEXT C" "r" == "CONTEXT C\nRULE AmpersandDaemonProbe : (r) |- (r)\n"
    ),
    ("safeName keeps a plain file name", safeName (Just "Library.adl") == "Library.adl"),
    ("safeName drops directories", safeName (Just "a/b/Library.adl") == "Library.adl"),
    ("safeName drops a path that climbs out", safeName (Just "../../etc/passwd") == "passwd"),
    ("safeName drops Windows directories", safeName (Just "..\\Library.adl") == "Library.adl"),
    ("safeName refuses ..", safeName (Just "..") == "script.adl"),
    ("safeName refuses a name that ends in a slash", safeName (Just "a/") == "script.adl"),
    ("safeName refuses the empty name", safeName (Just "") == "script.adl"),
    ("safeName has a default", safeName Nothing == "script.adl")
  ]

endpointChecks :: (Method -> [Text] -> BL.ByteString -> IO Reply) -> IO [(Text, Bool)]
endpointChecks service = do
  health <- ask' "GET" ["health"] JSON.Null
  unknown <- ask' "GET" ["nope"] JSON.Null
  malformed <- post "check" ["nope" JSON..= True]
  checkGood <- post "check" ["script" JSON..= goodScript]
  checkBad <- post "check" ["script" JSON..= badScript]
  translateGood <- post "translate" ["script" JSON..= goodScript, "term" JSON..= ("owner;owner~" :: Text)]
  translateBad <- post "translate" ["script" JSON..= goodScript, "term" JSON..= ("owner;owner" :: Text)]
  fspecEmpty <- post "fspec" ["dump" JSON..= ("{}" :: Text)]
  importEmpty <- post "import" ["dump" JSON..= ("{}" :: Text)]
  population1 <- post "population" ["script" JSON..= goodScript]
  population2 <- post "population" ["script" JSON..= goodScript]
  populationBad <- post "population" ["script" JSON..= badScript]
  simultaneous <- replicateConcurrently 8 (post "check" ["script" JSON..= goodScript])
  let scratchDirs = map takeDirectory (diagnosticFiles checkBad)
  scratchLeft <- or <$> mapM doesDirectoryExist scratchDirs
  pure
    [ ("GET /health answers 200 with status ok", fst health == 200 && field "status" health == Just (JSON.String "ok")),
      ("an unknown path answers 404", fst unknown == 404),
      ("a body without the expected key answers 400", fst malformed == 400),
      ("/check accepts a correct script", fst checkGood == 200 && isOk checkGood),
      ("/check rejects a script with a type error", fst checkBad == 200 && isNotOk checkBad),
      ("/check reports the error on the line where it is", diagnosticLines checkBad == [JSON.Number 3]),
      ("/translate accepts a well-typed term", isOk translateGood),
      ("/translate echoes the term", field "term" translateGood == Just (JSON.String "owner;owner~")),
      ("/translate rejects an ill-typed term", isNotOk translateBad),
      ("/fspec rejects a dump that is no Atlas population", isNotOk fspecEmpty),
      ("/import rejects a dump that is no Atlas population", isNotOk importEmpty),
      ("/population yields the atoms of the population", isJust (field "atoms" population1)),
      ("/population yields the same answer for the same script", snd population1 == snd population2),
      ("/population rejects a script with a type error", isNotOk populationBad),
      ("equal requests at the same moment all get the answer", all isOk simultaneous),
      ( "a script is written in a scratch directory of its own",
        not (null scratchDirs) && all (L.isPrefixOf "ampersand-serve-" . takeFileName) scratchDirs
      ),
      ("the scratch directory is gone after the request", not scratchLeft)
    ]
  where
    post endpoint = ask' "POST" [endpoint] . JSON.object
    ask' method path body = first statusCode <$> service method path (JSON.encode body)

-- | A response as the tests look at it: the status code and the body.
type Answer = (Int, BL.ByteString)

field :: JSON.Key -> Answer -> Maybe JSON.Value
field key (_, body) = case JSON.decode body of
  Just (JSON.Object o) -> KM.lookup key o
  _ -> Nothing

isOk, isNotOk :: Answer -> Bool
isOk answer = field "ok" answer == Just (JSON.Bool True)
isNotOk answer = field "ok" answer == Just (JSON.Bool False)

-- | One field of every structured diagnostic in the answer.
diagnosticFields :: JSON.Key -> Answer -> [JSON.Value]
diagnosticFields key answer = case field "diagnostics" answer of
  Just (JSON.Array ds) -> [v | JSON.Object d <- toList ds, Just v <- [KM.lookup key d]]
  _ -> []

diagnosticLines :: Answer -> [JSON.Value]
diagnosticLines = diagnosticFields "line"

diagnosticFiles :: Answer -> [FilePath]
diagnosticFiles answer = [T.unpack f | JSON.String f <- diagnosticFields "file" answer]
