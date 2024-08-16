{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Ampersand.Misc.Commands
  ( commandLineHandler,
    ExtendedRunner,
    DaemonOpts,
  )
where

import Ampersand.Basics
import Ampersand.Commands.Daemon
import Ampersand.Commands.Devoutput
import Ampersand.Commands.Documentation
import Ampersand.Commands.ExportAsADL
import Ampersand.Commands.Population
import Ampersand.Commands.Proof
import Ampersand.Commands.Proto
import Ampersand.Commands.Test
import Ampersand.Commands.Uml
import Ampersand.Commands.Validate
{-getProgName,-}
import Ampersand.FSpec (FSpec)
import Ampersand.FSpec.ToFSpec.CreateFspec
import Ampersand.Input.ADL1.CtxError
import Ampersand.Misc.HasClasses
import Ampersand.Options.DaemonParser
import Ampersand.Options.DevoutputOptsParser
import Ampersand.Options.DocOptsParser
import Ampersand.Options.FSpecGenOptsParser
import Ampersand.Options.GlobalParser
import Ampersand.Options.InputOutputOpts
import Ampersand.Options.PopulationOptsParser
import Ampersand.Options.ProofOptsParser
import Ampersand.Options.ProtoOptsParser
import Ampersand.Options.TestOptsParser
import Ampersand.Options.UmlOptsParser
import Ampersand.Options.ValidateOptsParser
import Ampersand.Types.Config
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Options.Applicative
import Options.Applicative.Builder.Internal hiding (name)
import Options.Applicative.Common
import Options.Applicative.Help hiding (fullDesc)
import Options.Applicative.Types
import RIO.Char
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import System.Environment (withArgs)

-- A lot of inspiration in this file comes from https://github.com/commercialhaskell/stack/

-- Vertically combine only the error component of the first argument with the
-- error component of the second.
-- vcatErrorHelp :: ParserHelp -> ParserHelp -> ParserHelp
-- vcatErrorHelp h1 h2 = h2 { helpError = vcatChunks [helpError h2, helpError h1] }

commandLineHandler ::
  FilePath ->
  Text -> -- the name of the program
  [Text] -> -- the (command-line) arguments
  IO (GlobalOptsMonoid, RIO Runner ())
commandLineHandler currentDir _progName args =
  complicatedOptions
    "ampersand - The Ampersand generator"
    ""
    "ampersand's documentation is available at https://ampersandtarski.github.io/"
    args
    globalOpts
    (Just failureCallback)
    addCommands
  where
    failureCallback ::
      ParserFailure ParserHelp ->
      [Text] ->
      IO (GlobalOptsMonoid, (RIO Runner (), t))
    failureCallback f _ = parseResultHandler f

    parseResultHandler :: ParserFailure ParserHelp -> IO a
    parseResultHandler f = handleParseResult (Failure f)

    addCommands ::
      ExceptT
        (RIO Runner ())
        (Writer (Mod CommandFields (RIO Runner (), GlobalOptsMonoid)))
        ()
    addCommands = do
      addCommand''
        Check
        "Use ampersand to check your model for syntactical and type errors."
        checkCmd
        (fSpecGenOptsParser False)
      addCommand''
        Daemon
        "Use ampersand to continuously check your model while you modify it."
        daemonCmd
        daemonOptsParser
      addCommand''
        Dataanalysis
        ( "Create an ADL model based on the content of a spreadsheet. The spreadsheet "
            <> "must comply to the specific format. "
            <> "This is an experimental feature."
        )
        (mkAction exportAsAdl)
        (outputFileOptsParser "MetaModel.adl")
      addCommand''
        Devoutput
        "Generate some diagnostic files, intended for developers of ampersand."
        (mkAction devoutput)
        (devoutputOptsParser ".")
      addCommand''
        Documentation
        ( "Generate a functional design document, to kick-start your "
            <> "functional specification."
        )
        documentationCmd
        docOptsParser
      --      addCommand'' Fpa
      --                  ""
      --                  (mkAction fpa)
      --                  fpaOptsParser
      --      addCommand'' Init
      --                  ""
      --                  (mkAction init)
      --                  initOptsParser
      addCommand''
        Population
        "Generate a file that contains the population of your script."
        (mkAction population)
        populationOptsParser
      addCommand''
        Proofs
        "Generate a report containing proofs."
        (mkAction proof)
        proofOptsParser
      addCommand''
        Proto
        "Generate prototype files from your specification. To be used with the prototype framework."
        (mkAction proto)
        protoOptsParser
      addCommand''
        Export
        "Generate a single .adl file of your script (prettyprinted)"
        (mkAction exportAsAdl)
        (outputFileOptsParser "export.adl")
      addCommand''
        Uml
        "Generate a data model in UML 2.0 style."
        (mkAction uml)
        umlOptsParser
      addCommand''
        Validate
        ( "Compare results of rule evaluation in Haskell and SQL, for "
            <> "testing term semantics. This requires command line php with "
            <> "MySQL support."
        )
        (mkAction validate)
        validateOptsParser
      addCommand''
        Test
        ( "Run testsuites in a given directory. This is meant to do regression testing "
            <> "during automatic build (e.g. Github actions)"
        )
        testCmd
        (testOptsParser ".")
      where
        -- addCommand hiding global options
        addCommand'' ::
          (HasOptions a) =>
          Command ->
          String ->
          (a -> RIO Runner ()) ->
          Parser a ->
          AddCommand
        addCommand'' cmd title constr =
          addCommand (map toLower . show $ cmd) title globalFooter constr' (\_ gom -> gom) globalOpts
          where
            constr' opts = do
              runner <- ask
              logDebug . display $ shortVersion appVersion <> " runs with the following settings:"
              showOptions (runner, opts)
              constr opts

    globalOpts :: Parser GlobalOptsMonoid
    globalOpts =
      --  extraHelpOption hide progName (Docker.dockerCmdName <> "*") Docker.dockerHelpOptName <*>
      --  extraHelpOption hide progName (Nix.nixCmdName <> "*") Nix.nixHelpOptName <*>
      globalOptsParser currentDir Nothing

    globalFooter = "Run 'ampersand --help' for global options that apply to all subcommands."

type AddCommand =
  ExceptT (RIO Runner ()) (Writer (Mod CommandFields (RIO Runner (), GlobalOptsMonoid))) ()

-- | Generate and execute a complicated options parser.
complicatedOptions ::
  (Monoid a) =>
  -- | header
  Text ->
  -- | program description (displayed between usage and options listing in the help output)
  Text ->
  -- | footer
  Text ->
  -- | command-line arguments (unparsed)
  [Text] ->
  -- | common settings
  Parser a ->
  -- | optional handler for parser failure; 'handleParseResult' is called by
  -- default
  Maybe (ParserFailure ParserHelp -> [Text] -> IO (a, (b, a))) ->
  -- | commands (use 'addCommand')
  ExceptT b (Writer (Mod CommandFields (b, a))) () ->
  IO (a, b)
complicatedOptions h pd footerStr args commonParser mOnFailure commandParser = do
  runSimpleApp $ do
    logDebug $ displayShow helpDoc'
  (a, (b, c)) <- case execParserPure myPreferences parser (T.unpack <$> args) of
    Failure _ | null args -> withArgs ["--help"] (execParser parser)
    -- call onFailure handler if it's present and parsing options failed
    Failure f | Just onFailure <- mOnFailure -> onFailure f args
    parseResult -> handleParseResult parseResult
  return (mappend c a, b)
  where
    helpDoc' :: Doc
    helpDoc' =
      fromMaybe (fatal "help could not be generated")
        . unChunk
        . vsepChunks
        . mapParser myDescriptionFunction
        $ infoParser parser
    myPreferences :: ParserPrefs
    myPreferences =
      prefs
        $ showHelpOnEmpty
        <> noBacktrack
        <> disambiguate
    myDescriptionFunction :: ArgumentReachability -> Option x -> Chunk Doc
    myDescriptionFunction _info' opt =
      annotate (colorDull Yellow)
        <$> paragraph (show opt) -- optHelp opt -- "Een of andere optie."
    parser = info (helpOption <*> versionOptions <*> complicatedParser "COMMAND" commonParser commandParser) desc
    desc = fullDesc <> header (T.unpack h) <> progDesc (T.unpack pd) <> footer (T.unpack footerStr)
    versionOptions :: Parser (a -> a)
    versionOptions = normal <*> numeric
      where
        normal :: Parser (a -> a)
        normal =
          infoOption
            (T.unpack $ longVersion appVersion)
            ( short 'V'
                <> long "version"
                <> help "Show version"
            )
        numeric :: Parser (a -> a)
        numeric =
          infoOption
            (T.unpack $ numericVersion appVersion)
            ( long "numeric-version"
                <> help "Show version in numeric format"
            )

-- | Add a command to the options dispatcher.
addCommand ::
  -- | command string
  String ->
  -- | title of command
  String ->
  -- | footer of command help
  String ->
  -- | constructor to wrap up command in common data type
  (a -> b) ->
  -- | extend common settings from local settings
  (a -> c -> c) ->
  -- | common parser
  Parser c ->
  -- | command parser
  Parser a ->
  ExceptT b (Writer (Mod CommandFields (b, c))) ()
addCommand cmd title footerStr constr extendCommon =
  addCommand' cmd title footerStr (\a c -> (constr a, extendCommon a c))

-- -- | Add a command that takes sub-commands to the options dispatcher.
-- addSubCommands
--   :: Monoid c
--   => Text                                             -- ^ command string
--   -> Text                                             -- ^ title of command
--   -> Text                                             -- ^ footer of command help
--   -> Parser c                                         -- ^ common parser
--   -> ExceptT b (Writer (Mod CommandFields (b,c))) ()  -- ^ sub-commands (use 'addCommand')
--   -> ExceptT b (Writer (Mod CommandFields (b,c))) ()
-- addSubCommands cmd title footerStr commonParser commandParser =
--   addCommand' cmd
--               title
--               footerStr
--               (\(c1,(a,c2)) c3 -> (a,mconcat [c3, c2, c1]))
--               commonParser
--               (complicatedParser "COMMAND" commonParser commandParser)

-- | Add a command to the options dispatcher.
addCommand' ::
  -- | command string
  String ->
  -- | title of command
  String ->
  -- | footer of command help
  String ->
  -- | constructor to wrap up command in common data type
  (a -> c -> (b, c)) ->
  -- | common parser
  Parser c ->
  -- | command parser
  Parser a ->
  ExceptT b (Writer (Mod CommandFields (b, c))) ()
addCommand' cmd title footerStr constr commonParser inner =
  lift
    ( tell
        ( command
            cmd
            ( info
                (constr <$> inner <*> commonParser)
                (progDesc title <> footer footerStr)
            )
        )
    )

-- | The Options parser type
-- type OptParser a = (RIO Runner ()) (Writer (Mod CommandFields ((RIO Runner (),a))) ()
-- P.ParsecT [Token] FilePos Identity a -- ^ The Parsec parser for a list of tokens with a file position.

-- | Generate a complicated options parser.
complicatedParser ::
  (Monoid a) =>
  -- | metavar for the sub-command
  String ->
  -- | common settings
  Parser a ->
  -- | commands (use 'addCommand')
  ExceptT b (Writer (Mod CommandFields (b, a))) () ->
  Parser (a, (b, a))
complicatedParser commandMetavar commonParser commandParser =
  (,)
    <$> commonParser
    <*> case runWriter (runExceptT commandParser) of
      (Right (), d) -> hsubparser' commandMetavar d
      (Left b, _) -> pure (b, mempty)

-- | Subparser with @--help@ argument. Borrowed with slight modification
-- from Options.Applicative.Extra.
hsubparser' :: String -> Mod CommandFields a -> Parser a
hsubparser' commandMetavar m = mkParser d g rdr
  where
    Mod _ d g = metavar commandMetavar `mappend` m
    (groupName, cmds) = mkCommand m
    rdr = CmdReader groupName ((fmap . fmap) add_helper cmds)
    add_helper pinfo =
      pinfo
        { infoParser = infoParser pinfo <**> helpOption
        }

-- | Non-hidden help option.
helpOption :: Parser (a -> a)
helpOption =
  abortOption (ShowHelpText $ Just "This is some text, but when does it show??")
    $ long "help"
    <> help "Show this help text"

daemonCmd :: DaemonOpts -> RIO Runner ()
daemonCmd daemonOpts =
  extendWith daemonOpts runDaemon

documentationCmd :: DocOpts -> RIO Runner ()
documentationCmd docOpts = do
  (extendWith docOpts . forceAllowInvariants . doOrDie) doGenDocument
  where
    forceAllowInvariants :: (HasFSpecGenOpts env) => RIO env a -> RIO env a
    forceAllowInvariants = local (set allowInvariantViolationsL True)

testCmd :: TestOpts -> RIO Runner ()
testCmd testOpts =
  extendWith testOpts test

checkCmd :: FSpecGenOpts -> RIO Runner ()
checkCmd = mkAction doNothing
  where
    doNothing fSpec = do
      logInfo $ "This script of " <> (display . fullName) fSpec <> " contains no type errors."

mkAction ::
  (HasFSpecGenOpts a) =>
  (FSpec -> RIO (ExtendedRunner a) ()) ->
  a ->
  RIO Runner ()
mkAction theAction opts =
  extendWith opts $ doOrDie theAction

doOrDie ::
  (HasTrimXLSXOpts env, HasLogFunc env, HasFSpecGenOpts env) =>
  (FSpec -> RIO env b) ->
  RIO env b
doOrDie theAction = do
  mFSpec <- createFspec
  case mFSpec of
    Checked a ws -> do
      mapM_ (logWarn . displayShow) ws
      theAction a
    Errors err ->
      exitWith
        . NoValidFSpec
        . T.lines
        . T.intercalate (T.replicate 30 "=" <> "\n")
        . NE.toList
        . fmap tshow
        $ err

data Command
  = AtlasImport
  | Check
  | Daemon
  | Dataanalysis
  | Devoutput
  | Documentation
  | Export
  | Population
  | Proofs
  | Proto
  | Test
  | Uml
  | Validate

instance Show Command where
  show AtlasImport = "atlas-import"
  show Check = "check"
  show Daemon = "daemon"
  show Dataanalysis = "data-analysis"
  show Devoutput = "dev-output"
  show Documentation = "documentation"
  show Export = "export"
  show Population = "population"
  show Proofs = "proofs"
  show Proto = "proto"
  show Test = "test"
  show Uml = "uml"
  show Validate = "validate"
