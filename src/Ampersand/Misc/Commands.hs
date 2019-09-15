{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Ampersand.Misc.Commands
        ( commandLineHandler
        , ExtendedRunner
        , DaemonOpts
        )
where
import           Ampersand.Basics
import           Ampersand.Commands.Proto
import           Ampersand.Commands.Daemon
import           Ampersand.Commands.Documentation
import           Ampersand.Commands.Devoutput
import           Ampersand.Commands.ExportAsADL
import           Ampersand.Commands.Init
import           Ampersand.Commands.Population
import           Ampersand.Commands.Proof
import           Ampersand.Commands.Uml
import           Ampersand.Commands.Validate
import           Ampersand.FSpec.ToFSpec.CreateFspec
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Misc.HasClasses
import           Ampersand.Options.GlobalParser
import           Ampersand.Options.ProtoParser
import           Ampersand.Options.DaemonParser
import           Ampersand.Options.InputOutputOpts
import           Ampersand.Options.DocOptsParser
import           Ampersand.Types.Config
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Writer
--import           Generics.Deriving.Monoid (memptydefault, mappenddefault)
import           Options.Applicative
import           Options.Applicative.Builder.Internal
import           Options.Applicative.Help hiding (fullDesc)
import           Options.Applicative.Types
import           Options.Applicative.Common
import qualified RIO.List as L
import           RIO.Char 
import qualified Data.List.NonEmpty as NEL
import           System.Environment ({-getProgName,-} withArgs)

--import           System.FilePath (isValid, pathSeparator, takeDirectory)

-- A lot of inspiration in this file comes from https://github.com/commercialhaskell/stack/

-- Vertically combine only the error component of the first argument with the
-- error component of the second.
--vcatErrorHelp :: ParserHelp -> ParserHelp -> ParserHelp
--vcatErrorHelp h1 h2 = h2 { helpError = vcatChunks [helpError h2, helpError h1] }

commandLineHandler
  :: FilePath
  -> String -- the name of the program
  -> [String] -- the (command-line) arguments
  -> IO (GlobalOptsMonoid, RIO Runner ())
commandLineHandler currentDir _progName args = complicatedOptions
  ampersandVersionWithoutBuildTimeStr
  "ampersand - The Ampersand generator"
  ""
  "ampersand's documentation is available at https://ampersandtarski.gitbook.io/documentation/"
  args
  (globalOpts)
  (Just failureCallback)
  addCommands
  where
    failureCallback :: 
           ParserFailure ParserHelp
        -> [String] 
        -> IO (GlobalOptsMonoid, (RIO Runner (), t))
    failureCallback f _ = parseResultHandler f

    parseResultHandler :: ParserFailure ParserHelp -> IO a
    parseResultHandler f = handleParseResult (Failure f)
    
    addCommands :: ExceptT
                      (RIO Runner ())
                      (Writer (Mod CommandFields (RIO Runner (), GlobalOptsMonoid)))
                      ()
    addCommands = do
      addCommand'' Daemon
                  "Use ampersand to check your model while you modify it."
                  daemonCmd
                  daemonOptsParser
      addCommand'' Documentation
                  ( "Generate a functional design document, to kick-start your "
                  <>"functional specification.")
                  documentationCmd
                  docOptsParser
      addCommand'' Proto
                  "Generate a prototype from your specification."
                  protoCmd
                  (protoOptsParser "DEFAULTDATABASENAME")
      addCommand'' Dataanalysis
                  ( "Export a data model as plain Ampersand script, for "
                  <>"analysing Excel-data.")
                  dataAnalysisCmd
                  (outputFileOptsParser "MetaModel.adl")
      where
        -- addCommand hiding global options
        addCommand'' :: Command -> String -> (a -> RIO Runner ()) -> Parser a
                    -> AddCommand
        addCommand'' cmd title constr =
            addCommand (map toLower . show $ cmd) title globalFooter constr (\_ gom -> gom) globalOpts

--        addSubCommands' :: String -> String -> AddCommand
--                        -> AddCommand
--        addSubCommands' cmd title =
--            addSubCommands cmd title globalFooter globalOpts



    globalOpts :: Parser GlobalOptsMonoid
    globalOpts =
      --  extraHelpOption hide progName (Docker.dockerCmdName ++ "*") Docker.dockerHelpOptName <*>
      --  extraHelpOption hide progName (Nix.nixCmdName ++ "*") Nix.nixHelpOptName <*>
        globalOptsParser currentDir Nothing

    globalFooter = "Run 'ampersand --help' for global options that apply to all subcommands."



type AddCommand =
    ExceptT (RIO Runner ()) (Writer (Mod CommandFields (RIO Runner (), GlobalOptsMonoid))) ()



-- | Generate and execute a complicated options parser.
complicatedOptions
  :: Monoid a
  => String
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description (displayed between usage and options listing in the help output)
  -> String
  -- ^ footer
  -> [String]
  -- ^ command-line arguments (unparsed)
  -> Parser a
  -- ^ common settings
  -> Maybe (ParserFailure ParserHelp -> [String] -> IO (a,(b,a)))
  -- ^ optional handler for parser failure; 'handleParseResult' is called by
  -- default
  -> ExceptT b (Writer (Mod CommandFields (b,a))) ()
  -- ^ commands (use 'addCommand')
  -> IO (a,b)
complicatedOptions stringVersion h pd footerStr args commonParser mOnFailure commandParser = do
     runSimpleApp $ do
          logDebug $ displayShow helpDoc'
     (a,(b,c)) <- case execParserPure (prefs noBacktrack) parser args of
       Failure _ | null args -> withArgs ["--help"] (execParser parser)
       -- call onFailure handler if it's present and parsing options failed
       Failure f | Just onFailure <- mOnFailure -> onFailure f args
       parseResult -> handleParseResult parseResult
     return (mappend c a,b)
  where helpDoc' :: Doc
        helpDoc' = 
                  fromMaybe (fatal "help could not be generated")
                . unChunk
                . vsepChunks
                . mapParser myDescriptionFunction
                $ infoParser parser
        myDescriptionFunction :: OptHelpInfo -> Option x -> Chunk Doc
        myDescriptionFunction _info' opt = dullyellow <$>
                paragraph (show opt) -- optHelp opt -- "Een of andere optie."
        parser = info (helpOption <*> versionOptions <*> complicatedParser "COMMAND" commonParser commandParser) desc
        desc = fullDesc <> header h <> progDesc pd <> footer footerStr
        versionOptions = versionOption stringVersion
        versionOption s =
          infoOption
            s
            (long "version" <>
             help "Show version")

-- | Add a command to the options dispatcher.
addCommand :: String   -- ^ command string
           -> String   -- ^ title of command
           -> String   -- ^ footer of command help
           -> (a -> b) -- ^ constructor to wrap up command in common data type
           -> (a -> c -> c) -- ^ extend common settings from local settings
           -> Parser c -- ^ common parser
           -> Parser a -- ^ command parser
           -> ExceptT b (Writer (Mod CommandFields (b,c))) ()
addCommand cmd title footerStr constr extendCommon =
  addCommand' cmd title footerStr (\a c -> (constr a,extendCommon a c))

-- -- | Add a command that takes sub-commands to the options dispatcher.
-- addSubCommands
--   :: Monoid c
--   => String
--   -- ^ command string
--   -> String
--   -- ^ title of command
--   -> String
--   -- ^ footer of command help
--   -> Parser c
--   -- ^ common parser
--   -> ExceptT b (Writer (Mod CommandFields (b,c))) ()
--   -- ^ sub-commands (use 'addCommand')
--   -> ExceptT b (Writer (Mod CommandFields (b,c))) ()
-- addSubCommands cmd title footerStr commonParser commandParser =
--   addCommand' cmd
--               title
--               footerStr
--               (\(c1,(a,c2)) c3 -> (a,mconcat [c3, c2, c1]))
--               commonParser
--               (complicatedParser "COMMAND" commonParser commandParser)

-- | Add a command to the options dispatcher.
addCommand' :: String   -- ^ command string
            -> String   -- ^ title of command
            -> String   -- ^ footer of command help
            -> (a -> c -> (b,c)) -- ^ constructor to wrap up command in common data type
            -> Parser c -- ^ common parser
            -> Parser a -- ^ command parser
            -> ExceptT b (Writer (Mod CommandFields (b,c))) ()
addCommand' cmd title footerStr constr commonParser inner =
  lift (tell (command cmd
                      (info (constr <$> inner <*> commonParser)
                            (progDesc title <> footer footerStr))))


-- | Generate a complicated options parser.
complicatedParser
  :: Monoid a
  => String
  -- ^ metavar for the sub-command
  -> Parser a
  -- ^ common settings
  -> ExceptT b (Writer (Mod CommandFields (b,a))) ()
  -- ^ commands (use 'addCommand')
  -> Parser (a,(b,a))
complicatedParser commandMetavar commonParser commandParser =
   (,) <$>
   commonParser <*>
   case runWriter (runExceptT commandParser) of
     (Right (),d) -> hsubparser' commandMetavar d
     (Left b,_) -> pure (b,mempty)

-- | Subparser with @--help@ argument. Borrowed with slight modification
-- from Options.Applicative.Extra.
hsubparser' :: String -> Mod CommandFields a -> Parser a
hsubparser' commandMetavar m = mkParser d g rdr
  where
    Mod _ d g = metavar commandMetavar `mappend` m
    (groupName, cmds, subs) = mkCommand m
    rdr = CmdReader groupName cmds (fmap add_helper . subs)
    add_helper pinfo = pinfo
      { infoParser = infoParser pinfo <**> helpOption }

-- | Non-hidden help option.
helpOption :: Parser (a -> a)
helpOption =
    abortOption ShowHelpText $
    long "help" <>
    help "Show this help text"

daemonCmd :: DaemonOpts -> RIO Runner ()
daemonCmd daemonOpts = 
    extendWith daemonOpts        
       runDaemon 
documentationCmd :: DocOpts -> RIO Runner ()
documentationCmd docOpts =
    extendWith docOpts $ do
        let recipe = []
        mFSpec <- createFspec recipe
        doOrDie mFSpec doGenDocument

-- | Create a prototype based on the current script.
protoCmd :: ProtoOpts -> RIO Runner ()
protoCmd protoOpts = 
    extendWith protoOpts $ do
        let recipe = []
        mFSpec <- createFspec recipe
        doOrDie mFSpec proto

dataAnalysisCmd :: InputOutputOpts -> RIO Runner ()
dataAnalysisCmd opts = 
    extendWith opts $ do
        let recipe = []
        mFSpec <- createFspec recipe
        doOrDie mFSpec dataAnalysis

populationCmd :: PopulationOpts -> RIO Runner ()
populationCmd opts = 
    extendWith opts $ do
        let recipe = []
        mFSpec <- createFspec recipe
        doOrDie mFSpec population

proofCmd :: ProofOpts -> RIO Runner ()
proofCmd opts = 
    extendWith opts $ do
        let recipe = []
        mFSpec <- createFspec recipe
        doOrDie mFSpec proof

initCmd :: InitOpts -> RIO Runner ()
initCmd opts = 
    extendWith opts $ do
        let recipe = []
        mFSpec <- createFspec recipe
        doOrDie mFSpec init

umlCmd :: UmlOpts -> RIO Runner ()
umlCmd opts = 
    extendWith opts $ do
        let recipe = []
        mFSpec <- createFspec recipe
        doOrDie mFSpec uml

validateCmd :: ValidateOpts -> RIO Runner ()
validateCmd opts = 
    extendWith opts $ do
        let recipe = []
        mFSpec <- createFspec recipe
        doOrDie mFSpec validate

devoutputCmd :: DevOutputOpts -> RIO Runner ()
devoutputCmd opts = 
    extendWith opts $ do
        let recipe = []
        mFSpec <- createFspec recipe
        doOrDie mFSpec devoutput

doOrDie :: HasLogFunc env => Guarded a -> (a -> RIO env b) -> RIO env b
doOrDie gA act = 
  case gA of
    Checked a ws -> do
      showWarnings ws
      act a
    Errors err -> exitWith . NoValidFSpec . L.intersperse  (replicate 30 '=') 
           . fmap show . NEL.toList $ err
  where
    showWarnings ws = mapM_ logWarn (fmap displayShow ws)  


data Command = 
        Daemon
      | Dataanalysis
      | Devoutput
      | Documentation
      | Fpa
      | Init
      | Population
      | Proto 
      | PPrint
      | Uml
      | Validate deriving Show
