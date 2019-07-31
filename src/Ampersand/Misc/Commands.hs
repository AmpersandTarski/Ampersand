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
import           Ampersand.FSpec.ToFSpec.CreateFspec
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Misc.HasClasses
import           Ampersand.Options.GlobalParser
import           Ampersand.Options.ProtoParser
import           Ampersand.Options.DaemonParser
import           Ampersand.Types.Config
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Writer
--import           Generics.Deriving.Monoid (memptydefault, mappenddefault)
import           Options.Applicative
import           Options.Applicative.Builder.Internal
--import           Options.Applicative.Help (errorHelp, stringChunk, vcatChunks)
import           Options.Applicative.Types
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NEL
import           System.Environment ({-getProgName,-} getArgs, withArgs)
--import           System.FilePath (isValid, pathSeparator, takeDirectory)

-- A lot of inspiration in this file comes from https://github.com/commercialhaskell/stack/

-- Vertically combine only the error component of the first argument with the
-- error component of the second.
--vcatErrorHelp :: ParserHelp -> ParserHelp -> ParserHelp
--vcatErrorHelp h1 h2 = h2 { helpError = vcatChunks [helpError h2, helpError h1] }

commandLineHandler
  :: FilePath
  -> String
  -> IO (GlobalOptsMonoid, RIO Runner ())
commandLineHandler currentDir _progName = complicatedOptions
  ampersandVersionWithoutBuildTimeStr
  "ampersand - The Ampersand generator"
  ""
  "ampersand's documentation is available at https://ampersandtarski.gitbook.io/documentation/"
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
      addCommand'' "proto"
                  "Generate a prototype from your specification."
                  protoCmd
                  (protoOptsParser Proto "DEFAULTDATABASENAME")
      addCommand'' "daemon"
                  "Use ampersand to check your model while you modify it."
                  daemonCmd
                  daemonOptsParser
      where
        -- addCommand hiding global options
        addCommand'' :: String -> String -> (a -> RIO Runner ()) -> Parser a
                    -> AddCommand
        addCommand'' cmd title constr =
            addCommand cmd title globalFooter constr (\_ gom -> gom) globalOpts

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
  -> Parser a
  -- ^ common settings
  -> Maybe (ParserFailure ParserHelp -> [String] -> IO (a,(b,a)))
  -- ^ optional handler for parser failure; 'handleParseResult' is called by
  -- default
  -> ExceptT b (Writer (Mod CommandFields (b,a))) ()
  -- ^ commands (use 'addCommand')
  -> IO (a,b)
complicatedOptions stringVersion h pd footerStr commonParser mOnFailure commandParser =
  do args <- getArgs
     (a,(b,c)) <- case execParserPure (prefs noBacktrack) parser args of
       Failure _ | null args -> withArgs ["--help"] (execParser parser)
       -- call onFailure handler if it's present and parsing options failed
       Failure f | Just onFailure <- mOnFailure -> onFailure f args
       parseResult -> handleParseResult parseResult
     return (mappend c a,b)
  where parser = info (helpOption <*> versionOptions <*> complicatedParser "COMMAND|FILE" commonParser commandParser) desc
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
daemonCmd opts = 
    extendWith opts $ do
       
       runDaemon 
-- | Create a prototype based on the current script.
protoCmd :: ProtoOpts -> RIO Runner ()
protoCmd protoOpts = 
    extendWith protoOpts $ do
        let recipe = []
        mFSpec <- createFspec recipe
        doOrDie mFSpec proto

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

-- extendWith :: a -> RIO (ExtendedRunner a) () -> RIO Runner ()
-- extendWith opts inner = do
--    env <- ask
--    runRIO (ExtendedRunner env opts) inner

-- instance (HasOutputLanguage a) => HasOutputLanguage (ExtendedRunner a) where
--   languageL = cmdOptsL . languageL
-- instance (HasFSpecGenOpts a) => HasFSpecGenOpts (ExtendedRunner a) where
--   fSpecGenOptsL = cmdOptsL . fSpecGenOptsL
-- instance (HasParserOptions a) => HasParserOptions (ExtendedRunner a) where
--   trimXLSXCellsL = cmdOptsL . trimXLSXCellsL
-- instance (HasRootFile a) => HasRootFile (ExtendedRunner a) where
--   rootFileL = cmdOptsL . rootFileL
-- instance (HasDaemonOpts a) => HasDaemonOpts (ExtendedRunner a) where 
--   daemonOptsL = cmdOptsL . daemonOptsL
-- instance (HasRunComposer a) => HasRunComposer (ExtendedRunner a) where
--   skipComposerL = cmdOptsL . skipComposerL
-- instance (HasDirCustomizations a) => HasDirCustomizations (ExtendedRunner a) where
--   dirCustomizationsL = cmdOptsL . dirCustomizationsL
-- instance (HasZwolleVersion a) => HasZwolleVersion (ExtendedRunner a) where
--   zwolleVersionL = cmdOptsL . zwolleVersionL
-- instance (HasAllowInvariantViolations a) => HasAllowInvariantViolations (ExtendedRunner a) where
--   allowInvariantViolationsL = cmdOptsL . allowInvariantViolationsL
-- instance (HasDirPrototype a) => HasDirPrototype (ExtendedRunner a) where
--   dirPrototypeL = cmdOptsL . dirPrototypeL
-- instance (HasProtoOpts a) => HasProtoOpts (ExtendedRunner a) where
--   protoOptsL = cmdOptsL . protoOptsL
-- instance (HasOutputFile a) => HasOutputFile (ExtendedRunner a) where
--   outputfileAdlL = cmdOptsL . outputfileAdlL
--   outputfileDataAnalisysL = cmdOptsL . outputfileDataAnalisysL

-- instance HasRunner (ExtendedRunner a) where
--   runnerL = lens eRunner  (\x y -> x { eRunner  = y })
-- instance HasLogFunc (ExtendedRunner a) where
--   logFuncL = runnerL . logFuncL
-- instance HasProcessContext (ExtendedRunner a) where
--   processContextL = runnerL . processContextL

-- data ExtendedRunner a = ExtendedRunner
--    { eRunner :: !Runner
--    , eCmdOpts :: a
--    } deriving Show
-- cmdOptsL :: Lens' (ExtendedRunner a) a
-- cmdOptsL = lens eCmdOpts (\x y -> x { eCmdOpts = y })

-- -- instance HasProtoOpts a => Has