{-|
Module: CLI
Description: The parsing of CLI arguments
Copyright: (c) Mihai Maruseac 2023
License: BSD-3-Clause
Maintainer: mihai.maruseac@gmail.com
Stability: experimental
Portability: portable

The implementation of CLI arguments parsing occurs in this module.

We are parsing the arguments ourselves instead of using Hakyll's
implementation even though these are just a subset of Hakyll's CLI. There are
two reasons for this: first, we don't want to use all of Hakyll's commands as
some don't make sense (e.g., given the way we use the generator, it does not
make sense to have a rebuild command, we always need clean and build). The
other reason is that we want to pass some configuration via a config file,
which will then be merged with the flags passed in CLI and with Hakyll's
defaults (flags take precedence over config which then takes precedence over
defaults).

See also "Config" module.
-}

module CLI
  ( -- * Argument parsing API
    parseCLI
    -- * Argument parsing results
  , CLI(..)
  , Command(..)
  ) where

import Control.Applicative ((<|>))
import qualified Options.Applicative as OA

-- | Holds the result of 'parseCLI', the result of parsing command line
-- arguments.
--
-- It is just a wrapper around 'Command', allowing for setting up verbosity
-- and a different config file if needed. The real work is happening inside
-- 'Command'.
data CLI = CLI
  { verbose :: Bool  -- ^ Verbosity (@[-v|--verbose]@)
  , configFile :: FilePath -- ^ Configuration file (@[-c|--config]@)
  , command :: Command -- ^ The real subcommand to be executed.
  } deriving (Eq, Show)

-- | The commands we support.
--
-- These are a subset of the commands that @hakyll@ supports and not all
-- arguments are provided there either. Each command will call the
-- corresponding @hakyll@ implementation.
data Command
  = Build {dry_run :: Bool} -- ^ Generates the site.
  | Clean -- ^ Removes all generated files.
  | Watch {host :: Maybe String, port :: Maybe Int, no_server :: Bool}
  -- ^ Automcompile on changes, start preview server.
  | Check {internal_links :: Bool} -- ^ Validate the output.
  deriving (Eq, Show)

-- | The 'parseCLI' function handles the command line arguments.
parseCLI :: IO CLI
parseCLI = OA.customExecParser preferences topLevelParserInfo

-- | Preferences for the help text.
preferences :: OA.ParserPrefs
preferences = OA.prefs $ mconcat
  [ OA.showHelpOnError
  , OA.showHelpOnEmpty
  , OA.helpLongEquals
  , OA.helpShowGlobals
  ]

-- | Parser + help text for top level program.
topLevelParserInfo :: OA.ParserInfo CLI
topLevelParserInfo = OA.info (OA.helper <*> topLevelParser) $ mconcat
  [ OA.fullDesc
  , OA.progDesc "hithlain - Blog generator for https://mihai.page"
  ]

-- | Parser for top level program.
topLevelParser :: OA.Parser CLI
topLevelParser = CLI <$> verbosityParser <*> configFileParser <*> cmdParser

-- | Adds @[-v|--verbose]@ to command flags (globally).
verbosityParser :: OA.Parser Bool
verbosityParser = OA.switch $ mconcat
  [ OA.long "verbose"
  , OA.short 'v'
  , OA.help "Run in verbose mode"
  ]

-- | Adds @[-c|--config=FILE]@ to command flags (globally).
configFileParser :: OA.Parser FilePath
configFileParser = OA.strOption $ mconcat
  [ OA.long "config"
  , OA.short 'c'
  , OA.help "Configuration file"
  , OA.metavar "FILE"
  , OA.value "config.yml"
  , OA.showDefault
  , OA.action "file"
  ]

-- | Parser for the commands to be sent to @hakyll@ library.
-- This is the real work.
cmdParser :: OA.Parser Command
cmdParser = OA.hsubparser buildCommandParser
  <|> OA.hsubparser cleanCommandParser
  <|> OA.hsubparser watchCommandParser
  <|> OA.hsubparser checkCommandParser

-- | Generates the build command.
buildCommandParser :: OA.Mod OA.CommandFields Command
buildCommandParser = OA.command "build" parser <> OA.metavar "build"
  where
    parser = OA.info (Build <$> dryRunParser) opts
    opts = OA.fullDesc <> OA.progDesc "Generate the site"

-- | Adds @[-n|--dry-run]@ to build command.
dryRunParser :: OA.Parser Bool
dryRunParser = OA.switch $ mconcat
  [ OA.long "dry-run"
  , OA.short 'n'
  , OA.help "Don't build, only print out-of-date items"
  ]

-- | Generates the clean command.
cleanCommandParser :: OA.Mod OA.CommandFields Command
cleanCommandParser = OA.command "clean" parser <> OA.metavar "clean"
  where
    parser = OA.info (pure Clean) opts
    opts = OA.fullDesc <> OA.progDesc "Clean up and remove cache"

-- | Generates the watch command.
watchCommandParser :: OA.Mod OA.CommandFields Command
watchCommandParser = OA.command "watch" parser <> OA.metavar "watch"
  where
    parser = OA.info (Watch <$> hostParser <*> portParser <*> serverParser) opts
    opts = OA.fullDesc <> OA.progDesc "Clean and build again"

-- | Adds @[-h|--host=HOST/IP]@ to watch command.
hostParser :: OA.Parser (Maybe String)
hostParser = OA.option OA.auto $ mconcat
  [ OA.long "host"
  , OA.short 'h'
  , OA.help "Host to bind on"
  , OA.metavar "HOST/IP"
  , OA.value Nothing
  , OA.showDefaultWith $ const "Use value from config or hakyll's default"
  , OA.action "hostname"
  ]

-- | Adds @[-p|--port=PORT]@ to watch command.
portParser :: OA.Parser (Maybe Int)
portParser = OA.option OA.auto $ mconcat
  [ OA.long "port"
  , OA.short 'p'
  , OA.help "Port to listen on"
  , OA.metavar "PORT"
  , OA.value Nothing
  , OA.showDefaultWith $ const "Use value from config or hakyll's default"
  ]

-- | Adds @[--no-server]@ to watch command.
serverParser :: OA.Parser Bool
serverParser = OA.switch $ mconcat
  [ OA.long "no-server"
  , OA.help "Disable the built-in web server"
  ]

-- | Generates the check command.
checkCommandParser :: OA.Mod OA.CommandFields Command
checkCommandParser = OA.command "check" parser <> OA.metavar "check"
  where
    parser = OA.info (Check <$> internalLinksParser) opts
    opts = OA.fullDesc <> OA.progDesc "Validate the site output"

-- | Adds @[--internal-links]@ to check command.
internalLinksParser :: OA.Parser Bool
internalLinksParser = OA.switch $ mconcat
  [ OA.long "internal-links"
  , OA.help "Check internal-links only"
  ]
