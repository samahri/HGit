module HGit.Cli.CliOptions
  ( CliOpts (..),
    showModeUsage,
    mainMode,
  )
where

import HGit.Cli.RawOptions
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text

data CliOpts = CliOpts
  { rawOpts :: RawOpts,
    command :: String
  }
  deriving (Show)

showModeUsage :: Mode a -> String
showModeUsage = showText defaultWrap . helpText [] HelpFormatDefault

mainMode :: Mode RawOpts
mainMode =
  Mode
    { modeNames = ["hgit CMD"],
      modeArgs = ([], Just $ flagArg dontUpdate "[ARGS]"),
      modeHelp = "hgit main command line interface. Type \"hgit\" to list available commands",
      modeGroupModes =
        Group
          { -- subcommands in the unnamed group, shown first:
            groupUnnamed = [],
            -- subcommands in named groups:
            groupNamed = [],
            -- subcommands handled but not shown in the help:
            groupHidden = []
          },
      modeGroupFlags =
        Group
          { -- flags in named groups:
            groupNamed =
              [ ("General Help Flags", [flagNone ["help", "h"] (setboolopt "help") "show general help (or after CMD, command help)"])
              ],
            -- flags in the unnamed group, shown last:
            groupUnnamed = [],
            -- flags handled but not shown in the help:
            groupHidden = []
          },
      modeHelpSuffix = ["Examples:"],
      modeCheck = Right,
      modeReform = const Nothing,
      modeExpandAt = True,
      modeValue = RawOpts []
    }
  where
    dontUpdate :: String -> RawOpts -> Either String RawOpts
    dontUpdate _ _ = Right $ RawOpts []

    setboolopt :: String -> RawOpts -> RawOpts
    setboolopt _name = RawOpts . (++ [(_name, "")]) . unRawOpts