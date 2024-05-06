module HGit.Cli.CliOptions
  ( CliOpts (..),
    showModeUsage,
    overrideDefaultMode,
    defaultMode,
    defaultGroupFlags,
    defaultGroupModes,
  )
where

import HGit.Cli.RawOptions
import Safe (headDef)
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

defaultMode :: Mode RawOpts
defaultMode =
  Mode
    { modeNames = [], -- program/command name(s)
      modeHelp = "", -- short help for this command
      modeHelpSuffix = [], -- text displayed after the usage
      modeGroupFlags = toGroup [],
      modeArgs = ([], Nothing), -- description of arguments accepted by the command
      modeValue = def, -- value returned when this mode is used to parse a command line
      modeCheck = Right, -- whether the mode's value is correct
      modeReform = const Nothing, -- function to convert the value back to a command line arguments
      modeExpandAt = True, -- expand @ arguments for program ?
      modeGroupModes = defaultGroupModes -- sub-modes
    }

defaultGroupFlags :: Group (Flag RawOpts)
defaultGroupFlags =
  Group -- description of flags accepted by the command
    { groupNamed = [], --  named groups of flags
      groupUnnamed = [], --  ungrouped flags
      groupHidden = [] --  flags not displayed in the usage
    }

defaultGroupModes :: Group (Mode RawOpts)
defaultGroupModes =
  Group
    { groupUnnamed = [], -- subcommands in the unnamed group, shown first:
      groupNamed = [], -- subcommands in named groups:
      groupHidden = [] -- subcommands handled but not shown in the help:
    }

overrideDefaultMode :: String -> [Flag RawOpts] -> Mode RawOpts
overrideDefaultMode _name _unnamedFlags =
  defaultMode
    { modeValue = setopt "command" (headDef "" [_name]) def,
      modeGroupFlags =
        defaultGroupFlags
          { groupNamed = [("General Flags", [flagNone ["help", "h"] (setboolopt "help") "show general help (or after CMD, command help)"])],
            groupUnnamed = _unnamedFlags
          }
    }