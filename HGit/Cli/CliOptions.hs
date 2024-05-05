module HGit.Cli.CliOptions
  ( CliOpts (..),
    showModeUsage,
    setopt,
    overrideDefaultMode,
    defaultMode,
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
      modeGroupFlags = defaultGroupFlags,
      modeArgs = ([], Nothing), -- description of arguments accepted by the command
      modeValue = def, -- value returned when this mode is used to parse a command line
      modeCheck = Right, -- whether the mode's value is correct
      modeReform = const Nothing, -- function to convert the value back to a command line arguments
      modeExpandAt = True, -- expand @ arguments for program ?
      modeGroupModes = toGroup [] -- sub-modes
    }

defaultGroupFlags :: Group (Flag RawOpts)
defaultGroupFlags =
  Group -- description of flags accepted by the command
    { groupNamed = [], --  named groups of flags
      groupUnnamed = [], --  ungrouped flags
      groupHidden = [] --  flags not displayed in the usage
    }

overrideDefaultMode :: String -> Mode RawOpts
overrideDefaultMode _name =
  defaultMode
    { modeValue = setopt "command" (headDef "" [_name]) def,
      modeGroupFlags =
        defaultGroupFlags
          { groupNamed = [("General Help Flags", [flagNone ["help", "h"] (setboolopt "help") "show general help (or after CMD, command help)"])]
          }
    }
  where
    setboolopt :: String -> RawOpts -> RawOpts
    setboolopt _name = RawOpts . (++ [(_name, "")]) . unRawOpts

setopt :: String -> String -> RawOpts -> RawOpts
setopt _name val = RawOpts . (++ [(_name, val)]) . unRawOpts