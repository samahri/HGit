module HGit.Cli.Commands.Help (helpMode, helpAction) where

import HGit.Cli.CliOptions
import HGit.Cli.RawOptions
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit

helpMode :: Mode RawOpts
helpMode =
  mainMode
    { modeNames = ["help"],
      modeHelp = "hgit Help Command"
    }

helpAction :: CliOpts -> IO ()
helpAction = error "Undefined Action"