module HGit.Cli.Commands (findCommand) where

import Data.List (find)
import HGit.Cli.CliOptions
import HGit.Cli.Commands.Help
import HGit.Cli.RawOptions
import System.Console.CmdArgs.Explicit

findCommand :: String -> Maybe (Mode RawOpts, CliOpts -> IO ())
findCommand cmdname = find (elem cmdname . modeNames . fst) builtinCommands

builtinCommands :: [(Mode RawOpts, CliOpts -> IO ())]
builtinCommands =
  [ (helpMode, helpAction)
  ]
