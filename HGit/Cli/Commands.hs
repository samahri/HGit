module HGit.Cli.Commands (findCommand, builtinCommands) where

import Data.List (find)
import HGit.Cli.CliOptions
import HGit.Cli.Commands.CatFile
import HGit.Cli.Commands.HashObject
import HGit.Cli.Commands.Help
import HGit.Cli.Commands.Init
import HGit.Cli.Commands.UpdateIndex
import HGit.Cli.Commands.WriteTree
import HGit.Cli.RawOptions
import System.Console.CmdArgs.Explicit

findCommand :: String -> Maybe (Mode RawOpts, CliOpts -> IO ())
findCommand cmdname = find (elem cmdname . modeNames . fst) builtinCommands

builtinCommands :: [(Mode RawOpts, CliOpts -> IO ())]
builtinCommands =
  [ (helpMode, helpAction),
    (initMode, initAction),
    (hashObjectMode, hashObjectAction),
    (catFileMode, catFileAction),
    (updateIndexMode, updateIndexAction),
    (writeTreeMode, writeTreeAction)
  ]