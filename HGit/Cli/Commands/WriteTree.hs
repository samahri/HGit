module HGit.Cli.Commands.WriteTree (writeTreeMode, writeTreeAction) where

import HGit.Cli.CliOptions
import HGit.Cli.RawOptions
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit (Mode (..))

writeTreeMode :: Mode RawOpts
writeTreeMode =
  (overrideDefaultMode "write-tree" [])
    { modeNames = ["write-tree"],
      modeHelp = "Creates a tree object using the current index. The name of the new tree object is printed to standard output.",
      modeArgs = ([], Nothing)
    }

writeTreeAction :: CliOpts -> IO ()
writeTreeAction CliOpts {rawOpts = rawOpts_} = undefined

-- read the blob object from index file
-- if the blob is not stored in object, fail
-- otherwise, extract blob and create a tree object, store that tree object in object/