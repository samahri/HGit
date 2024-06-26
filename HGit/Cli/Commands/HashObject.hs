module HGit.Cli.Commands.HashObject (hashObjectMode, hashObjectAction) where

import Control.Monad (unless, when)
import HGit.Cli.CliOptions
import HGit.Cli.Commands.Utils
import HGit.Cli.Data.Store
import HGit.Cli.RawOptions
import System.Console.CmdArgs.Explicit
import System.Exit (die, exitSuccess)
import System.IO (IOMode (ReadMode), openFile)

hashObjectMode :: Mode RawOpts
hashObjectMode =
  (overrideDefaultMode "hash-object" flags)
    { modeNames = ["hash-object"],
      modeHelp = "hgit hash-object Command",
      modeArgs = ([flagArg updateArg "[<file> | --stdin-paths]"], Nothing)
    }
  where
    flags =
      [ flagNone ["stdin-paths"] (setboolopt "stdin-paths") "Read file names from the standard input, one per line, instead of from the command-line.",
        flagNone ["w"] (setboolopt "w") "Actually write the object into the object database."
      ]

hashObjectAction :: CliOpts -> IO ()
hashObjectAction CliOpts {rawOpts = rawOpts_} = do
  when (notNull fileToHash && isStdinPath) $ die "Can't specify files with --stdin-paths"
  if isStdinPath
    then
      putStrLn "not implemented" >> exitSuccess
    else do
      unless (notNull fileToHash) exitSuccess
      handle <- openFile fileToHash ReadMode
      (hashValue, store) <- calculateStoreAndHash handle Blob
      putStrLn hashValue
      -- when -w flag is passed, save the object in the database
      when toWriteFile $ saveObjectToDatabase (hashValue, store)
  where
    fileToHash = getArg rawOpts_
    toWriteFile = boolopt "w" rawOpts_
    isStdinPath = boolopt "stdin-paths" rawOpts_
