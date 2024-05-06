module HGit.Cli.Commands.Init (initAction, initMode) where

import Control.Monad (when)
import Data.Functor ((<&>))
import HGit.Cli.CliOptions
import HGit.Cli.RawOptions
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Directory
import System.Exit (exitSuccess)
import System.IO

initMode :: Mode RawOpts
initMode =
  (overrideDefaultMode "init" [])
    { modeNames = ["init"],
      modeHelp = "hgit Init Command",
      modeArgs = ([flagArg updateArg "[<file>]"], Nothing)
    }

initAction :: CliOpts -> IO ()
initAction CliOpts {rawOpts = rawOpts_} = do
  currDir <- getCurrentDirectory <&> (<> "/" <> getArg rawOpts_)

  let gitDirectory = currDir <> "/.git/"
      headFile = gitDirectory <> "/HEAD"
      configFile = gitDirectory <> "config"
      indexFile = gitDirectory <> "index"
      objectsFolder = gitDirectory <> "objects/"
      infoFolder = objectsFolder <> "info/"
      packFolder = objectsFolder <> "pack/"
      refsFolder = gitDirectory <> "refs/"
      headsFolder = refsFolder <> "heads/"
      tagsFolder = refsFolder <> "tags/"

  doesGitExist <- doesPathExist gitDirectory
  when doesGitExist $ putStrLn ".git directory exist; nothing to do here" >> exitSuccess

  -- .git/
  createDirectoryIfMissing True gitDirectory

  -- HEAD
  handle1 <- openFile headFile WriteMode
  hPutStr handle1 "ref: refs/heads/master"
  hClose handle1

  -- config
  handle2 <- openFile configFile WriteMode
  hClose handle2

  -- index
  handle3 <- openFile indexFile WriteMode
  hClose handle3

  -- objects folder
  createDirectoryIfMissing False objectsFolder
  createDirectoryIfMissing False infoFolder
  createDirectoryIfMissing False packFolder

  -- refs folder
  createDirectoryIfMissing False refsFolder
  createDirectoryIfMissing False headsFolder
  createDirectoryIfMissing False tagsFolder

-- reusable; move to another module
