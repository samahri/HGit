{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module HGit.Cli (main, mainMode) where

import qualified Data.ByteString.Char8 as BS
import Data.FileEmbed (embedFileRelative)
import Data.List (isPrefixOf, sortBy)
import Data.Maybe
import Data.Ord (Down (Down), comparing)
import HGit.Cli.CliOptions
import HGit.Cli.Commands
import HGit.Cli.RawOptions
import Safe (headDef)
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Environment (getArgs)

-- arguments :: Mode [(String, String)]
-- arguments =
--   mode
--     "explicit"
--     []
--     "Explicit sample program"
--     (flagArg (upd "file") "FILE")
--     [ flagOpt "world" ["hello", "h"] (upd "world") "WHO" "World argument",
--       flagReq ["greeting", "g"] (upd "greeting") "MSG" "Greeting to give",
--       flagHelpSimple (("help", "") :)
--     ]
--   where
--     upd msg x v = Right $ (msg, x) : v

-- TODO: Have `hgit init` take a directory name as input
main :: IO ()
main = do
  rawArgs' <- getArgs

  -- all the flags are at the end of the list
  let rawArgs = sortBy (comparing Down) rawArgs'

  cliOpts <- rawArgsToCliOpts rawArgs
  print cliOpts
  let cmd :: String
      cmd = command cliOpts

      isFlag :: String -> Bool
      isFlag = ("-" `isPrefixOf`)

      isNonEmptyNoneFlag :: String -> Bool
      isNonEmptyNoneFlag s = not (isFlag s) && not (null s)

      rawCmd :: String
      rawCmd = headDef "" $ takeWhile isNonEmptyNoneFlag rawArgs

      -- no input is provided
      isNullCommand :: Bool
      isNullCommand = null rawCmd

      -- input is provided but meaningless
      isBadCommand :: Bool
      isBadCommand = not (null rawCmd) && null cmd

      hasHelpFlag :: [String] -> Bool
      hasHelpFlag args1 = any (`elem` args1) ["--help", "-h"]

      orShowHelp :: IO () -> Mode RawOpts -> IO ()
      f `orShowHelp` helpMode
        | hasHelpFlag rawArgs = putStrLn $ showModeUsage helpMode
        | otherwise = f

      runHgit :: IO ()
      runHgit
        | isNullCommand && hasHelpFlag rawArgs = putStrLn $ showModeUsage mainMode
        | isNullCommand = printCommandList
        | isBadCommand = error ("command " <> rawCmd <> " is not implemented")
        | Just (cmdMode, cmdAction) <- findCommand cmd =
            ( case True of
                _ | True -> cmdAction cliOpts
                -- _ -> error "no command found"
            )
              `orShowHelp` cmdMode
        | otherwise = error "should never be here"

  runHgit

mainMode :: Mode RawOpts
mainMode =
  defaultMode
    { modeNames = ["hgit CMD"],
      modeArgs = ([], Just $ argsFlag "[ARGS]"),
      modeHelp = "hgit main command line interface. Type \"hgit\" to list available commands",
      modeGroupModes =
        Group
          { -- subcommands in the unnamed group, shown first:
            groupUnnamed = [],
            -- subcommands in named groups:
            groupNamed = [],
            -- subcommands handled but not shown in the help:
            groupHidden = map fst builtinCommands
          },
      modeHelpSuffix = ["Examples:"],
      modeCheck = Right,
      modeReform = const Nothing,
      modeExpandAt = True,
      modeValue = def
    }
  where
    argsFlag :: FlagHelp -> Arg RawOpts
    argsFlag = flagArg (\s opts -> Right $ setopt "args" s opts)

printCommandList :: IO ()
printCommandList = (putStrLn . BS.unpack) ($(embedFileRelative "HGit/Commands.txt"))

rawArgsToCliOpts :: [String] -> IO CliOpts
rawArgsToCliOpts rawArgs = do
  case process mainMode rawArgs of
    Right rawopts -> pure $ CliOpts {rawOpts = rawopts, command = fromMaybe "" $ findCommandInRawOpts rawopts}
    Left eMsg -> print rawArgs >> error eMsg
  where
    findCommandInRawOpts :: RawOpts -> Maybe String
    findCommandInRawOpts = lookup "command" . reverse . unRawOpts