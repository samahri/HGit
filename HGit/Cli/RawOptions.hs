module HGit.Cli.RawOptions (RawOpts (..), updateArg, getArg, argsFlag, setopt, setboolopt, boolopt) where

import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import System.Console.CmdArgs (Default (..))
import System.Console.CmdArgs.Explicit

newtype RawOpts = RawOpts {unRawOpts :: [(String, String)]}
  deriving (Show)

instance Default RawOpts where
  def = RawOpts []

updateArg :: String -> RawOpts -> Either String RawOpts
updateArg s opts = Right $ setopt "args" s opts

getArg :: RawOpts -> String
getArg (RawOpts unRawOpts_) = snd $ fromMaybe ("", "") $ find (\(k, _) -> k == "args") unRawOpts_

setopt :: String -> String -> RawOpts -> RawOpts
setopt _name val = RawOpts . (++ [(_name, val)]) . unRawOpts

argsFlag :: FlagHelp -> Arg RawOpts
argsFlag = flagArg (\s opts -> Right $ setopt "args" s opts)

setboolopt :: String -> RawOpts -> RawOpts
setboolopt _name = RawOpts . (++ [(_name, "")]) . unRawOpts

boolopt :: String -> RawOpts -> Bool
boolopt name = isJust . lookup name . unRawOpts