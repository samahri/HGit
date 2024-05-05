module HGit.Cli.RawOptions (RawOpts (..)) where

import System.Console.CmdArgs (Default (..))

newtype RawOpts = RawOpts {unRawOpts :: [(String, String)]}
  deriving (Show)

instance Default RawOpts where
  def = RawOpts []