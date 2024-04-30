module HGit.Cli.RawOptions (RawOpts (..)) where

newtype RawOpts = RawOpts {unRawOpts :: [(String, String)]}
  deriving (Show)