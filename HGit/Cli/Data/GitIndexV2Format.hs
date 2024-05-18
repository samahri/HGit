{-# LANGUAGE InstanceSigs #-}

module HGit.Cli.Data.GitIndexV2Format
  ( GitIndexV2Format (..),
    GitIndexEntries (..),
    toByteString,
  )
where

import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)

data GitIndexV2Format = GitIndexV2Format
  { header_ :: B.ByteString,
    entries_ :: GitIndexEntries,
    extensions_ :: Maybe B.ByteString,
    checksum_ :: B.ByteString
  }
  deriving (Show)

data GitIndexEntries = GitIndexEntries
  { ctime_ :: (B.ByteString, B.ByteString),
    mtime_ :: (B.ByteString, B.ByteString),
    dev_ :: B.ByteString,
    ino_ :: B.ByteString,
    mode_ :: B.ByteString,
    uid_ :: B.ByteString,
    gid_ :: B.ByteString,
    fsize_ :: B.ByteString,
    objId_ :: B.ByteString,
    flags_ :: B.ByteString,
    fpath_ :: B.ByteString,
    padding_ :: B.ByteString
  }
  deriving (Show)

-- Look for an existing typeclass (Serialize?)
class ToByteString a where
  toByteString :: a -> B.ByteString

instance ToByteString GitIndexEntries where
  toByteString :: GitIndexEntries -> B.ByteString
  toByteString entries =
    (fst . ctime_) entries
      <> (snd . ctime_) entries
      <> (fst . mtime_) entries
      <> (snd . mtime_) entries
      <> dev_ entries
      <> ino_ entries
      <> mode_ entries
      <> uid_ entries
      <> gid_ entries
      <> fsize_ entries
      <> objId_ entries
      <> flags_ entries
      <> fpath_ entries
      <> padding_ entries

instance ToByteString GitIndexV2Format where
  toByteString :: GitIndexV2Format -> B.ByteString
  toByteString gitIndex =
    header_ gitIndex
      <> toByteString (entries_ gitIndex)
      <> fromMaybe B.empty (extensions_ gitIndex)
      <> checksum_ gitIndex
