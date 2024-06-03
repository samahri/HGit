{-# LANGUAGE BinaryLiterals #-}

module HGit.Cli.Commands.UpdateIndex
  ( updateIndexMode,
    updateIndexAction,
  )
where

import Control.Monad (unless)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word32, Word8)
import HGit.Cli.CliOptions
import HGit.Cli.Commands.Utils
import HGit.Cli.Data.GitIndexV2Format
import HGit.Cli.RawOptions
import System.Console.CmdArgs.Explicit
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess)
import System.Posix.Files

updateIndexMode :: Mode RawOpts
updateIndexMode =
  (overrideDefaultMode "update-index" flags)
    { modeNames = ["update-index"],
      modeHelp = "hgit update-index Command",
      modeArgs = ([], Just $ flagArg updateArg "[<file>]")
    }
  where
    flags =
      [ flagNone ["add"] (setboolopt "add") "If a specified file isn't in the index already then it's added. Default behaviour is to ignore new files."
      ]

updateIndexAction :: CliOpts -> IO ()
updateIndexAction CliOpts {rawOpts = rawOpts_} = do
  let isAddOpt = boolopt "add" rawOpts_
      fileName = getArg rawOpts_

      saveFiles :: IO ()
      saveFiles = do
        store <- getStore fileName "blob"
        let blobHash = hashStore store
        saveObjectToDatabase (blobHash, store)

      writeToIndexFile :: IO ()
      writeToIndexFile = do
        indexFile <- getCurrentDirectory <&> (\currentDir -> intercalate "/" [currentDir, ".git", "index"])
        indexFileContent <- getIndexByteCode fileName
        B.writeFile indexFile (toByteString indexFileContent)

  unless isAddOpt exitSuccess

  saveFiles
  writeToIndexFile

getIndexByteCode :: String -> IO GitIndexV2Format
getIndexByteCode fileName = do
  indexEntries <- gitIndexEntries fileName
  pure
    GitIndexV2Format
      { header_ = indexFileHeader,
        entries_ = indexEntries,
        extensions_ = Nothing,
        checksum_ = SHA1.hash $ toByteString indexEntries
      }

gitIndexEntries :: String -> IO GitIndexEntries
gitIndexEntries fileName = do
  filePath <- getCurrentDirectory <&> (\currentDir -> intercalate "/" [currentDir, fileName])
  fileStatus <- getFileStatus filePath
  store <- getStore filePath "blob" -- similar to getFileHash
  pure
    GitIndexEntries
      { ctime_ = getFileCtime fileStatus,
        mtime_ = getFileMtime fileStatus,
        dev_ = to32BitsByteString $ deviceID fileStatus,
        ino_ = to32BitsByteString $ fileID fileStatus,
        mode_ = to32BitsByteString $ fileMode fileStatus,
        uid_ = to32BitsByteString $ fileOwner fileStatus,
        gid_ = to32BitsByteString $ fileGroup fileStatus,
        fsize_ = to32BitsByteString $ fileSize fileStatus,
        objId_ = SHA1.hash . BC.pack $ store,
        flags_ = B.pack $ numberTo12BitWord8List (length fileName),
        fpath_ = BC.pack fileName,
        padding_ = BC.pack $ replicate 5 '\0' -- TODO: variable padding length
      }
  where
    to32BitsByteString :: (Integral a) => a -> B.ByteString
    to32BitsByteString = B.toStrict . toLazyByteString . word32BE . fromIntegral

getFileCtime :: FileStatus -> (B.ByteString, B.ByteString)
getFileCtime = getPosixTime statusChangeTimeHiRes

getFileMtime :: FileStatus -> (B.ByteString, B.ByteString)
getFileMtime = getPosixTime modificationTimeHiRes

getPosixTime :: (FileStatus -> POSIXTime) -> FileStatus -> (B.ByteString, B.ByteString)
getPosixTime xTimeFn fileStat = (upperBits, lowerBits)
  where
    posixTime :: Word32
    posixTime = fst $ properFraction $ xTimeFn fileStat

    posixTimeDecimal :: NominalDiffTime
    posixTimeDecimal = (* 10 ^ (9 :: Integer)) . snd . properFraction . xTimeFn $ fileStat

    upperBits = B.toStrict . toLazyByteString . word32BE $ posixTime
    lowerBits = B.toStrict . toLazyByteString . word32BE . truncate $ posixTimeDecimal

-- index 12 byte header
indexFileHeader :: B.ByteString
indexFileHeader = B.pack $ dirc <> versionNum <> numOfEntries
  where
    dirc = [68, 73, 82, 67]
    versionNum = [0, 0, 0, 2]
    numOfEntries = [0, 0, 0, 1] -- could change

numberTo12BitWord8List :: Int -> [Word8]
numberTo12BitWord8List n = if n >= 4095 then [15, 255] else go n []
  where
    go :: Int -> [Word8] -> [Word8]
    go 0 acc = replicate (2 - length acc) 0 <> acc
    go x acc = go (x `shiftR` 8) (fromIntegral (x .&. 0xFF) : acc)