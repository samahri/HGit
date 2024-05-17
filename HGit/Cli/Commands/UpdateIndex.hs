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
import Data.Word (Word32, Word8)
import HGit.Cli.CliOptions
import HGit.Cli.Commands.Utils
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
  currentDir <- getCurrentDirectory
  let isAddOpt = boolopt "add" rawOpts_
      fileName = intercalate "/" [currentDir, getArg rawOpts_]
      indexFile = intercalate "/" [currentDir, ".git", "index"]

  unless isAddOpt exitSuccess
  print fileName
  indexByteCode <- getIndexByteCode fileName

  B.writeFile indexFile indexByteCode

-- to write in index file: tree <contentLength>\NULL100644 blob <hash> <file_name>
getFileHash :: FilePath -> IO String
getFileHash file = getStore file "blob" <&> hashStore

getIndexByteCode :: FilePath -> IO B.ByteString
getIndexByteCode fileName = do
  fileStatus <- getFileStatus fileName
  store <- getStore fileName "blob"
  let fileCtime = getFileCtime fileStatus
      fileMtime = getFileMtime fileStatus
      deviceNum = B.toStrict . toLazyByteString . word32BE . fromIntegral . deviceID $ fileStatus
      inodeNum = B.toStrict . toLazyByteString . word32BE . fromIntegral . fileID $ fileStatus
      filePerm = B.toStrict . toLazyByteString . word32BE . fromIntegral . fileMode $ fileStatus
      ownerId = B.toStrict . toLazyByteString . word32BE . fromIntegral . fileOwner $ fileStatus
      groupId = B.toStrict . toLazyByteString . word32BE . fromIntegral . fileGroup $ fileStatus
      fSize = B.toStrict . toLazyByteString . word32BE . fromIntegral . fileSize $ fileStatus
      sha1Id = SHA1.hash . BC.pack $ store
      flags = B.pack $ numberTo12BitWord8List (length "testy") -- length fileName
      filepath = BC.pack "testy"
      padding = BC.pack $ replicate 5 '\0' -- TODO: variable padding length
      indexChecksum = SHA1.hash $ indexFileHeader <> fileCtime <> fileMtime <> deviceNum <> inodeNum <> filePerm <> ownerId <> groupId <> fSize <> sha1Id <> flags <> filepath <> padding

  pure $ indexFileHeader <> fileCtime <> fileMtime <> deviceNum <> inodeNum <> filePerm <> ownerId <> groupId <> fSize <> sha1Id <> flags <> filepath <> padding <> indexChecksum

getFileCtime :: FileStatus -> B.ByteString
getFileCtime fileStat = upperBits <> lowerBits
  where
    posixTime :: Word32
    posixTime = fst $ properFraction $ statusChangeTimeHiRes fileStat

    posixTimeDecimal :: NominalDiffTime
    posixTimeDecimal = (* 10 ^ (9 :: Integer)) . snd . properFraction . statusChangeTimeHiRes $ fileStat

    upperBits = B.toStrict . toLazyByteString . word32BE $ posixTime
    lowerBits = B.toStrict . toLazyByteString . word32BE . truncate $ posixTimeDecimal

getFileMtime :: FileStatus -> B.ByteString
getFileMtime fileStat = upperBits <> lowerBits
  where
    posixTime :: Word32
    posixTime = fst $ properFraction $ modificationTimeHiRes fileStat

    posixTimeDecimal :: NominalDiffTime
    posixTimeDecimal = (* 10 ^ (9 :: Integer)) . snd . properFraction . modificationTimeHiRes $ fileStat

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