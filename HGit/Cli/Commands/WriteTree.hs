module HGit.Cli.Commands.WriteTree (writeTreeMode, writeTreeAction) where

import BinaryParser
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import HGit.Cli.CliOptions
import HGit.Cli.Commands.Utils (calculateStoreAndHash, saveObjectToDatabase)
import HGit.Cli.Data.GitIndexV2Format
import HGit.Cli.Data.Store (StoreType (..))
import HGit.Cli.RawOptions
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit (Mode (..))
import System.Directory (getCurrentDirectory, removeFile)
import System.IO

writeTreeMode :: Mode RawOpts
writeTreeMode =
  (overrideDefaultMode "write-tree" [])
    { modeNames = ["write-tree"],
      modeHelp = "Creates a tree object using the current index. The name of the new tree object is printed to standard output.",
      modeArgs = ([], Nothing)
    }

writeTreeAction :: CliOpts -> IO ()
writeTreeAction _ = do
  -- read the blob object from index file
  curdir <- getCurrentDirectory
  binaryData <- B.readFile $ curdir <> "/.git/index"
  case run parser binaryData of
    Left err -> print $ "error: " <> show err
    Right val -> do
      let blobHash = objId_ . entries_ $ val
          content = "100644 testy" <> ['\0'] <> BC.unpack blobHash

      (hashValue, store) <- do
        (tmpFilePath, handle) <- openTempFile "/tmp/" "null"
        hPutStr handle content
        hSeek handle AbsoluteSeek 0
        hashAndStore <- calculateStoreAndHash handle Tree
        removeFile tmpFilePath
        pure hashAndStore

      saveObjectToDatabase (hashValue, store)
      putStrLn hashValue

parser :: BinaryParser GitIndexV2
parser = do
  -- validate header
  header <- headerParser
  -- validate entries
  ctimeUpper <- bytesOfSize 4
  ctimeLower <- bytesOfSize 4
  mtimeUpper <- bytesOfSize 4
  mtimeLower <- bytesOfSize 4
  dev <- bytesOfSize 4
  ino <- bytesOfSize 4
  mode <- bytesOfSize 4
  uid <- bytesOfSize 4
  gid <- bytesOfSize 4
  fsize <- bytesOfSize 4
  objId <- bytesOfSize 20
  flags <- bytesOfSize 4
  fpath <- bytesWhile (/= 0)
  padding <- bytesWhile (== 0)
  checksum <- bytesOfSize 20
  pure
    GitIndexV2
      { header_ = header,
        entries_ =
          GitIndexEntries
            { ctime_ = (ctimeUpper, ctimeLower),
              mtime_ = (mtimeUpper, mtimeLower),
              dev_ = dev,
              ino_ = ino,
              mode_ = mode,
              uid_ = uid,
              gid_ = gid,
              fsize_ = fsize,
              objId_ = objId,
              flags_ = flags,
              fpath_ = fpath,
              padding_ = padding
            },
        extensions_ = Nothing,
        checksum_ = checksum
      }

headerParser :: BinaryParser GitIndexHeader
headerParser = do
  unitOfBytes dirc
  unitOfBytes version
  entries <- bytesOfSize 4
  pure $ GitIndexHeader Dirc Version2 (byteStringToInt entries)
  where
    dirc = toByteString Dirc
    version = toByteString Version2

-- from gpt
byteStringToInt :: BC.ByteString -> Int
byteStringToInt bs = foldl (\acc x -> acc * 256 + fromIntegral (ord x)) 0 (BC.unpack bs)

-- if the blob is not stored in object, fail
-- otherwise, extract blob and create a tree object, store that tree object in object/

-- tree object file information
-- tree ${length}\0100644 blob {hash of testy} \t testy