module HGit.Cli.Commands.HashObject (hashObjectMode, hashObjectAction) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Monad (when)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import HGit.Cli.CliOptions
import HGit.Cli.RawOptions
import Safe.Exact (dropExact, takeExact)
import System.Console.CmdArgs.Explicit
import System.Directory
import System.Exit (die, exitSuccess)
import System.IO
import Text.Printf

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
  let fileToHash = getArg rawOpts_
      isStdinPath = boolopt "stdin-paths" rawOpts_
      toWriteFile = boolopt "w" rawOpts_
      notNull = not . null

  when (notNull fileToHash && isStdinPath) $ die "Can't specify files with --stdin-paths"

  if isStdinPath
    then
      putStrLn "not implemented" >> exitSuccess
    else
      hashFile fileToHash toWriteFile

-- create a hash value for the file
hashFile :: String -> Bool -> IO ()
hashFile inputFile toWriteFile = do
  -- read file
  fileHandle <- openFile inputFile ReadMode
  fileSize <- hFileSize fileHandle
  fileContent <- hGetContents' fileHandle
  hClose fileHandle

  -- create a header, which equals "blob {content length}\0"
  let header = "blob " <> show fileSize <> ['\0']
      store = header <> fileContent
      hashValue = SHA1.hash (B.pack store)

      saveObjectToDatabase :: IO ()
      saveObjectToDatabase = do
        currDir <- getCurrentDirectory

        let folderName = takeExact 2 $ byteStringToHex hashValue
            folderPath = currDir <> "/.git/objects/" <> folderName
            objectFileName = dropExact 2 $ byteStringToHex hashValue
            objectFilePath = folderPath <> "/" <> objectFileName
            compressedData = Zlib.compress $ (B.fromStrict . B.pack) store

        createDirectoryIfMissing False folderPath
        handle <- openFile objectFilePath WriteMode
        BS.hPut handle $ BS.toStrict compressedData
        hClose handle

  putStrLn $ byteStringToHex hashValue

  -- when -w flag is passed, save the object in the database
  when toWriteFile saveObjectToDatabase

byteStringToHex :: B.ByteString -> [Char]
byteStringToHex = concatMap (printf "%02X") . B.unpack
