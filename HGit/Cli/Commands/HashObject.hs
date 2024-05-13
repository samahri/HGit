module HGit.Cli.Commands.HashObject (hashObjectMode, hashObjectAction) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Monad (when)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Char (toLower)
import Data.Functor ((<&>))
import HGit.Cli.CliOptions
import HGit.Cli.RawOptions
import Safe.Exact (dropExact, takeExact)
import System.Console.CmdArgs.Explicit
import System.Directory
import System.Exit (die, exitSuccess)
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
  when (notNull fileToHash && isStdinPath) $ die "Can't specify files with --stdin-paths"
  if isStdinPath
    then
      putStrLn "not implemented" >> exitSuccess
    else
      hashFile fileToHash toWriteFile
  where
    fileToHash = getArg rawOpts_
    notNull = not . null
    toWriteFile = boolopt "w" rawOpts_
    isStdinPath = boolopt "stdin-paths" rawOpts_

-- create a hash value for the file
hashFile :: String -> Bool -> IO ()
hashFile inputFile toWriteFile = do
  store <- getStore inputFile
  let hashValue = (byteStringToHex . SHA1.hash . BC.pack) store
  putStrLn hashValue
  -- when -w flag is passed, save the object in the database
  when toWriteFile $ saveObjectToDatabase hashValue store

saveObjectToDatabase :: String -> String -> IO ()
saveObjectToDatabase hashValue store = do
  folderPath <- getCurrentDirectory <&> (<> "/.git/objects/" <> folderName)

  let objectFilePath = folderPath <> "/" <> objectFileName
      compressedData = compress store

  createDirectoryIfMissing False folderPath
  B.writeFile objectFilePath compressedData
  where
    folderName = takeExact 2 hashValue
    objectFileName = dropExact 2 hashValue

getStore :: FilePath -> IO String
getStore inputFile = do
  fileContent <- readFile inputFile
  pure ("blob " <> show (length fileContent) <> ['\0'] <> fileContent)

compress :: String -> B.ByteString
compress = B.toStrict . Zlib.compress . LBC.pack

byteStringToHex :: B.ByteString -> [Char]
byteStringToHex = map toLower . concatMap (printf "%02X") . B.unpack
