module HGit.Cli.Commands.CatFile
  ( catFileMode,
    catFileAction,
  )
where

import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Functor ((<&>))
import HGit.Cli.CliOptions
import HGit.Cli.RawOptions
import Safe.Exact
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess)

data CatFileOpt = PrettyPrint | ObjTypePrint deriving (Show)

catFileMode :: Mode RawOpts
catFileMode =
  (overrideDefaultMode "cat-file" flags)
    { modeNames = ["cat-file"],
      modeHelp = "hgit cat-file Command",
      modeArgs = ([flagArg updateArg "<object>"], Nothing)
    }
  where
    flags =
      [ flagNone ["p"] (setboolopt "p") "Pretty-print the contents of <object> based on its type.",
        flagNone ["t"] (setboolopt "t") "Instead of the content, show the object type identified by <object>."
      ]

catFileAction :: CliOpts -> IO ()
catFileAction CliOpts {rawOpts = rawOpts_} = do
  objectPath <- getObjectPath (getArg rawOpts_)
  objectContent <- readAndDecompress objectPath
  case getCatFileOpt rawOpts_ of
    Just PrettyPrint -> putStr (dropWhile (/= '\0') objectContent)
    Just ObjTypePrint -> putStrLn (takeWhile (/= ' ') objectContent)
    Nothing -> exitSuccess

getObjectPath :: String -> IO FilePath
getObjectPath objHash = getCurrentDirectory <&> (<> "/.git/objects/" <> objFolder <> "/" <> objFile)
  where
    objFolder = takeExact 2 objHash
    objFile = dropExact 2 objHash

readAndDecompress :: FilePath -> IO String -- use Text?
readAndDecompress fp = BS.readFile fp <&> decompress
  where
    decompress :: BS.ByteString -> String
    decompress = BSC.unpack . BS.toStrict . Zlib.decompress . BS.fromStrict

getCatFileOpt :: RawOpts -> Maybe CatFileOpt
getCatFileOpt rawOpts_
  | boolopt "p" rawOpts_ = Just PrettyPrint
  | boolopt "t" rawOpts_ = Just ObjTypePrint
  | otherwise = Nothing
