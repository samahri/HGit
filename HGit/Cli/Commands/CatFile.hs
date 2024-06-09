module HGit.Cli.Commands.CatFile
  ( catFileMode,
    catFileAction,
  )
where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import HGit.Cli.CliOptions
import HGit.Cli.RawOptions
import HGit.Cli.Utils.Codec as HGitCodec
import Safe.Exact
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (die, exitSuccess)

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
  unless hashIsValid (die $ "Invalid object name " <> objectHash)
  objectPath <- getObjectPath objectHash
  fileExists <- doesFileExist objectPath
  unless fileExists $ die "could not read file"
  objectContent <- readAndDecompress objectPath
  case getCatFileOpt rawOpts_ of
    Just PrettyPrint -> putStr (dropWhile (/= '\0') objectContent)
    Just ObjTypePrint -> putStrLn (takeWhile (/= ' ') objectContent)
    Nothing -> exitSuccess
  where
    objectHash = getArg rawOpts_
    hashIsValid = length objectHash == 40 && all (`elem` "0123456789abcdefABCDEF") objectHash

getObjectPath :: String -> IO FilePath
getObjectPath objHash = getCurrentDirectory <&> (<> "/.git/objects/" <> objFolder <> "/" <> objFile)
  where
    objFolder = takeExact 2 objHash
    objFile = dropExact 2 objHash

readAndDecompress :: FilePath -> IO String
readAndDecompress fp = BS.readFile fp <&> decompress

getCatFileOpt :: RawOpts -> Maybe CatFileOpt
getCatFileOpt rawOpts_
  | boolopt "p" rawOpts_ = Just PrettyPrint
  | boolopt "t" rawOpts_ = Just ObjTypePrint
  | otherwise = Nothing
