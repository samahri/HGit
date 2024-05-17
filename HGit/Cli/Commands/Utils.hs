module HGit.Cli.Commands.Utils
  ( getStore,
    hashStore,
    compress,
  )
where

import qualified Codec.Compression.Zlib as Zlib
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Char (toLower)
import Text.Printf (printf)

getStore :: FilePath -> String -> IO String
getStore inputFile storeType = do
  fileContent <- readFile inputFile
  pure (storeType <> " " <> show (length fileContent) <> ['\0'] <> fileContent)

-- create a hash value for the file
hashStore :: String -> [Char]
hashStore = byteStringToHex . SHA1.hash . BC.pack

compress :: String -> B.ByteString
compress = B.toStrict . Zlib.compress . LBC.pack

byteStringToHex :: B.ByteString -> [Char]
byteStringToHex = map toLower . concatMap (printf "%02X") . B.unpack