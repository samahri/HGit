module HGit.Cli.Commands.Utils
  ( calculateStoreAndHash,
    getStore,
    hashStore,
    compress,
    saveObjectToDatabase,
    notNull,
  )
where

import qualified Codec.Compression.Zlib as Zlib
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Char (toLower)
import Data.Functor ((<&>))
import HGit.Cli.Data.Store
import HGit.Cli.Utils.Codec (createStore)
import Safe.Exact (dropExact, takeExact)
import System.Directory
import Text.Printf (printf)

calculateStoreAndHash :: FilePath -> StoreType -> IO (String, Store)
calculateStoreAndHash inputFile storeType = do
  store <- readFile inputFile <&> createStore storeType
  pure (store, hashStore store)

-- deprecated
getStore :: FilePath -> String -> IO String
getStore inputFile storeType = do
  fileContent <- readFile inputFile
  pure (storeType <> " " <> show (length fileContent) <> ['\0'] <> fileContent)

-- create a hash value for the file
-- rename to getHash
hashStore :: String -> [Char]
hashStore = byteStringToHex . SHA1.hash . BC.pack

compress :: String -> B.ByteString
compress = B.toStrict . Zlib.compress . LBC.pack

byteStringToHex :: B.ByteString -> [Char]
byteStringToHex = map toLower . concatMap (printf "%02X") . B.unpack

saveObjectToDatabase :: (String, Store) -> IO ()
saveObjectToDatabase (hashValue, store) = do
  folderPath <- getCurrentDirectory <&> (<> "/.git/objects/" <> folderName)

  createDirectoryIfMissing False folderPath
  let objectFilePath = folderPath <> "/" <> objectFileName
  B.writeFile objectFilePath compressedData
  where
    compressedData = compress store
    folderName = takeExact 2 hashValue
    objectFileName = dropExact 2 hashValue

notNull :: (Foldable f) => f a -> Bool
notNull = not . null