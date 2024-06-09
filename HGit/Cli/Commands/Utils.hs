module HGit.Cli.Commands.Utils
  ( calculateStoreAndHash,
    saveObjectToDatabase,
    getHash,
    notNull,
  )
where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import Data.Functor ((<&>))
import HGit.Cli.Data.Store
import HGit.Cli.Utils.Codec (compress, createStore)
import Safe.Exact (dropExact, takeExact)
import System.Directory
import Text.Printf (printf)

type Hashcode = String

calculateStoreAndHash :: FilePath -> StoreType -> IO (Hashcode, Store)
calculateStoreAndHash inputFile storeType = do
  store <- readFile inputFile <&> createStore storeType
  pure (hashByteStringToHex store, store)
  where
    hashByteStringToHex :: Store -> [Char]
    hashByteStringToHex = map toLower . concatMap (printf "%02X") . B.unpack . getHash

saveObjectToDatabase :: (Hashcode, Store) -> IO ()
saveObjectToDatabase (hashValue, store) = do
  let compressedData = compress store
      folderName = takeExact 2 hashValue
      objectFileName = dropExact 2 hashValue

  folderPath <- getCurrentDirectory <&> (<> "/.git/objects/" <> folderName)

  createDirectoryIfMissing False folderPath
  let objectFilePath = folderPath <> "/" <> objectFileName
  B.writeFile objectFilePath compressedData

-- create a hash value for the file
getHash :: Store -> B.ByteString
getHash = SHA1.hash . BC.pack

notNull :: (Foldable f) => f a -> Bool
notNull = not . null