module HGit.Cli.Utils.Codec
  ( createStore,
    decompress,
    compress,
  )
where

import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LBC
import HGit.Cli.Data.Store

createStore :: StoreType -> String -> Store
createStore storeType fileContent = show storeType <> " " <> (show . length) fileContent <> ['\0'] <> fileContent

decompress :: B.ByteString -> String
decompress = LBC.unpack . Zlib.decompress . B.fromStrict

compress :: String -> B.ByteString
compress = B.toStrict . Zlib.compress . LBC.pack