module HGit.Cli.Utils.Codec
  ( createStore,
    decompress,
  )
where

import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BLSC
import HGit.Cli.Data.Store

createStore :: StoreType -> String -> Store
createStore storeType fileContent = show storeType <> " " <> (show . length) fileContent <> ['\0'] <> fileContent

decompress :: BS.ByteString -> String
decompress = BLSC.unpack . Zlib.decompress . BS.fromStrict