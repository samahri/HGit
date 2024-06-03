module HGit.Cli.Utils.CodecSpec (spec) where

import HGit.Cli.Data.Store
import HGit.Cli.Utils.Codec (createStore)
import Test.Hspec

spec :: Spec
spec = do
  describe "HGit.Cli.Utils.Codec" $ do
    describe "createStore" $ do
      it "creates the store of 'hello world'" $ do
        let store = createStore Blob "hello world"
        store `shouldBe` "blob 11\0hello world"