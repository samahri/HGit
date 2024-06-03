module HGit.Cli.Data.Store
  ( Store,
    StoreType (..),
  )
where

type Store = String

data StoreType = Blob

instance Show StoreType where
  show Blob = "blob"