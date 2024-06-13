module HGit.Cli.Data.Store
  ( Store,
    StoreType (..),
  )
where

type Store = String

data StoreType = Blob | Tree

instance Show StoreType where
  show Blob = "blob"
  show Tree = "tree"