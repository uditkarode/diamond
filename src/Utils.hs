module Utils where

data CliArgs = CliArgs
  { create :: Bool,
    remove :: Maybe String
  }
  deriving (Show)
