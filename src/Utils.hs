module Utils where

import Logger (logError)

data CliArgs = CliArgs
  { create :: Bool,
    remove :: Maybe String
  }
  deriving (Show)

bail :: Text -> IO ()
bail msg = do
  logError msg
  exitFailure
