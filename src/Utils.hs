module Utils where

import Logger (logError, logInfo)
import System.Console.Pretty (Color (Yellow), Pretty (color))

data CliArgs = CliArgs
  { create :: Bool,
    remove :: Maybe String
  }
  deriving (Show)

bail :: Text -> IO ()
bail msg = do
  logError msg
  exitFailure

askQuestion :: Text -> IO Text
askQuestion question = do
  putTextLn $ color Yellow question
  putText $ color Yellow "> "
  hFlush stdout
  getLine
