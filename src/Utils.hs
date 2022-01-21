module Utils where

import Logger (logErrorLn, logInfo)
import System.Console.Pretty (Color (Blue, Yellow), Pretty (color))

data CliArgs = CliArgs
  { create :: Bool,
    remove :: Maybe String
  }
  deriving (Show)

bail :: Text -> IO ()
bail msg = do
  logErrorLn msg
  exitFailure

askQuestion :: Text -> IO Text
askQuestion question = do
  putTextLn $ color Blue question
  putText $ color Blue "> "
  hFlush stdout
  getLine
