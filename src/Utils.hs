module Utils where

import Control.Exception (try)
import Data.Text (replace, toLower)
import GHC.IO.Exception (IOError)
import Logger (logErrorLn, logInfo)
import System.Console.Pretty
import System.Process (readProcess)

data CliArgs = CliArgs
  { create :: Bool,
    remove :: Maybe String
  }
  deriving (Show)

sanitise :: Text -> Text
sanitise = replace " " "-" . toLower

actionHeader' :: Text -> Int -> Int -> IO ()
actionHeader' txt cur tot = do
  putText $ style Bold . color Blue $ "[" <> "" <> "/" <> "" <> "]"
  putText $ style Bold . color Green $ txt

actionHeader :: Text -> Int -> Int -> IO ()
actionHeader txt cur tot = putTextLn "" >> actionHeader' txt cur tot

bail :: Text -> IO ()
bail msg = do
  logErrorLn msg
  exitFailure

run :: String -> [String] -> IO (Either Text Text)
run prog args = do
  res <- try (readProcess prog args []) :: IO (Either IOError String)
  case res of
    Left e -> pure $ Left (toText $ displayException e)
    Right s -> pure $ Right $ toText s

askQuestion :: Text -> IO Text
askQuestion question = do
  putTextLn $ color Blue question
  putText $ color Blue "> "
  hFlush stdout
  getLine
