module Utils where

import Control.Exception (try)
import Data.Text (replace, toLower)
import GHC.IO.Exception (IOError)
import Logger (logErrorLn, logInfo)
import System.Console.Pretty
  ( Color (Blue, Green),
    Pretty (color, style),
    Style (Bold),
  )
import System.Process (readProcess)
import SystemUtils (bail)
import Transaction (TransactionT (TransactionT))

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

run :: String -> [String] -> TransactionT (Either Text Text)
run prog args = do
  res <- liftIO (try (readProcess prog args []) :: IO (Either IOError String))
  case res of
    Left e -> pure $ Left (toText $ displayException e)
    Right s -> pure $ Right $ toText s

run' :: String -> [String] -> TransactionT Text
run' prog args = do
  res <- liftIO (try (readProcess prog args []) :: IO (Either IOError String))
  case res of
    Left e -> liftIO (bail (toText $ displayException e)) >> pure ""
    Right s -> pure $ toText s

run'' :: String -> [String] -> TransactionT ()
run'' prog args = run' prog args >> pure ()

askQuestion :: Text -> IO Text
askQuestion question = do
  putTextLn $ color Blue question
  putText $ color Blue "> "
  hFlush stdout
  getLine
