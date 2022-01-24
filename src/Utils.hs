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
import Transaction (Transaction (Transaction))

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

run' :: Text -> [Text] -> Transaction (Either Text Text)
run' prog args = do
  res <- liftIO (try (readProcess (toString prog) (map toString args) []) :: IO (Either IOError String))
  case res of
    Left e -> pure $ Left (toText $ displayException e)
    Right s -> pure $ Right $ toText s

runAs' :: Text -> Text -> [Text] -> Transaction (Either Text Text)
runAs' user prog args = run' "sudo" $ ["-su", user, prog] <> args

run :: Text -> [Text] -> Transaction Text
run prog args = do
  res <- liftIO (try (readProcess (toString prog) (map toString args) []) :: IO (Either IOError String))
  case res of
    Left e -> fail $ displayException e
    Right s -> pure $ toText s

runR :: Text -> [Text] -> Transaction Text
runR prog args = do
  res <- liftIO (try (readProcess (toString prog) (map toString args) []) :: IO (Either IOError String))
  case res of
    Left e -> liftIO (logErrorLn . toText $ displayException e) >> pure ""
    Right s -> pure $ toText s

runAs :: Text -> Text -> [Text] -> Transaction Text
runAs user prog args = run "sudo" $ ["-su", user, prog] <> args

runAsR :: Text -> Text -> [Text] -> Transaction Text
runAsR user prog args = runR "sudo" $ ["-su", user, prog] <> args

askQuestion :: Text -> IO Text
askQuestion question = do
  putTextLn $ color Blue question
  putText $ color Blue "> "
  hFlush stdout
  getLine
