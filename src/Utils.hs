module Utils where

import Control.Exception (try)
import Control.Exception.Base (try)
import Data.Text (replace, toLower)
import Data.Text.Internal.Search (indices)
import GHC.IO.Exception (IOError)
import Logger (logErrorLn, logInfo, logSuccessLn)
import System.Console.Pretty
  ( Color (Blue, Green),
    Pretty (color, style),
    Style (Bold),
  )
import System.Process (readProcess)
import SystemUtils (bail)
import Text.Regex.TDFA ((=~))
import Transaction (Transaction (Transaction))

foldl :: Foldable f => f a -> b -> (a -> b -> b) -> b
foldl a d l = flipfoldl' l d a

replacePlaceholders :: Text -> [(Text, Text)] -> Text
replacePlaceholders txt vals = foldl vals txt $ \curr acc -> do
  replace ("<diamond-" <> fst curr <> ">") (snd curr) acc

sanitise :: Text -> Text
sanitise = replace " " "-" . toLower

contains :: Text -> Text -> Bool
contains x y = case indices y x of
  (idx : _) -> True
  _ -> False

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

isServiceActive :: Text -> Transaction Bool
isServiceActive name = do
  v <- run' "systemctl" ["is-active", name]
  case v of
    Left e -> pure False
    Right v -> pure $ v `contains` "active"

askQuestion :: Text -> Transaction Text
askQuestion question = do
  putTextLn $ color Blue question
  putText $ color Blue "> "
  hFlush stdout
  v' <- (liftIO $ try getLine) :: Transaction (Either SomeException Text)
  case v' of
    Left _ -> (liftIO . logErrorLn) "Invalid input or I/O error" >> askQuestion question
    Right v -> if v == "exit" then fail "Cancelled by user" else pure v

askQuestionRegex :: Text -> Text -> Transaction Text
askQuestionRegex question regex = do
  v <- askQuestion question
  if v =~ regex
    then pure v
    else do
      liftIO $ logErrorLn "Invalid input!"
      askQuestionRegex question regex

askQuestionCondition :: Text -> (Text -> IO (Text, Bool)) -> Transaction Text
askQuestionCondition qn fn = do
  v <- askQuestion qn
  cres <- liftIO $ fn v
  liftIO . (if snd cres then logSuccessLn else logErrorLn) $ fst cres
  if snd cres
    then pure v
    else askQuestion qn
