module Utils where

import Control.Exception (try)
import Control.Exception.Base (try)
import Data.Text (replace, toLower)
import qualified Data.Text as T
import Data.Text.Internal.Search (indices)
import GHC.IO.Exception (IOError)
import Logger (logErrorLn, logInfo, logSuccessLn)
import System.Console.Pretty
  ( Color (Blue, Green, Red),
    Pretty (color, style),
    Style (Bold),
  )
import System.Process (readProcess)
import SystemUtils (DataEntry (diskImagePrefix, name, prefix))
import Text.Regex.TDFA ((=~))
import Transaction (Transaction (Transaction))

foldl :: Foldable f => f a -> b -> (a -> b -> b) -> b
foldl a d l = flipfoldl' l d a

-- e.g. [("a", "b"), ("c", "d")] becomes:
-- a: b\nc: d
formatForLog :: [(Text, Text)] -> Text
formatForLog arr = T.dropEnd 1 (foldl arr "" $ \curr acc -> acc <> style Bold (fst curr) <> ": " <> snd curr <> "\n")

-- logs an entry over multiple lines showing necessary details in a nice format
logEntryMln :: (Text -> Transaction ()) -> DataEntry -> Bool -> Bool -> Transaction ()
logEntryMln fn v mntd running = do
  let bcol c = style Bold . color c
  let yn bool = if bool then bcol Green "yes" else bcol Red "no"
  fn . formatForLog $
    [ ("name", name v),
      ("mountpoint", prefix v <> "/mountpoint"),
      ("disk image", diskImagePrefix v <> "/" <> name v <> ".img"),
      ("mounted", yn mntd),
      ("running", yn running)
    ]

-- e.g. [("something", 3)] replaces <diamond-something> with 3
replacePlaceholders :: Text -> [(Text, Text)] -> Text
replacePlaceholders txt vals = foldl vals txt $ \curr acc -> do
  replace ("<diamond-" <> fst curr <> ">") (snd curr) acc

sanitise :: Text -> Text
sanitise = replace " " "-" . toLower

-- checks if x is contained in y
contains :: Text -> Text -> Bool
contains x y = case indices y x of
  (idx : _) -> True
  _ -> False

-- like `read`, but `fail`s on failure
readInt :: Text -> Transaction Int
readInt t = do
  let v = readEither (toString t) :: Either Text Int
  case v of
    Left _ -> fail "Invalid input, please enter an integer"
    Right v -> pure v

-- checks if a systemd service by the given name is active
isServiceActive :: Text -> Transaction Bool
isServiceActive name = do
  v <- run' "systemctl" ["is-active", name]
  case v of
    Left e -> pure False
    Right v -> pure $ v `contains` "active"

-- asks a question on the cli
askQuestion :: Text -> Transaction Text
askQuestion question = do
  putTextLn $ color Blue question
  putText $ color Blue "> "
  hFlush stdout
  v' <- (liftIO $ try getLine) :: Transaction (Either SomeException Text)
  case v' of
    Left _ -> (liftIO . logErrorLn) "Invalid input or I/O error" >> askQuestion question
    Right v -> if v == "exit" then fail "Cancelled by user" else pure v

-- keeps asking until the given regex is satisfied
askQuestionRegex :: Text -> Text -> Transaction Text
askQuestionRegex question regex = do
  v <- askQuestion question
  if v =~ regex
    then pure v
    else do
      liftIO $ logErrorLn "Invalid input!"
      askQuestionRegex question regex

-- keeps asking until a custom given condition is met
askQuestionCondition :: Text -> (Text -> IO (Text, Bool)) -> Transaction Text
askQuestionCondition qn fn = do
  v <- askQuestion qn
  cres <- liftIO $ fn v
  liftIO . (if snd cres then logSuccessLn else logErrorLn) $ fst cres
  if snd cres
    then pure v
    else askQuestion qn

--
-- helper functions for running commands

-- runs a command and denotes failures with Either
run' :: Text -> [Text] -> Transaction (Either Text Text)
run' prog args = do
  res <- liftIO (try (readProcess (toString prog) (map toString args) []) :: IO (Either IOError String))
  case res of
    Left e -> pure $ Left (toText $ displayException e)
    Right s -> pure $ Right $ toText s

-- same as run' but runs as another user
runAs' :: Text -> Text -> [Text] -> Transaction (Either Text Text)
runAs' user prog args = run' "sudo" $ ["-su", user, prog] <> args

-- runs a command and `fail`s on command failure
run :: Text -> [Text] -> Transaction Text
run prog args = do
  res <- liftIO (try (readProcess (toString prog) (map toString args) []) :: IO (Either IOError String))
  case res of
    Left e -> fail $ displayException e
    Right s -> pure $ toText s

-- same as `run`, but does not `fail` so that reversals don't recurse
runR :: Text -> [Text] -> Transaction Text
runR prog args = do
  res <- liftIO (try (readProcess (toString prog) (map toString args) []) :: IO (Either IOError String))
  case res of
    Left e -> liftIO (logErrorLn . toText $ displayException e) >> pure ""
    Right s -> pure $ toText s

-- same as run but runs as another user
runAs :: Text -> Text -> [Text] -> Transaction Text
runAs user prog args = run "sudo" $ ["-su", user, prog] <> args

-- same as runR but runs as another user
runAsR :: Text -> Text -> [Text] -> Transaction Text
runAsR user prog args = runR "sudo" $ ["-su", user, prog] <> args
