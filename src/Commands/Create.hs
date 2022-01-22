module Commands.Create where

import Data.Text (replace, toLower)
import Logger (logErrorLn, logInfoLn)
import System.Process (cwd, runCommand, shell)
import SystemUtils (doesUserExist)
import Transaction (Reversal)
import Utils (askQuestion, bail, run, sanitise)

create :: StateT [Reversal] IO ()
create = do
  name <- liftIO $ sanitise <$> askQuestion "What is the name of the application?"

  -- check if a user by the same name exists
  liftIO $ logInfoLn "Checking for availability..."
  liftIO $ doesUserExist name >>= flip when (bail "A user by this name already exists!")

  -- create a user by the target application's sanitised name
  let action = "creating user account"
  liftIO $ run ("sudo useradd -s /bin/zsh " <> toString name) []

  liftIO $ bail $ name <> " -- coming soon!"