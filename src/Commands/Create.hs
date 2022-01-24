module Commands.Create where

import Data.Text (replace, toLower)
import Logger (logErrorLn, logInfoLn, logSuccessLn)
import System.Process (cwd, runCommand, shell)
import SystemUtils (bail, doesUserExist, userHomeDir)
import Transaction (Reversal (..), Step, TransactionT (TransactionT), addReversal, getReversals, makeStep)
import Utils (askQuestion, run, run', runAs, sanitise)

-- create a user by the target application's sanitised name
addUser :: Text -> Step
addUser name = do
  run "useradd" ["--home-dir", "/var/apps/" <> name, "--system", "--create-home", name]
  makeStep "Creating user account" $
    Reversal
      { userMsg = "Reversing creation of user account",
        reversal = void $ run "userdel" ["--remove", name]
      }

cloneRepo :: Text -> Text -> Step
cloneRepo name url = do
  homeDir <- liftIO . userHomeDir $ name
  run "git" ["clone", url, homeDir <> "/src"]
  makeStep "Cloning repo in new user's home" $
    Reversal
      { userMsg = "Removing cloned source directory",
        reversal = void $ runAs name "rm" ["-r", homeDir <> "/" <> name <> "/src"]
      }

create :: TransactionT ()
create = do
  name <- liftIO $ sanitise <$> askQuestion "What is the name of the application?"

  -- create a user for the service
  liftIO $ logInfoLn "Checking for availability..."
  liftIO $ doesUserExist name >>= flip when (bail "A user by this name already exists!")

  st <- addUser name
  liftIO $ logSuccessLn st

  -- clone the source code
  url <- liftIO $ sanitise <$> askQuestion "Link to the git repository of the application"

  st <- cloneRepo name url
  liftIO $ logSuccessLn st

  liftIO $ bail $ name <> " -- coming soon!"