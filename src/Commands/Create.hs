module Commands.Create where

import Data.Text (replace, toLower)
import Logger (logErrorLn, logInfoLn, logSuccessLn)
import System.Process (cwd, runCommand, shell)
import SystemUtils (bail, doesUserExist)
import Transaction (Reversal (..), Step, TransactionT (TransactionT), addReversal, getReversals, makeStep)
import Utils (askQuestion, run, run', run'', sanitise)

-- create a user by the target application's sanitised name
addUser :: Text -> Step
addUser name = do
  let strName = toString name
  run' "echo" ["sudo", "useradd", "-s", strName]
  makeStep "Creating user account" $
    Reversal
      { userMsg = "Reversing creation of user account",
        reversal = run'' "echo" ["userremov", strName]
      }

create :: TransactionT ()
create = do
  name <- liftIO $ sanitise <$> askQuestion "What is the name of the application?"

  -- check if a user by the same name exists
  liftIO $ logInfoLn "Checking for availability..."
  liftIO $ doesUserExist name >>= flip when (bail "A user by this name already exists!")

  name <- addUser name
  liftIO $ logSuccessLn name

  run'' "ahahahah" []

  liftIO $ bail $ name <> " -- coming soon!"