module Commands.Create where

import Data.Text (replace, toLower)
import Logger (logErrorLn, logInfoLn, logSuccessLn)
import System.Process (cwd, runCommand, shell)
import SystemUtils (bail, doesUserExist)
import Transaction (Reversal (..), Transaction, TransactionT (TransactionT), makeTransaction)
import Utils (askQuestion, run, run', run'', sanitise)

-- create a user by the target application's sanitised name
addUser :: Text -> Transaction
addUser name = do
  let strName = toString name
  run' "echoo" ["sudo", "userad", "-ms (check this)", strName]
  makeTransaction "Creating user account" $
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

  txt <- addUser name
  liftIO $ logSuccessLn txt

  liftIO $ bail $ name <> " -- coming soon!"