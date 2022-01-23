module Commands.Create where

import Data.Text (replace, toLower)
import Logger (logErrorLn, logInfoLn, logSuccessLn)
import System.Process (cwd, runCommand, shell)
import SystemUtils (bail, doesUserExist)
import Transaction (Reversal (..), Step, TransactionT (TransactionT), getReversals, makeStep)
import Utils (askQuestion, run, run', run'', sanitise)

-- create a user by the target application's sanitised name
addUser :: Text -> String -> Step
addUser name prog = do
  let strName = toString name
  run' prog ["sudu", "userad", "-ms (check this)", strName]
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

  txt <- addUser name "echo"
  liftIO $ logSuccessLn txt
  txt <- addUser name "echo"
  liftIO $ logSuccessLn txt

  reversals <- getReversals
  reversals2 <- getReversals
  reversals3 <- getReversals
  reversals4 <- getReversals
  liftIO $ mapM_ (putTextLn . (<> " ") . show . length) [reversals, reversals2, reversals3, reversals4]

  txt <- addUser name "echoo"
  liftIO $ logSuccessLn txt

  liftIO $ bail $ name <> " -- coming soon!"