module Commands.Create where

import Data.Text (replace, toLower)
import Logger (logInfoLn)
import System.Process (cwd, runCommand, shell)
import SystemUtils (doesUserExist)
import Utils (askQuestion, bail)

sanitise :: Text -> Text
sanitise = replace " " "-" . toLower

create :: IO ()
create = do
  name <- sanitise <$> askQuestion "What is the name of the application?"

  -- check if a user by the same name exists
  logInfoLn "Checking for availability..."
  exists <- doesUserExist name
  when exists $ bail "A user by this name already exists!"

  bail $ name <> " -- coming soon!"