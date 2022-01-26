module Commands.Create where

import Data.Text (replace, toLower)
import Logger (logErrorLn, logInfoLn, logSuccessLn)
import System.Directory (createDirectory)
import System.Process (cwd, runCommand, shell)
import SystemUtils (bail, doesUserExist, dummyService, userHomeDir)
import Transaction (Reversal (..), Step, Transaction (Transaction), addReversal, getReversals, makeStep)
import Utils (askQuestion, replacePlaceholders, run, run', runAs, runAsR, runR, sanitise)

-- create a user by the target application's sanitised name
addUser :: Text -> Step
addUser name = do
  run "useradd" ["--home-dir", "/var/apps/" <> name, "--system", "--create-home", name]
  liftIO $ createDirectory $ "/var/apps/" <> toString name <> "/mountpoint"
  makeStep "Creating user account" $
    Reversal
      { userMsg = "Removing daemon user account",
        reversal = void $ runR "userdel" ["--remove", name]
      }

-- clone the repo in the new user's home
cloneRepo :: Text -> Text -> Text -> Step
cloneRepo name url homeDir = do
  runAs name "git" ["clone", url, homeDir <> "/src"]
  makeStep "Cloning repo in new user's home" $
    Reversal
      { userMsg = "Removing cloned source directory",
        reversal = void $ runAsR name "rm" ["-r", homeDir <> "/src"]
      }

createSystemdService :: Text -> Text -> Text -> Text -> Text -> Step
createSystemdService name homeDir command ramLimit cpuLimit = do
  let serviceTxt =
        replacePlaceholders
          dummyService
          [ ("name", name),
            ("home", homeDir),
            ("command", command),
            ("ram-limit", ramLimit),
            ("cpu-limit", cpuLimit)
          ]
  let serviceFilePath = "/etc/systemd/system/" <> toString name <> ".service"
  liftIO $ writeFileText serviceFilePath serviceTxt
  makeStep "Creating the systemd service" $
    Reversal
      { userMsg = "Removing the systemd service",
        reversal = void $ runR "rm" [toText serviceFilePath]
      }

createDiskImage :: Text -> Text -> Text -> Step
createDiskImage name homeDir size = do
  run "fallocate" ["--length", size, homeDir <> "/" <> name <> ".img"]
  makeStep "Creating the disk image" $
    Reversal
      { userMsg = "Removing the disk image",
        reversal = void $ runR "rm" []
      }

-- the root command function
create :: Transaction ()
create = do
  name <- liftIO $ sanitise <$> askQuestion "Name of the service"

  -- create a user for the service
  liftIO $ logInfoLn "Checking for availability..."
  liftIO $ doesUserExist name >>= flip when (bail "A user by this name already exists!")

  st <- addUser name
  homeDir <- liftIO . userHomeDir $ name
  liftIO $ logSuccessLn st

  -- clone the source code
  url <- liftIO $ askQuestion "Link to the git repository of the service"
  st <- cloneRepo name url homeDir
  liftIO $ logSuccessLn st

  -- ask questions about the disk image size
  -- TODO free space check and regex validation of size
  diSize <- liftIO $ askQuestion "What is the size of the disk image for this service?"
  createDiskImage name homeDir diSize

  -- ask a few more general questions for the service setup
  command <- liftIO $ askQuestion "Command to run the service"
  ramLimit <- liftIO $ askQuestion "What RAM limit do you want to assign? (e.g. 800M / 2G)"
  cpuLimit <- liftIO $ askQuestion "What CPU limit do you want to assign? (e.g.) 200%"

  st <- createSystemdService name homeDir command ramLimit cpuLimit
  liftIO $ logSuccessLn st

  liftIO $ bail $ name <> " -- coming soon!"