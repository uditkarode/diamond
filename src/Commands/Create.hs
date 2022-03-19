module Commands.Create where

import Data.ByteUnits (ByteUnit (Bytes), ByteValue (ByteValue), getAppropriateUnits, getShortHand)
import Data.Text (replace, toLower)
import Logger (logErrorLn, logInfoLn, logSuccessLn)
import System.Directory (createDirectory, doesDirectoryExist)
import System.DiskSpace (getAvailSpace)
import System.Process (cwd, runCommand, shell)
import SystemUtils
  ( Data (Data, entries),
    DataEntry (DataEntry, diskImagePrefix, name, prefix),
    bail,
    dataPath,
    doesUserExist,
    dummyService,
    readData,
    userHomeDir,
    writeDataTransac,
  )
import Transaction (Command, Reversal (..), Step, Transaction (Transaction), addReversal, getReversals, makeStep)
import Utils (askQuestion, askQuestionRegex, replacePlaceholders, run, run', runAs, runAsR, runR, sanitise)

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
  runAs name "git" ["clone", url, homeDir <> "/mountpoint/src"]
  makeStep "Cloning repo in new user's home" $
    Reversal
      { userMsg = "Removing cloned source directory",
        reversal = void $ runAsR name "rm" ["-r", homeDir <> "/mountpoint/src"]
      }

createSystemdService :: Text -> Text -> Text -> Text -> Text -> Step
createSystemdService name homeDir command ramLimit cpuLimit = do
  let serviceTxt =
        replacePlaceholders
          dummyService
          [ ("name", name),
            ("home", homeDir <> "/mountpoint/src"),
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
createDiskImage name loc size = do
  let path = loc <> "/" <> name <> ".img"
  run "fallocate" ["--length", size, path]
  run "mkfs.ext4" [path]
  makeStep "Creating the disk image" $
    Reversal
      { userMsg = "Removing the disk image",
        reversal = void $ runR "rm" [path]
      }

mountDiskImage :: Text -> Text -> Text -> Step
mountDiskImage name loc homeDir = do
  run "mount" [loc <> "/" <> name <> ".img", homeDir <> "/mountpoint"]
  run "chown" [name, homeDir <> "/mountpoint"]
  makeStep "Mounting the disk image" $
    Reversal
      { userMsg = "Unmounting the disk image",
        reversal = void $ runR "umount" [homeDir <> "/mountpoint"]
      }

addToData :: Text -> Text -> Text -> Step
addToData name prefix diPrefix = do
  let v =
        DataEntry
          { name = name,
            prefix = prefix,
            diskImagePrefix = diPrefix
          }
  steps <- entries <$> readData
  writeDataTransac $ Data $ steps <> [v]
  p <- liftIO $ toText <$> dataPath
  makeStep ("Adding entry to '" <> p <> "'") $
    Reversal
      { userMsg = "Removing entry from '" <> p <> "'",
        reversal = writeDataTransac $ Data steps
      }

-- the root command function
create :: Command
create = do
  name <- sanitise <$> askQuestion "Name of the service"

  -- create a user for the service
  liftIO $ logInfoLn "Checking for availability..."
  liftIO $ doesUserExist name >>= flip when (bail "A user by this name already exists!")

  st <- addUser name
  homeDir <- userHomeDir name
  liftIO $ logSuccessLn st

  -- ask questions about the disk image
  rloc <- askQuestion "Where do you want to place the disk image? Leave blank for user home"
  let loc = if rloc == "" then homeDir else rloc
  v <- liftIO $ (doesDirectoryExist . toString) loc
  unless v $ fail "No such directory exists!"

  ds <- liftIO $ getAvailSpace (toString loc)
  liftIO $ logInfoLn $ "Free space remaining on target: " <> (toText . getShortHand . getAppropriateUnits $ ByteValue (fromIntegral ds) Bytes)
  diSize <- askQuestionRegex "What is the size of the disk image for this service?" "\\d{0,5}G|M|K"

  st <- createDiskImage name loc diSize
  liftIO $ logSuccessLn st

  st <- mountDiskImage name loc homeDir
  liftIO $ logSuccessLn st

  -- clone the source code
  url <- askQuestion "Link to the git repository of the service"
  st <- cloneRepo name url homeDir
  liftIO $ logSuccessLn st

  -- ask a few more general questions for the service setup
  command <- askQuestion "Command to run the service"
  ramLimit <- askQuestion "What RAM limit do you want to assign? (e.g. 800M / 2G)"
  cpuLimit <- askQuestion "What CPU limit do you want to assign? (e.g.) 200%"

  st <- createSystemdService name homeDir command ramLimit cpuLimit
  liftIO $ logSuccessLn st

  st <- addToData name homeDir loc
  liftIO $ logSuccessLn st

  liftIO $ logSuccessLn $ "'" <> name <> "' has been created successfully!"
