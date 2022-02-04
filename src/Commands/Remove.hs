module Commands.Remove where

import Data.List (delete)
import Logger (logInfoMln, logSuccessLn)
import SystemUtils (Data (Data, entries), DataEntry (diskImagePrefix, name, prefix), dataPath, readData, userHomeDir, writeDataTransac)
import Transaction as TR (Command, fromMaybe)
import Utils (askQuestionRegex, contains, formatForLog, isServiceActive, logEntryMln, readInt, run)

remove :: Text -> Command
remove ind = do
  d <- entries <$> readData
  mntd <- run "mount" []

  choice <- readInt ind
  choice <-
    TR.fromMaybe
      (d !!? (choice - 1))
      ( "The given index is too large! \
        \Number of services in config is only "
          <> show (length d)
      )

  running <- isServiceActive $ name choice
  let mounted = mntd `contains` (name choice <> ".img")

  logEntryMln (liftIO . logInfoMln "" ind) choice mounted running

  askQuestionRegex
    "Enter the name of the service to delete it, \
    \otherwise enter 'exit'"
    ("^" <> name choice <> "$")

  when running $
    fail $
      "The service must not be running!\n\
      \Please stop it manually using the command: \
      \systemctl stop "
        <> toString (name choice)

  hd <- userHomeDir (name choice)
  res <-
    askQuestionRegex
      ( "This systemd service, system user, and disk image along with ALL the files in " <> hd
          <> " will\
             \be permanently deleted -- are you 100% sure you want to do this? (y/n)"
      )
      "^y|n$"
  when (res == "n") $ fail "User cancelled action"

  when mounted $
    void $ do
      let path = prefix choice <> "/mountpoint"
      run "umount" [path]
      liftIO . logSuccessLn $ "Unmounted " <> path

  -- the remove flag also removes the user home directory files
  run "userdel" ["--remove", name choice]
  liftIO . logSuccessLn $ "Removed the user and files in ~"

  -- TODO dynamically fetch this
  run "rm" ["/etc/systemd/system/" <> name choice <> ".service"]
  liftIO . logSuccessLn $ "Removed the systemd service"

  run "rm" [diskImagePrefix choice <> "/" <> name choice <> ".img"]
  liftIO . logSuccessLn $ "Removed the disk image"

  writeDataTransac $ Data (delete choice d)
  p <- liftIO $ toText <$> dataPath
  liftIO . logSuccessLn $ "Removed entry from '" <> p <> "'"

  liftIO . logSuccessLn $ "Successfully removed the service '" <> name choice <> "'"