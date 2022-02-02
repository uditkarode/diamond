module Commands.Manual where

import Logger (logInfoLn, logSuccessLn)
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory)
import System.Posix.User (UserEntry (homeDirectory))
import SystemUtils
  ( Data (Data, entries),
    DataEntry (DataEntry, diskImagePrefix, name, prefix),
    dataPath,
    readData,
    userHomeDir,
    writeDataTransac,
  )
import Transaction (Command)
import Utils (askQuestionCondition, contains, run)

manual :: Command
manual = do
  services <- run "systemctl" ["list-unit-files"]
  name <- askQuestionCondition "Name of the systemd service" $ \name -> do
    let exists = services `contains` (name <> ".service")
    let msg = if exists then "Service exists" else "No such service exists!"
    pure (msg, exists)

  liftIO . logInfoLn $ "The prefix is just a folder that contains another folder called 'mountpoint' where the disk image will be mounted"
  defPrefix <- userHomeDir name

  prefix <- askQuestionCondition ("Prefix location (leave blank for '" <> defPrefix <> "'") $ \dir' -> do
    let dir = if dir' == "" then defPrefix else dir'
    exists <- liftIO $ doesDirectoryExist $ toString dir <> "/mountpoint"
    let msg = if exists then "Mountpoint exists" else "'" <> dir <> "/mountpoint'  does not exist!"
    pure (msg, exists)

  diPrefix <- askQuestionCondition "Disk image prefix location" $ \dir -> do
    exists <- doesFileExist $ toString dir <> "/" <> toString name <> ".img"
    let msg = if exists then "Disk image exists" else "Disk image '" <> name <> ".img" <> "' does not exist in this directory!"
    pure (msg, exists)

  let v =
        DataEntry
          { name = name,
            prefix = prefix,
            diskImagePrefix = diPrefix
          }
  steps <- entries <$> readData
  writeDataTransac $ Data $ steps <> [v]
  p <- liftIO $ toText <$> dataPath

  liftIO . logSuccessLn $ "Entry saved to '" <> p <> "'"
