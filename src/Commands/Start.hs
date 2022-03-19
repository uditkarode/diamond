module Commands.Start where

import Logger (logErrorLn, logInfoLn, logSuccessLn)
import SystemUtils
  ( Data (entries),
    DataEntry (diskImagePrefix, name, prefix),
    readData,
  )
import Transaction (Command)
import Utils (contains, isServiceActive, run, run')

start :: Command
start = do
  d <- entries <$> readData
  mntd <- run "mount" []
  forM_ d $ \v -> do
    running <- isServiceActive $ name v
    let mounted = mntd `contains` (name v <> ".img")
    if running
      then liftIO $ logInfoLn $ "service '" <> name v <> "' is already running"
      else
        if mounted
          then do
            r <- run' "systemctl" ["start", name v]
            case r of
              Left e -> liftIO . logErrorLn $ "Failed to start '" <> name v <> "': " <> e
              Right _ -> liftIO . logSuccessLn $ "Started '" <> name v <> "'"
          else liftIO $ logInfoLn $ "image for '" <> name v <> "' is not mounted"