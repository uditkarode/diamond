module Commands.Mount where

import Data.Text as T
import Logger (logErrorLn, logInfoLn, logSuccessLn)
import SystemUtils
  ( Data (entries),
    DataEntry (diskImagePrefix, name, prefix),
    readData,
  )
import Transaction (Command)
import Utils (contains, run, run')

mount :: Command
mount = do
  d <- entries <$> readData
  mntd <- run "mount" []
  forM_ d $ \v -> do
    if mntd `contains` (name v <> ".img")
      then liftIO $ logInfoLn $ "image for '" <> name v <> "' is already mounted"
      else do
        r <- run' "mount" [diskImagePrefix v <> "/" <> name v <> ".img", prefix v <> "/mountpoint"]
        case r of
          Left e -> liftIO . logErrorLn $ "Failed for '" <> name v <> "': " <> e
          Right _ -> liftIO . logSuccessLn $ "Mounted for '" <> name v <> "'"
