module Commands.Mount where

import Logger (logErrorLn, logSuccessLn)
import SystemUtils
  ( Data (entries),
    DataEntry (diskImagePrefix, name, prefix),
    readData,
  )
import Transaction (Command)
import Utils (run')

mount :: Command
mount = do
  d <- entries <$> readData
  forM_ d $ \v -> do
    r <- run' "mount" [diskImagePrefix v <> "/" <> name v <> ".img", prefix v <> "/mountpoint"]
    case r of
      Left e -> liftIO . logErrorLn $ "Failed for '" <> name v <> "': " <> e
      Right _ -> liftIO . logSuccessLn $ "Mounted for '" <> name v <> "'"
