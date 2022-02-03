module Commands.Remove where

import Logger (logInfoMln)
import SystemUtils (Data (entries), DataEntry (name), readData)
import Transaction as TR (Command, fromMaybe)
import Utils (askQuestionRegex, contains, formatForLog, isServiceActive, logEntryMln, readInt, run)

remove :: Text -> Command
remove ind = do
  d <- entries <$> readData
  mntd <- run "mount" []

  choice <- readInt ind
  choice <-
    TR.fromMaybe
      (d !!? choice)
      ("The given index is too large! Number of services in config is only " <> show (length d))

  running <- isServiceActive $ name choice
  logEntryMln (liftIO . logInfoMln "" ind) choice (mntd `contains` (name choice <> ".img")) running

  askQuestionRegex "Enter the name of the service to delete it, otherwise enter 'exit'" ("^" <> name choice <> "$")

  undefined
