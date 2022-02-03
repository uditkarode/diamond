module Commands.List where

import Logger (logErrorMln, logSuccessMln)
import SystemUtils (Data (entries), DataEntry (name), readData)
import TextShow (TextShow (showt))
import Transaction (Command)
import Utils (contains, isServiceActive, logEntryMln, run)

list :: Command
list = do
  d <- entries <$> readData
  mntd <- run "mount" []
  forM_ (zip [(1 :: Int) ..] d) $ \v' -> do
    let v = snd v'
    let ind = showt $ fst v'

    running <- isServiceActive $ name v
    let fn = liftIO . (if running then logSuccessMln else logErrorMln) "" ind
    logEntryMln fn v (mntd `contains` (name v <> ".img")) running
    putTextLn ""
