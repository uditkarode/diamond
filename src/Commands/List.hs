module Commands.List where

import qualified Data.Text as T
import Logger (logErrorMln, logInfoLn, logSuccessMln)
import System.Console.Pretty (Color (Green, Red), Pretty (color, style), Style (Bold))
import SystemUtils (Data (entries), DataEntry (diskImagePrefix, name, prefix), readData)
import Transaction (Command, Transaction (Transaction))
import Utils (contains, foldl, isServiceActive, run)

formatForLog :: [(Text, Text)] -> Text
formatForLog arr = T.dropEnd 1 (foldl arr "" $ \curr acc -> acc <> style Bold (fst curr) <> ": " <> snd curr <> "\n")

bcol :: Pretty c => Color -> c -> c
bcol c = style Bold . color c

list :: Command
list = do
  d <- entries <$> readData
  mntd <- run "mount" []
  forM_ d $ \v -> do
    running <- isServiceActive $ name v
    let yn bool = if bool then bcol Green "yes" else bcol Red "no"
    let fn = liftIO . if running then logSuccessMln "" else logErrorMln ""
    fn . formatForLog $
      [ ("name", name v),
        ("mountpoint", prefix v <> "/mountpoint"),
        ("disk image", diskImagePrefix v <> "/" <> name v <> ".img"),
        ("mounted", yn $ mntd `contains` (name v <> ".img")),
        ("running", yn running)
      ]
    putTextLn ""
