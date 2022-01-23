module Logger where

import qualified Data.Text.IO as TIO
import System.Console.Pretty
  ( Color (Green, Red, Yellow),
    Pretty (color, style),
    Style (Bold),
  )

log :: Text -> Style -> Color -> Bool -> Text -> IO ()
log marker s c n toLog = do
  TIO.hPutStr stderr $ style s . color c $ "[" <> marker <> "] "
  (if n then TIO.hPutStrLn else TIO.hPutStr) stderr toLog

-- error
le :: Bool -> Text -> IO ()
le = log "x" Bold Red

logError :: Text -> IO ()
logError = le False

logErrorLn :: Text -> IO ()
logErrorLn = le True

-- info
li :: Bool -> Text -> IO ()
li = log "!" Bold Yellow

logInfo :: Text -> IO ()
logInfo = li False

logInfoLn :: Text -> IO ()
logInfoLn = li True

-- success
ls :: Bool -> Text -> IO ()
ls = log "✓" Bold Green

logSuccess :: Text -> IO ()
logSuccess = ls False

logSuccessLn :: Text -> IO ()
logSuccessLn = ls True
