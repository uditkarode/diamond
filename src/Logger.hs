module Logger where

import qualified Data.Text.IO as TIO
import System.Console.Pretty
  ( Color (Red),
    Pretty (color, style),
    Style (Bold),
  )

logError :: Text -> IO ()
logError toLog = do
  TIO.hPutStr stderr $ style Bold . color Red $ "[×] "
  TIO.hPutStrLn stderr toLog