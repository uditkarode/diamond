module Diamond where

import qualified Commands.Create as CreateCommand (create)
import SystemUtils (CliArgs (CliArgs), bail)
import Transaction (Transaction (runTransaction))

diamond :: CliArgs -> IO ()
diamond (CliArgs create remove) =
  if
      | create -> runTransaction CreateCommand.create [] >> pure ()
      | create && isJust remove -> bail "Cannot remove and create at the same time!"
      | otherwise -> bail "Invalid or no command! Please read --help"