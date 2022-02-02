module Diamond where

import qualified Commands.Create as CreateCommand (create)
import qualified Commands.Manual as ManualCommand (manual)
import qualified Commands.Mount as MountCommand (mount)
import SystemUtils (CliArgs (CliArgs), bail)
import Transaction (Transaction (runTransaction))

diamond :: CliArgs -> IO ()
diamond (CliArgs create mount manual remove) =
  if
      | create -> runTransaction CreateCommand.create [] >> pure ()
      | mount -> runTransaction MountCommand.mount [] >> pure ()
      | manual -> runTransaction ManualCommand.manual [] >> pure ()
      | create && isJust remove -> bail "Cannot remove and create at the same time!"
      | otherwise -> bail "Invalid or no command! Please read --help"