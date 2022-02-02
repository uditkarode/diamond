module Diamond where

import qualified Commands.Create as CreateCommand (create)
import qualified Commands.List as ListCommand (list)
import qualified Commands.Manual as ManualCommand (manual)
import qualified Commands.Mount as MountCommand (mount)
import SystemUtils (CliArgs (CliArgs), bail)
import Transaction (Command, Transaction (runTransaction))

runCommand :: Command -> IO ()
runCommand cmd = runTransaction cmd [] >> pure ()

diamond :: CliArgs -> IO ()
diamond (CliArgs create mount manual list remove) =
  if
      | create -> runCommand CreateCommand.create
      | mount -> runCommand MountCommand.mount
      | manual -> runCommand ManualCommand.manual
      | list -> runCommand ListCommand.list
      | otherwise -> bail "Invalid or no command! Please read --help"