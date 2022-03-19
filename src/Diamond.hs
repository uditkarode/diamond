module Diamond where

import qualified Commands.Create as CreateCommand (create)
import qualified Commands.List as ListCommand (list)
import qualified Commands.Manual as ManualCommand (manual)
import qualified Commands.Mount as MountCommand (mount)
import qualified Commands.Remove as RemoveCommand (remove)
import qualified Commands.Start as StartCommand (start)
import Relude.Unsafe (fromJust)
import SystemUtils (CliArgs (CliArgs), bail)
import Transaction (Command, Transaction (runTransaction))

runCommand :: Command -> IO ()
runCommand cmd = runTransaction cmd [] >> pure ()

diamond :: CliArgs -> IO ()
diamond (CliArgs create list mount manual start remove) =
  if
      | create -> runCommand CreateCommand.create
      | list -> runCommand ListCommand.list
      | mount -> runCommand MountCommand.mount
      | manual -> runCommand ManualCommand.manual
      | start -> runCommand StartCommand.start
      | isJust remove ->
        runCommand $
          RemoveCommand.remove (toText . fromJust $ remove)
      | otherwise -> bail "Invalid or no command! Please read --help"