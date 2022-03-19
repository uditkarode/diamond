module Diamond where

import qualified Commands.Create as CreateCommand (create)
import qualified Commands.List as ListCommand (list)
import qualified Commands.Manual as ManualCommand (manual)
import qualified Commands.Mount as MountCommand (mountAll)
import qualified Commands.Remove as RemoveCommand (remove)
import qualified Commands.Start as StartCommand (startAll)
import Relude.Unsafe (fromJust)
import SystemUtils (CliArgs (CliArgs), bail)
import Transaction (Command, Transaction (runTransaction))

runCommand :: Command -> IO ()
runCommand cmd = runTransaction cmd [] >> pure ()

diamond :: CliArgs -> IO ()
diamond (CliArgs create list mountAll manual startAll remove) =
  if
      | create -> runCommand CreateCommand.create
      | list -> runCommand ListCommand.list
      | mountAll -> runCommand MountCommand.mountAll
      | manual -> runCommand ManualCommand.manual
      | startAll -> runCommand StartCommand.startAll
      | isJust remove ->
        runCommand $
          RemoveCommand.remove (toText . fromJust $ remove)
      | otherwise -> bail "Invalid or no command! Please read --help"