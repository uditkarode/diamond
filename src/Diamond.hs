module Diamond where

import qualified Commands.Create as CreateCommand (create)
import Utils (CliArgs (CliArgs), bail)

diamond :: CliArgs -> IO ()
diamond (CliArgs create remove) =
  if
      | create -> evalStateT CreateCommand.create []
      | create && isJust remove -> bail "Cannot remove and create at the same time!"
      | otherwise -> bail "Invalid or no command! Please read --help"