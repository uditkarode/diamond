module Cli where

import Data.Version (showVersion)
import Diamond (diamond)
import Logger (logInfoLn)
import Options.Applicative as O
  ( Parser,
    customExecParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    infoOption,
    long,
    prefs,
    progDesc,
    short,
    showHelpOnEmpty,
    strOption,
    switch,
  )
import Paths_diamond (version)
import qualified System.Console.Pretty as SP
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getEnv)
import System.Posix (Handler (Catch), installHandler, keyboardSignal)
import System.Posix.User (getEffectiveUserID)
import SystemUtils (CliArgs (CliArgs), Data (Data), bail, configRoot, dataPath, writeData)

cliArgs :: O.Parser CliArgs
cliArgs =
  CliArgs
    <$> switch
      ( long "create"
          <> help "create a diamond application"
      )
      <*> switch
        ( long "mount"
            <> help "mount all the existing diamond applications"
        )
      <*> switch
        ( long "manual"
            <> help "manually add an entry to data"
        )
      <*> optional
        ( strOption
            ( long "remove"
                <> short 'r'
                <> help "remove the specified diamond application"
            )
        )

prepare :: CliArgs -> IO ()
prepare args = do
  euid <- getEffectiveUserID
  when (euid /= 0) $ bail "Please run this program with sudo!"

  installHandler keyboardSignal (Catch (logInfoLn "\nTo exit, respond 'exit' to any asked question.")) Nothing

  createDirectoryIfMissing True =<< configRoot

  fe <- doesFileExist =<< dataPath
  unless fe $ writeData $ Data []

  diamond args

main :: IO ()
main = prepare =<< customExecParser (prefs showHelpOnEmpty) opts
  where
    opts =
      info
        (cliArgs <**> helper <**> infoOption ("Diamond v" <> (SP.style SP.Bold . SP.color SP.Yellow) (showVersion version)) (O.long "version" <> O.short 'v' <> O.help "Show version"))
        ( fullDesc
            <> progDesc "Easily create contained applications without containers"
            <> header "Diamond"
        )