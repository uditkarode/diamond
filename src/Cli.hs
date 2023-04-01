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
      ( long "list"
          <> help "list recorded entries"
      )
    <*> switch
      ( long "mount-all"
          <> help "mount all the existing diamond applications"
      )
    <*> switch
      ( long "manual"
          <> help "manually add an entry to data"
      )
    <*> switch
      ( long "start-all"
          <> help "systemctl start all existing diamond applications provided their disk images are mounted"
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
  home <- getEnv "HOME"

  when (euid /= 0 || home == "/root") $ bail $ "Please run this program with sudo -E!" <> "\neuid: " <> show euid <> "\nhome: " <> toText home

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
            <> progDesc "Easily host reasonably contained applications with sane limits"
            <> header "Diamond"
        )