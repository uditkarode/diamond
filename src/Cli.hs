module Cli where

import Data.Version (showVersion)
import Diamond (diamond)
import Options.Applicative as O
import Paths_diamond (version)
import qualified System.Console.Pretty as SP
import Utils (CliArgs (CliArgs))

cliArgs :: O.Parser CliArgs
cliArgs =
  CliArgs
    <$> switch
      ( long "create"
          <> short 'q'
          <> help "create a diamond application"
      )
      <*> optional
        ( strOption
            ( long "remove"
                <> short 'r'
                <> help "remove the specified diamond application"
            )
        )

main :: IO ()
main = diamond =<< execParser opts
  where
    opts =
      info
        (cliArgs <**> helper <**> infoOption ("Diamond v" <> (SP.style SP.Bold . SP.color SP.Yellow) (showVersion version)) (O.long "version" <> O.short 'v' <> O.help "Show version"))
        ( fullDesc
            <> progDesc "Easily create contained applications without containers"
            <> header "Diamond"
        )