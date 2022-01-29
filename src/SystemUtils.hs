{-# LANGUAGE TemplateHaskell #-}

module SystemUtils where

import Data.Aeson (FromJSON, encode)
import Data.Aeson.Types (ToJSON (toJSON))
import Data.FileEmbed (embedFile)
import Data.Text (splitOn)
import Logger (logErrorLn)
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)

data CliArgs = CliArgs
  { create :: Bool,
    remove :: Maybe String
  }
  deriving (Show)

data DataEntry = DataEntry
  { name :: Text,
    prefix :: Text,
    diskImagePrefix :: Text
  }
  deriving (Generic, Show)

newtype Data = Data [DataEntry] deriving (Generic, Show)

instance FromJSON DataEntry

instance ToJSON DataEntry

instance FromJSON Data

instance ToJSON Data

configRoot :: IO FilePath
configRoot = getXdgDirectory XdgConfig "diamond"

configPath :: IO FilePath
configPath = configRoot <&> (<> "/diamond.conf")

dataPath :: IO FilePath
dataPath = configRoot <&> (<> "/data.json")

dummyService :: Text
dummyService = decodeUtf8 $(embedFile "dummy.service")

writeData :: Data -> IO ()
writeData d = flip writeFileText (decodeUtf8 $ encode d) =<< dataPath

bail :: Text -> IO ()
bail msg = do
  logErrorLn msg
  exitFailure

userHomeDir :: Text -> IO Text
userHomeDir user = do
  passwd <- readFileText "/etc/passwd"
  usernames <- forM (lines passwd) $ \char -> do
    let username = maybeAt 0 (splitOn ":" char)
    let homeDir = maybeAt 5 (splitOn ":" char)
    case (username, homeDir) of
      (Just username, Just homeDir) -> pure (username, homeDir)
      _ -> do
        bail "Could not parse /etc/passwd!"
        pure ("type", "haggling")
  case find ((==) user . fst) usernames of
    Nothing -> bail "Daemon user does not exist" >> pure "type haggling"
    Just v -> pure . snd $ v

doesUserExist :: Text -> IO Bool
doesUserExist user = do
  passwd <- readFileText "/etc/passwd"
  usernames <- forM (lines passwd) $ \char -> do
    let username = viaNonEmpty head (splitOn ":" char)
    case username of
      Nothing -> do
        bail "Could not parse /etc/passwd!"
        pure ("type haggling placeholder" :: Text)
      Just username -> pure username
  pure . isJust $ find (user ==) usernames
