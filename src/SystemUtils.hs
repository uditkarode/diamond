{-# LANGUAGE TemplateHaskell #-}

module SystemUtils where

import Control.Exception (try)
import Data.Aeson (FromJSON, decode, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (ToJSON (toJSON))
import Data.FileEmbed (embedFile)
import Data.Text (splitOn)
import GHC.IO.Exception (IOError, IOException (IOError))
import Logger (logErrorLn)
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import Transaction (Transaction (Transaction))

data CliArgs = CliArgs
  { create :: Bool,
    mount :: Bool,
    manual :: Bool,
    remove :: Maybe String
  }
  deriving (Show)

data DataEntry = DataEntry
  { name :: Text,
    prefix :: Text,
    diskImagePrefix :: Text
  }
  deriving (Generic, Show)

newtype Data = Data {entries :: [DataEntry]} deriving (Generic, Show)

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
writeData d = flip writeFileText (decodeUtf8 $ encodePretty d) =<< dataPath

writeDataTransac :: Data -> Transaction ()
writeDataTransac d = do
  v <- liftIO $ try (writeData d) :: Transaction (Either SomeException ())
  case v of
    Left e -> fail $ "Unable to read the data file because " <> displayException e
    Right v -> pure v

readData :: Transaction Data
readData = do
  txt <- liftIO $ try (readFileLBS =<< dataPath) :: Transaction (Either IOError LByteString)
  case txt of
    Left e -> fail $ "Unable to read the data file because " <> displayException e
    Right txt -> do
      let v = decode txt :: Maybe Data
      case v of
        Nothing -> fail "Could not decode the data file! Make sure the syntax is correct"
        Just v' -> pure v'

bail :: Text -> IO ()
bail msg = do
  logErrorLn msg
  exitFailure

userHomeDir :: Text -> Transaction Text
userHomeDir user = do
  passwd <- readFileText "/etc/passwd"
  usernames <- forM (lines passwd) $ \char -> do
    let username = maybeAt 0 (splitOn ":" char)
    let homeDir = maybeAt 5 (splitOn ":" char)
    case (username, homeDir) of
      (Just username, Just homeDir) -> pure (username, homeDir)
      _ -> do
        fail "Could not parse /etc/passwd!"
        pure ("type", "haggling")
  case find ((==) user . fst) usernames of
    Nothing -> fail "Daemon user does not exist" >> pure "type haggling"
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
