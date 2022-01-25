{-# LANGUAGE TemplateHaskell #-}

module SystemUtils where

import Data.FileEmbed (embedFile)
import Data.Text (splitOn)
import Logger (logErrorLn)

dummyService :: Text
dummyService = decodeUtf8 $(embedFile "dummy.service")

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
