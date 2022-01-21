module SystemUtils where

import Data.Text (splitOn)
import Utils (bail)

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
